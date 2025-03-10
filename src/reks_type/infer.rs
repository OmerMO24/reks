use std::collections::{HashMap, HashSet};

// Assuming these are defined in your existing code
use crate::reks_parse::operators::InfixOpKind; // Adjust path as needed
use crate::reks_parse::utnode::{Const, Param, TypePath, UntypedExpr, Value}; // Adjust path
use crate::reks_type::resolve::{NameResolutionMap, NodeId}; // Adjust path

// Typed AST Node
#[derive(Debug, Clone)]
pub struct TypedExpr<'src> {
    pub kind: UntypedExpr<'src>,
    pub type_info: TypeInfo,
    pub node_id: NodeId,
}

// Type Information
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Known(Type),
    Unknown,
    Inferred(Type),
}

// Core Type Representation
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    TypeVar(usize),
    Struct(String, Vec<(String, Type)>),
    Function(Vec<Type>, Box<Type>),
    List(Box<Type>),
    Unit,
}

// Type Inference Errors
#[derive(Debug)]
pub enum TypeError {
    UnificationFail(Type, Type),
    InfiniteType(usize, Type),
    UnknownVariable(String),
    NotAFunction(Type),
    InvalidTypeAnnotation(String),
}

// Type Environment and Substitution
type TypeEnv = HashMap<NodeId, Type>;
type Substitution = HashMap<usize, Type>;

impl Type {
    fn occurs_in(&self, var: usize) -> bool {
        match self {
            Type::TypeVar(v) => *v == var,
            Type::Function(params, ret) => {
                params.iter().any(|p| p.occurs_in(var)) || ret.occurs_in(var)
            }
            Type::Struct(_, fields) => fields.iter().any(|(_, t)| t.occurs_in(var)),
            Type::List(elem_ty) => elem_ty.occurs_in(var),
            _ => false,
        }
    }

    fn apply_subst(&self, subst: &Substitution) -> Type {
        match self {
            Type::TypeVar(v) => subst.get(v).cloned().unwrap_or_else(|| self.clone()),
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| p.apply_subst(subst)).collect(),
                Box::new(ret.apply_subst(subst)),
            ),
            Type::Struct(name, fields) => Type::Struct(
                name.clone(),
                fields
                    .iter()
                    .map(|(f_name, f_type)| (f_name.clone(), f_type.apply_subst(subst)))
                    .collect(),
            ), // Updated: Apply to field types
            Type::List(elem_ty) => Type::List(Box::new(elem_ty.apply_subst(subst))),
            _ => self.clone(),
        }
    }
}

fn unify(t1: &Type, t2: &Type, subst: &Substitution) -> Result<Substitution, TypeError> {
    let t1 = t1.apply_subst(subst);
    let t2 = t2.apply_subst(subst);

    match (&t1, &t2) {
        (Type::Int, Type::Int)
        | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool)
        | (Type::String, Type::String)
        | (Type::Unit, Type::Unit) => Ok(subst.clone()),
        (Type::Function(p1, r1), Type::Function(p2, r2)) if p1.len() == p2.len() => {
            let mut new_subst = subst.clone();
            for (p1, p2) in p1.iter().zip(p2.iter()) {
                new_subst = unify(p1, p2, &new_subst)?;
            }
            unify(r1, r2, &new_subst)
        }
        (Type::TypeVar(v), t) | (t, Type::TypeVar(v)) => {
            if t.occurs_in(*v) {
                return Err(TypeError::InfiniteType(*v, t.clone()));
            }
            let mut new_subst = subst.clone();
            new_subst.insert(*v, t.clone());
            Ok(new_subst)
        }
        (Type::Struct(n1, f1), Type::Struct(n2, f2)) if n1 == n2 && f1.len() == f2.len() => {
            let mut new_subst = subst.clone();
            for ((f1_name, f1_type), (f2_name, f2_type)) in f1.iter().zip(f2.iter()) {
                if f1_name != f2_name {
                    return Err(TypeError::UnificationFail(t1.clone(), t2.clone()));
                }
                new_subst = unify(f1_type, f2_type, &new_subst)?;
            }
            Ok(new_subst)
        }
        (Type::List(t1_elem), Type::List(t2_elem)) => unify(t1_elem, t2_elem, subst),
        _ => Err(TypeError::UnificationFail(t1.clone(), t2.clone())),
    }
}

pub struct TypeInferencer {
    pub env: TypeEnv,
    pub subst: Substitution,
    pub next_var_id: usize,
    pub resolution_map: NameResolutionMap,
    pub struct_names: HashSet<String>,
    pub struct_fields: HashMap<String, Vec<(String, Type)>>,
}

impl TypeInferencer {
    pub fn new(resolution_map: NameResolutionMap) -> Self {
        Self {
            env: HashMap::new(),
            subst: HashMap::new(),
            next_var_id: 0,
            resolution_map,
            struct_names: HashSet::new(),
            struct_fields: HashMap::new(),
        }
    }

    fn fresh_type_var(&mut self) -> Type {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Type::TypeVar(id)
    }

    pub fn convert_to_typed_ast<'src>(&mut self, expr: &UntypedExpr<'src>) -> TypedExpr<'src> {
        let node_id = self.resolution_map.get_id();
        let type_info = match expr {
            UntypedExpr::Value(Value::Num(_)) => TypeInfo::Known(Type::Int),
            UntypedExpr::Value(Value::Float(_)) => TypeInfo::Known(Type::Float),
            UntypedExpr::Value(Value::String(_)) => TypeInfo::Known(Type::String),
            _ => TypeInfo::Unknown,
        };
        TypedExpr {
            kind: expr.clone(),
            type_info,
            node_id,
        }
    }

    fn infer_expr(&mut self, expr: &mut TypedExpr) -> Result<Type, TypeError> {
        if let TypeInfo::Known(t) | TypeInfo::Inferred(t) = &expr.type_info {
            return Ok(t.clone());
        }

        let inferred_type = match &mut expr.kind {
            UntypedExpr::Value(Value::Num(_)) => Type::Int,
            UntypedExpr::Value(Value::Float(_)) => Type::Float,
            UntypedExpr::Value(Value::String(_)) => Type::String,
            UntypedExpr::Value(Value::Identifier(name)) => {
                if let Some(decl_id) = self.resolution_map.resolve_name(name) {
                    self.env
                        .get(&decl_id)
                        .cloned()
                        .ok_or_else(|| TypeError::UnknownVariable(name.to_string()))?
                } else {
                    return Err(TypeError::UnknownVariable(name.to_string()));
                }
            }
            UntypedExpr::Let {
                id: Value::Identifier(name),
                pat,
                expr: init_expr,
                ..
            } => {
                let mut init_typed = self.convert_to_typed_ast(init_expr);
                let init_type = self.infer_expr(&mut init_typed)?;
                *init_expr = Box::new(init_typed.kind);

                let var_type = match pat {
                    TypePath::Typed {
                        ident: Value::Identifier(ty_name),
                    } => {
                        let ann_type = self.type_from_annotation(ty_name)?;
                        self.subst = unify(&init_type, &ann_type, &self.subst)?;
                        ann_type
                    }
                    _ => init_type,
                };

                if let Some(decl_id) = self.resolution_map.get_declaration_id(name) {
                    self.env.insert(decl_id, var_type.clone());
                }
                Type::Unit
            }
            UntypedExpr::BinOp { left, op, right } => {
                let mut left_typed = self.convert_to_typed_ast(left);
                let mut right_typed = self.convert_to_typed_ast(right);
                let t_left = self.infer_expr(&mut left_typed)?;
                let t_right = self.infer_expr(&mut right_typed)?;
                *left = Box::new(left_typed.kind);
                *right = Box::new(right_typed.kind);

                match op {
                    InfixOpKind::Add | InfixOpKind::Sub | InfixOpKind::Mul | InfixOpKind::Div => {
                        self.subst = unify(&t_left, &t_right, &self.subst)?;
                        self.subst = unify(&t_left, &Type::Int, &self.subst)?; // Assuming Int for simplicity
                        Type::Int
                    }
                    InfixOpKind::Equals | InfixOpKind::NotEq => {
                        self.subst = unify(&t_left, &t_right, &self.subst)?;
                        Type::Bool
                    }
                    _ => unimplemented!("Operator {:?}", op),
                }
            }
            UntypedExpr::Call { name, args } => {
                let mut name_typed = self.convert_to_typed_ast(name);
                let func_type = self.infer_expr(&mut name_typed)?;
                *name = Box::new(name_typed.kind);

                let mut arg_types = Vec::new();
                for arg in args.iter_mut() {
                    let mut arg_typed = self.convert_to_typed_ast(arg);
                    arg_types.push(self.infer_expr(&mut arg_typed)?);
                    *arg = arg_typed.kind;
                }

                let ret_type = self.fresh_type_var();
                let expected_type = Type::Function(arg_types, Box::new(ret_type.clone()));
                self.subst = unify(&func_type, &expected_type, &self.subst)?;
                ret_type.apply_subst(&self.subst)
            }
            UntypedExpr::Block { statements } => {
                let mut block_type = Type::Unit;
                for stmt in statements.iter_mut() {
                    let mut stmt_typed = self.convert_to_typed_ast(stmt);
                    block_type = self.infer_expr(&mut stmt_typed)?;
                    *stmt = stmt_typed.kind;
                }
                block_type
            }
            UntypedExpr::Fn {
                name,
                params,
                retty,
                body,
            } => {
                // Treat retty as a type annotation, not an expression to infer
                let ret_type = match retty.as_ref() {
                    UntypedExpr::Value(Value::Identifier(ty_name)) => {
                        self.type_from_annotation(ty_name)?
                    }
                    _ => {
                        return Err(TypeError::InvalidTypeAnnotation(
                            "Invalid return type".to_string(),
                        ))
                    }
                };

                let mut param_types = Vec::new();
                for param in params {
                    if let (Value::Identifier(p_name), Value::Identifier(ty_name)) =
                        (&param.name, &param.ty)
                    {
                        let p_type = self.type_from_annotation(ty_name)?;
                        param_types.push(p_type.clone());
                        if let Some(decl_id) = self.resolution_map.get_declaration_id(p_name) {
                            self.env.insert(decl_id, p_type);
                        }
                    }
                }

                let mut body_typed = self.convert_to_typed_ast(body);
                let body_type = self.infer_expr(&mut body_typed)?;
                *body = Box::new(body_typed.kind);

                self.subst = unify(&body_type, &ret_type, &self.subst)?;
                let fn_type = Type::Function(param_types, Box::new(ret_type));

                if let Value::Identifier(fn_name) = name {
                    if let Some(decl_id) = self.resolution_map.get_declaration_id(fn_name) {
                        self.env.insert(decl_id, fn_type.clone());
                    }
                }
                fn_type
            }
            UntypedExpr::Struct {
                id: Value::Identifier(name),
                fields,
            } => {
                let mut field_types = Vec::new();
                for param in fields {
                    if let (Value::Identifier(f_name), Value::Identifier(ty_name)) =
                        (&param.name, &param.ty)
                    {
                        let f_type = self.type_from_annotation(ty_name)?;
                        field_types.push((f_name.to_string(), f_type));
                    }
                }
                self.struct_names.insert(name.to_string());
                self.struct_fields
                    .insert(name.to_string(), field_types.clone()); // Store fields
                let struct_type = Type::Struct(name.to_string(), field_types);
                if let Some(decl_id) = self.resolution_map.get_declaration_id(name) {
                    self.env.insert(decl_id, struct_type.clone());
                }
                Type::Unit
            }
            UntypedExpr::FieldAccess { id, field } => {
                let mut expr_typed = self.convert_to_typed_ast(id);
                let expr_type = self.infer_expr(&mut expr_typed)?;
                *id = Box::new(expr_typed.kind);

                match field {
                    Value::Identifier(field_name) => {
                        match expr_type {
                            Type::Struct(struct_name, fields) => {
                                for (f_name, f_type) in &fields {
                                    if f_name == *field_name {
                                        return Ok(f_type.clone());
                                    }
                                }
                                Err(TypeError::InvalidTypeAnnotation(format!(
                                    "Field {} not found in struct {}",
                                    field_name, struct_name
                                )))
                            }
                            _ => Err(TypeError::UnificationFail(
                                expr_type,
                                Type::Struct("unknown".to_string(), vec![]), // Dummy struct type for error
                            )),
                        }
                    }
                    _ => Err(TypeError::InvalidTypeAnnotation(
                        "Field must be an identifier".to_string(),
                    )),
                }?
            }
            UntypedExpr::List { items } => {
                if items.is_empty() {
                    // Empty list: polymorphic type List(a)
                    Type::List(Box::new(self.fresh_type_var()))
                } else {
                    // Non-empty list: unify all elements to the same type
                    let mut elem_type = None;
                    for elem in items.iter_mut() {
                        let mut elem_typed = self.convert_to_typed_ast(elem);
                        let t = self.infer_expr(&mut elem_typed)?;
                        *elem = elem_typed.kind;
                        match elem_type {
                            None => elem_type = Some(t),
                            Some(ref prev_t) => {
                                self.subst = unify(prev_t, &t, &self.subst)?;
                            }
                        }
                    }
                    Type::List(Box::new(elem_type.unwrap())) // Safe since non-empty
                }
            }
            UntypedExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Infer condition type and unify with Bool
                let mut cond_typed = self.convert_to_typed_ast(condition);
                let cond_type = self.infer_expr(&mut cond_typed)?;
                *condition = Box::new(cond_typed.kind);
                self.subst = unify(&cond_type, &Type::Bool, &self.subst)?;

                // Infer then and else branch types and unify them
                let mut then_typed = self.convert_to_typed_ast(then_branch);
                let then_type = self.infer_expr(&mut then_typed)?;
                *then_branch = Box::new(then_typed.kind);

                let mut else_typed = self.convert_to_typed_ast(else_branch);
                let else_type = self.infer_expr(&mut else_typed)?;
                *else_branch = Box::new(else_typed.kind);

                self.subst = unify(&then_type, &else_type, &self.subst)?;
                then_type // Could use else_type too, since they're unified
            }
            _ => self.fresh_type_var(),
        };

        expr.type_info = TypeInfo::Inferred(inferred_type.clone());
        Ok(inferred_type)
    }

    fn type_from_annotation(&self, type_name: &str) -> Result<Type, TypeError> {
        match type_name {
            "i8" | "i32" | "i64" => Ok(Type::Int),
            "f64" => Ok(Type::Float),
            "bool" => Ok(Type::Bool),
            "string" => Ok(Type::String),
            name if self.struct_names.contains(name) => {
                let fields = self.struct_fields.get(name).cloned().unwrap_or(vec![]); // Should always exist due to struct_names
                Ok(Type::Struct(name.to_string(), fields))
            }
            _ => Err(TypeError::InvalidTypeAnnotation(type_name.to_string())),
        }
    }

    pub fn infer_program(&mut self, program: &mut [TypedExpr]) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();
        for expr in program.iter_mut() {
            if let Err(e) = self.infer_expr(expr) {
                errors.push(e);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn convert_program<'src>(&mut self, program: &[UntypedExpr<'src>]) -> Vec<TypedExpr<'src>> {
        program
            .iter()
            .map(|expr| self.convert_to_typed_ast(expr))
            .collect()
    }
}
