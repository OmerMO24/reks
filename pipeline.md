Source -> AST -> Untyped IR -> Type Checking -> Typed IR -> Compile Time evaluation -> Exectuable binary 


const x: i32 = 45;
const y = 5;


IntLiteral {value: 45, width: i32},
IntLiteral {value: 5, width: ?????},

fn add(x: i32, y: i32) -> i32 {
  return x + y;
}

              |
              |
              |
              v

name: add
%0 = arg1(x)
%1 = arg2(y)
%2 = add(%0, %1)
%3 = %2
%4 = return(%3)

