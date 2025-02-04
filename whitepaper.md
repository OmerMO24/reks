# The (still haven't decided on a name) programming language

# Project Structure 

- The language will be implemented using rust. 
- It will be statically typed, with the ability for the compiler to infer types. 
- For the backend, I have outlined three options: LLVM, CraneLift, and my own optimizing compiler (highly unlikely).
- If I decide to use LLVM, the backend will be written in C++ but link to rust via ffi. 

- Frontend:
  -
  - Lexing: Done with the logos crate. Logos transforms a lexer defintiion to a jump-table driven state machine. 
  - Parsing: I can use nom, which is a parser combinator (which would nullify the need for a dedicated lexer), or I can use my own recursive descent parser that I've written (uses pratt parsing for expressions).
  - Lower the AST into some form of IR where type inference can be performed.
  - After type inference the compiler performs type checking to verify the correctness of the program. 
  - I can then either convert it to another IR and perform optimizations on that or, I can just perform the optimizations on the existing IR. 
  - Once all the type checking is done, we can start to perform compile time evaluation of values by essentially having an "Interpreter within the compiler" (see zig)
  - After we evaluate all the values that can be evaluated at compile time, the IR is mapped to LLVM and the LLVM is passed to the backend to be optimized further. (If I decide to use LLVM)


- Backend:
    -
    - I'll use either a LLVM or cranelift backend to create the resulting binary. 
    - As a side note, LLVM documentation and projects are plentiful in comparison to cranelift, I stil have not decided what backend I want to use. 


- The focus of the project 
  -
- Compile Time Code Execution (Metaprogramming)
  -
  - For many languages, the only way to achieve compile time code execution is via macros, and existing macro systems in rust and C are a bit of a pain to use. 
  - "Macros are amazing to use, but are horrible to write". I want to change this. 
  - I want compile-time code to be able to be written the same way normal runtime code is written:

      // calculated at runtime 
      const x: int = 50 * 5;

      // comptime keyword denotes compile time evaluation
      const x: comptime int = 50 * 5;
   
  - constexpr in C++ and comptime in zig are my main inspirations. 
    - this is how zig does compile time code execution: https://mitchellh.com/zig/sema#what-does-air-look-like

-Type System
  -
  - With the type system, i'd rather a user create their own type based on the primtives of the language than use the primitives themselves. This can eliminate an entire class of runtime errors and make code easier to read. 
    - https://www.gtf.io/musings/why-haskell
  - I'm not sure how I'd implement this, but I have a book on hand that goes through the practical implementation of type systems. (Types and Programming Languages, Benjamin C. Pierce | 
    Crafting Interpreters also goes through typing)
  - Ideally, I also would like to implement monads in the form of Option types.
  - This would also mean having to implement generic data types. 

- This is just what I have in mind for now, a lot of this is non-trivial and will take time to A. learn how its implemented, B. to actually implement CORRECTLY. 
- What is more likely to happen is that I focus on either the type system or compile time code execution, not both. 
- With compile time code execution, I might limit what can be evaluated at compile time. The domain of what can be evaluated at compile time is already limited, so i'd focus on basic expressions, for loops, while loops, etc.).
- Type systems are heavily researched, and are just as much plentiful as they are varied in their implementations. 
- I'm not sure if this is enough for the project to be "novel" however. 


- Apologies for any spelling mistakes.

Sincereley,
Omer. 


  

  
    

  
  
