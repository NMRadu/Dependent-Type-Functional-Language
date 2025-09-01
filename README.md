# AFP Project: Dependently Typed Language

This project implements a dependently typed lambda calculus interpreter as part of the TU Delft Advanced Functional Programming course. Inspired by Andrej Bauer’s minimal core design [1], it avoids built-in primitives — all constructs such as `Bool`, `Nat`, and `Vec` are defined in the standard library using user-defined inductive types.

## Project Structure

- `app/`: Entry point of the program (`Main.hs`)
- `grammar/`: Auto-generated parser and lexer using BNFC
- `src/`: Source code for parsing, desugaring, type checking, normalization, and evaluation
- `libraries/`: Built-in inductive types (e.g., `Bool`, `Nat`, `Either`, `Vec`)
- `examples/`: Sample programs written in the language
- `test/`: Unit tests

## Building, Running, and Testing

You will need to have Cabal installed for this project.

### Setup

Once done, use cabal to install Alex, Happy, and BNFC:

```
cabal update
cabal install alex happy BNFC --overwrite-policy=always
```

*TIP:* Install the [LBNF](https://marketplace.visualstudio.com/items?itemName=agurodriguez.vscode-lbnf) extension for VS Code for syntax highlighting on `.cf` files.

### Building
Please do not rebuild the grammar as it will lead the program to not compile. I have added special extensions to the Exp and Stmt that are not in lang.cf file so rebuilding it will lead to the project not compiling 

To build the project use:

```
cabal build
```

### Running

To run the interpreter on a program in a file, pass the path to the file as a parameter to cabal, e.g.:

```
cabal run exes -- test/Rubric/Bool/elimFalse.afp
```

### Testing

Then, simply run

```
cabal test
```

### Makefile

If you have `make` available, you can use the following commands to interact with the project:

```bash
make install            # installing Alex, Happy, and BNFC via Cabal

make build              # building the project
make run                # running the interactive interpreter
make run FILE="<path>"  # running the interpreter on a program file
                        # e.g. make run FILE="examples/sample.afp"

make test               # running the test suite

make clean              # cleans all generated files and cabal
```

## Language Specification

This language is a minimal dependently typed lambda calculus with inductive types, Pi types, and native dependent pattern matching. The type checker integrates normalization and supports match-based reasoning.

### Program Structure

A program consists of a list of top-level statements followed by a single final expression:

```
<stmt_1>;
<stmt_2>;
...
<stmt_n>;

<final_expression>
```

### Top-Level Statements

- `data ... where ... end;` — Inductive type declaration
- `bind x : A;` — Declare the type of variable `x`
- `let x = e;` — Define `x` with expression `e`
- `bind f : (x : A) -> B; let f x = e;` — Dependent function definition

All declarations are added to the typing context before evaluating the final expression.

### Core Expressions

These expression forms are supported:

- `U 0`, `U 1`, ... — Type universes
- `x` — Variable reference
- `zero`, `suc n` — Natural numbers (via `Nat` in the library)
- `true`, `false` — Booleans
- `e1 e2` — Function application
- `\x -> e` — Lambda abstraction
- `(x : A) -> B` — Dependent function type (Pi)
- `A -> B` — Shorthand non-dependent Pi type
- `case e of ... end` — Native dependent pattern matching

### Pattern Matching

Pattern matching is a first-class construct in the language. It supports matching on user-defined inductive types using `case ... of` expressions. Matching is dependent, meaning the return type can vary by constructor.

Example:

```haskell
let not b = case b of
  true => false,
  false => true
end;
```

This works with any inductive type declared by the user (e.g., `Bool`, `Either`, `Vec`) and is type-checked using a recursive branch analysis. Pattern variables are bound and scoped correctly.

### Types and Universes

Types are checked using a simple universe hierarchy:

- `U 0` : the base universe
- `U 1`, `U 2`, etc., if needed

The system supports:

- Dependent function types
- Indexed types (e.g., `Vec A n`)
- Type families via Pi types

### Context Representation

Internally, every bound variable tracks:

- its type
- its (optional) value
- its user name (for pretty-printing)
- optional domain (used in dependent pattern matching)

This allows accurate type checking and readable error messages.

### Evaluation

The system performs evaluation by:

- Normalizing expressions via substitution and β-reduction
- Resolving match branches
- Respecting de Bruijn indices internally and restoring names for output

Programs are evaluated to normal form unless a type error occurs.

## Example

Below is a full example using the `Bool` type and native dependent pattern matching:

```haskell
data Bool : U 0 where
  true : Bool,
  false : Bool
end;

bind not : Bool -> Bool;
let not b = case b of
  true => false,
  false => true
end;

not true
```

This program returns `false`.

## Standard Library

The language comes with a prelude of user-defined inductive types and functions located in the `libraries/` folder. These are automatically loaded at startup and provide foundational types and utilities.

### Included Modules

- **Bool.lib**  
  Declares the `Bool` type with constructors `true` and `false`.  
  Includes pattern-matching and elimination helpers.

- **Nat.lib**  
  Defines the natural numbers using `zero` and `suc`, including recursive eliminators.  
  Includes addition, multiplication, and comparison functions.

- **Either.lib**  
  A sum type `Either A B` with constructors `left` and `right`.  
  Provides a dependent eliminator `elimEither`.

- **Vec.lib**  
  Defines length-indexed lists: `Vec A n`, with `nil` and `cons`.  
  Includes recursive functions like `map`, `append`, and length-preserving transformations.

- **Fin.lib**  
  Provides finite sets of size `n` with constructors for zero and successor.  
  Useful for safe indexing into `Vec`.

- **Pair.lib**  
  Defines pairs with `pair`, `fst`, and `snd` projection functions.

- **Id.lib**  
  Equality type: `Id A a b` represents the type-level assertion that `a = b`.  
  Includes reflexivity and transport-like operations.

- **TnB.lib**  
  An example type for True-n-Boolean functions. Can be used for testing eliminator generality.

These modules demonstrate the power of dependent typing: all types, eliminators, and functions are written in the language itself — no primitives are hardcoded in the interpreter.

Each module is type-checked at runtime and merged into the global context before evaluating user code.

These are automatically loaded at runtime before interpreting user programs.

## Developer Notes

This section documents the internal structure and design rationale of the codebase.

### Module Structure

The project follows a modular architecture:

- `Main.hs`: The entry point that loads libraries, reads input, and runs the evaluator.
- `App.hs`: Defines the `AppM` monad used across the pipeline for environment passing and IO.
- `Logger.hs`: Concurrency-safe debug logging infrastructure.
- `IR/*.hs`: Desugaring passes and intermediate representation transformations:
  - `NatDesugar`, `LambdaDesugar`, `FunctionDesugar`, `InductiveDesugar`, etc.
- `TypeCheck/*.hs`: Modules for context handling, type inference, and normalization.
- `Lang/Abs.hs`, `Lang/Par.hs`: Automatically generated syntax tree and parser from `Lang.cf`.

Each stage of the pipeline (parsing → desugaring → type checking → evaluation) is handled in a separate module, aiding readability and isolation of responsibilities.

---

### Monads and Effects

The custom `AppM` monad is defined in `App.hs` as a wrapper over a `ReaderT` environment combined with `IO`. It is used to:

- Access configuration (e.g., debug logger)
- Perform logging and error handling
- Carry effects during parsing, transformation, and type checking

This keeps side effects explicit and modular. Logging uses `liftIO` internally and can be enabled or disabled through the `AppEnv`.

---

### Data Types

Key data types include:

- **Expressions (`Exp`)** in `Lang.Abs.hs`: the core language tree including Pi types, lambdas, match, application, and universes.
- **Statements (`Stmt`)**: top-level declarations such as `bind`, `let`, `data`, etc.
- **Program**: a list of statements plus a final expression.
- **Context**: a list of `(type, value, name, domain)` used for type checking and pretty-printing.
- **Branches, Identifiers, Const**: for pattern matching and inductive declarations.

All data types are recursive and designed to support dependent typing semantics and normalization.

---

### Functions

Key functions and their roles:

- `run` (in `Run.hs`): orchestrates loading, parsing, desugaring, type checking, and final evaluation.
- `eval`, `generateProgram` (in `Main.hs` and `IrGen.hs`): handle input and invoke transformation pipeline.
- `normalize`, `checkType`, `inferType`, `inferBranch` (in `Expr.hs`): form the heart of type inference and normalization logic.
- Desugar passes like `desugarNat`, `desugarLambda` (in `IR/*Desugar.hs`) handle transformation of user syntax into the core IR.
- Pattern-matching logic is implemented in `inferBranch` and `normalizeBranch`, supporting dependent elimination.

These functions are all documented via type signatures and structured to separate core logic from IO.

## Reference

[1] Andrej Bauer — *How to implement dependent type theory (I)*  
https://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/

