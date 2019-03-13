# Logical

Logical is a minimalistic logic programming inspired by [microKanren](http://minikanren.org/), which is
- Simple implementation with only a few building blocks
- Easy to understand and use
- Supports negation free constraint logic programming

## How does it work?
To understand Logical first you need to understands it's basic building blocks which are the following:
- Value
- State
- Goal

### Value
Logical is basically could be seen as a mini programming language or DLS, which allows us to use logic programming in Ocaml/ReasonML. Because it's a programming language it means that it has it own (value) type system:
```ocaml
type variable = string

type value = 
    | Int of int
    | Float of float
    | Str of string
    | Bool of bool
    | Var of variable
    | Set of value Base.Set.t
```
You can see from the simplified code that Logical has all the base types as Ocaml(`Int`, `Float`, `Str`, `Bool`), but also has `Var` for variable declarations and `Set` for declaring sets in Logical.

### State
State represents the current state of your program in Logical by storing the value of every used variable.
It's kind of the same as stack frames in other languages.

State's signature:
```ocaml
type state = (variable * value) list
```
You can clearly see this in state's type decleration, where `variable` is a variable name and `value` is it's value. You can also see that we are storing multiple variables, because the assignments are stored in a list.

### Goal
Goal is basically a function, which takes in a state and generates a stream of new states based on that state. The only twist is that sometimes we end up in invalid state(or deadend state), which is why sometimes we can't produce new state based on the input state. 

You can see this in Goal's signature:
```ocaml
type goal = state -> state option Base.Sequence.t
```

Logical supports the following goals:
```ocaml
val equal : value -> value -> goal (* A = B *)

val either : goal -> goal -> goal (* A or B*)

val either_multi : goal list -> goal (* or [A,B,C,..,Y]*)

val both : goal -> goal -> goal (* A and B *)

val in_set : value -> value -> goal (* A in (A,B,C,...,Y) *)
```
You can see from the type declarations that there are two kinds of goals:
- Basic building block, which expects 2 `value` and generates a `goal`. You can think of them as constructors for goals.
- Goal combinators, which expects 2 `goals` and generates based on that a one new `goal`

## How to use it?
General rules for using Logical:
- Goal constructors are the most basic building block of every goal
- Goals can be made bigger and more complex by combining them with goal combinators
- Goals are inactive until you give them an initial state

### equal
```ocaml
let equal_goal = Goal.equal (Value.var "a") (Value.int 42)
let state_list = equal_goal State.empty |> Base.Sequence.to_list
(* state_list is [ [("a",Value.Int 42)] ]*)
```
In this case `state_list` only has one state were `a` is equal with `42`.

### either and either_multi
```ocaml
let a_goal = Goal.equal (Value.var "a") (Value.int 42)
let b_goal = Goal.equal (Value.var "b") (Value.int 21)
let either_goal = Goal.either a_goal b_goal
let state_list = either_goal State.empty |> Base.Sequence.to_list
(* state_list is [ [("a",Value.Int 42)]; [("b",Value.Int 21)] ]*)
```
In this case `state_list` has two states:
- state where `a` is equal with `42`
- state where `b` is equal with `21`

`either_multi` is the same as `either` only more general, because it expects a list of goals.
```ocaml
let a_goal = Goal.equal (Value.var "a") (Value.int 42)
let b_goal = Goal.equal (Value.var "b") (Value.int 21)
let goal_list = [a_goal; b_goal]
let either_goal = Goal.either_multi goal_list
let state_list = either_goal State.empty |> Base.Sequence.to_list
(* state_list is [ [("a",Value.Int 42)]; [("b",Value.Int 21)] ]*)
```

### both
```ocaml
let a_goal = Goal.equal (Value.var "a") (Value.int 42)
let b_goal = Goal.equal (Value.var "b") (Value.int 21)
let both_goal = Goal.both a_goal b_goal
let state_list = both_goal State.empty |> Base.Sequence.to_list
(* state_list is [ [("a",Value.Int 42); ("b",Value.Int 21)] ]*)
```
In this case `state_list` has a state with two assignments:
- in the first assignment `a` is equal with `42`
- in the second assignment `b` is equal with `21`

### in_set
in_set goal is basically a sintactic sugar for an `either_multi` where every goal has the same variable.
```ocaml
let my_set = Base.Set.of_list (module Value.Comparator) [Value.int 42; Value.int 21]
let in_set_gaol = Goal.in_set (Value.var "a") (Value.set my_set)
let state_list = in_set_gaol State.empty |> Base.Sequence.to_list
(* state_list is [ [("a",Value.Int 42)]; [("a",Value.Int 21)] ]*)
```
In this case `state_list` has two states with the same variable(`a`) with two different values: `42` and `21`

`in_set` goal is useful, when you want negation like `x != 6`, which is basically the same as `x in (-infinity,..,5,7,...,infinity)`. From this example you can also see that in_set is only really useful on small finite domains, where the universal set is small and well defined.

### Note
If you like reading code more than guides than you can find the example in the `bin` folder in this repository.

## How to run the example?
Prerequisite: Install [Esy](https://esy.sh/)

Run it: `esy x example`

## How to for contributors
Prerequisite: Install [Esy](https://esy.sh/)

Build it: `esy`

Test it: `esy test`

Run repl: `esy utop`

Generate Doc: `esy doc`

Update docs folder: `esy update-doc`
