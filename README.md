# Logical

Logical is a minimalistic logic programming inspired by [microKanren](http://minikanren.org/), which is
- Simple implementation with only a few building blocks
- Easy to understand and use
- Supports negation free constraint logic programming

## How does it work?
To understand Logical first you need to understands it's basic building blocks which are the following:
- Type
- Value
- State
- Goal

### Type
Logical is basically could be seen as a embedded programming language(or DSL), which allows us to use logic programming in Ocaml. It also means that it has it own type system:
```ocaml
type variable_name = string

type t = 
    | Int of int
    | Float of float
    | Str of string
    | Bool of bool
    | Var of variable_name
```
You can see from the type declaration Logical has all the base types as Ocaml(`Int`, `Float`, `Str`, `Bool`), but also has `Var` for variable declarations.

### Value
The value module is responsible for making it easier to create value's based on Logical's type system and has the following constructor functions:
```ocaml
val int value : int -> int Type.t

val float value : float -> float Type.t

val str value = : string -> string Type.t

val bool value = : bool -> bool Type.t

val var variable_name : variable_name -> variable_name Type.t
```

### State
State represents the current state of your program in Logical. State is a map, which stores the value of every currently used variable by using a `variable name -> variable value` association.
It's kind of the same as stack frames in other languages.

State's signature:
```ocaml
type state = (variable_name, value) Base.Map.t
```
You can clearly see this in state's type declaration, where `variable_name` is the key of the Map and the `value` is the associated value. You can think of state as a list of variable assignments.

State module's most relevant function are the following:
```ocaml 
(* represents an empty state *)
val empty : state 

(* creates a new state based on the input association list, which gives back None for duplicated variable names *)
val create : (variable_name * t) list -> state option

(* creates a new state based on the input association list, which gives back an exception for duplicated variable names *)
val create_exn : (variable_name * t) list -> state

(* gives back the requested logical value based on the provided state *)
val value_of : state -> t -> t
```

### Goal
Goal is a function, which takes in a state and generates a stream of new states based on that state. The only twist is that sometimes we end up with invalid state(or deadend state), which is why sometimes we can't produce new state based on the input state. 

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

val both_multi : goal list -> goal (* and [A,B,C,..,Y]*)

val in_set : value -> value -> goal (* A in (A,B,C,...,Y) *)
```
You can see from the type declarations that there are two kinds of goals:
- Basic building block, which expects 2 `value` and generates a `goal`. You can think of them as constructors for goals.
- Goal combinators, which expects 2 `goals` and generates new goals based on that.

## How to use it?
General rules for using Logical:
- Goal constructors are the most basic building block of every goal
- Goals can be made bigger and more complex by combining them with goal combinators
- Goals can be grouped by creating custom goals
- Goals are inactive until you give them an initial state

### equal
```ocaml
let equal_goal = Goal.equal (Value.var "a") (Value.int 42)
let state_list = equal_goal State.empty |> Base.Sequence.to_list
(* state_list is [ Some[("a",Value.Int 42)] ]*)
```
In this case `state_list` only has one state were `a` is equal with `42`.

### either and either_multi
```ocaml
let a_goal = Goal.equal (Value.var "a") (Value.int 42)
let b_goal = Goal.equal (Value.var "b") (Value.int 21)
let either_goal = Goal.either a_goal b_goal
let state_list = either_goal State.empty |> Base.Sequence.to_list
(* state_list is [ Some[("a",Value.Int 42)]; Some[("b",Value.Int 21)] ]*)
```
In this case `state_list` has two states where:
- `a` is equal with `42`
- `b` is equal with `21`

`either_multi` is the same as `either` only more general, because it expects a list of goals.
```ocaml
let a_goal = Goal.equal (Value.var "a") (Value.int 42)
let b_goal = Goal.equal (Value.var "b") (Value.int 21)
let goal_list = [a_goal; b_goal]
let either_multi_goal = Goal.either_multi goal_list
let state_list = either_multi_goal State.empty |> Base.Sequence.to_list
(* state_list is [ Some[("a",Value.Int 42)]; Some[("b",Value.Int 21)] ]*)
```

### both and both_multi
```ocaml
let a_goal = Goal.equal (Value.var "a") (Value.int 42)
let b_goal = Goal.equal (Value.var "b") (Value.int 21)
let both_goal = Goal.both a_goal b_goal
let state_list = both_goal State.empty |> Base.Sequence.to_list
(* state_list is [ Some[("b",Value.Int 21); ("a",Value.Int 42)] ]*)
```
In this case `state_list` has a state with two assignments where:
- `a` is equal with `42`
- `b` is equal with `21`

`both_multi` is the same as `both` only more general, because it expects a list of goals.
```ocaml
let a_goal = Goal.equal (Value.var "a") (Value.int 42)
let b_goal = Goal.equal (Value.var "b") (Value.int 21)
let goal_list = [a_goal; b_goal]
let both_multi_goal = Goal.both_multi goal_list
let state_list = both_multi_goal State.empty |> Base.Sequence.to_list
(* state_list is [ Some[("b",Value.Int 21); ("a",Value.Int 42)] ]*)
```

### in_set
in_set goal is basically a syntactic sugar for an `either_multi` where every goal has the same variable.
```ocaml
let my_set = Base.Set.of_list (module Type) [Value.int 42; Value.int 21]
let in_set_goal = Goal.in_set (Value.var "a") my_set
let state_list = in_set_goal State.empty |> Base.Sequence.to_list
(* state_list is [ Some[("a",Value.Int 42)]; Some[("a",Value.Int 21)] ]*)
```
In this case `state_list` has two states with the same variable(`a`) with two different values: `42` and `21`

`in_set` goal is useful, when you want negation like `x != 6`, which is basically the same as `x in (-infinity,..,5,7,...,infinity)`. From this example you can also see that in_set is only really useful on small finite domains, where the universal set is small and well defined.

### Custom goals
Goal is basically just a function, which accepts a state and based on that generate a sequence of states, so in order to create custom goals you only have to follow the type definition of the goal function, which is:
```ocaml
type goal = state -> state option Base.Sequence.t
```
For example:
```ocaml
let my_simple_goal state =
    (*my_simple_goal's implementation*)
let my_complex_goal custom_input state = 
    (*my_complex_goal's implementation based on custom_input*)

let my_big_goal state = 
    Goal.both_multi [
        my_simple_goal;
        my_complex_goal custom_input;
        (fun state -> my_simple_goal state) (*Lambda goal*)
        (fun state -> my_simple_goal State.empty) (*Independent lambda goal*)
    ] state
```

Custom goals are excellent tools for breaking up bigger complex goals into smaller ones and they are also really great fit for generating or creating goals based on some custom extra input parameters. One thing you should be aware is that you are in control of what state you run your custom goals, which gives you a great deal of flexibility.

**Warning**: using `State.value_of` inside of a custom goal could make it dependent on the order of your other goals.

### Note
If you like reading code more than guides than you can find the examples in the `bin` folder in this repository.

## How to run the examples?
Prerequisite: Install [Esy](https://esy.sh/)

Run it: `esy x <example name>`

For instance: `esy x example`

## How to for contributors
Prerequisite: Install [Esy](https://esy.sh/)

Build it: `esy`

Test it: `esy test`

Run repl: `esy utop`

Generate Doc: `esy doc`

Update docs folder: `esy update-doc`

## Notable resources
- [Hello, declarative world](https://codon.com/hello-declarative-world)
- [miniKanren](http://minikanren.org/)
- [Solving murder with Prolog](https://xmonader.github.io/prolog/2018/12/21/solving-murder-prolog.html)