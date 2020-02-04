type variable_name = string

module T = struct
  type t = 
    | Int of int
    | Float of float
    | Str of string
    | Bool of bool
    | Var of variable_name
  let compare first_tagged_value second_tagged_value =
    match first_tagged_value, second_tagged_value with
    | Int first_value, Int second_value -> Base.Int.compare first_value second_value
    | Float first_value, Float second_value -> Base.Float.compare first_value second_value
    | Str first_value, Str second_value -> Base.String.compare first_value second_value
    | Bool first_value, Bool second_value -> Base.Bool.compare first_value second_value
    | Var first_value, Var second_value -> Base.String.compare first_value second_value
    | _, _ -> -1
  let sexp_of_t tagged_value =
    match tagged_value with
    | Int value -> Base.Sexp.List [Base.Sexp.Atom "Int"; Base.Sexp.Atom (Base.Int.to_string value)]
    | Float value -> Base.Sexp.List [Base.Sexp.Atom "Float"; Base.Sexp.Atom (Base.Float.to_string value)]
    | Str value -> Base.Sexp.List [Base.Sexp.Atom "Str"; Base.Sexp.Atom value]
    | Bool value -> Base.Sexp.List [Base.Sexp.Atom "Bool"; Base.Sexp.Atom (Base.Bool.to_string value)]
    | Var value -> Base.Sexp.List [Base.Sexp.Atom "Var"; Base.Sexp.Atom value]
  let to_string tagged_value = sexp_of_t tagged_value |> Base.Sexp.to_string
end
include T
include Base.Comparator.Make(T)

type state = (variable_name, t, Base.String.comparator_witness) Base.Map.t

type goal = state -> state option Base.Sequence.t