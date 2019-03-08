type variable = string

module rec ValueType: sig
  type t = 
    | Int of int
    | Float of float
    | Str of string
    | Bool of bool
    | Var of variable
    | Set of (t, ValueComparator.comparator_witness) Base.Set.t
  val compare: t -> t -> int
  val sexp_of_t: t -> Base.Sexp.t
  val to_string: t -> string
end = struct 
  type t = 
    | Int of int
    | Float of float
    | Str of string
    | Bool of bool
    | Var of variable
    | Set of (t, ValueComparator.comparator_witness) Base.Set.t
  let compare first_tagged_value second_tagged_value =
    match first_tagged_value, second_tagged_value with
    | Int first_value, Int second_value -> Base.Int.compare first_value second_value
    | Float first_value, Float second_value -> Base.Float.compare first_value second_value
    | Str first_value, Str second_value -> Base.String.compare first_value second_value
    | Bool first_value, Bool second_value -> Base.Bool.compare first_value second_value
    | Var first_value, Var second_value -> Base.String.compare first_value second_value
    | Set first_value, Set second_value -> Base.Set.compare_direct first_value second_value
    | _, _ -> -1
  let rec sexp_of_t tagged_value =
    match tagged_value with
    | Int value -> Base.Sexp.List [Base.Sexp.Atom "Int"; Base.Sexp.Atom (Base.Int.to_string value)]
    | Float value -> Base.Sexp.List [Base.Sexp.Atom "Float"; Base.Sexp.Atom (Base.Float.to_string value)]
    | Str value -> Base.Sexp.List [Base.Sexp.Atom "Str"; Base.Sexp.Atom value]
    | Bool value -> Base.Sexp.List [Base.Sexp.Atom "Bool"; Base.Sexp.Atom (Base.Bool.to_string value)]
    | Var value -> Base.Sexp.List [Base.Sexp.Atom "Var"; Base.Sexp.Atom value]
    | Set value -> Base.Sexp.List [Base.Sexp.Atom "Set"; Base.Sexp.List (set_to_string value) ]
  and set_to_string set =
    let converted_set = Base.Set.to_list set in
    Base.List.map converted_set ~f:sexp_of_t
  let to_string tagged_value = sexp_of_t tagged_value |> Base.Sexp.to_string
end
and ValueComparator : Base.Comparator.S with type t = ValueType.t =
struct
  type t = ValueType.t
  include Base.Comparator.Make(ValueType)
end

type state = (variable * ValueType.t) list

type goal = state -> state Base.Sequence.t

let int value = ValueType.Int value

let float value = ValueType.Float value

let str value = ValueType.Str value

let bool value = ValueType.Bool value

let var value = ValueType.Var value

let set value = ValueType.Set value