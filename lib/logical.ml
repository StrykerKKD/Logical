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

let assignment_to_string (variable, value) =
  variable ^ ": " ^ ValueType.to_string value

let state_to_string state =
  let assignments = Base.List.map state ~f:assignment_to_string in
  Base.String.concat ~sep:"\n" ("Assigments:" :: assignments)

let empty_state = []

let add_assignment state assignment = 
  List.cons assignment state

let rec value_of_state state value =
  match value with
  | ValueType.Var variable -> 
    (match List.assoc_opt variable state with
    | Some value -> value_of_state state value
    | None -> ValueType.Var variable)
  | _ -> value
  
let unify_state state value_a value_b =
  let new_value_a = value_of_state state value_a in
  let new_value_b = value_of_state state value_b in
  match new_value_a, new_value_b with
  | a, b when a = b -> Some state
  | ValueType.Var variable, value -> Some (add_assignment state (variable, value))
  | value, ValueType.Var variable -> Some (add_assignment state (variable, value))
  | _, _ -> None

let pursue_goal goal state =
  goal state

let equal_goal value_a value_b = (fun state -> 
  let state = unify_state state value_a value_b in
  Base.Sequence.singleton state
)

let either_goal first_goal second_goal = (fun state -> 
  let first_stream = pursue_goal first_goal state in
  let second_stream = pursue_goal second_goal state in
  Base.Sequence.round_robin [first_stream; second_stream]
)

let either_goal_general goals = (fun state -> 
  let streams = Base.List.map goals ~f:(fun goal -> pursue_goal goal state) in
  Base.Sequence.round_robin streams
)

let both_goal first_goal second_goal = (fun state ->
  let first_states = pursue_goal first_goal state in
  let sum_states = Base.Sequence.filter_map first_states ~f:(fun first_state -> 
    match first_state with
    | Some state -> Some (pursue_goal second_goal state)
    | None -> None
  ) in
  Base.Sequence.interleave sum_states
)

let desugar_set_to_logic set variable state =
  let value_list = Base.Set.to_list set in
  let both_goals = Base.List.map value_list ~f:(fun value -> equal_goal (ValueType.Var variable) value) in
  let either_goal = either_goal_general both_goals in 
  pursue_goal either_goal state

let in_set_goal value_a value_b = (fun state -> 
  match value_a, value_b with
  | ValueType.Var variable, ValueType.Set set -> desugar_set_to_logic set variable state
  | ValueType.Set set, ValueType.Var variable -> desugar_set_to_logic set variable state
  | _ -> Base.Sequence.singleton None
)