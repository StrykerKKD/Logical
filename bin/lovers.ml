open Logical

let first_lover_var = "first_lover_var"
let second_lover_var = "second_lover_var"

let loves first_lover second_lover =
  Goal.both
    (Goal.equal (Value.var first_lover_var) (Value.str first_lover))
    (Goal.equal (Value.var second_lover_var) (Value.str second_lover))

let loves_db = 
  Goal.either_multi [
    loves "john" "julia";
    loves "julia" "sam";
    loves "sam" "julia";
  ]

let friendzoned_query lover = 
  Goal.both_multi [
    (fun state -> loves_db state);
    (fun state -> 
      let other_lover = State.value_of state (Value.var second_lover_var) in
      loves_db (State.create_exn [first_lover_var,other_lover; second_lover_var,lover])
    )
  ] (State.create_exn [first_lover_var,lover])

let friendzoned_state_stream = friendzoned_query (Value.str "john")
let _ = Base.Sequence.exists friendzoned_state_stream ~f:Base.Option.is_empty |> Printf.printf "Has been friendzoned: %B \n"
let _ = Util.print_state_stream "Lovers querying" friendzoned_state_stream
