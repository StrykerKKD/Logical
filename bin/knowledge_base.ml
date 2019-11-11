open Logical

let sequence_to_list sequece = 
  sequece |> Base.Sequence.to_list |> Base.List.filter_opt

let print_state_stream title state_stream =
  print_endline title;
  state_stream |> sequence_to_list |>
  Base.List.iter ~f:(fun element ->
    State.to_string element |> print_endline
  );
  print_newline ()

let first_friend_var = "first_friend_var"
let second_friend_var = "second_friend_var"

let friend first_friend second_friend =
  Goal.both
    (Goal.equal (Value.var first_friend_var) (Value.str first_friend))
    (Goal.equal (Value.var second_friend_var) (Value.str second_friend))

let friend_refl first_friend second_friend = 
  Goal.either 
    (friend first_friend second_friend) 
    (friend second_friend first_friend)

let friend_db = 
  Goal.either_multi [
    friend "john" "julia";
    friend "john" "jack";
    friend "julia" "sam";
    friend "julia" "molly";
  ]

let friend_refl_db = 
  Goal.either_multi [
    friend_refl "john" "julia";
    friend_refl "john" "jack";
    friend_refl "julia" "sam";
    friend_refl "julia" "molly";
  ]

let friend_querying first_friend second_friend = 
  Base.Sequence.round_robin [
    friend_db [first_friend_var,first_friend; second_friend_var,second_friend];
    friend_db [first_friend_var,second_friend; second_friend_var,first_friend]; 
  ]

let friend_state = [first_friend_var, Value.str "julia"; second_friend_var, Value.str "john"]

let friend_state_stream = friend_refl_db friend_state
let another_friend_state_stream = (friend_querying (Value.str "julia") (Value.str "john"))

let _ = print_state_stream "friends" friend_state_stream

let _ = print_state_stream "another friends" another_friend_state_stream


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

let male_var = "male_var"
let female_var = "female_var"

let male male = 
  Goal.equal (Value.var male_var) (Value.str male)

let female female = 
  Goal.equal (Value.var female_var) (Value.str female)

let male_db =
  Goal.either_multi [
    male "brand";
    male "john";
    male "jim";
    male "alfred";
  ]

let female_db = 
  Goal.either_multi [
    female "marry"
  ]

let child_var = "child_var"
let parent_var = "parent_var"

let child child parent =
  Goal.both
    (Goal.equal (Value.var child_var) (Value.str child))
    (Goal.equal (Value.var parent_var) (Value.str parent))

let child_db =
  Goal.either_multi [
    child "brad" "alfred";
    child "john" "jim";
    child "john" "marry";
  ]

let parent_querying parent child = 
  child_db [child_var,child; parent_var,parent]

let parent_state_stream = parent_querying (Value.str "jim") (Value.str "john")

let _ = print_state_stream "parent" parent_state_stream

let father_querying father child =
  Goal.both
    (Goal.equal (Value.var parent_var) (Value.var male_var))
    (Goal.both child_db male_db) 
      [child_var,child; parent_var,father; male_var,father]

let father_state_stream = father_querying (Value.str "jim") (Value.str "john")

let _ = print_state_stream "father" father_state_stream

let mother_querying mother child =
  Goal.both
    (Goal.equal (Value.var parent_var) (Value.var female_var))
    (Goal.both child_db female_db) 
      [child_var,child; parent_var,mother; female_var,mother]

let mother_state_stream = mother_querying (Value.str "marry") (Value.str "john")

let _ = print_state_stream "mother" mother_state_stream

let friendzoned_list_querying lover = 
  let states = loves_db [first_lover_var,lover] |> Base.Sequence.to_list |> Base.List.filter_opt in
  Base.List.exists states ~f:(fun state -> 
    let other_lover = State.value_of state (Value.var second_lover_var) in
    let other_states = loves_db [first_lover_var,other_lover; second_lover_var,lover] |> Base.Sequence.to_list |> Base.List.filter_opt in
    Base.List.length other_states = 0
  )

let _ = friendzoned_list_querying (Value.str "john") |> Printf.printf "%B \n"

let friendzoned_sequence_querying lover = 
  loves_db [first_lover_var,lover] |> Base.Sequence.filter_opt |> Base.Sequence.exists ~f:(fun state -> 
    let other_lover = State.value_of state (Value.var second_lover_var) in
    loves_db [first_lover_var,other_lover; second_lover_var,lover] |> Base.Sequence.filter_opt |> Base.Sequence.length = 0
  )

let _ = friendzoned_sequence_querying (Value.str "john") |> Printf.printf "%B \n"

let friendzoned_sequence_querying_2 lover = 
  loves_db [first_lover_var,lover] 
    |> Base.Sequence.filter_opt
    |> Base.Sequence.concat_map ~f:(fun state ->
      let other_lover = State.value_of state (Value.var second_lover_var) in
      loves_db [first_lover_var,other_lover; second_lover_var,lover]) 
    |> Base.Sequence.filter_opt |> Base.Sequence.length = 0

let _ = friendzoned_sequence_querying_2 (Value.str "john") |> Printf.printf "%B \n"

let friendzoned_set_querying lover = 
  let other_lovers = loves_db [first_lover_var,lover] 
    |> Base.Sequence.to_list 
    |> Base.List.filter_opt 
    |> Base.List.map ~f:(fun state -> State.value_of state (Value.var second_lover_var))
    |> Base.Set.of_list (module Value.Comparator) in
  Goal.both
    (Goal.in_set (Value.var first_lover_var) (Value.set other_lovers))
    loves_db
      [second_lover_var,lover]

let friendzoned_state_stream = friendzoned_set_querying (Value.str "john")

let _ = print_state_stream "friendzoned" friendzoned_state_stream


let domain_a_var = Value.var "domain_a_var"
let domain_b_var = Value.var "domain_b_var"
let domain_c_var = Value.var "domain_c_var"
let domain_d_var = Value.var "domain_d_var"
let domain_e_var = Value.var "domain_e_var"

let universal_set = Base.Set.of_list (module Value.Comparator) [Value.str "red"; Value.str "green"; Value.str "blue"]
let domain_state_stream = Goal.in_set domain_a_var (Value.set universal_set) [] 
  |> Base.Sequence.filter_opt
  |> Base.Sequence.concat_map ~f:(fun state -> 
    let smaller_set = Base.Set.remove universal_set (State.value_of state domain_a_var) in
    (Goal.both
      (Goal.in_set domain_b_var (Value.set smaller_set))
      (Goal.equal domain_b_var domain_d_var)) state)
  |> Base.Sequence.filter_opt
  |> Base.Sequence.concat_map ~f:(fun state -> 
    let smaller_set = Base.Set.remove universal_set (State.value_of state domain_a_var) in
    let even_smaller_set =  Base.Set.remove smaller_set (State.value_of state domain_b_var) in
    (Goal.both
      (Goal.in_set domain_c_var (Value.set even_smaller_set))
      (Goal.equal domain_c_var domain_e_var)) state)

let _ = print_state_stream "domain" domain_state_stream