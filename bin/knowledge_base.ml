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
let second_lover_var = "first_lover_var"

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