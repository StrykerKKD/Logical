open Logical

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

let friend_refl_querying first_friend second_friend =
  Goal.either
    (fun state -> friend_db (State.create_exn [first_friend_var,first_friend; second_friend_var,second_friend]))
    (fun state -> friend_db (State.create_exn [first_friend_var,second_friend; second_friend_var,first_friend]))
    State.empty

let friend_querying first_friend second_friend = 
  friend_refl_db (State.create_exn [first_friend_var, first_friend; second_friend_var, second_friend])

let friend_normal_query_state_stream = friend_querying (Value.str "julia") (Value.str "john")
let _ = Util.print_state_stream "Friends: using normal query on refl DB" friend_normal_query_state_stream


let friend_refl_query_state_stream = friend_refl_querying (Value.str "julia") (Value.str "john")
let _ = Util.print_state_stream "Friends: using refl query on simple DB" friend_refl_query_state_stream