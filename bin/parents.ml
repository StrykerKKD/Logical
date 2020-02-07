open Logical

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
  child_db (State.create_exn [child_var,child; parent_var,parent])

let parent_state_stream = parent_querying (Value.str "jim") (Value.str "john")
let _ = Util.print_state_stream "Parent querying" parent_state_stream

let father_querying father child =
  Goal.both
    (Goal.equal (Value.var parent_var) (Value.var male_var))
    (Goal.both child_db male_db) 
      (State.create_exn [child_var,child; parent_var,father; male_var,father])

let father_state_stream = father_querying (Value.str "jim") (Value.str "john")
let _ = Util.print_state_stream "Father querying" father_state_stream

let mother_querying mother child =
  Goal.both
    (Goal.equal (Value.var parent_var) (Value.var female_var))
    (Goal.both child_db female_db) 
      (State.create_exn [child_var,child; parent_var,mother; female_var,mother])

let mother_state_stream = mother_querying (Value.str "marry") (Value.str "john")
let _ = Util.print_state_stream "Mother querying" mother_state_stream