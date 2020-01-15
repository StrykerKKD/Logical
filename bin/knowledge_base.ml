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
let domain_state_stream = Goal.both_multi [
  Goal.in_set domain_a_var (Value.set universal_set);
  (fun state -> 
      let smaller_set = Base.Set.remove universal_set (State.value_of state domain_a_var) in
      (Goal.both
        (Goal.in_set domain_b_var (Value.set smaller_set))
        (Goal.equal domain_b_var domain_d_var)) state);
  (fun state -> 
    let smaller_set = Base.Set.remove universal_set (State.value_of state domain_a_var) in
    let even_smaller_set =  Base.Set.remove smaller_set (State.value_of state domain_b_var) in
    (Goal.both
      (Goal.in_set domain_c_var (Value.set even_smaller_set))
      (Goal.equal domain_c_var domain_e_var)) state)
] []

let _ = print_state_stream "domain" domain_state_stream


let man_set = Base.Set.of_list (module Value.Comparator) [Value.str "george"; Value.str "john"; Value.str "robert"]
let woman_set = Base.Set.of_list (module Value.Comparator) [Value.str "barbara"; Value.str "christine"; Value.str "yolanda"]
let person_set = Base.Set.union man_set woman_set
let location_set = Base.Set.of_list (module Value.Comparator) [Value.str "bathroom"; Value.str "dining"; Value.str "kitchen"; Value.str "living room"; Value.str "pantry"; Value.str "study"]
let weapon_set = Base.Set.of_list (module Value.Comparator) [Value.str "bag"; Value.str "firearm"; Value.str "gas"; Value.str "knife"; Value.str "poison"; Value.str "rope"]

let bathroom_var = Value.var "bathroom"
let dining_var = Value.var "dining"
let kitchen_var = Value.var "kitchen"
let living_room_var = Value.var "living room"
let pantry_var = Value.var "pantry"
let study_var = Value.var "study"

let bag_var = Value.var "bag"
let firearm_var = Value.var "firearm"
let gas_var = Value.var "gas"
let knife_var = Value.var "knife"
let poison_var = Value.var "poison"
let rope_var = Value.var "rope"

(*let unique_people a b c d e f = 
  Goal.in_set a (Value.set person_set) State.empty
  |> Base.Sequence.filter_opt
  |> Base.Sequence.concat_map ~f:(fun state -> 
    let selected_set = Base.Set.of_list (module Value.Comparator) [(State.value_of state a)] in
    let minus_a = Base.Set.diff person_set selected_set in
    Goal.in_set b (Value.set minus_a) state)
  |> Base.Sequence.filter_opt
  |> Base.Sequence.concat_map ~f:(fun state -> 
    let selected_set = Base.Set.of_list (module Value.Comparator) [(State.value_of state a); (State.value_of state b)] in
    let minus_a_b = Base.Set.diff person_set selected_set in
    Goal.in_set b (Value.set minus_a_b) state)*)

let unique_people person_vars state =
  let first_person_var = Base.List.hd_exn person_vars in
  let rest_person_vars = Base.List.tl_exn person_vars in
  let first_accum_goal = Goal.in_set first_person_var (Value.set person_set) state in
  let used_person_vars_set = Base.Set.singleton (module Value.Comparator) first_person_var in
  let (result, _) = Base.List.fold rest_person_vars ~init:(first_accum_goal, used_person_vars_set) ~f:(fun (accum_goal, used_person_vars_set) person_var -> 
    let next_used_person_vars_set = Base.Set.add used_person_vars_set person_var in
    let next_accum_goal = accum_goal |> Base.Sequence.filter_opt |> Base.Sequence.concat_map ~f:(fun state -> 
      let used_person_set = Base.Set.map (module Value.Comparator) used_person_vars_set ~f:(fun used_person_var -> State.value_of state used_person_var) in
      let available_person_set = Base.Set.diff person_set used_person_set in
      Goal.in_set person_var (Value.set available_person_set) state
    ) in
    (next_accum_goal, next_used_person_vars_set)
  ) in
  result

(*TODO: implement the rest of the example*)

let list_to_str_set list =
  let mapped_list = Base.List.map list ~f:(fun element -> Value.str element) in
  (Base.Set.of_list (module Value.Comparator) mapped_list)

let list_to_var_set list =
  let mapped_list = Base.List.map list ~f:(fun element -> Value.var element) in
  (Base.Set.of_list (module Value.Comparator) mapped_list)

let murderer x =
  Goal.both_multi [
    (fun state -> unique_people [bathroom_var; dining_var; kitchen_var; living_room_var; pantry_var; study_var] state);
    (fun state -> unique_people [bag_var; firearm_var; gas_var; knife_var; poison_var; rope_var] state);
    Goal.both_multi [Goal.in_set kitchen_var (Value.set man_set); Goal.in_set kitchen_var (Value.set(list_to_var_set ["gas"; "poison"]))];
    Goal.both_multi [Goal.in_set study_var (Value.set (list_to_str_set ["barbara"; "yolanda"])); 
      (fun state -> 
        let study_val = State.value_of state study_var in
        let set_without_study = Base.Set.remove (list_to_str_set ["barbara"; "yolanda"]) study_val |> Value.set in
        Goal.in_set bathroom_var set_without_study state)];
    Goal.both_multi [
      (fun state -> 
        let person_without_bg_set = Base.Set.diff person_set (list_to_str_set ["barbara"; "george"]) |> Value.set in
        Goal.in_set bag_var person_without_bg_set state
      );
      (fun state ->
        let variable_set = list_to_var_set ["kitchen"; "living room"; "pantry"; "study"] |> Value.set in
        Goal.in_set bag_var variable_set state
    )];
    Goal.both_multi [Goal.in_set rope_var (woman_set |> Value.set); Goal.equal rope_var study_var];
    (fun state -> 
      let jg_set = list_to_str_set ["john"; "george"] |> Value.set in
      Goal.in_set living_room_var jg_set state
    );
    (fun state -> 
      let variable_set = list_to_var_set ["bathroom"; "kitchen"; "living room"; "pantry"; "study"] |> Value.set in
      Goal.in_set knife_var variable_set state
    );
    (*(fun state ->
      let variable_set = list_to_var_set ["bathroom"; "dining"; "kitchen"; "living room"] |> Value.set in
      Goal.in_set (Value.str "yolanda") variable_set state
    );*)
    Goal.either_multi [Goal.equal bathroom_var (Value.str "yolanda"); Goal.equal dining_var (Value.str "yolanda"); Goal.equal kitchen_var (Value.str "yolanda"); Goal.equal living_room_var (Value.str "yolanda")];
    Goal.equal firearm_var (Value.str "george");
    Goal.both_multi [Goal.equal pantry_var gas_var; Goal.equal pantry_var x]
  ]

let x = Value.var "x"

let murderer_stream = murderer x State.empty

let _ = print_state_stream "murderer" murderer_stream