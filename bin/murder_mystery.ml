open Logical

let man_set = Base.Set.of_list (module Type) [Value.str "george"; Value.str "john"; Value.str "robert"]
let woman_set = Base.Set.of_list (module Type) [Value.str "barbara"; Value.str "christine"; Value.str "yolanda"]
let person_set = Base.Set.union man_set woman_set
let location_set = Base.Set.of_list (module Type) [Value.str "bathroom"; Value.str "dining"; Value.str "kitchen"; Value.str "living room"; Value.str "pantry"; Value.str "study"]
let weapon_set = Base.Set.of_list (module Type) [Value.str "bag"; Value.str "firearm"; Value.str "gas"; Value.str "knife"; Value.str "poison"; Value.str "rope"]

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

let unique_people_goal person_vars state =
  let first_person_var = Base.List.hd_exn person_vars in
  let rest_person_vars = Base.List.tl_exn person_vars in
  let first_accum_goal = Goal.in_set first_person_var person_set in
  let used_person_vars_set = Base.Set.singleton (module Type) first_person_var in
  let (result_goals, _) = Base.List.fold rest_person_vars ~init:([first_accum_goal], used_person_vars_set) ~f:(fun (accum_goal, used_person_vars_set) person_var -> 
    let next_used_person_vars_set = Base.Set.add used_person_vars_set person_var in
    let next_goal = (fun state -> 
      let used_person_set = Base.Set.map (module Type) used_person_vars_set ~f:(fun used_person_var -> State.value_of state used_person_var) in
      let available_person_set = Base.Set.diff person_set used_person_set in
      Goal.in_set person_var available_person_set state
    ) in
    (Base.List.append accum_goal [next_goal], next_used_person_vars_set)
  ) in
  Goal.both_multi result_goals state

let list_to_str_set list =
  let mapped_list = Base.List.map list ~f:(fun element -> Value.str element) in
  (Base.Set.of_list (module Type) mapped_list)

let list_to_var_set list =
  let mapped_list = Base.List.map list ~f:(fun element -> Value.var element) in
  (Base.Set.of_list (module Type) mapped_list)

(* You can make this faster by moving the two unique_people_goal calls to the bottom. *)
let murderer x =
  Goal.both_multi [
    (fun state -> unique_people_goal [bathroom_var; dining_var; kitchen_var; living_room_var; pantry_var; study_var] state);
    (fun state -> unique_people_goal [bag_var; firearm_var; gas_var; knife_var; poison_var; rope_var] state);
    Goal.both_multi [Goal.in_set kitchen_var man_set; Goal.in_set kitchen_var (list_to_var_set ["gas"; "poison"])];
    Goal.both_multi [Goal.in_set study_var (list_to_str_set ["barbara"; "yolanda"]); 
      (fun state -> 
        let study_val = State.value_of state study_var in
        let set_without_study = Base.Set.remove (list_to_str_set ["barbara"; "yolanda"]) study_val in
        Goal.in_set bathroom_var set_without_study state)];
    Goal.both_multi [
      (fun state -> 
        let person_without_bg_set = Base.Set.diff person_set (list_to_str_set ["barbara"; "george"]) in
        Goal.in_set bag_var person_without_bg_set state
      );
      (fun state ->
        let variable_set = list_to_var_set ["kitchen"; "living room"; "pantry"; "study"] in
        Goal.in_set bag_var variable_set state
    )];
    Goal.both_multi [Goal.in_set rope_var woman_set; Goal.equal rope_var study_var];
    (fun state -> 
      let jg_set = list_to_str_set ["john"; "george"] in
      Goal.in_set living_room_var jg_set state
    );
    (fun state -> 
      let variable_set = list_to_var_set ["bathroom"; "kitchen"; "living room"; "pantry"; "study"] in
      Goal.in_set knife_var variable_set state
    );
    (fun state ->
      let variable_set = list_to_var_set ["bathroom"; "dining"; "kitchen"; "living room"] in
      Goal.in_set (Value.str "yolanda") variable_set state
    );
    Goal.equal firearm_var (Value.str "george");
    Goal.both_multi [Goal.equal pantry_var gas_var; Goal.equal pantry_var x]
  ]

let x = Value.var "x"

let murder_stream = murderer x State.empty

let _ = Util.print_state_stream "Solved murder mystery:" murder_stream