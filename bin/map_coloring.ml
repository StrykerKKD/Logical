open Logical

let domain_a_var = Value.var "domain_a_var"
let domain_b_var = Value.var "domain_b_var"
let domain_c_var = Value.var "domain_c_var"
let domain_d_var = Value.var "domain_d_var"
let domain_e_var = Value.var "domain_e_var"

let universal_set = Base.Set.of_list (module Type) [Value.str "red"; Value.str "green"; Value.str "blue"]
let domain_state_stream = Goal.both_multi [
  Goal.in_set domain_a_var universal_set;
  (fun state -> 
      let smaller_set = Base.Set.remove universal_set (State.value_of state domain_a_var) in
      (Goal.both
        (Goal.in_set domain_b_var smaller_set)
        (Goal.equal domain_b_var domain_d_var)) state);
  (fun state -> 
    let smaller_set = Base.Set.remove universal_set (State.value_of state domain_a_var) in
    let even_smaller_set =  Base.Set.remove smaller_set (State.value_of state domain_b_var) in
    (Goal.both
      (Goal.in_set domain_c_var even_smaller_set)
      (Goal.equal domain_c_var domain_e_var)) state)
] State.empty

let _ = Util.print_state_stream "Solved map coloring:" domain_state_stream