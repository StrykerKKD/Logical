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