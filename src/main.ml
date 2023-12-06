
let filename = "output"

let () =
  let file = open_out filename in
  try
    let input = String.split_on_char ' ' (read_line()) in
    let ast  = Term.build input in
    let typ   = Term.compute_type ast in
    Printf.printf "%s\n" (Term.get_str typ);
    close_out file
  with
  | Term.SyntaxError msg  ->
    Printf.printf "!\n";
    Printf.fprintf file "Syntax Error: %s\n" msg;
  | Term.TypeError msg    ->
    Printf.printf "-\n";
    Printf.fprintf file "Type Error: %s\n" msg;
;;

