exception SyntaxError of string

exception TypeError of string

type tipo = 
    | Bool
    | Nat
    | Arrow of tipo * tipo

let rec get_tip t =
  match t with
  | "Bool"::r -> (Bool, r)
  | "Nat" ::r -> (Nat,  r)
  | "("   ::r ->
    let (ty, r2) = get_tip r in
    begin
      match r2 with
      | "->"::r3 ->
        let (ty2, r4) = get_tip r3 in
        begin
          match r4 with
          | ")"::r5   -> (Arrow(ty, ty2), r5)
          | _         -> raise (SyntaxError "expression ')' expected")
        end
      | _ ->  raise (SyntaxError "invalid type")
    end
  | _      -> raise (SyntaxError (Printf.sprintf "unbound type"))
;;

type termo =
  | True
  | False
  | If       of termo * termo * termo
  | Number   of int
  | Suc      of termo
  | Pred     of termo
  | Ehzero   of termo
  | Variavel of string
  | Lambda   of termo * tipo * termo
  | Abs      of string * termo
  | App      of termo * termo
  | Empty


let rec build (str: string list) =
  let div_token t (pat: string) msg =
    match t with
    | p :: rest when p = pat -> rest
    | _ -> raise (SyntaxError msg)
  in
  
  let rec build_tmp (s: string list) =
    try
        match s with
        | []          ->   (Empty, [])
        | "true" ::t  ->   (True, t)
        | "false"::t  ->   (False, t)
        | "if"   ::t  ->
            let (cond, t1)      = build_tmp t in
            let err t = Printf.sprintf "'if' statement expects '%s' term" t in
            let (then_expr, t2) = build_tmp   (div_token t1 "then"  (err "then")) in
            let (else_expr, t3) = build_tmp   (div_token t2 "else"  (err "else")) in
            (If (cond, then_expr, else_expr), (div_token t3 "endif" (err "endif")))
        | "lambda" :: lt ->
          begin
            match build_tmp lt with
            | Abs(var, Empty), t ->
              let r = div_token t ":" "'lambda' statement expects ':' expression" in
              let (typ, r2) = get_tip r in
              let r3 = div_token r2 "." "'lambda' statement expects '.' expression" in
              let (expr, t2) = build_tmp r3 in
              (Lambda (Variavel var, typ, expr), (div_token t2 "end" "'lambda' statement expects 'end' expression"))
            | _ -> raise (SyntaxError "invalid name for variable.")
          end
        | "suc"::t     ->
            (Suc Empty, t)
        | "pred"::t    ->
            (Pred Empty, t)
        | "ehzero"::t  ->
            (Ehzero Empty, t)
        | "(" :: t ->
            let (t1, r1) = build_tmp t in
            let (t2, r2) = build_tmp r1 in
            begin
            match r2 with
            | ")"::r3   ->  (App (t1, t2), r3)
            | [] | _    -> raise (SyntaxError "expression ')' expected")
            end
        | h::t when h = "Bool" || h = "Nat" || h = "end" || h = "then" || h = "else" || h = "endif" ->
            raise (SyntaxError (Printf.sprintf "unbound '%s' expression" h))
        | h::t ->
            match int_of_string_opt h with
            | Some n -> (Number n, t)
            | None   -> (Abs(h, Empty), t)
    with
    | SyntaxError msg -> raise (SyntaxError (msg))
  in
  let (t, tf) = build_tmp str in
  if tf != []   then raise (SyntaxError 
    (Printf.sprintf "unrelated '%s' expression" (List.fold_left (^) " " tf)));
  t
;;

let compute_type t = 

  let ctx = ref [] in
  
  let rec compute term =
      match term with
      | True | False  ->          Bool
      | Number a when a >= 0 ->   Nat
      | Suc Empty | Pred Empty -> Arrow (Nat, Nat)
      | Ehzero Empty    -> Arrow(Nat, Bool)
      | If (t1, t2, t3) ->
          begin
          match (compute t1) with
          | Bool | Arrow (_, Bool)  ->
              begin
                match (compute t2), (compute t3) with
                | a, b                         when a=b          ->  a
                | _     -> raise ( TypeError "invalid if type\n")
              end
          | _ ->  raise (TypeError "if statement expects Bool type")
          end
        | Lambda (v, u, t) ->
          begin
          match v with
          | Variavel s ->
              ctx := (s, u) :: !ctx;
              Arrow (u, (compute t))
          | _ ->  raise (TypeError "invalid lambda type")
          end
      | App (t1, t2) ->
        begin
          match compute t1, compute t2 with
          | Arrow (a1, b1), x when a1=x -> b1
          | a, b when a = b ->
            begin
              match t1 with
              | Abs(_, _) -> a
              | _         -> raise (TypeError "invalid type for abstraction.")
            end
          | _  -> raise (TypeError "invalid types")
        end
      | Abs(a, Empty) ->
          begin
            match List.find_opt (fun (s, _) -> s=a) !ctx with
              | Some (_, u) -> u
              | None        -> raise (TypeError (Printf.sprintf "unbound value '%s'" a))
          end
      | _  -> raise ( TypeError "unidentified expression")
  in
  compute t
;;

let rec get_str ty = 
  match ty with
  | Bool  ->  "Bool"
  | Nat   ->  "Nat"
  | Arrow (a, b) ->
  "( " ^ (get_str a) ^ " -> " ^ (get_str b) ^ " )"
;;

