(* module SubHelp = struct 
   let expr_of_value (v : value) : expr = 
    match v with
    | VInt n -> Int n
    | VBool b -> Bool b

   let rec sub (e : expr) (v : value) (x : string) : expr = 
    match e with 
    | Var y -> if x = y then expr_of_value v else e
    | Int _ -> e 
    | Bool _ -> e 
    | Binop (b, e1, e2) -> Binop (b, sub e1 v x, sub e2 v x)
    | Let (y, e1, e2) -> begin 
        let e1' = sub e1 v x in 
        if x = y then Let (y, e1', e2)
        else Let (y, e1', sub e2 v x)
      end
    | If (b, t, f) -> begin 
        let b' = sub b v x in 
        let t' = sub t v x in 
        let f' = sub t v x in 
        If (b', t', f')
      end
   end

   module BigSub : MODEL = struct 
   open SubHelp

   let rec eval (e : expr) : value =
    match e with 
    | Var x -> raise (EvalError "Unbound Variable")
    | Int n -> VInt n 
    | Bool b -> VBool b 
    | Binop (bop, e1, e2) -> eval_binop bop e1 e2 
    | Let (x, e1, e2) -> eval_let x e1 e2
    | If (b, t, f) -> eval_if b t f

   and eval_binop bop e1 e2 = 
    match eval e1, eval e2 with
    | VInt n1, VInt n2 -> begin match bop with 
        | Add -> VInt (n1 + n2)
        | Mult -> VInt (n1 * n2)
        | Leq -> VBool (n1 <= n2)
      end 
    | _ -> raise (EvalError "Bop requires Int")

   and eval_let x e1 e2 = 
    let v1 = eval e1 in 
    eval (sub e2 v1 x)

   and eval_if b t f = 
    match eval b with 
    | VBool b' -> if b' then eval t else eval f
    | _ -> raise (EvalError "If requires bool")
   end  *)