open Ast 

exception EvalError of string


module rec Value : sig 
  type t = 
    | VInt of int
    | VBool of bool
    | VFun of string * expr * Env.t

end = Value

and Env : sig 
  type t 
  val empty : t
  val update : string -> Value.t -> t -> t 
  val lookup : string -> t -> Value.t 

end = struct 
  type t = (string * Value.t) list 
  (* AF([]) = {}
     AF((x,v)::t) = {x:v} U AF(t) *)
  (* RI: No duplicate keys. *)

  let empty = [] 

  let update x v env = (x, v)::(List.remove_assoc x env)

  let lookup x env = 
    match List.assoc_opt x env with 
    | Some v -> v 
    | None -> raise (EvalError "Env Unbound Variable")
end 

type value = Value.t

module type MODEL = sig 
  val eval : expr -> value
end 

module EnvModel : MODEL = struct 

  open Env

  let rec eval_exp (env : Env.t) (e : expr) : value = 
    match e with 
    | Var x -> lookup x env 
    | Int n -> VInt n 
    | Bool b -> VBool b
    | Binop (bop, e1, e2) -> eval_binop env bop e1 e2
    | Let (x, e1, e2) -> eval_let env x e1 e2 
    | If (b, t, f) -> eval_if env b t f
  (* | Fun (x, e) -> VFun (x, e, env)
     | App (e1, e2) -> eval_app env e1 e2 *)

  and eval_binop env bop e1 e2 = 
    let v1 = eval_exp env e1 in 
    let v2 = eval_exp env e2 in 
    match v1, v2 with 
    | VInt n1, VInt n2 -> 
      begin match bop with 
        | Add -> VInt (n1 + n2)
        | Mult -> VInt (n1 * n2) 
        | Leq -> VBool (n1 <= n2)
      end
    | _ -> raise (EvalError "Bop requires Int")

  and eval_let env x e1 e2 = 
    let v1 = eval_exp env e1 in 
    let env' = update x v1 env in 
    eval_exp env' e2

  and eval_if env b t f = 
    match eval_exp env b with 
    | VBool b -> if b then eval_exp env t else eval_exp env f 
    | _ -> raise (EvalError "If requires bool")

  and eval_app env e1 e2 = 
    let a = eval_exp env e2 in 
    match eval_exp env e1 with 
    | VFun (x, e_body, def_env) -> begin 
        let def_env' = update x a def_env in 
        eval_exp def_env' e_body
      end 
    | _ -> raise (EvalError "App requires Fun")

  let eval (e : expr) : value = eval_exp empty e

end
