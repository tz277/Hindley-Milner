exception TypeError of string

type typ = 
  | TInt 
  | TBool

module type CONTEXT = sig 
  type t 
  val empty : t 
  val lookup : t -> string -> typ
  val extend : t -> string -> typ -> t
end

module Context : CONTEXT = struct 
  type t = (string * typ) list 

  let empty = []

  let lookup ctx x = 
    match List.assoc_opt x ctx with 
    | Some t -> t 
    | None -> raise (TypeError "Unbound Variable")

  let extend ctx x t = 
    (x, t) :: ctx
end

let rec typeof (ctx : Context.t) (e : Ast.expr) : typ = 
  let open Ast in 
  match e with 
  | Var x -> Context.lookup ctx x 
  | Int _ -> TInt 
  | Bool _ -> TBool 
  | Binop (bop, e1, e2) -> typeof_binop ctx bop e1 e2
  | Let (x, e1, e2) -> typeof_let ctx x e1 e2
  | If (b, t, f) -> typeof_if ctx b t f

and typeof_binop ctx bop e1 e2 =
  let t1 = typeof ctx e1 in 
  let t2 = typeof ctx e2 in 
  match t1, t2 with 
  | TInt, TInt -> begin 
      match bop with 
      | Add | Mult -> TInt
      | Leq -> TBool
    end
  | _ -> raise (TypeError "Binop Expected Int")

and typeof_let ctx x e1 e2 = 
  let t1 = typeof ctx e1 in 
  let ctx' = Context.extend ctx x t1 in 
  typeof ctx' e2

and typeof_if ctx b t f = 
  match typeof ctx b with 
  | TBool -> begin 
      let t1 = typeof ctx t in 
      let t2 = typeof ctx f in 
      if t1 = t2 then t1 else raise (TypeError "If expects branches of same type")
    end
  | _ -> raise (TypeError "If expects bool")

let typecheck (e : Ast.expr) : Ast.expr = 
  ignore (typeof Context.empty e); e