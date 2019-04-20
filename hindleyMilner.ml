open Ast 

module rec UExpr : sig 
  type t = 
    | UVar of UVar.t 
    | UInt
    | UBool
    | UBinop of bop * UExpr.t * UExpr.t 
    | ULet of UVar.t * UExpr.t * UExpr.t 
    | UIf of UExpr.t * UExpr.t * UExpr.t
    | UFun of UVar.t * UExpr.t
    | UApp of UExpr.t * UExpr.t
end = UExpr

and UVar : sig
  type t = int
  val next : t -> t
  val first : t
end = struct
  type t = int
  (* RI: Not less than 0 *)

  let next var = var + 1
  let first = 0
end

and UExprOfExpr : sig 
  val uexpr_of_exp : expr -> UExpr.t
end = struct 

  type sub = string * UVar.t 

  let sub x sublst = 
    match List.assoc_opt x sublst with
    | Some uvar -> uvar 
    | None -> failwith "HM Unbound Variable"

  let rec uexpr_sub (last_uvar : UVar.t) (sublst : sub list) (e : expr) : UVar.t * UExpr.t = 
    match e with
    | Var x -> last_uvar, UVar (sub x sublst)
    | Int n -> last_uvar, UInt
    | Bool b -> last_uvar, UBool
    | Binop (bop, e1, e2) -> begin 
        let last_uvar', ue1 = uexpr_sub last_uvar sublst e1 in 
        let last_uvar'', ue2 = uexpr_sub last_uvar' sublst e2 in 
        last_uvar'', UBinop (bop, ue1, ue2)
      end
    | Let (x, e1, e2) -> begin 
        let last_uvar', ue1 = uexpr_sub last_uvar sublst e1 in 
        let next_uvar = UVar.next last_uvar' in 
        let next_uvar', ue2 = uexpr_sub next_uvar ((x, next_uvar)::sublst) e2 in 
        next_uvar', ULet (next_uvar, ue1, ue2)
      end
    | If (b, t, f) -> begin 
        let last_uvar', ub = uexpr_sub last_uvar sublst b in 
        let last_uvar'', ut = uexpr_sub last_uvar' sublst t in 
        let last_uvar''', uf = uexpr_sub last_uvar'' sublst f in 
        last_uvar''', UIf (ub, ut, uf)
      end
    | Fun (x, e) -> begin
        let next_uvar = UVar.next last_uvar in 
        let next_uvar', ue = uexpr_sub next_uvar ((x, next_uvar)::sublst) e in 
        next_uvar', UFun (next_uvar, ue)
      end
    | App (e1, e2) -> begin 
        let last_uvar', ue1 = uexpr_sub last_uvar sublst e1 in 
        let last_uvar'', ue2 = uexpr_sub last_uvar' sublst e2 in 
        last_uvar'', UApp (ue1, ue2)
      end

  let uexpr_of_exp e = snd (uexpr_sub UVar.first [] e)
end

and Type : sig 
  type t = TVar of TypeVar.t | TInt | TBool | TFun of t * t
end = Type 

and AExpr : sig 
  type t = 
    | AVar of Type.t * UVar.t
    | AInt
    | ABool
    | ABinop of Type.t * bop * AExpr.t * AExpr.t
    | ALet of Type.t * (Type.t * UVar.t) * AExpr.t * AExpr.t
    | AIf of Type.t * AExpr.t * AExpr.t * AExpr.t
    | AFun of Type.t * (Type.t * UVar.t) * AExpr.t
    | AApp of Type.t * AExpr.t * AExpr.t
end = AExpr

and AssignPreliminaryTypes : sig
  val assign_of_exp : expr -> AExpr.t
end = struct
  let rec assign_of_uexpr (last_tvar : TypeVar.t) (ue : UExpr.t) : TypeVar.t * AExpr.t = 
    let next_tvar = TypeVar.next last_tvar in 
    match ue with 
    | UVar x -> next_tvar, AVar (TVar next_tvar, x)
    | UInt -> last_tvar, AInt
    | UBool -> last_tvar, ABool
    | UBinop (bop, ue1, ue2) -> begin 
        let next_tvar', ae1 = assign_of_uexpr next_tvar ue1 in 
        let next_tvar'', ae2 = assign_of_uexpr next_tvar' ue2 in 
        next_tvar'', ABinop (TVar next_tvar, bop, ae1, ae2)
      end 
    | ULet (x, ue1, ue2) -> begin 
        let next_tvar' = TypeVar.next next_tvar in 
        let next_tvar'', ae1 = assign_of_uexpr next_tvar' ue1 in 
        let next_tvar''', ae2 = assign_of_uexpr next_tvar'' ue2 in 
        next_tvar''', ALet (TVar next_tvar, (TVar next_tvar', x), ae1, ae2)
      end
    | UIf (b, t, f) -> begin 
        let next_tvar', ae1 = assign_of_uexpr next_tvar b in 
        let next_tvar'', ae2 = assign_of_uexpr next_tvar' t in 
        let next_tvar''', ae3 = assign_of_uexpr next_tvar' f in 
        next_tvar''', AIf ((TVar next_tvar), ae1, ae2, ae3)
      end
    | UFun (x, ue) -> begin 
        let next_tvar' = TypeVar.next next_tvar in
        let next_tvar'', ae = assign_of_uexpr next_tvar' ue in 
        next_tvar'', AFun (TVar next_tvar, (TVar next_tvar', x), ae)
      end
    | UApp (ue1, ue2) -> begin 
        let next_tvar', ae1 = assign_of_uexpr next_tvar ue1 in 
        let next_tvar'', ae2 = assign_of_uexpr next_tvar' ue2 in 
        next_tvar'', AApp (TVar next_tvar, ae1, ae2)
      end

  let assign_of_exp e = 
    let ue = UExprOfExpr.uexpr_of_exp e in 
    let _, ae = assign_of_uexpr TypeVar.first ue in 
    ae
end

and CollectConstraints : sig
  type t
end = struct
  type t
end

and TypeVar : sig
  type t = int
  val next : t -> t
  val first : t
end = struct
  type t = int
  (* RI: Not less than 0 *)

  (* let next tvars = 1 + (List.fold_left max (-1) tvars) *)
  let next tvar = tvar + 1
  let first = 0
end

(* and Var : sig
   type t = int
   val next : t -> t
   end = struct
   type t = int
   (* RI: Not less than 0 *)

   let next var = var + 1
   end *)



