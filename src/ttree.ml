module Var : sig
  type var
  type t = var [@@deriving show, eq, ord]

  val name : var -> Ltree.name
  val new_var : name:Ltree.name -> var
end = struct
  type var = { id : int; name : string }
  type t = var

  let pp fmt var =
    let { id; name } = var in
    Format.fprintf fmt "%s/%d" name id

  let show var = Format.asprintf "%a" pp var
  let equal a b = a.id = b.id
  let compare a b = compare a.id b.id
  let name a = a.name

  let new_var =
    let acc = Atomic.make 0 in
    fun ~name ->
      let id = Atomic.fetch_and_add acc 1 in
      { id; name }
end

type typ_ = T_var of Var.t | T_forall of Var.t * typ_ | T_arrow of typ_ * typ_

open Format

let pp_type_syntax ~pp_funct ~pp_atom fmt typ_ =
  match typ_ with
  | T_var var -> Var.pp fmt var
  | T_forall (var, return) ->
      fprintf fmt "forall %a. %a" Var.pp var pp_funct return
  | T_arrow (param, return) ->
      fprintf fmt "%a -> %a" pp_atom param pp_funct return

type prec = Funct | Atom

let rec pp_type prec fmt typ_ =
  let pp_funct fmt typ_ = pp_type Funct fmt typ_ in
  let pp_atom fmt typ_ = pp_type Atom fmt typ_ in
  match (typ_, prec) with
  | T_var _, (Funct | Atom) | (T_forall _ | T_arrow _), Funct ->
      pp_type_syntax ~pp_funct ~pp_atom fmt typ_
  | (T_forall _ | T_arrow _), Atom -> fprintf fmt "(%a)" pp_funct typ_

let pp_type fmt term = pp_type Funct fmt term
