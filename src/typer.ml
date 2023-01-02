open Ltree
open Ttree

exception Unbound_var of { name : name }
exception Variable_is_value of { name : name }
exception Variable_is_type of { name : name }
exception Variable_is_type_constructor of { name : name }
exception Variable_not_used of { name : name }
exception Invariant_unbound_variable of { name : name }
exception Invariant_variable_is_not_value of { name : name }
exception Variable_already_used of { name : name }
exception Var_clash
exception Type_clash of { expected : typ_; received : typ_ }
exception Type_not_a_forall of { typ_ : typ_ }
exception Type_not_an_arrow of { typ_ : typ_ }
exception Type_annotation_required
exception Mismatch_in_args

let () =
  Printexc.register_printer (function
    | Type_clash { expected; received } ->
        Some
          (Format.asprintf "expected: %a, received: %a" Ttree.pp_type expected
             Ttree.pp_type received)
    | _ -> None)

let rec subst ~from ~to_ typ_ =
  match typ_ with
  | T_var var -> (
      match Var.equal var from with true -> to_ | false -> T_var var)
  | T_forall (var, body) ->
      let body =
        match Var.equal var from with
        | true -> body
        | false -> subst ~from ~to_ body
      in
      T_forall (var, body)
  | T_arrow (param, return) ->
      let param = subst ~from ~to_ param in
      let return = subst ~from ~to_ return in
      T_arrow (param, return)

let rec equal ~received ~expected =
  match (received, expected) with
  | T_var received, T_var expected -> (
      match received = expected with true -> () | false -> raise Var_clash)
  | ( T_forall (received_var, received_body),
      T_forall (expected_var, expected_body) ) ->
      let received_body =
        let to_ = T_var expected_var in
        subst ~from:received_var ~to_ received_body
      in
      equal ~received:received_body ~expected:expected_body
  | ( T_arrow (received_param, expected_body),
      T_arrow (expected_param, received_body) ) ->
      equal ~received:received_param ~expected:expected_param;
      equal ~received:received_body ~expected:expected_body
  | received, expected -> raise (Type_clash { expected; received })

module Env : sig
  type 'a env

  val run : (unit -> 'a env) -> 'a

  module Syntax : sig
    val return : 'a -> 'a env
    val ( let* ) : 'a env -> ('a -> 'b env) -> 'b env
    val ( let+ ) : 'a env -> ('a -> 'b) -> 'b env
  end

  (* typ_ *)
  val find_type : name:name -> (Var.t, Ltree.typ_) Either.t env
  val with_type : var:Var.t -> (unit -> 'a env) -> 'a env
  val with_type_alias : name:name -> Ltree.typ_ -> (unit -> 'a env) -> 'a env

  (* constructor *)
  val find_type_constructor : name:name -> (Var.t list * typ_) env

  val with_type_constructor :
    name:name -> params:Var.t list -> typ_ -> (unit -> 'a env) -> 'a env

  (* expr *)
  val find_value : name:name -> (typ_, Ltree.expr) Either.t env
  val with_value : name:name -> typ_ -> (unit -> 'a env) -> 'a env
  val with_value_alias : name:name -> Ltree.expr -> (unit -> 'a env) -> 'a env
end = struct
  module Name_map = Map.Make (String)

  type var_desc =
    | Value of { typ_ : typ_; used : bool }
    | Value_alias of { alias : Ltree.expr }
    | Type of { var : Var.t }
    | Type_alias of { alias : Ltree.typ_ }
    | Type_constructor of { params : Var.t list; body : typ_ }

  type 'a env = names:var_desc Name_map.t -> 'a * var_desc Name_map.t

  let run f =
    let names = Name_map.empty in
    let value, _names = f () ~names in
    value

  module Syntax = struct
    let return v ~names = (v, names)

    let ( let* ) v f ~names =
      let value, names = v ~names in
      f value ~names

    let ( let+ ) v f ~names =
      let value, names = v ~names in
      let value = f value in
      (value, names)
  end

  let find_type ~name ~names =
    match Name_map.find_opt name names with
    | Some (Value { typ_ = _; used = _ } | Value_alias { alias = _ }) ->
        raise (Variable_is_value { name })
    | Some (Type { var }) -> (Either.Left var, names)
    | Some (Type_alias { alias }) -> (Either.Right alias, names)
    | Some (Type_constructor { params = _; body = _ }) ->
        raise (Variable_is_type_constructor { name })
    | None -> raise (Unbound_var { name })

  let find_type_constructor ~name ~names =
    match Name_map.find_opt name names with
    | Some (Value { typ_ = _; used = _ } | Value_alias { alias = _ }) ->
        raise (Variable_is_value { name })
    | Some (Type { var = _ } | Type_alias { alias = _ }) ->
        raise (Variable_is_type { name })
    | Some (Type_constructor { params; body }) -> ((params, body), names)
    | None -> raise (Unbound_var { name })

  let find_value ~name ~names =
    match Name_map.find_opt name names with
    | Some (Value { typ_; used }) -> (
        match used with
        | true -> raise (Variable_already_used { name })
        | false ->
            let used = true in
            let names = Name_map.add name (Value { typ_; used }) names in
            (Either.Left typ_, names))
    | Some (Value_alias { alias }) -> (Either.Right alias, names)
    | Some (Type { var = _ } | Type_alias { alias = _ }) ->
        raise (Variable_is_type { name })
    | Some (Type_constructor { params = _; body = _ }) ->
        raise (Variable_is_type_constructor { name })
    | None -> raise (Unbound_var { name })

  let with_var name desc ~names f =
    let snapshot = Name_map.find_opt name names in
    let names = Name_map.add name desc names in
    let value, names = f () ~names in
    let names =
      match snapshot with
      | Some desc -> Name_map.add name desc names
      | None -> Name_map.remove name names
    in
    (value, names)

  let with_type ~var f ~names =
    let name = Var.name var in
    with_var name (Type { var }) ~names f

  let with_type_alias ~name alias f ~names =
    with_var name (Type_alias { alias }) ~names f

  let with_type_constructor ~name ~params body f ~names =
    with_var name (Type_constructor { params; body }) ~names f

  let with_value ~name typ_ f ~names =
    let desc = Value { typ_; used = false } in
    with_var name desc ~names @@ fun () ~names ->
    let value, names = f () ~names in
    (match Name_map.find_opt name names with
    | Some (Value { typ_ = _; used = true }) -> ()
    | Some (Value { typ_ = _; used = false }) ->
        raise (Variable_not_used { name })
    | Some
        ( Value_alias { alias = _ }
        | Type { var = _ }
        | Type_alias { alias = _ }
        | Type_constructor { params = _; body = _ } ) ->
        raise (Invariant_variable_is_not_value { name })
    | None -> raise (Invariant_unbound_variable { name }));
    (value, names)

  let with_value_alias ~name alias f ~names =
    with_var name (Value_alias { alias }) ~names f
end

open Env.Syntax

let rec transl_type typ_ =
  match typ_ with
  | LT_var name -> (
      let* desc = Env.find_type ~name in
      match desc with
      | Left var -> return @@ T_var var
      | Right alias -> transl_type alias)
  | LT_forall (name, body) ->
      let var = Var.new_var ~name in
      let+ body = Env.with_type ~var @@ fun () -> transl_type body in
      T_forall (var, body)
  | LT_arrow (param, return) ->
      let* param = transl_type param in
      let+ return = transl_type return in
      T_arrow (param, return)
  | LT_apply (name, args) ->
      let* params, body = Env.find_type_constructor ~name in
      let rec elim (params, args) body =
        match (params, args) with
        | [], [] -> return body
        | param :: params, arg :: args ->
            let* arg = transl_type arg in
            elim (params, args) @@ subst ~from:param ~to_:arg body
        | _, [] | [], _ -> raise Mismatch_in_args
      in
      elim (params, args) body

let rec infer_expr expr =
  match expr with
  | LE_var name -> (
      let* desc = Env.find_value ~name in
      match desc with
      | Left typ_ -> return typ_
      | Right alias -> infer_expr alias)
  | LE_t_lambda (name, body) ->
      let var = Var.new_var ~name in
      let+ body = Env.with_type ~var @@ fun () -> infer_expr body in
      T_forall (var, body)
  | LE_t_apply (lambda, arg) -> (
      let* forall = infer_expr lambda in
      match forall with
      | T_forall (var, body) ->
          let+ arg = transl_type arg in
          subst ~from:var ~to_:arg body
      | typ_ -> raise (Type_not_a_forall { typ_ }))
  | LE_e_lambda (name, Some param, body) ->
      let* param = transl_type param in
      let+ return = Env.with_value ~name param @@ fun () -> infer_expr body in
      T_arrow (param, return)
  | LE_e_lambda (_var, None, _body) -> raise Type_annotation_required
  | LE_e_apply (lambda, arg) -> (
      let* arrow = infer_expr lambda in
      match arrow with
      | T_arrow (param, return) ->
          let+ () = check_expr arg ~expected:param in
          return
      | typ_ -> raise (Type_not_an_arrow { typ_ }))
  | LE_t_alias (name, params, typ_, body) -> (
      match params with
      | [] -> Env.with_type_alias ~name typ_ @@ fun () -> infer_expr body
      | params ->
          let params = List.map (fun param -> Var.new_var ~name:param) params in
          let rec transl_body params body =
            match params with
            | [] -> transl_type body
            | param :: params ->
                Env.with_type ~var:param @@ fun () -> transl_body params body
          in
          let* typ_ = transl_body params typ_ in
          Env.with_type_constructor ~name ~params typ_ @@ fun () ->
          infer_expr body)
  | LE_e_alias (name, value, body) ->
      Env.with_value_alias ~name value @@ fun () -> infer_expr body
  | LE_annot (value, annot) ->
      let* annot = transl_type annot in
      let+ () = check_expr value ~expected:annot in
      annot

and check_expr expr ~expected =
  match (expr, expected) with
  | LE_t_lambda (name, body), T_forall (expected_var, expected_return) ->
      let var = Var.new_var ~name in
      let return = subst ~from:expected_var ~to_:(T_var var) expected_return in
      Env.with_type ~var @@ fun () -> check_expr body ~expected:return
  | LE_e_lambda (name, None, body), T_arrow (param, return) ->
      Env.with_value ~name param @@ fun () -> check_expr body ~expected:return
  | expr, expected ->
      let+ received = infer_expr expr in
      equal ~received ~expected

let transl_type typ_ = Env.run @@ fun () -> transl_type typ_
let infer_expr expr = Env.run @@ fun () -> infer_expr expr
