%{
open Ltree

(* TODO: better naming *)
let wrap_body (params, return) body =
  let body =
    match return with
    | Some annot -> LE_annot (body, annot)
    | None -> body in
  List.fold_right (fun param body ->
    match param with
    | `Type var -> LE_t_lambda (var, body)
    | `Value (var, typ_) -> LE_e_lambda (var, typ_, body)
  ) params body
%}
%token <string> VAR (* x *)
%token FUN (* fun *)
%token COLON (* : *) 
%token DOT (* . *)
%token ARROW (* -> *)
%token ALIAS (* === *)
%token LET (* let *)
%token TYPE (* type *)
%token FORALL (* forall *)
%token IN (* in *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token EOF

%start <Ltree.expr option> expr_opt
%start <Ltree.typ_ option> type_opt

%%

let expr_opt :=
  | EOF;
    { None }
  | expr = expr; EOF;
    { Some expr }
let type_opt :=
  | EOF;
    { None }
  | typ_ = typ_; EOF;
    { Some typ_ }

let expr == expr_annot

let expr_annot ==
  | value = expr_funct; COLON; annot = typ_;
    { LE_annot (value, annot) }
  | expr_bind_in

let expr_bind_in :=
  | TYPE; var = VAR; params = list(VAR); ALIAS; value = typ_; IN; body = expr_bind_in;
    { LE_t_alias (var, params, value, body) }
  | LET; var = VAR; (params, annot) = expr_bind_params; ALIAS; value = expr_bind_in; IN; body = expr_bind_in;
    { let value = wrap_body (params, annot) value in
      LE_e_alias (var, value, body) }
  | expr_funct

let expr_bind_params ==
  | params = list(expr_param);
    { (params, None) }
  | params = list(expr_param); COLON; return = typ_funct;
    { (params, Some return) }

let expr_funct :=
  | FUN; (params, return) = expr_funct_params; ARROW; body = expr_funct;
    { wrap_body (params, return) body }
  | expr_apply

let expr_funct_params ==
  | params = nonempty_list(expr_param);
    { (params, None) }
  | params = nonempty_list(expr_param); COLON; return = typ_apply;
    { (params, Some return) }

let expr_param ==
  | var = VAR;
    { `Value (var, None) }
  | LEFT_PARENS; var = VAR; COLON; typ_ = typ_; RIGHT_PARENS;
    { `Value (var, Some typ_) }
  | LEFT_PARENS; TYPE; var = VAR; RIGHT_PARENS;
    { `Type var }

let expr_apply :=
  | lambda = expr_apply; LEFT_PARENS; TYPE; arg = typ_; RIGHT_PARENS;
    { LE_t_apply (lambda, arg) }
  | lambda = expr_apply; arg = expr_atom;
    { LE_e_apply (lambda, arg) }
  | expr_atom

let expr_atom ==
  | var = VAR;
    { LE_var var }
  | LEFT_PARENS; expr = expr; RIGHT_PARENS;
    { expr }

let typ_ == typ_funct

let typ_funct :=
  | FORALL; vars = nonempty_list(VAR); DOT; body = typ_funct;
    { List.fold_right (fun var body -> LT_forall (var, body)) vars body }
  | left = typ_apply; ARROW; right = typ_funct;
    { LT_arrow (left, right) }
  | typ_apply

let typ_apply ==
  | var = VAR; args = nonempty_list(typ_atom);
    { LT_apply (var, args) }
  | typ_atom

let typ_atom ==
  | var = VAR;
    { LT_var var }
  | LEFT_PARENS; typ_ = typ_; RIGHT_PARENS;
    { typ_ }
