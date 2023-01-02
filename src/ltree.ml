type name = string

and expr =
  | LE_var of name
  | LE_t_lambda of name * expr
  | LE_t_apply of expr * typ_
  | LE_e_lambda of name * typ_ option * expr
  | LE_e_apply of expr * expr
  | LE_t_alias of name * name list * typ_ * expr
  | LE_e_alias of name * expr * expr
  | LE_annot of expr * typ_

and typ_ =
  | LT_var of name
  | LT_forall of name * typ_
  | LT_arrow of typ_ * typ_
  | LT_apply of name * typ_ list
