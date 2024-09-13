(**
   Boilerplate to be used as a template when mapping the sml CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_pat_213dc3e (env : env) (tok : CST.pat_213dc3e) =
  (* pattern [0-9] *) token env tok

let map_primealphanumeric_ident (env : env) (tok : CST.primealphanumeric_ident) =
  (* primealphanumeric_ident *) token env tok

let map_unit_pat (env : env) ((v1, v2) : CST.unit_pat) =
  let v1 = (* "(" *) token env v1 in
  let v2 = (* ")" *) token env v2 in
  R.Tuple [v1; v2]

let map_block_comment (env : env) (tok : CST.block_comment) =
  (* block_comment *) token env tok

let map_real_scon (env : env) (tok : CST.real_scon) =
  (* real_scon *) token env tok

let map_tok_semi (env : env) (tok : CST.tok_semi) =
  (* tok_semi *) token env tok

let map_block_comment_explicit (env : env) (() : CST.block_comment_explicit) =
  R.Tuple []

let map_pat_dcab316 (env : env) (tok : CST.pat_dcab316) =
  (* pattern [1-9][0-9]* *) token env tok

let map_integer_scon (env : env) (tok : CST.integer_scon) =
  (* integer_scon *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_tok_comma (env : env) (tok : CST.tok_comma) =
  (* tok_comma *) token env tok

let map_tok_pat_6ca5299 (env : env) (tok : CST.tok_pat_6ca5299) =
  (* tok_pat_6ca5299 *) token env tok

let map_string_scon (env : env) (tok : CST.string_scon) =
  (* string_scon *) token env tok

let map_tok_bar (env : env) (tok : CST.tok_bar) =
  (* tok_bar *) token env tok

let map_line_comment_explicit (env : env) (() : CST.line_comment_explicit) =
  R.Tuple []

let map_word_scon (env : env) (tok : CST.word_scon) =
  (* word_scon *) token env tok

let map_symbolic_ident (env : env) (tok : CST.symbolic_ident) =
  (* symbolic_ident *) token env tok

let map_char_scon (env : env) (tok : CST.char_scon) =
  (* char_scon *) token env tok

let map_line_comment (env : env) (tok : CST.line_comment) =
  (* line_comment *) token env tok

let map_anon_choice_COLON_49dafc4 (env : env) (x : CST.anon_choice_COLON_49dafc4) =
  (match x with
  | `COLON tok -> R.Case ("COLON",
      (* ":" *) token env tok
    )
  | `COLONGT tok -> R.Case ("COLONGT",
      (* ":>" *) token env tok
    )
  )

let map_tyvar (env : env) (x : CST.tyvar) =
  (match x with
  | `Prim_id tok -> R.Case ("Prim_id",
      (* primealphanumeric_ident *) token env tok
    )
  )

let map_alphaalphanumeric_ident (env : env) (x : CST.alphaalphanumeric_ident) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Tok_pat_6ca5299 x -> R.Case ("Tok_pat_6ca5299",
      map_tok_pat_6ca5299 env x
    )
  )

let map_scon (env : env) (x : CST.scon) =
  (match x with
  | `Int_scon tok -> R.Case ("Int_scon",
      (* integer_scon *) token env tok
    )
  | `Word_scon tok -> R.Case ("Word_scon",
      (* word_scon *) token env tok
    )
  | `Real_scon tok -> R.Case ("Real_scon",
      (* real_scon *) token env tok
    )
  | `Str_scon tok -> R.Case ("Str_scon",
      (* string_scon *) token env tok
    )
  | `Char_scon tok -> R.Case ("Char_scon",
      (* char_scon *) token env tok
    )
  )

let map_tyvar_ty (env : env) (x : CST.tyvar_ty) =
  map_tyvar env x

let map_strid (env : env) (x : CST.strid) =
  (match x with
  | `Alph_id x -> R.Case ("Alph_id",
      map_alphaalphanumeric_ident env x
    )
  )

let map_tycon (env : env) (x : CST.tycon) =
  (match x with
  | `Alph_id x -> R.Case ("Alph_id",
      map_alphaalphanumeric_ident env x
    )
  | `Symb_id tok -> R.Case ("Symb_id",
      (* symbolic_ident *) token env tok
    )
  )

let map_fctid (env : env) (x : CST.fctid) =
  (match x with
  | `Alph_id x -> R.Case ("Alph_id",
      map_alphaalphanumeric_ident env x
    )
  )

let map_vid (env : env) (x : CST.vid) =
  (match x with
  | `Alph_id x -> R.Case ("Alph_id",
      map_alphaalphanumeric_ident env x
    )
  | `Symb_id tok -> R.Case ("Symb_id",
      (* symbolic_ident *) token env tok
    )
  )

let map_lab (env : env) (x : CST.lab) =
  (match x with
  | `Alph_id x -> R.Case ("Alph_id",
      map_alphaalphanumeric_ident env x
    )
  | `Symb_id tok -> R.Case ("Symb_id",
      (* symbolic_ident *) token env tok
    )
  | `Pat_dcab316 x -> R.Case ("Pat_dcab316",
      map_pat_dcab316 env x
    )
  )

let map_sigid (env : env) (x : CST.sigid) =
  (match x with
  | `Alph_id x -> R.Case ("Alph_id",
      map_alphaalphanumeric_ident env x
    )
  )

let map_scon_exp (env : env) (x : CST.scon_exp) =
  map_scon env x

let map_scon_pat (env : env) (x : CST.scon_pat) =
  map_scon env x

let map_tyvarseq (env : env) (x : CST.tyvarseq) =
  (match x with
  | `Tyvar x -> R.Case ("Tyvar",
      map_tyvar_ty env x
    )
  | `LPAR_blank_blank_tyvar_rep_COMMA_tyvar_RPAR (v1, v2, v3, v4, v5, v6) -> R.Case ("LPAR_blank_blank_tyvar_rep_COMMA_tyvar_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_line_comment_explicit env v2 in
      let v3 = map_line_comment_explicit env v3 in
      let v4 = map_tyvar_ty env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_tyvar_ty env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

let map_longstrid (env : env) ((v1, v2) : CST.longstrid) =
  let v1 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_strid env v1 in
      let v2 = (* "." *) token env v2 in
      R.Tuple [v1; v2]
    ) v1)
  in
  let v2 = map_strid env v2 in
  R.Tuple [v1; v2]

let map_longtycon (env : env) ((v1, v2) : CST.longtycon) =
  let v1 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_strid env v1 in
      let v2 = (* "." *) token env v2 in
      R.Tuple [v1; v2]
    ) v1)
  in
  let v2 = map_tycon env v2 in
  R.Tuple [v1; v2]

let map_infix_dec (env : env) ((v1, v2, v3) : CST.infix_dec) =
  let v1 = (* "infix" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_pat_213dc3e env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_vid env) v3) in
  R.Tuple [v1; v2; v3]

let map_nonfix_dec (env : env) ((v1, v2) : CST.nonfix_dec) =
  let v1 = (* "nonfix" *) token env v1 in
  let v2 = R.List (List.map (map_vid env) v2) in
  R.Tuple [v1; v2]

let map_longvid (env : env) ((v1, v2) : CST.longvid) =
  let v1 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_strid env v1 in
      let v2 = (* "." *) token env v2 in
      R.Tuple [v1; v2]
    ) v1)
  in
  let v2 = map_vid env v2 in
  R.Tuple [v1; v2]

let map_infixr_dec (env : env) ((v1, v2, v3) : CST.infixr_dec) =
  let v1 = (* "infixr" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_pat_213dc3e env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_vid env) v3) in
  R.Tuple [v1; v2; v3]

let map_sigid_sigexp (env : env) (x : CST.sigid_sigexp) =
  map_sigid env x

let map_typedesc_ (env : env) ((v1, v2) : CST.typedesc_) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tyvarseq env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_tycon env v2 in
  R.Tuple [v1; v2]

let map_strid_strexp (env : env) (x : CST.strid_strexp) =
  map_longstrid env x

let map_sharingtype_spec (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.sharingtype_spec) =
  let v1 = (* "sharing" *) token env v1 in
  let v2 = (* "type" *) token env v2 in
  let v3 = map_line_comment_explicit env v3 in
  let v4 = map_line_comment_explicit env v4 in
  let v5 = map_longtycon env v5 in
  let v6 = (* "=" *) token env v6 in
  let v7 = map_longtycon env v7 in
  let v8 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "=" *) token env v1 in
      let v2 = map_longtycon env v2 in
      R.Tuple [v1; v2]
    ) v8)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_datarepl_dec (env : env) ((v1, v2, v3, v4, v5) : CST.datarepl_dec) =
  let v1 = (* "datatype" *) token env v1 in
  let v2 = map_tycon env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = (* "datatype" *) token env v4 in
  let v5 = map_longtycon env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_datarepl_spec (env : env) ((v1, v2, v3, v4, v5) : CST.datarepl_spec) =
  let v1 = (* "datatype" *) token env v1 in
  let v2 = map_tycon env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = (* "datatype" *) token env v4 in
  let v5 = map_longtycon env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_atty (env : env) (x : CST.atty) =
  (match x with
  | `Tyvar_ty x -> R.Case ("Tyvar_ty",
      map_tyvar_ty env x
    )
  | `Record_ty (v1, v2, v3, v4) -> R.Case ("Record_ty",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_comma env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Opt_ellips_tyrow_opt_rep1_tok_comma opt -> R.Case ("Opt_ellips_tyrow_opt_rep1_tok_comma",
            (match opt with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = map_ellipsis_tyrow env v1 in
                let v2 =
                  (match v2 with
                  | Some xs -> R.Option (Some (
                      R.List (List.map (fun x ->
                        map_tok_comma env x
                      ) xs)
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          )
        | `Tyrow_rep_rep1_tok_comma_tyrow_opt_rep1_tok_comma_ellips_tyrow_opt_rep1_tok_comma (v1, v2, v3) -> R.Case ("Tyrow_rep_rep1_tok_comma_tyrow_opt_rep1_tok_comma_ellips_tyrow_opt_rep1_tok_comma",
            let v1 = map_tyrow env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 =
                  R.List (List.map (fun x ->
                    map_tok_comma env x
                  ) v1)
                in
                let v2 = map_tyrow env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 =
                    R.List (List.map (fun x ->
                      map_tok_comma env x
                    ) v1)
                  in
                  let v2 = map_ellipsis_tyrow env v2 in
                  let v3 =
                    (match v3 with
                    | Some xs -> R.Option (Some (
                        R.List (List.map (fun x ->
                          map_tok_comma env x
                        ) xs)
                      ))
                    | None -> R.Option None)
                  in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Tycon_ty (v1, v2) -> R.Case ("Tycon_ty",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_tyseq env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_longtycon env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_ellipsis_tyrow (env : env) ((v1, v2, v3) : CST.ellipsis_tyrow) =
  let v1 = (* "..." *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_ty env v3 in
  R.Tuple [v1; v2; v3]

and map_fn_ty (env : env) (x : CST.fn_ty) =
  (match x with
  | `Fn_ty_ (v1, v2, v3) -> R.Case ("Fn_ty_",
      let v1 = map_tuple_ty env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_ty env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tuple_ty x -> R.Case ("Tuple_ty",
      map_tuple_ty env x
    )
  )

and map_paren_ty (env : env) (x : CST.paren_ty) =
  (match x with
  | `Paren_ty_ (v1, v2, v3) -> R.Case ("Paren_ty_",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_ty env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Atty x -> R.Case ("Atty",
      map_atty env x
    )
  )

and map_tuple_ty (env : env) (x : CST.tuple_ty) =
  (match x with
  | `Tuple_ty_ (v1, v2, v3, v4, v5, v6) -> R.Case ("Tuple_ty_",
      let v1 = map_line_comment_explicit env v1 in
      let v2 = map_line_comment_explicit env v2 in
      let v3 = map_paren_ty env v3 in
      let v4 = (* "*" *) token env v4 in
      let v5 = map_paren_ty env v5 in
      let v6 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "*" *) token env v1 in
          let v2 = map_paren_ty env v2 in
          R.Tuple [v1; v2]
        ) v6)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Paren_ty x -> R.Case ("Paren_ty",
      map_paren_ty env x
    )
  )

and map_ty (env : env) (x : CST.ty) =
  map_fn_ty env x

and map_tyrow (env : env) ((v1, v2, v3) : CST.tyrow) =
  let v1 = map_lab env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_ty env v3 in
  R.Tuple [v1; v2; v3]

and map_tyseq (env : env) (x : CST.tyseq) =
  (match x with
  | `Atty x -> R.Case ("Atty",
      map_atty env x
    )
  | `LPAR_blank_blank_ty_rep_COMMA_ty_RPAR (v1, v2, v3, v4, v5, v6) -> R.Case ("LPAR_blank_blank_ty_rep_COMMA_ty_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_line_comment_explicit env v2 in
      let v3 = map_line_comment_explicit env v3 in
      let v4 = map_ty env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_ty env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

let map_vid_pat (env : env) ((v1, v2) : CST.vid_pat) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "op" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_longvid env v2 in
  R.Tuple [v1; v2]

let map_typedesc (env : env) ((v1, v2, v3, v4) : CST.typedesc) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_typedesc_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_typedesc_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_open_dec (env : env) ((v1, v2) : CST.open_dec) =
  let v1 = (* "open" *) token env v1 in
  let v2 = R.List (List.map (map_strid_strexp env) v2) in
  R.Tuple [v1; v2]

let map_sharing_spec (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.sharing_spec) =
  let v1 = (* "sharing" *) token env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_line_comment_explicit env v3 in
  let v4 = map_strid_strexp env v4 in
  let v5 = (* "=" *) token env v5 in
  let v6 = map_strid_strexp env v6 in
  let v7 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "=" *) token env v1 in
      let v2 = map_strid_strexp env v2 in
      R.Tuple [v1; v2]
    ) v7)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_typbind_ (env : env) ((v1, v2, v3, v4) : CST.typbind_) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tyvarseq env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_tycon env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_ty env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_condesc_ (env : env) ((v1, v2) : CST.condesc_) =
  let v1 = map_vid env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "of" *) token env v1 in
        let v2 = map_ty env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_conbind_ (env : env) ((v1, v2, v3) : CST.conbind_) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "op" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_vid env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "of" *) token env v1 in
        let v2 = map_ty env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_exdesc_ (env : env) ((v1, v2) : CST.exdesc_) =
  let v1 = map_vid env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "of" *) token env v1 in
        let v2 = map_ty env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_valdesc_ (env : env) ((v1, v2, v3) : CST.valdesc_) =
  let v1 = map_vid env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_ty env v3 in
  R.Tuple [v1; v2; v3]

let rec map_anon_choice_line_comm_expl_b3a4c01 (env : env) (x : CST.anon_choice_line_comm_expl_b3a4c01) =
  (match x with
  | `Blank x -> R.Case ("Blank",
      map_line_comment_explicit env x
    )
  | `Pat_rep_rep1_tok_comma_pat_opt_rep1_tok_comma (v1, v2, v3) -> R.Case ("Pat_rep_rep1_tok_comma_pat_opt_rep1_tok_comma",
      let v1 = map_pat env v1 in
      let v2 = map_anon_rep_rep1_tok_comma_pat_097de3b env v2 in
      let v3 =
        (match v3 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_comma env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_rep_rep1_tok_comma_pat_097de3b (env : env) (xs : CST.anon_rep_rep1_tok_comma_pat_097de3b) =
  R.List (List.map (fun (v1, v2) ->
    let v1 =
      R.List (List.map (fun x ->
        map_tok_comma env x
      ) v1)
    in
    let v2 = map_pat env v2 in
    R.Tuple [v1; v2]
  ) xs)

and map_atpat (env : env) (x : CST.atpat) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Choice_wild_pat x -> R.Case ("Choice_wild_pat",
      (match x with
      | `Wild_pat tok -> R.Case ("Wild_pat",
          (* "_" *) token env tok
        )
      | `Scon_pat x -> R.Case ("Scon_pat",
          map_scon_pat env x
        )
      | `Vid_pat x -> R.Case ("Vid_pat",
          map_vid_pat env x
        )
      | `Record_pat x -> R.Case ("Record_pat",
          map_record_pat env x
        )
      | `Unit_pat x -> R.Case ("Unit_pat",
          map_unit_pat env x
        )
      | `Tuple_pat x -> R.Case ("Tuple_pat",
          map_tuple_pat env x
        )
      | `List_pat x -> R.Case ("List_pat",
          map_list_pat env x
        )
      | `Vec_pat x -> R.Case ("Vec_pat",
          map_vec_pat env x
        )
      | `Paren_pat x -> R.Case ("Paren_pat",
          map_paren_pat env x
        )
      )
    )
  )

and map_ellipsis_listpat (env : env) ((v1, v2, v3) : CST.ellipsis_listpat) =
  let v1 = (* "..." *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_pat env v3 in
  R.Tuple [v1; v2; v3]

and map_ellipsis_patrow (env : env) ((v1, v2) : CST.ellipsis_patrow) =
  let v1 = (* "..." *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_pat env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_list_pat (env : env) ((v1, v2, v3, v4) : CST.list_pat) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some xs -> R.Option (Some (
        R.List (List.map (fun x ->
          map_tok_comma env x
        ) xs)
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Opt_ellips_list_opt_rep1_tok_comma opt -> R.Case ("Opt_ellips_list_opt_rep1_tok_comma",
        (match opt with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_ellipsis_listpat env v1 in
            let v2 =
              (match v2 with
              | Some xs -> R.Option (Some (
                  R.List (List.map (fun x ->
                    map_tok_comma env x
                  ) xs)
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      )
    | `Pat_rep_rep1_tok_comma_pat_opt_rep1_tok_comma_ellips_list_opt_rep1_tok_comma (v1, v2, v3) -> R.Case ("Pat_rep_rep1_tok_comma_pat_opt_rep1_tok_comma_ellips_list_opt_rep1_tok_comma",
        let v1 = map_pat env v1 in
        let v2 = map_anon_rep_rep1_tok_comma_pat_097de3b env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2, v3) -> R.Option (Some (
              let v1 =
                R.List (List.map (fun x ->
                  map_tok_comma env x
                ) v1)
              in
              let v2 = map_ellipsis_listpat env v2 in
              let v3 =
                (match v3 with
                | Some xs -> R.Option (Some (
                    R.List (List.map (fun x ->
                      map_tok_comma env x
                    ) xs)
                  ))
                | None -> R.Option None)
              in
              R.Tuple [v1; v2; v3]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_paren_pat (env : env) ((v1, v2, v3) : CST.paren_pat) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_pat env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pat (env : env) (x : CST.pat) =
  (match x with
  | `Atpat x -> R.Case ("Atpat",
      map_atpat env x
    )
  | `App_pat (v1, v2) -> R.Case ("App_pat",
      let v1 = map_atpat env v1 in
      let v2 = R.List (List.map (map_atpat env) v2) in
      R.Tuple [v1; v2]
    )
  | `Typed_pat (v1, v2, v3) -> R.Case ("Typed_pat",
      let v1 = map_pat env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_ty env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Conj_pat (v1, v2, v3) -> R.Case ("Conj_pat",
      let v1 = map_pat env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_pat env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Disj_pat (v1, v2, v3) -> R.Case ("Disj_pat",
      let v1 = map_pat env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_pat env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_patrow (env : env) (x : CST.patrow) =
  (match x with
  | `Patrow_ (v1, v2, v3) -> R.Case ("Patrow_",
      let v1 = map_lab env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_pat env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Labvar_patrow (v1, v2, v3) -> R.Case ("Labvar_patrow",
      let v1 = map_vid env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_ty env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "as" *) token env v1 in
            let v2 = map_pat env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_record_pat (env : env) ((v1, v2, v3, v4) : CST.record_pat) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some xs -> R.Option (Some (
        R.List (List.map (fun x ->
          map_tok_comma env x
        ) xs)
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Opt_ellips_patrow_opt_rep1_tok_comma opt -> R.Case ("Opt_ellips_patrow_opt_rep1_tok_comma",
        (match opt with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_ellipsis_patrow env v1 in
            let v2 =
              (match v2 with
              | Some xs -> R.Option (Some (
                  R.List (List.map (fun x ->
                    map_tok_comma env x
                  ) xs)
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      )
    | `Patrow_rep_rep1_tok_comma_patrow_opt_rep1_tok_comma_ellips_patrow_opt_rep1_tok_comma (v1, v2, v3) -> R.Case ("Patrow_rep_rep1_tok_comma_patrow_opt_rep1_tok_comma_ellips_patrow_opt_rep1_tok_comma",
        let v1 = map_patrow env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 =
              R.List (List.map (fun x ->
                map_tok_comma env x
              ) v1)
            in
            let v2 = map_patrow env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some (v1, v2, v3) -> R.Option (Some (
              let v1 =
                R.List (List.map (fun x ->
                  map_tok_comma env x
                ) v1)
              in
              let v2 = map_ellipsis_patrow env v2 in
              let v3 =
                (match v3 with
                | Some xs -> R.Option (Some (
                    R.List (List.map (fun x ->
                      map_tok_comma env x
                    ) xs)
                  ))
                | None -> R.Option None)
              in
              R.Tuple [v1; v2; v3]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_tuple_pat (env : env) ((v1, v2, v3, v4) : CST.tuple_pat) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some xs -> R.Option (Some (
        R.List (List.map (fun x ->
          map_tok_comma env x
        ) xs)
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_choice_line_comm_expl_b3a4c01 env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_vec_pat (env : env) ((v1, v2, v3, v4) : CST.vec_pat) =
  let v1 = (* "#[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some xs -> R.Option (Some (
        R.List (List.map (fun x ->
          map_tok_comma env x
        ) xs)
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_choice_line_comm_expl_b3a4c01 env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_eqtype_spec (env : env) ((v1, v2) : CST.eqtype_spec) =
  let v1 = (* "eqtype" *) token env v1 in
  let v2 = map_typedesc env v2 in
  R.Tuple [v1; v2]

let map_typbind (env : env) ((v1, v2, v3, v4) : CST.typbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_typbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_typbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_condesc (env : env) ((v1, v2, v3, v4, v5) : CST.condesc) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tok_bar env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_line_comment_explicit env v3 in
  let v4 = map_condesc_ env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_condesc_ env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

let map_conbind (env : env) ((v1, v2, v3, v4, v5) : CST.conbind) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tok_bar env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_line_comment_explicit env v3 in
  let v4 = map_conbind_ env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_conbind_ env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

let map_exbind_ (env : env) (x : CST.exbind_) =
  (match x with
  | `Opt_op_vid_opt_of_ty x -> R.Case ("Opt_op_vid_opt_of_ty",
      map_conbind_ env x
    )
  | `Opt_op_vid_EQ_opt_op_long (v1, v2, v3, v4, v5) -> R.Case ("Opt_op_vid_EQ_opt_op_long",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "op" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_vid env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "op" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_longvid env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_exdesc (env : env) ((v1, v2, v3, v4) : CST.exdesc) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_exdesc_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_exdesc_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_valdesc (env : env) ((v1, v2, v3, v4) : CST.valdesc) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_valdesc_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_valdesc_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_type_spec (env : env) ((v1, v2) : CST.type_spec) =
  let v1 = (* "type" *) token env v1 in
  let v2 =
    (match v2 with
    | `Type x -> R.Case ("Type",
        map_typedesc env x
      )
    | `Typb x -> R.Case ("Typb",
        map_typbind env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_type_dec (env : env) ((v1, v2) : CST.type_dec) =
  let v1 = (* "type" *) token env v1 in
  let v2 = map_typbind env v2 in
  R.Tuple [v1; v2]

let map_datdesc_ (env : env) ((v1, v2, v3, v4) : CST.datdesc_) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tyvarseq env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_tycon env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_condesc env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_datbind_ (env : env) ((v1, v2, v3, v4) : CST.datbind_) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tyvarseq env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_tycon env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_conbind env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_exbind (env : env) ((v1, v2, v3, v4) : CST.exbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_exbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_exbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_exception_spec (env : env) ((v1, v2) : CST.exception_spec) =
  let v1 = (* "exception" *) token env v1 in
  let v2 = map_exdesc env v2 in
  R.Tuple [v1; v2]

let map_val_spec (env : env) ((v1, v2) : CST.val_spec) =
  let v1 = (* "val" *) token env v1 in
  let v2 = map_valdesc env v2 in
  R.Tuple [v1; v2]

let map_datdesc (env : env) ((v1, v2, v3, v4) : CST.datdesc) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_datdesc_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_datdesc_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_datbind (env : env) ((v1, v2, v3, v4) : CST.datbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_datbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_datbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_exception_dec (env : env) ((v1, v2) : CST.exception_dec) =
  let v1 = (* "exception" *) token env v1 in
  let v2 = map_exbind env v2 in
  R.Tuple [v1; v2]

let map_datatype_spec (env : env) ((v1, v2, v3) : CST.datatype_spec) =
  let v1 = (* "datatype" *) token env v1 in
  let v2 = map_datdesc env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "withtype" *) token env v1 in
        let v2 = map_typbind env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_datatype_dec (env : env) ((v1, v2, v3) : CST.datatype_dec) =
  let v1 = (* "datatype" *) token env v1 in
  let v2 = map_datbind env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "withtype" *) token env v1 in
        let v2 = map_typbind env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let rec map_anon_choice_SEMI_aa47496 (env : env) (x : CST.anon_choice_SEMI_aa47496) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Spec x -> R.Case ("Spec",
      map_spec env x
    )
  )

and map_include_spec (env : env) ((v1, v2) : CST.include_spec) =
  let v1 = (* "include" *) token env v1 in
  let v2 =
    (match v2 with
    | `Sigexp x -> R.Case ("Sigexp",
        map_sigexp env x
      )
    | `Sigid_rep1_sigid (v1, v2) -> R.Case ("Sigid_rep1_sigid",
        let v1 = map_sigid_sigexp env v1 in
        let v2 = R.List (List.map (map_sigid_sigexp env) v2) in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

and map_sigexp (env : env) (x : CST.sigexp) =
  (match x with
  | `Sig_sigexp (v1, v2, v3) -> R.Case ("Sig_sigexp",
      let v1 = (* "sig" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_SEMI_aa47496 env) v2)
      in
      let v3 = (* "end" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Sigid_sigexp x -> R.Case ("Sigid_sigexp",
      map_sigid_sigexp env x
    )
  | `Wher_sigexp (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("Wher_sigexp",
      let v1 = map_sigexp env v1 in
      let v2 = (* "where" *) token env v2 in
      let v3 = map_line_comment_explicit env v3 in
      let v4 = map_line_comment_explicit env v4 in
      let v5 = (* "type" *) token env v5 in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_tyvarseq env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_longtycon env v7 in
      let v8 = (* "=" *) token env v8 in
      let v9 = map_ty env v9 in
      let v10 =
        R.List (List.map (fun (v1, v2, v3, v4, v5, v6) ->
          let v1 = (* "and" *) token env v1 in
          let v2 = (* "type" *) token env v2 in
          let v3 =
            (match v3 with
            | Some x -> R.Option (Some (
                map_tyvarseq env x
              ))
            | None -> R.Option None)
          in
          let v4 = map_longtycon env v4 in
          let v5 = (* "=" *) token env v5 in
          let v6 = map_ty env v6 in
          R.Tuple [v1; v2; v3; v4; v5; v6]
        ) v10)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  )

and map_spec (env : env) (x : CST.spec) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Choice_val_spec x -> R.Case ("Choice_val_spec",
      (match x with
      | `Val_spec x -> R.Case ("Val_spec",
          map_val_spec env x
        )
      | `Type_spec x -> R.Case ("Type_spec",
          map_type_spec env x
        )
      | `Eqtype_spec x -> R.Case ("Eqtype_spec",
          map_eqtype_spec env x
        )
      | `Data_spec_d5fa734 x -> R.Case ("Data_spec_d5fa734",
          map_datatype_spec env x
        )
      | `Data_spec_86dd5a7 x -> R.Case ("Data_spec_86dd5a7",
          map_datarepl_spec env x
        )
      | `Exc_spec x -> R.Case ("Exc_spec",
          map_exception_spec env x
        )
      | `Stru_spec x -> R.Case ("Stru_spec",
          map_structure_spec env x
        )
      | `Incl_spec x -> R.Case ("Incl_spec",
          map_include_spec env x
        )
      | `Shar_spec_5735bb3 x -> R.Case ("Shar_spec_5735bb3",
          map_sharingtype_spec env x
        )
      | `Shar_spec_a6177ec x -> R.Case ("Shar_spec_a6177ec",
          map_sharing_spec env x
        )
      )
    )
  )

and map_strdesc (env : env) ((v1, v2, v3, v4) : CST.strdesc) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_strdesc_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_strdesc_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_strdesc_ (env : env) ((v1, v2, v3) : CST.strdesc_) =
  let v1 = map_strid env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_sigexp env v3 in
  R.Tuple [v1; v2; v3]

and map_structure_spec (env : env) ((v1, v2) : CST.structure_spec) =
  let v1 = (* "structure" *) token env v1 in
  let v2 = map_strdesc env v2 in
  R.Tuple [v1; v2]

let rec map_abstype_dec (env : env) ((v1, v2, v3, v4, v5, v6) : CST.abstype_dec) =
  let v1 = (* "abstype" *) token env v1 in
  let v2 = map_datbind env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "withtype" *) token env v1 in
        let v2 = map_typbind env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* "with" *) token env v4 in
  let v5 =
    R.List (List.map (map_anon_choice_SEMI_d1f1a0a env) v5)
  in
  let v6 = (* "end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_anon_choice_SEMI_d1f1a0a (env : env) (x : CST.anon_choice_SEMI_d1f1a0a) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Dec x -> R.Case ("Dec",
      map_dec env x
    )
  )

and map_anon_choice_line_comm_expl_8d74692 (env : env) (x : CST.anon_choice_line_comm_expl_8d74692) =
  (match x with
  | `Blank x -> R.Case ("Blank",
      map_line_comment_explicit env x
    )
  | `Exp_rep_rep1_tok_comma_exp_opt_rep1_tok_comma (v1, v2, v3) -> R.Case ("Exp_rep_rep1_tok_comma_exp_opt_rep1_tok_comma",
      let v1 = map_exp env v1 in
      let v2 = map_anon_rep_rep1_tok_comma_exp_3e034b9 env v2 in
      let v3 =
        (match v3 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_comma env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_rep_rep1_tok_comma_exp_3e034b9 (env : env) (xs : CST.anon_rep_rep1_tok_comma_exp_3e034b9) =
  R.List (List.map (fun (v1, v2) ->
    let v1 =
      R.List (List.map (fun x ->
        map_tok_comma env x
      ) v1)
    in
    let v2 = map_exp env v2 in
    R.Tuple [v1; v2]
  ) xs)

and map_anon_rep_rep1_tok_semi_exp_116885f (env : env) (xs : CST.anon_rep_rep1_tok_semi_exp_116885f) =
  R.List (List.map (fun (v1, v2) ->
    let v1 =
      R.List (List.map (fun x ->
        map_tok_semi env x
      ) v1)
    in
    let v2 = map_exp env v2 in
    R.Tuple [v1; v2]
  ) xs)

and map_atexp (env : env) (x : CST.atexp) =
  (match x with
  | `Scon_exp x -> R.Case ("Scon_exp",
      map_scon_exp env x
    )
  | `Vid_exp (v1, v2) -> R.Case ("Vid_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "op" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_longvid env v2 in
      R.Tuple [v1; v2]
    )
  | `Record_exp (v1, v2, v3, v4) -> R.Case ("Record_exp",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_comma env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Opt_ellips_exprow_opt_rep1_tok_comma opt -> R.Case ("Opt_ellips_exprow_opt_rep1_tok_comma",
            (match opt with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = map_ellipsis_exprow env v1 in
                let v2 =
                  (match v2 with
                  | Some xs -> R.Option (Some (
                      R.List (List.map (fun x ->
                        map_tok_comma env x
                      ) xs)
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          )
        | `Exprow_rep_rep1_tok_comma_exprow_opt_rep1_tok_comma_ellips_exprow_opt_rep1_tok_comma (v1, v2, v3) -> R.Case ("Exprow_rep_rep1_tok_comma_exprow_opt_rep1_tok_comma_ellips_exprow_opt_rep1_tok_comma",
            let v1 = map_exprow env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 =
                  R.List (List.map (fun x ->
                    map_tok_comma env x
                  ) v1)
                in
                let v2 = map_exprow env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 =
                    R.List (List.map (fun x ->
                      map_tok_comma env x
                    ) v1)
                  in
                  let v2 = map_ellipsis_exprow env v2 in
                  let v3 =
                    (match v3 with
                    | Some xs -> R.Option (Some (
                        R.List (List.map (fun x ->
                          map_tok_comma env x
                        ) xs)
                      ))
                    | None -> R.Option None)
                  in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Reco_exp (v1, v2) -> R.Case ("Reco_exp",
      let v1 = (* "#" *) token env v1 in
      let v2 = map_lab env v2 in
      R.Tuple [v1; v2]
    )
  | `Unit_exp (v1, v2) -> R.Case ("Unit_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Tuple_exp (v1, v2, v3, v4) -> R.Case ("Tuple_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_comma env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v3 = map_anon_choice_line_comm_expl_8d74692 env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `List_exp (v1, v2, v3, v4) -> R.Case ("List_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_comma env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Opt_ellips_list_opt_rep1_tok_comma opt -> R.Case ("Opt_ellips_list_opt_rep1_tok_comma",
            (match opt with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = map_ellipsis_listexp env v1 in
                let v2 =
                  (match v2 with
                  | Some xs -> R.Option (Some (
                      R.List (List.map (fun x ->
                        map_tok_comma env x
                      ) xs)
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          )
        | `Exp_rep_rep1_tok_comma_exp_opt_rep1_tok_comma_ellips_list_opt_rep1_tok_comma (v1, v2, v3) -> R.Case ("Exp_rep_rep1_tok_comma_exp_opt_rep1_tok_comma_ellips_list_opt_rep1_tok_comma",
            let v1 = map_exp env v1 in
            let v2 = map_anon_rep_rep1_tok_comma_exp_3e034b9 env v2 in
            let v3 =
              (match v3 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 =
                    R.List (List.map (fun x ->
                      map_tok_comma env x
                    ) v1)
                  in
                  let v2 = map_ellipsis_listexp env v2 in
                  let v3 =
                    (match v3 with
                    | Some xs -> R.Option (Some (
                        R.List (List.map (fun x ->
                          map_tok_comma env x
                        ) xs)
                      ))
                    | None -> R.Option None)
                  in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Vec_exp (v1, v2, v3, v4) -> R.Case ("Vec_exp",
      let v1 = (* "#[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_comma env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v3 = map_anon_choice_line_comm_expl_8d74692 env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Seq_exp (v1, v2, v3, v4) -> R.Case ("Seq_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_semi env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Blank x -> R.Case ("Blank",
            map_line_comment_explicit env x
          )
        | `Exp_rep_rep1_tok_semi_exp_opt_rep1_tok_semi (v1, v2, v3) -> R.Case ("Exp_rep_rep1_tok_semi_exp_opt_rep1_tok_semi",
            let v1 = map_exp env v1 in
            let v2 = map_anon_rep_rep1_tok_semi_exp_116885f env v2 in
            let v3 =
              (match v3 with
              | Some xs -> R.Option (Some (
                  R.List (List.map (fun x ->
                    map_tok_semi env x
                  ) xs)
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Let_exp (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Let_exp",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_SEMI_d1f1a0a env) v2)
      in
      let v3 = (* "in" *) token env v3 in
      let v4 =
        (match v4 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_semi env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v5 = map_line_comment_explicit env v5 in
      let v6 = map_exp env v6 in
      let v7 = map_anon_rep_rep1_tok_semi_exp_116885f env v7 in
      let v8 =
        (match v8 with
        | Some xs -> R.Option (Some (
            R.List (List.map (fun x ->
              map_tok_semi env x
            ) xs)
          ))
        | None -> R.Option None)
      in
      let v9 = (* "end" *) token env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Paren_exp (v1, v2, v3) -> R.Case ("Paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_dec (env : env) (x : CST.dec) =
  (match x with
  | `Dec_no_local x -> R.Case ("Dec_no_local",
      map_dec_no_local env x
    )
  | `Local_dec (v1, v2, v3, v4, v5) -> R.Case ("Local_dec",
      let v1 = (* "local" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_SEMI_d1f1a0a env) v2)
      in
      let v3 = (* "in" *) token env v3 in
      let v4 =
        R.List (List.map (map_anon_choice_SEMI_d1f1a0a env) v4)
      in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_dec_no_local (env : env) (x : CST.dec_no_local) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Choice_do_dec x -> R.Case ("Choice_do_dec",
      (match x with
      | `Do_dec x -> R.Case ("Do_dec",
          map_do_dec env x
        )
      | `Val_dec x -> R.Case ("Val_dec",
          map_val_dec env x
        )
      | `Fun_dec x -> R.Case ("Fun_dec",
          map_fun_dec env x
        )
      | `Type_dec x -> R.Case ("Type_dec",
          map_type_dec env x
        )
      | `Data_dec_c9b9a7a x -> R.Case ("Data_dec_c9b9a7a",
          map_datatype_dec env x
        )
      | `Data_dec_4d86b7f x -> R.Case ("Data_dec_4d86b7f",
          map_datarepl_dec env x
        )
      | `Abst_dec x -> R.Case ("Abst_dec",
          map_abstype_dec env x
        )
      | `Exc_dec x -> R.Case ("Exc_dec",
          map_exception_dec env x
        )
      | `Open_dec x -> R.Case ("Open_dec",
          map_open_dec env x
        )
      | `Infix_dec x -> R.Case ("Infix_dec",
          map_infix_dec env x
        )
      | `Infixr_dec x -> R.Case ("Infixr_dec",
          map_infixr_dec env x
        )
      | `Nonfix_dec x -> R.Case ("Nonfix_dec",
          map_nonfix_dec env x
        )
      )
    )
  )

and map_do_dec (env : env) ((v1, v2) : CST.do_dec) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_exp env v2 in
  R.Tuple [v1; v2]

and map_ellipsis_exprow (env : env) ((v1, v2, v3) : CST.ellipsis_exprow) =
  let v1 = (* "..." *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_exp env v3 in
  R.Tuple [v1; v2; v3]

and map_ellipsis_listexp (env : env) ((v1, v2, v3) : CST.ellipsis_listexp) =
  let v1 = (* "..." *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_exp env v3 in
  R.Tuple [v1; v2; v3]

and map_exp (env : env) (x : CST.exp) =
  (match x with
  | `Atexp x -> R.Case ("Atexp",
      map_atexp env x
    )
  | `App_exp (v1, v2) -> R.Case ("App_exp",
      let v1 = map_atexp env v1 in
      let v2 = R.List (List.map (map_atexp env) v2) in
      R.Tuple [v1; v2]
    )
  | `Typed_exp (v1, v2, v3) -> R.Case ("Typed_exp",
      let v1 = map_exp env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_ty env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Conj_exp (v1, v2, v3) -> R.Case ("Conj_exp",
      let v1 = map_exp env v1 in
      let v2 = (* "andalso" *) token env v2 in
      let v3 = map_exp env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Disj_exp (v1, v2, v3) -> R.Case ("Disj_exp",
      let v1 = map_exp env v1 in
      let v2 = (* "orelse" *) token env v2 in
      let v3 = map_exp env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Handle_exp (v1, v2, v3) -> R.Case ("Handle_exp",
      let v1 = map_exp env v1 in
      let v2 = (* "handle" *) token env v2 in
      let v3 = map_match_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Raise_exp (v1, v2) -> R.Case ("Raise_exp",
      let v1 = (* "raise" *) token env v1 in
      let v2 = map_exp env v2 in
      R.Tuple [v1; v2]
    )
  | `Cond_exp (v1, v2, v3, v4, v5) -> R.Case ("Cond_exp",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 = (* "then" *) token env v3 in
      let v4 = map_exp env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_exp env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Iter_exp (v1, v2, v3, v4) -> R.Case ("Iter_exp",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 = (* "do" *) token env v3 in
      let v4 = map_exp env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Case_exp (v1, v2, v3, v4) -> R.Case ("Case_exp",
      let v1 = (* "case" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 = (* "of" *) token env v3 in
      let v4 = map_match_ env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Fn_exp (v1, v2) -> R.Case ("Fn_exp",
      let v1 = (* "fn" *) token env v1 in
      let v2 = map_match_ env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_exprow (env : env) (x : CST.exprow) =
  (match x with
  | `Exprow_ (v1, v2, v3) -> R.Case ("Exprow_",
      let v1 = map_lab env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_exp env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Labvar_exprow (v1, v2) -> R.Case ("Labvar_exprow",
      let v1 = map_vid env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_ty env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_fmatch (env : env) ((v1, v2, v3, v4, v5) : CST.fmatch) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tok_bar env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_line_comment_explicit env v3 in
  let v4 = map_fmrule env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_fmrule env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_fmrule (env : env) ((v1, v2, v3, v4) : CST.fmrule) =
  let v1 =
    (match v1 with
    | `Opt_op_vid_rep1_atpat (v1, v2, v3) -> R.Case ("Opt_op_vid_rep1_atpat",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "op" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = map_vid env v2 in
        let v3 = R.List (List.map (map_atpat env) v3) in
        R.Tuple [v1; v2; v3]
      )
    | `LPAR_atpat_vid_atpat_RPAR_rep_atpat (v1, v2, v3, v4, v5, v6) -> R.Case ("LPAR_atpat_vid_atpat_RPAR_rep_atpat",
        let v1 = (* "(" *) token env v1 in
        let v2 = map_atpat env v2 in
        let v3 = map_vid env v3 in
        let v4 = map_atpat env v4 in
        let v5 = (* ")" *) token env v5 in
        let v6 = R.List (List.map (map_atpat env) v6) in
        R.Tuple [v1; v2; v3; v4; v5; v6]
      )
    | `Atpat_vid_atpat (v1, v2, v3) -> R.Case ("Atpat_vid_atpat",
        let v1 = map_atpat env v1 in
        let v2 = map_vid env v2 in
        let v3 = map_atpat env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_ty env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_exp env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_fun_dec (env : env) ((v1, v2, v3) : CST.fun_dec) =
  let v1 = (* "fun" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_tyvarseq env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_fvalbind env v3 in
  R.Tuple [v1; v2; v3]

and map_fvalbind (env : env) ((v1, v2, v3, v4) : CST.fvalbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_fvalbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_fvalbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_fvalbind_ (env : env) (x : CST.fvalbind_) =
  map_fmatch env x

and map_match_ (env : env) ((v1, v2, v3, v4, v5) : CST.match_) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_tok_bar env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_line_comment_explicit env v3 in
  let v4 = map_mrule env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_mrule env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_mrule (env : env) (x : CST.mrule) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Pat_EQGT_exp (v1, v2, v3) -> R.Case ("Pat_EQGT_exp",
      let v1 = map_pat env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_exp env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_val_dec (env : env) ((v1, v2, v3, v4) : CST.val_dec) =
  let v1 = (* "val" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "rec" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_tyvarseq env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_valbind env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_valbind (env : env) ((v1, v2, v3, v4) : CST.valbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_valbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_valbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_valbind_ (env : env) ((v1, v2, v3) : CST.valbind_) =
  let v1 = map_pat env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_exp env v3 in
  R.Tuple [v1; v2; v3]

let map_sigbind_ (env : env) ((v1, v2, v3) : CST.sigbind_) =
  let v1 = map_sigid_sigexp env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_sigexp env v3 in
  R.Tuple [v1; v2; v3]

let rec map_anon_choice_SEMI_338101e (env : env) (x : CST.anon_choice_SEMI_338101e) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Strdec x -> R.Case ("Strdec",
      map_strdec env x
    )
  )

and map_strbind (env : env) ((v1, v2, v3, v4) : CST.strbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_strbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_strbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_strbind_ (env : env) ((v1, v2, v3, v4) : CST.strbind_) =
  let v1 = map_strid env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_COLON_49dafc4 env v1 in
        let v2 = map_sigexp env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_strexp env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_strdec (env : env) (x : CST.strdec) =
  (match x with
  | `Dec_no_local x -> R.Case ("Dec_no_local",
      map_dec_no_local env x
    )
  | `Stru_strdec (v1, v2) -> R.Case ("Stru_strdec",
      let v1 = (* "structure" *) token env v1 in
      let v2 = map_strbind env v2 in
      R.Tuple [v1; v2]
    )
  | `Local_strdec (v1, v2, v3, v4, v5) -> R.Case ("Local_strdec",
      let v1 = (* "local" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_SEMI_338101e env) v2)
      in
      let v3 = (* "in" *) token env v3 in
      let v4 =
        R.List (List.map (map_anon_choice_SEMI_338101e env) v4)
      in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_strexp (env : env) (x : CST.strexp) =
  (match x with
  | `Struct_strexp (v1, v2, v3) -> R.Case ("Struct_strexp",
      let v1 = (* "struct" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_SEMI_338101e env) v2)
      in
      let v3 = (* "end" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Strid_strexp x -> R.Case ("Strid_strexp",
      map_strid_strexp env x
    )
  | `Constr_strexp (v1, v2, v3) -> R.Case ("Constr_strexp",
      let v1 = map_strexp env v1 in
      let v2 = map_anon_choice_COLON_49dafc4 env v2 in
      let v3 = map_sigexp env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Fctapp_strexp (v1, v2, v3, v4) -> R.Case ("Fctapp_strexp",
      let v1 = map_fctid env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Strexp x -> R.Case ("Strexp",
            map_strexp env x
          )
        | `Rep_choice_SEMI xs -> R.Case ("Rep_choice_SEMI",
            R.List (List.map (map_anon_choice_SEMI_338101e env) xs)
          )
        )
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Let_strexp (v1, v2, v3, v4, v5) -> R.Case ("Let_strexp",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_SEMI_338101e env) v2)
      in
      let v3 = (* "in" *) token env v3 in
      let v4 = map_strexp env v4 in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_sigbind (env : env) ((v1, v2, v3, v4) : CST.sigbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_sigbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_sigbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_fctbind_ (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.fctbind_) =
  let v1 = map_fctid env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | `Strid_COLON_sigexp x -> R.Case ("Strid_COLON_sigexp",
        map_strdesc_ env x
      )
    | `Rep_choice_SEMI xs -> R.Case ("Rep_choice_SEMI",
        R.List (List.map (map_anon_choice_SEMI_aa47496 env) xs)
      )
    )
  in
  let v4 = (* ")" *) token env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_COLON_49dafc4 env v1 in
        let v2 = map_sigexp env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 = (* "=" *) token env v6 in
  let v7 = map_strexp env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_fctbind (env : env) ((v1, v2, v3, v4) : CST.fctbind) =
  let v1 = map_line_comment_explicit env v1 in
  let v2 = map_line_comment_explicit env v2 in
  let v3 = map_fctbind_ env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_fctbind_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_sigdec (env : env) (x : CST.sigdec) =
  (match x with
  | `Sign_sigdec (v1, v2) -> R.Case ("Sign_sigdec",
      let v1 = (* "signature" *) token env v1 in
      let v2 = map_sigbind env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_topdec (env : env) (x : CST.topdec) =
  (match x with
  | `Strdec x -> R.Case ("Strdec",
      map_strdec env x
    )
  | `Sigdec x -> R.Case ("Sigdec",
      map_sigdec env x
    )
  | `Fctdec (v1, v2) -> R.Case ("Fctdec",
      let v1 = (* "functor" *) token env v1 in
      let v2 = map_fctbind env v2 in
      R.Tuple [v1; v2]
    )
  )

let rec map_program (env : env) (x : CST.program) =
  (match x with
  | `Rectype (v1, v2) -> R.Case ("Rectype",
      let v1 =
        (match v1 with
        | `Rep1_topdec xs -> R.Case ("Rep1_topdec",
            R.List (List.map (map_topdec env) xs)
          )
        | `Exp x -> R.Case ("Exp",
            map_exp env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ";" *) token env v1 in
            let v2 = map_source_file env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_source_file (env : env) (opt : CST.source_file) =
  (match opt with
  | Some x -> R.Option (Some (
      map_program env x
    ))
  | None -> R.Option None)

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Block_comment (_loc, x) -> ("block_comment", "block_comment", map_block_comment env x)
  | `Line_comment (_loc, x) -> ("line_comment", "line_comment", map_line_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
