(*
   Convert Bash-specific AST to generic AST.

   A Bash program is organized into the following hierarchy:

   - List: a list of pipelines. A whole program is a list.

       echo hello | tr h y; echo bye

   - Pipeline: a list of commands whose standard output and input are
     connected

       echo hello | tr h y

   - Command: everything else including calls to external programs and
     control structures that the language has to offer.

     Simple command:

       echo hello

     For loop, a compound command:

       for i in 1 2 3; do echo "$i"; done

     Variable assignment:

       answer=42

     + many others

   - Literals: space-separated elements within a simple command, which will
     be expanded into a possibly different number of elements.

       hello
       "$x"
       $items
       "$(cat foo/bar)"
       $?

       etc.

   Mapping to the generic AST:

   We consider that bash is a domain-specific language. It has a
   variety of syntactic constructs that can be expressed with
   functions in general-purpose programming languages. The whole
   program is considered an expression to be evaluated by the shell
   runtime. The program is more like data than statements, so we
   translate it to an expression. The translation goes roughly like this:

   list -> Constructor ("!sh_list!", pipelines)
   pipeline -> Constructor ("!sh_pipeline!", commands)
   simple command -> Constructor ("!sh_simple_cmd!", arguments)
   literal 'hello' -> L ("hello")
   simple $ expansion -> Constructor ("!sh_expand!",
                           Constructor("!sh_get!", [var_name])
                         )
   ${x#.c} expansion -> Constructor ("!sh_expand!", [
                          Constructor ("!sh_rm_prefix!",
                            [Constructor("!sh_get!", [var_name]);
                             L (".c")]
                          )]
                        )
*)

open! Common
open AST_bash
module PI = Parse_info
module G = AST_generic

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str (tok : Parse_info.t) : string * tok = (PI.str_of_info tok, tok)

(*****************************************************************************)
(* Builtin Bash constructors *)
(*****************************************************************************)

let mkcons s =
  let name = "!sh_" ^ s ^ "!" in
  (name, G.fake name)

let sh_list = lazy (mkcons "list")

let sh_ctrl = lazy (mkcons "ctrl")

let sh_pipeline = lazy (mkcons "pipeline")

let sh_simple_cmd = lazy (mkcons "simple_cmd")

let sh_get = lazy (mkcons "get")

let sh_expand = lazy (mkcons "expand")

(*
   'cons' and 'invis' are for representing most nodes of a shell program.

   Use 'cons' when the original construct starts with a token which
   otherwise would be discarded. This allows accurate location tracking.

   Sample usage:
     cons dollar_tok sh_get [var_name]
*)
let cons start_tok lazy_cons_name expressions =
  G.Constructor ([ (snd (Lazy.force lazy_cons_name), start_tok) ], expressions)

(*
   Use 'invis' when the first token of the first argument is also
   the first token of the whole construct. This is the case for simple
   commands such as 'echo hello'.

   Sample usage:
     invis sh_pipeline [cmd1; cmd2]
*)
let invis lazy_cons_name expressions =
  G.Constructor ([ Lazy.force lazy_cons_name ], expressions)

(*
   This is broken because we should represent the whole region, not
   just its first token. Can we have a location = (start, end_)
   systematically for each node of the generic AST?
*)
let expr_todo tokens =
  let any = List.map (fun tok -> G.TodoK (str tok)) tokens in
  G.OtherExpr (G.OE_Todo, any)

(*****************************************************************************)
(* Mapping *)
(*****************************************************************************)

let pipeline_control_operator = function
  | Foreground tok (* ';' or '\n' or ';;' *)
  | Background tok (* & *)
  | And tok (* && *)
  | Or tok (* || *) ->
      G.String (str tok)

let rec list_ (x : list_) : G.expr = invis sh_list (List.map pipeline x)

and pipeline ((cmds, control_op) : pipeline) : G.expr =
  invis sh_ctrl [ bare_pipeline cmds; pipeline_control_operator control_op ]

and bare_pipeline (cmds : command list) : G.expr =
  List.filter_map
    (fun (_opt_bar, cmd_redir) -> command_with_redirects cmd_redir)
    commands

and command_with_redirects (x : command_with_redirects) : G.stmt option =
  let { command = cmd; redirects } = x in
  ignore redirects;
  command cmd

and command (cmd : command) : G.stmt option =
  match cmd with
  | Simple_command { assignments = _; arguments } -> (
      match arguments with
      | [] -> None
      | arg0 :: args ->
          let args = List.map (fun x -> G.Arg (expression x)) args in
          let e = G.Call (expression arg0, G.fake_bracket args) in
          Some (G.s (ExprStmt (e, G.fake ""))) )
  | Compound_command _ -> None
  | Coprocess _ -> None
  | Assignment _ -> None
  | Declaration _ -> None
  | Negated_command _ -> None
  | Function_definition _ -> None

and expression (e : expression) : G.expr =
  let todo = G.L (String ("", G.fake "")) in
  match e with
  | Word x -> G.L (G.Atom (G.fake "", x))
  | String _ -> todo
  | String_fragment frag -> (
      match frag with
      | String_content x -> G.L (G.Atom (G.fake "", x))
      | Expansion ex -> expansion ex
      | Command_substitution (_open, _x, _close) -> todo )
  | Raw_string _ -> todo
  | Ansii_c_string _ -> todo
  | Special_character _ -> todo
  | String_expansion _ -> todo
  | Concatenation _ -> todo
  | Semgrep_ellipsis _ -> todo
  | Semgrep_metavariable _ -> todo
  | Expression_TODO -> todo

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (x : expansion) : G.expr =
  let todo = G.L (String ("", G.fake "")) in
  match x with
  | Simple_expansion (dollar_tok, var_name) ->
      let arg =
        match var_name with
        | Simple_variable_name name | Special_variable_name name ->
            G.Arg (G.N (G.Id (name, G.empty_id_info ())))
      in
      let function_ = G.N (G.Id (("$", dollar_tok), G.empty_id_info ())) in
      let e = G.Call (function_, G.fake_bracket [ arg ]) in
      e
  | Complex_expansion _ -> todo

let program x = list_ x

let any x = G.Ss (program x)
