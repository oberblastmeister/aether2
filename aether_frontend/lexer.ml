open Core
open Token
open Lexing
open Lexer_generated
open Options

(* This lexer chooses between [inital] or [initial_linebegin],
   depending on whether we are at the beginning of the line or
   not. *)

let lexer : lexbuf -> token =
  fun lexbuf ->
  if lexbuf.lex_curr_p.pos_cnum = lexbuf.lex_curr_p.pos_bol
  then initial_linebegin lexbuf
  else initial lexbuf
;;

(* In the following, we define a new lexer, which wraps [lexer], and applies
   the following two transformations to the token stream:

   - A [NAME] token is replaced with a sequence of either [NAME VARIABLE] or
     [NAME TYPE]. The decision is made via a call to [Context.is_typedefname].
     The call takes place only when the second element of the sequence is
     demanded.

   - When [Options.atomic_strict_syntax] is [true] and an opening parenthesis
     [LPAREN] follows an [ATOMIC] keyword, the parenthesis is replaced by a
     special token, [ATOMIC_LPAREN], so as to allow the parser to treat it
     specially. *)

(* This second lexer is implemented using a 3-state state machine, whose
   states are as follows. *)

type lexer_state =
  | SRegular (* Nothing to recall from the previous tokens. *)
  | SAtomic
    (* The previous token was [ATOMIC]. If an opening
       parenthesis follows, then it needs special care. *)
  | SIdent of string
(* We have seen an identifier: we have just
   emitted a [NAME] token. The next token will be
   either [VARIABLE] or [TYPE], depending on
   what kind of identifier this is. *)

let lexer ~is_typedefname : (lexbuf -> token) Staged.t =
  let st = ref SRegular in
  Staged.stage
  @@ fun lexbuf ->
  match !st with
  | SIdent id ->
    st := SRegular;
    if is_typedefname id then TYPE else VARIABLE
  | SAtomic | SRegular ->
    let token = lexer lexbuf in
    (match !st, token with
     | _, NAME id ->
       st := SIdent id;
       token
     | SAtomic, LPAREN ->
       st := SRegular;
       ATOMIC_LPAREN
     | _, ATOMIC ->
       st := if !atomic_strict_syntax then SAtomic else SRegular;
       token
     | _, _ ->
       st := SRegular;
       token)
;;
