(*
Jacques-Henri Jourdan, Inria Paris
François Pottier, Inria Paris

Copyright (c) 2016-2017, Inria
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Inria nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL INRIA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(* WARNING. When processing this grammar, Menhir should announce that
   ONE shift/reduce conflict was silently solved and that ONE state
   has 3 reduce/reduce conflicts on RPAREN, LPAREN, and LBRACK. If you
   modify the grammar, you should check that this is still the case. *)

%{
open Core
module Declarator = Declarator.Make (Context)
open Declarator
open Context
open Folds.O

let todo pos = raise_s [%sexp "TODO", (pos : Source_code_position.t)]
%}

(* This makes the global state a functor parameter, so that the parser is reentrant *)
%parameter <Context:Context.S>

%token<string> NAME
%token VARIABLE TYPE
%token<Ast.encoded_string> STRING_LITERAL
%token<Ast.encoded_string> CHAR_LITERAL
%token<string> INT_LITERAL
%token CONSTANT
%token ALIGNAS "_Alignas"
%token ALIGNOF "_Alignof"
%token ATOMIC "_Atomic"
%token AUTO "auto"
%token BOOL "_Bool"
%token BREAK "break"
%token CASE "case"
%token CHAR "char"
%token COMPLEX "_Complex"
%token CONST "const"
%token CONTINUE "continue"
%token DEFAULT "default"
%token DO "do"
%token DOUBLE "double"
%token ELSE "else"
%token ENUM "enum"
%token EXTERN "extern"
%token FLOAT "float"
%token FOR "for"
%token GENERIC "_Generic"
%token GOTO "goto"
%token IF "if"
%token IMAGINARY "_Imaginary"
%token INLINE "inline"
%token INT "int"
%token LONG "long"
%token NORETURN "_Noreturn"
%token REGISTER "register"
%token RESTRICT "restrict"
%token RETURN "return"
%token SHORT "short"
%token SIGNED "signed"
%token SIZEOF "sizeof"
%token STATIC "static"
%token STATIC_ASSERT "_Static_assert"
%token STRUCT "struct"
%token SWITCH "switch"
%token THREAD_LOCAL "_Thread_local"
%token TYPEDEF "typedef"
%token UNION "union"
%token UNSIGNED "unsigned"
%token VOID "void"
%token VOLATILE "volatile"
%token WHILE "while"

%token ELLIPSIS "..."
%token ADD_ASSIGN "+="
%token SUB_ASSIGN "-="
%token MUL_ASSIGN "*="
%token DIV_ASSIGN "/="
%token MOD_ASSIGN "%="
%token OR_ASSIGN "|="
%token AND_ASSIGN "&="
%token XOR_ASSIGN "^="
%token LEFT_ASSIGN "<<="
%token RIGHT_ASSIGN ">>="
%token LEFT "<<"
%token RIGHT ">>"
%token EQEQ "=="
%token NEQ "!="
%token LEQ "<="
%token GEQ ">="
%token EQ "="
%token LT "<"
%token GT ">"
%token INC "++"
%token DEC "--"
%token PTR "->"
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token PERCENT "%"
%token BANG "!"
%token ANDAND "&&"
%token BARBAR "||"
%token AND "&"
%token BAR "|"
%token HAT "^"
%token QUESTION "?"
%token COLON ":"
%token TILDE "~"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token LPAREN "("
%token RPAREN ")"
%token SEMICOLON ";"
%token COMMA ","
%token DOT "."

(* ATOMIC_LPAREN is "special"; it's used for left parentheses that
   follow the ["_Atomic"] keyword. It isn't given a token alias *)
%token ATOMIC_LPAREN

%token EOF

%type<context> save_context 
%type<context * Ast.spec list * Ast.decl_name> function_definition1
%type<context * Ast.param list * bool> parameter_type_list
%type<string> typedef_name var_name general_identifier enumeration_constant
%type<declarator * Ast.decl_name> declarator direct_declarator declarator_varname declarator_typedefname
%type<Ast.expr>
    primary_expression generic_selection constant_expression
    postfix_expression unary_expression cast_expression multiplicative_expression additive_expression
    shift_expression relational_expression equality_expression and_expression 
    exclusive_or_expression inclusive_or_expression logical_and_expression logical_or_expression
    conditional_expression assignment_expression expression
%type<Ast.param> parameter_declaration
%type<Ast.decl_type> direct_abstract_declarator
%type<Ast.stmt> selection_statement statement

(* There is a reduce/reduce conflict in the grammar. It corresponds to the
   conflict in the second declaration in the following snippet:

     typedef int T;
     int f(int(T));

   It is specified by 6.7.6.3 11: 'T' should be taken as the type of the
   parameter of the anonymous function taken as a parameter by f (thus,
   f has type (T -> int) -> int).

   The reduce/reduce conflict is solved by letting menhir reduce the production
   appearing first in this file. This is the reason why we have the
   [typedef_name_spec] proxy: it is here just to make sure the conflicting
   production appears before the other (which concerns [general_identifier]). *)

(* These precedence declarations solve the dangling else conflict. *)
%nonassoc below_ELSE
%nonassoc ELSE

%start<Ast.decl list> translation_unit_file

%%

(* Helpers *)

(* [option(X)] represents a choice between nothing and [X].
   [ioption(X)] is the same thing, but is inlined at its use site,
   which in some cases is necessary in order to avoid a conflict.
   By convention, [X?] is syntactic sugar for [option(X)]. *)

%inline ioption(X):
| (* nothing *) { None }
| x=X { Some x }

option(X):
| o = ioption(X) { o }

(* By convention, [X*] is syntactic sugar for [list(X)]. *)

list(X):
| (* nothing *) { [] }
| x=X xs=list(X) { x :: xs }

list1(X):
| x=X xs=list(X) { x :: xs }

(* A list of A's and B's that contains exactly one A: *)

list_eq1(A, B):
| x=A xs=B* { x :: xs }
| x=B xs=list_eq1(A, B) { x :: xs }

(* A list of A's and B's that contains at least one A: *)

list_ge1(A, B):
| x=A xs=B* { x :: xs }
| x=A xs=list_ge1(A, B) { x :: xs }
| x=B xs=list_ge1(A, B) { x :: xs }

(* A list of A's, B's and C's that contains exactly one A and exactly one B: *)

list_eq1_eq1(A, B, C):
| x=A xs=list_eq1(B, C) { x :: xs }
| x=B xs=list_eq1(A, C) { x :: xs }
| x=C xs=list_eq1_eq1(A, B, C) { x :: xs }

(* A list of A's, B's and C's that contains exactly one A and at least one B: *)

list_eq1_ge1(A, B, C):
| x=A xs=list_ge1(B, C) { x :: xs }
| x=B xs=list_eq1(A, C) { x :: xs }
| x=B xs=list_eq1_ge1(A, B, C) { x :: xs }
| x=C xs=list_eq1_ge1(A, B, C) { x :: xs }

list_sep_rev1(X, SEP):
| x=X { [x] }
| xs=list_sep_rev1(X, SEP) SEP x=X { x :: xs }

(* inline this because we don't want this to be a possible reduction,
   it just reverses the list *)
%inline list_sep1(X, SEP):
| xs=list_sep_rev1(X, SEP) { List.rev xs }

list_sep(X, SEP):
| (* empty *) { [] }
| xs=list_sep1(X, SEP) { xs }

(* Upon finding an identifier, the lexer emits two tokens. The first token,
   [NAME], indicates that a name has been found; the second token, either [TYPE]
   or [VARIABLE], tells what kind of name this is. The classification is
   performed only when the second token is demanded by the parser. *)

typedef_name:
| i=NAME TYPE { i }

var_name:
| i=NAME VARIABLE { i }

(* [typedef_name_spec] must be declared before [general_identifier], so that the
   reduce/reduce conflict is solved the right way. *)

typedef_name_spec:
| typedef_name
    {}

general_identifier:
| i=typedef_name { i }
| i=var_name { i }

save_context:
| (* empty *)
    { save_context () }

scoped(X):
| ctx=save_context x=X { restore_context ctx; x }

(* [declarator_varname] and [declarator_typedefname] are like [declarator]. In
   addition, they have the side effect of introducing the declared identifier as
   a new variable or typedef name in the current context. *)

declarator_varname:
| d=declarator
    { declare_varname (identifier (fst d)); d }

declarator_typedefname:
| d=declarator
    { declare_typedefname (identifier (fst d)); d }

(* Merge source-level string literals. *)
string_literal:
| xs=STRING_LITERAL+ { xs }

(* End of the helpers, and beginning of the grammar proper: *)

primary_expression:
| v=var_name { Variable v }
| i=INT_LITERAL { Int i }
| s=string_literal { String s }
| s=CHAR_LITERAL { Char s }
| "(" e=expression ")" { todo [%here] }
| e=generic_selection { todo [%here] }

generic_selection:
| "_Generic" "(" assignment_expression "," generic_assoc_list ")" { todo [%here] }

generic_assoc_list:
| generic_association
| generic_assoc_list "," generic_association { todo [%here] }

generic_association:
| type_name ":" assignment_expression
| "default" ":" assignment_expression { todo [%here] }

postfix_expression:
| e=primary_expression { e }
| expr=postfix_expression "[" index=expression "]" { Index { expr; index } }
| func=postfix_expression "(" args=argument_expression_list ")" { Call { func; args } }
| expr=postfix_expression "." field=general_identifier { Member { expr; field } }
| postfix_expression "->" general_identifier
| postfix_expression "++"
| postfix_expression "--"
| "(" type_name ")" "{" initializer_list ","? "}" { todo [%here] }

argument_expression_list:
| es=list_sep(assignment_expression, ",") { es }

unary_expression:
| e=postfix_expression { e }
| "++" unary_expression 
| "--" unary_expression
| unary_operator cast_expression
| "sizeof" unary_expression
| "sizeof" "(" type_name ")"
| "_Alignof" "(" type_name ")"
    { todo [%here] }

unary_operator:
| "&"
| "*"
| "+"
| "-"
| "~"
| "!"
    { todo [%here] }

cast_expression:
| e=unary_expression { e }
| "(" type_name ")" cast_expression
    { todo [%here] }

multiplicative_operator:
  "*" | "/" | "%" {}

multiplicative_expression:
| e=cast_expression { e }
| multiplicative_expression multiplicative_operator cast_expression
    { todo [%here] }

additive_operator:
  "+" | "-" {}

additive_expression:
| e=multiplicative_expression { e }
| additive_expression additive_operator multiplicative_expression
    { todo [%here] }

shift_operator:
  "<<" | ">>" {}

shift_expression:
| e=additive_expression { e }
| shift_expression shift_operator additive_expression
    { todo [%here] }

relational_operator:
  "<" | ">" | "<=" | ">=" {}

relational_expression:
| e=shift_expression { e }
| relational_expression relational_operator shift_expression
    { todo [%here] }

equality_operator:
  "==" | "!=" {}

equality_expression:
| e=relational_expression { e }
| equality_expression equality_operator relational_expression
    { todo [%here] }

and_expression:
| e=equality_expression { e }
| and_expression "&" equality_expression
    { todo [%here] }

exclusive_or_expression:
| e=and_expression { e }
| exclusive_or_expression "^" and_expression
    { todo [%here] }
 
inclusive_or_expression:
| e=exclusive_or_expression { e }
| inclusive_or_expression "|" exclusive_or_expression
    { todo [%here] }

logical_and_expression:
| e=inclusive_or_expression { e }
| logical_and_expression "&&" inclusive_or_expression
    { todo [%here] }

logical_or_expression:
| e=logical_and_expression { e }
| logical_or_expression "||" logical_and_expression
    { todo [%here] }

conditional_expression:
| e=logical_or_expression { e }
| logical_or_expression "?" expression ":" conditional_expression
    { todo [%here] }

assignment_expression:
| e=conditional_expression { e }
| unary_expression o=assignment_operator assignment_expression { todo [%here] }

assignment_operator:
| "=" { Ast.Assign }
| "*=" { Ast.AddAssign }
| "/=" { Ast.SubAssign }
| "%=" { Ast.ModAssign }
| "+=" { Ast.AddAssign }
| "-=" { Ast.SubAssign }
| "<<=" { Ast.ShlAssign }
| ">>=" { Ast.ShrAssign }
| "&=" { Ast.BandAssign }
| "^=" { Ast.XorAssign }
| "|=" { Ast.BorAssign }

expression:
| e=assignment_expression { e }
| expression "," assignment_expression
    { todo [%here] }


constant_expression:
| e=conditional_expression { e }

(* We separate type declarations, which contain an occurrence of ["typedef"], and
   normal declarations, which do not. This makes it possible to distinguish /in
   the grammar/ whether a declaration introduces typedef names or variables in
   the context. *)

declaration:
| declaration_specifiers         init_declarator_list(declarator_varname)?     ";"
| declaration_specifiers_typedef init_declarator_list(declarator_typedefname)? ";"
| static_assert_declaration
    { todo [%here] }

(* [declaration_specifier] corresponds to one declaration specifier in the C18
   standard, deprived of "typedef" and of type specifiers. *)

declaration_specifier:
 (* deprived of "typedef" *)
| s=storage_class_specifier { Ast.StorageSpec s }
| s=type_qualifier { Ast.QualSpec s }
| s=function_specifier { Ast.FuncSpec s }
| alignment_specifier { todo [%here] }

(* [declaration_specifiers] requires that at least one type specifier be
   present, and, if a unique type specifier is present, then no other type
   specifier be present. In other words, one should have either at least one
   nonunique type specifier, or exactly one unique type specifier.

   This is a weaker condition than 6.7.2 2. Encoding this condition in the
   grammar is necessary to disambiguate the example in 6.7.7 6:

     typedef signed int t;
     struct tag {
     unsigned t:4;
     const t:5;
     };

   The first field is a named t, while the second is unnamed of type t.

   [declaration_specifiers] forbids the ["typedef"] keyword. *)

declaration_specifiers:
| xs=list_eq1(type_specifier_unique_spec, declaration_specifier) { xs }
| xs=list_ge1(type_specifier_nonunique_spec, declaration_specifier) { xs }

(* [declaration_specifiers_typedef] is analogous to [declaration_specifiers],
   but requires the ["typedef"] keyword to be present (exactly once). *)

declaration_specifiers_typedef:
| xs=list_eq1_eq1(
    "typedef" { Ast.StorageSpec Ast.Typedef },
    t=type_specifier_unique { Ast.TypeSpec t },
    s=declaration_specifier { s }
  )
  { xs }
| xs=list_eq1_ge1(
    "typedef" { Ast.StorageSpec Ast.Typedef },
    t=type_specifier_nonunique { Ast.TypeSpec t },
    s=declaration_specifier { s }
  )
  { xs }

(* The parameter [declarator] in [init_declarator_list] and [init_declarator]
   is instantiated with [declarator_varname] or [declarator_typedefname]. *)

init_declarator_list(declarator):
| xs=list_sep1(init_declarator(declarator), ",") { xs }

init_declarator(declarator):
| d=declarator { }
| declarator "=" c_initializer { }

(* [storage_class_specifier] corresponds to storage-class-specifier in the
   C18 standard, deprived of ["typedef"] (which receives special treatment). *)

storage_class_specifier:
| "extern" { Ast.Extern }
| "static" { Ast.Static }
| "_Thread_local" { Ast.ThreadLocal }
| "auto" { Ast.Auto }
| "register" { Ast.Register }

(* A type specifier which can appear together with other type specifiers. *)

type_specifier_nonunique:
| "char" { Ast.Char }
| "short" { Ast.Short }
| "int" { Ast.Int }
| "long" { Ast.Long }
| "float" { Ast.Float }
| "double" { Ast.Double }
| "signed" { Ast.Signed }
| "unsigned" { Ast.Unsigned }
| "_Complex" { todo [%here] }

%inline type_specifier_nonunique_spec:
| t=type_specifier_nonunique { Ast.TypeSpec t }

(* A type specifier which cannot appear together with other type specifiers. *)

type_specifier_unique:
| "void" { Ast.Void }
| "_Bool" { Ast.Bool }
| atomic_type_specifier { todo [%here] }
| struct_or_union_specifier { todo [%here] }
| enum_specifier { todo [%here] }
| typedef_name_spec { todo [%here] }

%inline type_specifier_unique_spec:
| t=type_specifier_unique { Ast.TypeSpec t }

struct_or_union_specifier:
| struct_or_union general_identifier? "{" struct_declaration_list "}"
| struct_or_union general_identifier
    {}

struct_or_union:
| "struct" { Ast.Struct }
| "union" { Ast.Union }

struct_declaration_list:
| struct_declaration
| struct_declaration_list struct_declaration
    {}

struct_declaration:
// | specifier_qualifier_list struct_declarator_list ";"
| declaration_specifiers struct_declarator_list ";"
| static_assert_declaration
    { todo [%here] }


(* [specifier_qualifier_list] is as in the standard, except it also encodes the
   same constraint as [declaration_specifiers] (see above). *)

specifier_qualifier_list:
| specs=list_eq1(type_specifier_unique_spec,    t=type_qualifier_spec | t=alignment_specifier_spec { t }) { specs }
| specs=list_ge1(type_specifier_nonunique_spec, t=type_qualifier_spec | t=alignment_specifier_spec { t }) { specs }

struct_declarator_list:
| xs=list_sep1(struct_declarator, ",") { xs }

struct_declarator:
| declarator
| declarator? ":" constant_expression
    { todo [%here] }

enum_specifier:
| "enum" general_identifier? "{" enumerator_list ","? "}"
| "enum" general_identifier
    {}

enumerator_list:
| enumerator
| enumerator_list "," enumerator
    {}

enumerator:
| i = enumeration_constant
| i = enumeration_constant "=" constant_expression
    { declare_varname i }

enumeration_constant:
| i = general_identifier
    { i }

atomic_type_specifier:
| "_Atomic" "(" type_name ")"
| "_Atomic" ATOMIC_LPAREN type_name ")"
    {}

type_qualifier:
| "const" { Ast.Const : Ast.qual_spec }
| "restrict" { Ast.Restrict }
| "volatile" { Ast.Volatile }
| "_Atomic" { Ast.Atomic }

%inline type_qualifier_spec:
| t=type_qualifier { Ast.QualSpec t }

function_specifier:
  | "inline" { Ast.Inline }
  | "_Noreturn" { Ast.Noreturn }

alignment_specifier:
| "_Alignas" "(" type_name ")" { todo [%here] }
| "_Alignas" "(" constant_expression ")" { todo [%here] }

%inline alignment_specifier_spec:
| a=alignment_specifier { Ast.QualSpec (Ast.Attr a) }

declarator:
| add_pointer=ioption(pointer) d=direct_declarator
    { other_declarator (fst d), Ast.map_decl_name_ty (snd d) ~f:(Option.value ~default:Fn.id add_pointer) }

(* The occurrences of [save_context] inside [direct_declarator] and
   [direct_abstract_declarator] seem to serve no purpose. In fact, they are
   required in order to avoid certain conflicts. In other words, we must save
   the context at this point because the LR automaton is exploring multiple
   avenues in parallel and some of them do require saving the context. *)

direct_declarator:
| i=general_identifier
    { identifier_declarator i, { name = i; ty = JustBase } }
| "(" save_context d=declarator ")" { d }
| d=direct_declarator "[" specs=type_qualifier* size=assignment_expression? "]"
    {
        let d, ({ name; ty } : Ast.decl_name) = d in
        d, ({ name; ty = Array { ty; specs; size } } : Ast.decl_name)
    }
// | d=direct_declarator "[" "static" type_qualifier* assignment_expression "]"
// | d=direct_declarator "[" list1(type_qualifier) "static" assignment_expression "]"
// | d=direct_declarator "[" type_qualifier* "*" "]"
//     { other_declarator d }
| d=direct_declarator "(" params=scoped(parameter_type_list) ")"
    {
      let ctx, params, variadic = params in
      let d, ({ name; ty } : Ast.decl_name) = d in
      function_declarator d ctx, { name; ty = Proto { ty; params; variadic } }
    }
| d=direct_declarator "(" save_context params=identifier_list? ")"
    (* this is the old way to define proto, don't support this yet *)
    {
      let d, ({ name; ty } : Ast.decl_name) = d in
      other_declarator d, { name; ty = ProtoOld { ty; params = Option.value ~default:[] params } }
    }

pointer:
| "*" qs=type_qualifier_list? p=pointer?
  {
    fun ty ->
      Ast.Ptr
        { specs = Option.value ~default:[] qs;
          ty = (Option.value ~default:Fn.id p) ty
        } 
  }

type_qualifier_list:
| xs=list1(type_qualifier) { xs }

parameter_type_list:
| params=parameter_list varargs=option("," "..." { }) ctx=save_context
    { ctx, params, Option.is_some varargs }

%inline parameter_list:
| ps=list_sep1(parameter_declaration, ",") { ps }

parameter_declaration:
| specs=declaration_specifiers d=declarator_varname
  { 
    let decl_name = snd d in
    ({ specs; name = Some decl_name.name; ty = decl_name.ty } : Ast.param)
  }
| specs=declaration_specifiers ty=abstract_declarator?
  {
    ({ specs; name = None; ty = Option.value ~default:Ast.JustBase ty } : Ast.param)
  }

identifier_list:
| vs=list_sep1(var_name, ",") { vs }

type_name:
| specs=specifier_qualifier_list ty=abstract_declarator? { { specs; ty = Option.value ~default:Ast.JustBase ty } : Ast.full_type }

abstract_declarator:
| add_pointer=pointer { add_pointer Ast.JustBase }
| add_pointer=ioption(pointer) ty=direct_abstract_declarator { (Option.value ~default:Fn.id add_pointer) ty }

direct_abstract_declarator:
| "(" save_context ty=abstract_declarator ")" { ty }
| ty=direct_abstract_declarator? "[" specs=ioption(type_qualifier_list) size=assignment_expression? "]"
  { Array { ty = Option.value ~default:Ast.JustBase ty; specs = Option.value ~default:[] specs; size; } }
// | direct_abstract_declarator? "[" "static" type_qualifier_list? assignment_expression "]"
// | direct_abstract_declarator? "[" type_qualifier_list "static" assignment_expression "]"
// | direct_abstract_declarator? "[" "*" "]"
| ty=ioption(direct_abstract_declarator) "(" params=scoped(parameter_type_list) ")"
    {
      let ctx, params, variadic = params in
      Proto { ty = Option.value ~default:Ast.JustBase ty; params; variadic }
    }
| ty=ioption(direct_abstract_declarator) "(" ")"
    {
      (ProtoOld { ty = Option.value ~default:Ast.JustBase ty; params = []; } : Ast.decl_type)
    }


c_initializer:
| assignment_expression
| "{" initializer_list ","? "}"
    {}

initializer_list:
| designation? c_initializer
| initializer_list "," designation? c_initializer
    {}

designation:
| designator_list "="
    {}

designator_list:
| designator_list? designator
    {}

designator:
| "[" e=constant_expression "]" { Ast.AtIndex e }
| "." f=general_identifier { Ast.AtField f }

static_assert_declaration:
| "_Static_assert" "(" constant_expression "," string_literal ")" ";"
    {}

statement:
| s=labeled_statement { s }
| s=scoped(compound_statement) { s }
| s=expression_statement { s }
| s=scoped(selection_statement) { s }
| s=scoped(iteration_statement) { s }
| s=jump_statement { todo [%here] }

labeled_statement:
| general_identifier ":" statement
| "case" constant_expression ":" statement
| "default" ":" statement
    { todo [%here] }

compound_statement:
| "{" stmts=block_item_list? "}"
    { Ast.Block { stmts = Option.value ~default:[] stmts; span = Span.garbage } }

block_item_list:
| stmts=block_item+ { stmts }

block_item:
| decl=declaration { Ast.Def decl }
| stmt=statement { stmt }

expression_statement:
| e=expression? ";"
  { e |> Option.map ~f:(fun expr -> Ast.Expr { expr; span = Span.garbage }) |> Option.value ~default:(Ast.Noop Span.garbage) }

selection_statement:
| "if" "(" cond=expression ")" then_stmt=scoped(statement) "else" else_stmt=scoped(statement)
  { Ast.If { cond; then_stmt; else_stmt = Some else_stmt; span = Span.garbage } }
| "if" "(" cond=expression ")" then_stmt=scoped(statement) %prec below_ELSE
  { Ast.If { cond; then_stmt; else_stmt = None; span = Span.garbage } }
| "switch" "(" cond=expression ")" body=scoped(statement)
    { Ast.Switch { cond; body; span = Span.garbage } }

iteration_statement:
| "while" "(" cond=expression ")" body=scoped(statement)
   { Ast.While { cond; body; span = Span.garbage } }
| "do" scoped(statement) "while" "(" expression ")" ";"
   { todo [%here] }
| "for" "(" clause=expression? ";" cond=expression? ";" update=expression? ")" body=scoped(statement)
   {
     Ast.For
       {
         clause = Option.map ~f:(fun e -> Ast.ForExpr e) clause;
         cond;
         update;
         body;
         span = Span.garbage
       }
   }
(* decl includes the semicolon *)
| "for" "(" decl=declaration cond=expression? ";" update=expression? ")" body=scoped(statement)
    {
      Ast.For
        {
          clause = Some (Ast.ForDecl decl);
          cond;
          update;
          body;
          span = Span.garbage
        }
    }

jump_statement:
| "goto" general_identifier ";"
| "continue" ";"
| "break" ";"
| "return" expression? ";"
    { todo [%here] }

translation_unit_file:
| decls=external_declaration* EOF { decls }

external_declaration:
| decl=function_definition { decl }
| decl=declaration { decl }

function_definition1:
| specs=declaration_specifiers d=declarator_varname
    {
      let d, decl_name = d in
      let ctx = save_context () in
      reinstall_function_context d;
      ctx, specs, decl_name
    }

function_definition:
| def=function_definition1 body=compound_statement
    {
      let ctx, specs, decl_name = def in
      restore_context ctx;
      Ast.FunDecl { specs; decl_name; kr_params = []; body; span = Span.garbage }
    }
(* old style K&R function definition, see
   https://stackoverflow.com/questions/1585390/c-function-syntax-parameter-types-declared-after-parameter-list *)
| def=function_definition1 declaration_list body=compound_statement
  {
    let ctx, specs, decl_name = def in
    restore_context ctx;
    todo [%here]
  }

declaration_list:
| declaration
| declaration_list declaration
    { todo [%here] }
