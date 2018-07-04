(* codeGen.ml 

--

This file is part of aw2c. Copyright 2008 Glyn Webster.

aw2c is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aw2c is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with aw2c.  If not, see <http://www.gnu.org/licenses/>.

*)

(* 'grep -n -e '\(\* \*|^let|^and' codeGen.ml' for an index *)

open Printf
open Type


(** The C code representing an Algol W expression or designator with a
    particular Algol W simple type. For example, function [expression
    scope tree] returns [typed_code_t]. (C code for expressions that a
    known simple type are usually represented by just a Code.t
    value. For example, function [expression_expect t scope tree]
    returns [Code.t], the code will always be for expressions of type
    [t].)
*)
type typed_code_t = {
  t : simple_t;
  c : Code.t
}

(** The C code representing designators must be translated into C
    pointer lvalues in some places, C lavalues in others. See function
    [designator_or_expression].
*)
type designator_t =
  | Pointer
  | Lvalue

(** [designator_or_expression] returns C code that represents either
    an Algol W expression or an Algol W designator. Either can appear
    as a procedure's actual parameter - they will be treated
    differently deprending on the kind of formal parameter.
*)
type designator_or_expression_t =
  | Designator of typed_code_t
  | Expression of typed_code_t


(** These types are used to accumulate code and data for the
    declarations in block bodies.

    Translation of procedure bodies is delayed until all declarations in
    the surrounding scope have been found. This is so that Algol W
    doesn't need the equivalent of Pascal's "forward" declarations. 
*)
type block_t = {
  scope          : Scope.t;          (** the scope introduced by the block. *)
  outsidescope   : Code.t;           (** code to be executed outside the block (array bounds expressions.) *)
  labels         : Code.t;           (** __label__ definitions, which must appear first in the C block *)
  prototypes     : Code.t;           (** function prototypes, which must all appear before any function definitions *) 
  structs        : Code.t;           (** struct declarations for record classes *)
  variables      : Code.t;           (** simple variable declarations *)
  functions      : Code.t;           (** function definitions *) 
  initialization : Code.t;           (** assignment statements to initialize simple variables *)
  procedures     : procedure_header_t list
}
and procedure_header_t = {
  returntype : simple_t;
  proc_id    : Table.Id.t;
  proc_loc   : Location.t;
  parameters : formal_parameters_t;  (** definitions of a procedure's formal parameters *)
  header     : Code.t;               (** C-code header for a procedure's C function and prototype *)
  body       : Tree.t                (** the parse tree of the body of a procedure, translated 
                                         once the surrounding block's entire scope is known.*)
}
and formal_parameters_t = {
  procedure_locals : Scope.Local.t;  (** definitions of the formal parameters, valid in a procedure's body *)
  formal_types     : formal_t list;  (** the types of the parameters, in order *)
  arguments        : Code.t list;    (** C function arguments for the parameters, in order (C identifiers) *)
}


let empty_block =
  { scope          = Scope.empty;
    outsidescope   = Code.empty;
    prototypes     = Code.empty;
    labels         = Code.empty;
    variables      = Code.empty;
    structs        = Code.empty;
    functions      = Code.empty;
    initialization = Code.empty;
    procedures     = [] 
  }

let empty_formal_parameters =
  { procedure_locals = Scope.Local.empty;
    formal_types     = [];
    arguments        = [] }


(* * Support functions ---------------------------------------------------------------- *)


(** Shorthand for [Code.template] 
*)
let ($) template arguments = Code.template template arguments


(** [Error (source_location, message)] The all-purpose error
    message. This is mostly used to report type errors. 
*)
exception Error of Location.t * string


(** [error loc "message" arg1 arg2 ...] is a printf-like function for raising [Error]. 
*)
let error loc = Printf.ksprintf (fun message -> (raise (Error(loc, message))))


(** [mapi start f xs] maps [f] over the a sequence of integer starting
    with [start] and the elements of list [xs].
*)
let mapi (start : int) (f : int -> 'a -> 'b) (xs : 'a list) : 'b list =
  let rec loop i =
    function
    | [] -> []
    | x :: xs' -> f i x :: loop (i + 1) xs'
  in
  loop start xs


(** [mapi first last xs] maps [f] over the a sequence of integers from [first] to [last].
*)
let mapn (first : int) (last : int) (f : int -> 'a) : 'a list =
  assert (first <= last);
  let rec loop i xs =
    if i >= first then 
      loop (i - 1) (f i :: xs)
    else 
      xs
  in 
  loop last []


(** [snip_last xs] returns a tuple of the list [xs] with the last
    element removed and the last element of [xs]. (It is like [(List.hd
    xs, List.tl xs)], but working from the other end.)
*)
let snip_last (xs : 'a list) : ('a list * 'a) =
  match List.rev xs with
  | []       -> failwith "snip_last"
  | [x]      -> ([], x)
  | x :: xs' -> (List.rev xs', x)


(* * Types and scopes ---------------------------------------------------------------- *)

(** [set loc scope id defn] sets [id] to [defn] in [scope] and returns
    the modified scope. It reports an error at source location [loc]  if
    this would redefine [id] in the inner block.
*)
let set (loc : Location.t) (scope : Scope.t) (id : Table.Id.t) (defn : definition_t) = 
  try 
    Scope.set scope id defn
  with Scope.Redefined (_, defn) -> 
    error loc "'%s' is already defined here, as %s" 
      (Table.Id.to_string id) 
      (describe_definition defn)


(** Same as [set], but working on a single-level scope. This is used
    when accumulating the formal parameters of a procedure.
*)
let set_local (loc : Location.t) (scope : Scope.Local.t) (id : Table.Id.t) (defn : definition_t) : Scope.Local.t = 
  try 
    Scope.Local.set scope id defn
  with Scope.Redefined (_, defn) -> 
    error loc "'%s' is already defined here, as %s" 
      (Table.Id.to_string id) 
      (describe_definition defn)


(** [get loc scope id] gets the definition of [id] in [scope].It
    reports an error at source location [loc] if [id] is not defined
    in [scope].
*)
let get (loc : Location.t) (scope : Scope.t) (id : Table.Id.t) : definition_t = 
  try 
    Scope.get scope id
  with Scope.Undefined _ -> 
    error loc "'%s' is undefined here" (Table.Id.to_string id)



(** [simple scope tree] returns the simple type represented by parse
    tree [tree].  [scope] is used to look up reference call
    identifiers.
*)
let simple (scope : Scope.t) (tree : Tree.t) : simple_t =
  match tree with
  | Tree.INTEGER              -> Number (Long, Integer)
  | Tree.REAL                 -> Number (Short, Real)
  | Tree.LONG_REAL            -> Number (Long, Real)
  | Tree.COMPLEX              -> Number (Short, Complex)
  | Tree.LONG_COMPLEX         -> Number (Long, Complex)
  | Tree.BITS                 -> Bits
  | Tree.LOGICAL              -> Logical
  | Tree.STRING (Some length) -> String length
  | Tree.STRING (None)        -> String 16
  | Tree.REFERENCE (loc, ids) ->
      let classes =
        List.fold_left
          ( fun classes id ->
              match get loc scope id with
              | Record (n, _) -> ClassSet.add n classes
              | defn -> error loc "expected %s to be a record class, it is %s"
                  (Table.Id.to_string id) (describe_definition defn) )
          ClassSet.empty
          ids
      in
      Reference (classes)
  | _ -> failwith "Parser error in simple type"


(* * C code generation ---------------------------------------------------------------- *)


(** [is_valid_c_identifier s] returns true is [s] is a valid GCC C
    identifier.
*)
let is_valid_c_identifier s =
  let first c = (c = '_' || c = '$' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let rest c = (first c || (c >= '0' && c <= '9')) in
  try
    assert (String.length s > 0);
    assert (first s.[0]);
    for i = 1 to String.length s - 1 do
      assert (rest s.[i])
    done;
    true
  with _ ->
    false

(** A table of C and Gnu C reserved words. Algol identifiers must be
    renamed if they clash with these. "Time", "round" and "odd" also
    get renamed to prevent the aw2c runtime and C standard library
    names clashing. 
*)
let gnuc_keywords = 
  List.fold_left
    (fun set kw -> Table.IdSet.add (Table.Id.create kw) set)
    Table.IdSet.empty
    [ "__asm__"; "__const__"; "__extension__"; "__inline__"; "__signed__"; "__typeof__";
      "__volatile__"; "asm"; "auto"; "break"; "case"; "char";
      "const"; "continue"; "default"; "do"; "double"; "else";
      "enum"; "extern"; "float"; "for"; "goto"; "if";
      "inline"; "int"; "long"; "register"; "return"; "short";
      "signed"; "sizeof"; "static"; "struct"; "switch"; "typedef";
      "typeof"; "union"; "unsigned"; "void"; "volatile"; "while" ;
      "_Complex";
      (* these get renamed to prevent the aw2c runtime and C standard library names clashing: *)
      "time"; "round"; "odd"; "argc"; "argv"; 
      (* GCC thinks that all functions named "main" should return an int, not 
         just the outermost one, so let's rename that too to keep GCC happy: *)
      "main"
    ]


(** [string_of_id id] returns the C code version of identifier [id], as a string. 
*)
let string_of_id (id : Table.Id.t) : string = 
  if Table.IdSet.mem id gnuc_keywords then
    Table.Id.to_string id ^ "_"
  else
    Table.Id.to_string id


(** [code_of_id id] returns the C code version of identifier [id], as C code. 
*)
let code_of_id (id : Table.Id.t) : Code.t = 
  Code.string (string_of_id id)


(** [code_of_int id] converts integer [i] to C code.
*)
let code_of_int i = 
  Code.string (string_of_int i)


(** [code_of_loc loc] returns an C code function argument denoting the
    Algol W source location [loc].  It is used in runtime library
    function calls that might raise runtime errors.
*)
let code_of_loc loc =
  let _, line, column = Location.contents loc in
  Code.string (sprintf "alw_at(%i,%i)" line column)


(** [c_char_const s] returns the C code character constant that
    represents string [s].
*)
let c_char_const (s : string) : Code.t = 
  assert (String.length s = 1);
  Code.string 
    ( match s.[0] with
    | '\x00' -> "'\\0'"
    | '\x07' -> "'\\a'"
    | '\x08' -> "'\\b'"
    | '\x09' -> "'\\t'"
    | '\x0A' -> "'\\n'"
    | '\x0B' -> "'\\v'"
    | '\x0C' -> "'\\f'"
    | '\x0D' -> "'\\r'"
    | '\''   -> "'\\''"
    | '"'    -> "'\"'"
    | '\\'   -> "'\\\\'"
    | c when (c >= ' ' && c <= '~') -> sprintf "'%c'" c
    | c -> sprintf "'\\x%02X'" (int_of_char c) )
        

(** [c_string_const s] returns the C code string constant that
    represents string [s].
*)
let c_str_const (s : string) : Code.t  =
  let b = Buffer.create 32 in
  Buffer.add_char b '"';
  for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\x00' -> Buffer.add_string b "\\0"
      | '\x07' -> Buffer.add_string b "\\a"
      | '\x08' -> Buffer.add_string b "\\b"
      | '\x09' -> Buffer.add_string b "\\t"
      | '\x0A' -> Buffer.add_string b "\\n"
      | '\x0B' -> Buffer.add_string b "\\v"
      | '\x0C' -> Buffer.add_string b "\\f"
      | '\x0D' -> Buffer.add_string b "\\r"
      | '\''   -> Buffer.add_string b "\\'"
      | '"'    -> Buffer.add_string b "\\\""
      | '\\'   -> Buffer.add_string b "\\\\"
      | c when (c >= ' ' && c <= '~') -> Buffer.add_char b c
      | c -> bprintf b "\" \"\\x%02X\" \"" (int_of_char c)
          (* Isolate hexadecimal escape codes in their own C string constants.
             They will confuse GCC if followed by digits.  Strange but true. *)
  done;
  Buffer.add_char b '"';
  Code.string (Buffer.contents b)


(** [cast loc t e] returns the C code that casts a typed_code_t
    expression [e] to simple type [t]. This raises a type error at [loc]
    if the cast is not allowed by Algol W's assignment compatibility rules
    (c.f. 7.7.2.)

    [cast ~copy:true loc t e] is a special cast that ensures that
    string expressions are cast to unique character arrays on the
    heap; this creates values that be safely returned from Algol W
    functions.
*)
let cast ?(copy = false) (loc : Location.t) (desired_type : simple_t) (e : typed_code_t) : Code.t = 
  if assignment_compatible desired_type e.t  then
    match desired_type, e.t with
    | String 1, String 1 -> 
        e.c
    | String n, String 1 -> 
        assert copy; (* this only happens when returning values *)
        "alw_str_new_c($, $)" $ [e.c; code_of_int n]
    | String desired_length, String length -> 
        if copy then
          "alw_str_new($, $, $)" $ [e.c; code_of_int length; code_of_int desired_length]
        else
          e.c
    | Reference d_set, Reference e_set when not (ClassSet.subset e_set d_set) ->
        let d_classes = 
          ClassSet.fold (fun c lc -> (code_of_id (Class.to_id c)) :: lc) d_set [] 
        in
        "alw_ref_cast($, $, $)" $ 
          [code_of_loc loc; e.c; Code.rseparate ", " d_classes]
    | _, _ -> 
        e.c
  else
    error loc "%s is not compatible with %s" 
      (describe_simple e.t)
      (describe_simple desired_type)


(** [default t] return the C code value that variables of type [t] are
    to be intialized to. Reference variables must be initalized to
    "(void *)0" to prevent "stray pointer" errors, variables for the
    other types do not really need to be initialized, but they are.
*)
let default (t : simple_t) : Code.t =
  match t with
  | Number (Short, Integer) -> failwith "default: Number(Short, Integer) shouldn't exist"
  | Statement               -> failwith "default: Statements shouldn't be initialized"
  | Number (Long, Integer)  -> Code.string "0"
  | Number (Short, Real)    -> Code.string "0.0"
  | Number (Long, Real)     -> Code.string "0.0"
  | Number (Short, Complex) -> Code.string "0.0"
  | Number (Long, Complex)  -> Code.string "0.0"
  | Logical                 -> Code.string "0"
  | Bits                    -> Code.string "0"
  | String 1                -> Code.string "' '"
  | String n                -> "alw_str_var($)" $ [code_of_int n]
  | Reference(_)            -> Code.string "(void *)0"
  | Null                    -> Code.string "(void *)0"


(** String(n) values, where n >= 2, are represented by "alw_str",
    which points an array of characters; all other C types represent
    Algol W values directly.  String(n) will be treated specially all
    through this program, but the special treatment is mostly wrapped
    up in these next few functions.
*)


(** The C type used pass and return values of simple type [t].
*)
let ctype (t : simple_t) : Code.t =
  match t with
  | Number (Short, Integer) -> failwith "ctype: Number(Short, Integer) shouldn't exist"
  | Number (Long, Integer)  -> Code.string "int"
  | Number (Short, Real)    -> Code.string "double"
  | Number (Long, Real)     -> Code.string "double"
  | Number (Short, Complex) -> Code.string "_Complex double"  (* <complex.h> is not included *)
  | Number (Long, Complex)  -> Code.string "_Complex double"
  | Statement               -> Code.string "void"   (* 'proper procedure parameters' return 'void' *)
  | Logical                 -> Code.string "int"
  | Bits                    -> Code.string "unsigned int"
  | String 1                -> Code.string "alw_chr"
  | String _                -> Code.string "alw_str"
  | Reference(_)            -> Code.string "void *"
  | Null                    -> Code.string "void *"
      

(** The C type that points to a value of the simple type [t]. 
*)
let c_pointer_type (t : simple_t) : Code.t =
  match t with
  | String n when n > 1 -> ctype t
  | _                   -> "$ *" $ [ctype t]
      

(** Obtain a pointer to the simple type [t] value stored in C variable [var]. 
*)
let address_of (t : simple_t) (var : Code.t) : Code.t =
  match t with
  | String n when n > 1 -> var
  | _                   -> "&$" $ [var]


(** Declare [var] as a simple type [t] variable.
*)
let declare_simple (t : simple_t) (var : Code.t) : Code.t =
  match t with
  | String n when n > 1 -> "alw_chr $[$];\n" $ [var; code_of_int n]  (* character array *)
  | t ->                   "$ $;\n" $ [ctype t; var]


(** The C statement that initializes a varaible of simple type [t].
    This is used in simple variable, array and RESULT actual parameter
    initializations.

    XXX if there is to be a compiler flag to turn off unnecessary
    initializations, it would be affect this function, making it
    return [Code.empty] for all simple types but [Reference _].
*)
let initialize_simple (t : simple_t) (var : Code.t) : Code.t =
  match t with
  | String n when n > 1 -> "alw_str_init($, $);\n" $ [var; code_of_int n]
  | t ->                   "$ = $;\n" $ [var; default t]


(** Assign expression [expr] to variable [lvalue], and return the assignment expression.
*)
let assignment_expression (loc : Location.t) (lvalue : typed_code_t) (expr : typed_code_t) : typed_code_t =
  if assignment_compatible lvalue.t expr.t then
    match lvalue.t, expr.t with
    | String 1, String 1 -> (* character to character *)
        { t = expr.t; 
          c = "$ = $" $ [lvalue.c; expr.c] }
    | String dstlen, String 1 ->  (* character to array *)
        { t = expr.t; 
          c = "alw_str_cpy_sc($, $, $)"
            $ [lvalue.c; code_of_int dstlen; expr.c] }
    | String dstlen, String srclen ->  (* array to array *)
        { t = expr.t; 
          c = "alw_str_cpy($, $, $, $)"
            $ [lvalue.c; code_of_int dstlen; expr.c; code_of_int srclen] }
    | _, _ ->
        { t = expr.t; 
          c = "$ = $" $ [lvalue.c; cast loc lvalue.t expr] }
  else
    error loc "%s cannot be assigned to %s variable" (describe_simple expr.t) (describe_simple lvalue.t)


(** Assign expression [expr] to variable [lvalue]. and return the
    assignment statement (which is always simple type Statement.)
*)
let assignment_statement (loc : Location.t) (d : typed_code_t) (e : typed_code_t) : Code.t =
  "$;\n" $ [(assignment_expression loc d e).c]



(* * Programs ---------------------------------------------------------------------------- *)

(** The combined type checking and code generation pass is one huge
    recursive function that starts here: the parse tree goes in one
    end and C code pops out the other.

    [c_header] is the optional C code at the start of a program (an
    aw2c extention); [algolw_body] is the statement or procedure
    declaration that defines the Algol W program.
*)
let rec entry_point ((c_header, algolw_body) : Tree.t option * Tree.t) : Code.t =
  let source_file_name, _, _ = Location.contents (Tree.to_loc algolw_body) in
  match algolw_body with
  | Tree.PROCEDURE (_,_,_,_,_) as proc_decl -> 
      separately_compiled_procedure source_file_name c_header proc_decl
  | _ -> 
      program source_file_name c_header algolw_body


(** The expression part of a program can be a Statement (standard
    Algol W) or return an integer exit code like a C program (a2wc
    extension).
*)
and program (source_file_name : string) (c_header : Tree.t option) (body : Tree.t) : Code.t =
  let program_expr = expression Alwstd.scope body in
  let loc = code_of_loc (Tree.to_loc body) in
  let return =
    if program_expr.t = Statement then
      "$alw_exit($, 0);" $ [program_expr.c; loc]
    else if program_expr.t = integer then
      "alw_exit($, $);" $ [loc; program_expr.c]
    else
      error (Tree.to_loc body) "a program should be a statement or return an INTEGER, this returns %s"
        (describe_simple program_expr.t)
  in
  "$
   int main (int argc, char **argv) {
     alw_init($);
     $
     return 0;
   }
   \n" $ [ c_program_headers source_file_name c_header;
           loc; 
           return ]


and separately_compiled_procedure (source_file_name : string) (c_header : Tree.t option) (proc : Tree.t) : Code.t =
  match proc with
  | Tree.PROCEDURE (loc, t_opt, id, formal_trees, body) -> 
      let block = {empty_block with scope = Alwstd.scope} in
      let block = add_procedure_declaration loc block t_opt id formal_trees body in
      let block = add_procedure_functions block in
      "$\n$\n" $ [c_program_headers source_file_name c_header; block.functions]
  | _ -> 
      failwith "separately_compiled_procedure"


(** Four things head a C code output file:

    1. #include "alw.h". The protoypes and macros of the a2wc run-time 
       library.

    2. An optional C code declaration block, if any. See the "***
       Inline C code" section of "Notes/aw2c.txt".

    3. "alw_src", the name of the Algol W source file, used by the
       "alw_loc" macro in "alw.h".

    4. A list of string declarations for each record class in the
       program. Pointer to these are used identify records' classes at
       runtime, see the 'References' section of "alw.h".
*)
and c_program_headers (source_name : string) (c_header : Tree.t option) : Code.t =
  let c_code = 
    match c_header with
    | None                         -> Code.empty
    | Some (Tree.C_code (_, _, s)) -> Code.string s
    | _                            -> failwith "c_program_headers"
  in
  let class_name_code =
    let decls = 
      List.map 
        (fun (id, name) -> "static const char * const $ = $;" $ [code_of_id id; c_str_const name])
        (Class.contents ())
    in
    Code.rseparate "\n" decls
  in
  "#include \"alw.h\"
$
static const char * const alw_src = $;
$
" $ [ c_code;
      c_str_const source_name;
      class_name_code
    ]


(* * Expressions -------------------------------------------------------------------------------- *)


and expression (scope : Scope.t) (tree : Tree.t) : typed_code_t =
  match tree with

  (* *** Constants - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

  (* Positive integers *)
  | Tree.Integer (loc, s) -> 
      ( try 
          (* [Int32.of_string] wraps around after 2^32-1, so if it
             returns a negative number or fails, then the integer in [s]
             will not fit in 32 bits. *)
          (if Int32.of_string s < 0l then raise (Failure "int_of_string")) ;
          { t = Number(Long, Integer); c = Code.string s }
        with Failure "int_of_string" ->
          error loc "integer %s will not fit in a 32 bit word" s
      )

  (* Negative integers (integers preceded by negation signs) *)
  | Tree.Unary (loc, Tree.NEG, Tree.Integer (_, s)) ->
      ( try 
          (* If [Int32.of_string] fails then the integer in [s] will
             not fit in 32 bits. *)
          ignore (Int32.of_string ("-" ^ s));
          (* This allows "-2147483648", GCC doesn't like it as a constant. *)
          if s = "2147483648" then
            { t = Number(Long, Integer); c = Code.string "(-2147483647 - 1)" }
          else
            { t = Number(Long, Integer); c = Code.string ("-" ^ s) }
        with Failure "int_of_string" ->
          error loc "integer -%s will not fit in a 32 bit word" s
      )

  | Tree.Bits (loc, s) -> 
      ( try
          let hex = "0x" ^ s in
          ignore (Int32.of_string hex);
          { t = Bits; c = Code.string hex }
        with Failure "int_of_string" ->
          error loc "BITS constant #%s will not fit in a 32 bit word" s
      )

  (* the real and exponent parts of an Algol real number are assembled
     into a C integer here (Algol W seperates them with a "'", C with an "e") *)
  | Tree.Real          (loc, r, "") -> {t = Number(Short, Real);    c = Code.string r}
  | Tree.Real          (loc, r, e)  -> {t = Number(Short, Real);    c = Code.string (sprintf "%se%s" r e)}
  | Tree.LongReal      (loc, r, "") -> {t = Number(Long, Real);     c = Code.string r}
  | Tree.LongReal      (loc, r, e)  -> {t = Number(Long, Real);     c = Code.string (sprintf "%se%s" r e)}
  | Tree.Imaginary     (loc, r, "") -> {t = Number(Short, Complex); c = Code.string (r ^ "i")}
  | Tree.Imaginary     (loc, r, e)  -> {t = Number(Short, Complex); c = Code.string (sprintf "%se%si" r e)}
  | Tree.LongImaginary (loc, r, "") -> {t = Number(Long, Complex);  c = Code.string (r ^ "i")}
  | Tree.LongImaginary (loc, r, e)  -> {t = Number(Long, Complex);  c = Code.string (sprintf "%se%si" r e)}

  | Tree.TRUE  loc -> { t = Logical; c = Code.string "1" }
  | Tree.FALSE loc -> { t = Logical; c = Code.string "0" }
  | Tree.NULL  loc -> { t = Null;    c = Code.string "(void *)0" }

   (* String constants. The type "alw_str" is "char *" but C string
      constants are "const char *". In some places (only conditional
      expressions, it appears) that would cause a type mismatch, hence
      the cast. String constants are Algol W expressions, so they are
      safe from modification regardless.  *)
  | Tree.String (loc, s) -> 
      ( match String.length s with
        | 0 -> error loc "Empty strings are not allowed in Algol W"
        | 1 -> {t = String 1; c = c_char_const s}
        | n -> {t = String n; c = "(alw_str)$" $ [c_str_const s]}
      )

  (* *** Unary operators - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

  | Tree.Unary (loc, o, a) ->
      unary_expression loc scope o a

  (* *** Binary operators - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  *)

  (* IS is a special case among the binary operators because its right operand is not an expression.
     C.f. section 6.4. *)
  | Tree.Binary (loc, reference, Tree.IS, record) ->
      let record_id, record_class = 
        match record with
        | Tree.Variable (loc, id) ->
            ( match get loc scope id with
            | Record(c, _) -> id, c
            | defn -> error loc "expected a record class identifier here, this is %s" (describe_definition defn) )
        | _ ->
            error (Tree.to_loc record) "expected a record class identifier here"
      in
      let rc = expression scope reference in
      ( match rc.t with
      | Reference(class_set) when ClassSet.mem record_class class_set ->
          { t = Logical;
            c = "alw_is($, $)" $ [rc.c; code_of_id (Class.to_id record_class)] }
      | Null
      | Reference(_) ->
          error loc "%s can never refer to a %s record" (describe_simple rc.t) (Table.Id.to_string record_id)
      | _ ->
          error loc "expected a reference, this is %s" (describe_simple rc.t) )

  | Tree.Binary (loc, a, o, b) ->
      binary_expression loc scope a o b

  (* *** Conditional expressions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* IF-THEN without an ELSE is always a statement. *)
  | Tree.IF (loc, condition, then_clause) -> 
      let cc = expression_expect Logical scope condition in
      let ct = expression_expect Statement scope then_clause in 
      { t = Statement; 
        c = "if ($)\n $" $ [cc; ct] }

  (* IF-THEN-ELSE is a Statement if both its branches are Statements,
     otherwise it is an expression.  If the IF-THEN expression is a
     String(n), its C-code expression returns a pointer to a temporary
     array of characters on the heap.  This is the only way to return
     a string value from a non-designator expression without declaring
     a temporary variable outside of the C-expression being built -
     aw2c has no way to do that. *)
  | Tree.IF_else (loc, condition, then_clause, else_clause) -> 
      let cc = expression_expect Logical scope condition in
      let ct = expression scope then_clause in
      let ce = expression scope else_clause in
      let rtype =  
        try
          unifying_type ct.t ce.t
        with Incompatible ->
          error loc "incompatible types: the THEN clause is %s and the ELSE clause is %s"
            (describe_simple ct.t)
            (describe_simple ce.t)
      in
      { t = rtype; 
        c = match rtype with
        | Statement ->
            "if ($)\n $ \n else \n $ \n" $ [cc; ct.c; ce.c] 
        | String n when n > 1 ->  (* i.e. arrays of chars, not single chars *)
            let value = {t = rtype; c = Code.string "_value"} in
            "({ alw_str _value = $;\n if ($) $ else $ _value; })" $ 
              [ default rtype;
                cc; 
                assignment_statement loc value ct; 
                assignment_statement loc value ce] 
        | _ ->
            "($ ? $ : $)" $ [cc; cast loc rtype ct; cast loc rtype ce] 
      }

  (* The CASE-OF-BEGIN_END statement. The selector expression is
     stored in a temprory variable so that it does not get called a
     second time if there is a range error. *)
  | Tree.CASE (loc, selector, branches) -> 
      let cselector = expression_expect integer scope selector in
      let make_branch index branch =
        "case $: $; break;\n" $ [ code_of_int index; (expression_expect Statement scope branch) ]
      in
      let cbranches = Code.concat (mapi 1 make_branch branches) in
      { t = Statement;
        c = 
          "{ 
             const int _selector = $;
             switch (_selector) {
             $
             default: alw_case_range_error($, _selector);
             }
           }
          " $ [cselector; cbranches; code_of_loc loc]
      }

  (* If the CASE expression selects String(n), its C-code expression
     returns a pointer to a temporary array of characters on the heap.
     (See 'Tree.IF_else' above.)
  *)
  | Tree.CASE_expr (loc, selector, branches) -> 
      let cselector = expression_expect integer scope selector in
      let bs = List.map (expression scope) branches in
      let rt = 
        try 
          List.fold_left 
            (fun t c -> unifying_type t c.t)
            (List.hd bs).t 
            (List.tl bs)
        with Incompatible -> 
          error loc "This CASE expression's branch expressions have incompatible types." 
      in
      let value = {t = rt; c = Code.string "_value"} in
      let make_branch index branch =
        "case $: $ break;\n" 
          $ [ code_of_int index; 
              assignment_statement loc value branch ]
      in
      let cbranches = Code.concat (mapi 1 make_branch bs) in
      { t = rt;
        c = 
          "({ $ _value = $;
                int _selector = $;
                switch (_selector) {
                  $
                  default: alw_case_range_error($, _selector);
                } 
                _value;
             })" $
            [ctype rt; default rt; cselector; cbranches; code_of_loc loc]
      }


  (* *** Iterative statements - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  | Tree.WHILE (loc, condition, then_clause) -> 
      let cc = expression_expect Logical scope condition in
      let ct = expression_expect Statement scope then_clause in
      { t = Statement; 
        c = "while ($)\n $ \n" $ [cc; ct] }

  (* The limit and step expression of FOR statments are placed in
     temporary variables to prevent them from being called more than
     once. *)
  | Tree.FOR (loc, control, first, last, body) -> 
      let ccontrol = code_of_id control in
      let cfirst = expression_expect integer scope first in
      let clast = expression_expect integer scope last in
      let for_body_scope = set loc (Scope.push scope) control Control in
      let cbody = expression_expect Statement for_body_scope body in
      { t = Statement;
        c = 
          "{ 
             const int _start = $;
             const int _limit = $;
             int $ = _start;
             while ($ <= _limit) {
               $
               ++$;
             }
           }
          " $ [   cfirst; 
                  clast; 
                  ccontrol; 
                  ccontrol; 
                  cbody; 
                  ccontrol ]
      }

  | Tree.FOR_step (loc, control, first, step, last, body) -> 
      let ccontrol = code_of_id control in
      let cfirst = expression_expect integer scope first in
      let cstep = expression_expect integer scope step in
      let clast = expression_expect integer scope last in
      let for_body_scope = set loc (Scope.push scope) control Control in
      let cbody = expression_expect Statement for_body_scope body in
      { t = Statement;
        c = 
          "{
             const int _start = $;
             const int _step = $;
             const int _limit = $;
             int $ = _start;
             alw_check_for_step($, _step);
             while ($ > 0 ? $ <= _limit : $ >= _limit) {
               $
               $ += _step;
             }
           }
          " $ [ cfirst; 
                cstep; 
                clast; 
                ccontrol;
                code_of_loc loc;
                cstep; ccontrol; ccontrol; 
                cbody; 
                ccontrol ]
      }

  (* As far as I can tell, all of the expressions in the list form of
     the FOR statement must be evaluated before the body is
     executed. That's why they are being stored in a temporary array
     here.  *)
  | Tree.FOR_list (loc, control, expressions, body) -> 
      let ccontrol = code_of_id control in
      let nexpressions = code_of_int (List.length expressions) in
      let es = List.map (expression_expect integer scope) expressions in
      let cassignments = Code.concat (mapi 0 (fun i e -> "_a[$] = $;\n" $ [code_of_int i; e]) es) in
      let for_body_scope = set loc (Scope.push scope) control Control in
      let cbody = expression_expect Statement for_body_scope body in
      { t = Statement;
        c = 
          "{
             int $, _i, _a[$];
             $
             for (_i = 0; _i < $; ++_i) {
               $ = _a[_i];
               $
             }
           }
          " $ [ ccontrol; nexpressions;
                cassignments; 
                nexpressions;
                ccontrol; 
                cbody ]
      }

  (* *** Other statements - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  | Tree.GOTO (loc, label) -> 
      ( match (get loc scope label) with
      | Label ->
          { t = Statement; 
            c = "goto $;\n" $ [code_of_id label]
          }
      | defn -> 
          error loc "'%s' should be a label here, it is %s" 
            (Table.Id.to_string label) 
            (describe_definition defn)
      )

  | Tree.ASSERT (loc, condition) -> 
      let e = expression_expect Logical scope condition in
      { t = Statement; 
        c = "alw_assert($, $);\n" $ [code_of_loc loc; e]
      }

  (* This deals with assignment statements, which are always expressions. *)
  | Tree.Assignment (loc, desig, expr) -> 
      let rec multiple_assignment loc' desig' expr' =
        let ecode = 
          match expr' with
          | Tree.Assignment (loc, d, e) -> multiple_assignment loc d e
          | _ -> expression scope expr' 
        in
        assignment_expression loc' (designator Lvalue scope desig') ecode
      in
      { t = Statement;
        c = "$;\n" $ [(multiple_assignment loc desig expr).c]
      }

  | Tree.Empty (_) -> 
      { t = Statement; 
        c = Code.string "; /*empty*/\n"
      }

  (* *** Designator expressions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* The substring expression returns a pointer into the character
     array of a string variable. This is safe because string expressions
     never appear in any context where their contents can be modified. *)
  | Tree.Substring (_, _, _, _) as substring ->
      let char_ptr_expr = designator Pointer scope substring in
      ( match char_ptr_expr.t with
        | String 1 ->
            {t = String 1; c = "*$" $ [char_ptr_expr.c]}
        | String n ->
            {t = String n; c = char_ptr_expr.c}
        | _ -> 
            failwith "expression:Substring"
      )

  (* "Variables" are actually any expression or statement that
     consists of a lone identifier. In the Algol W language descrption
     these are told apart syntatically, aw2c tells them apart by the
     types of their definitions. *)
  | Tree.Variable (loc, id) as variable -> 
      ( match (get loc scope id) with
        | Variable _ | Result _ | Name _ -> designator Lvalue scope variable
        | Control                     -> {t = integer;     c = code_of_id id}
        | Procedure procedure_type  -> procedure_call loc scope id procedure_type []
        | Record (record_class, field_types) ->
            record_allocation_expression loc scope id record_class field_types []
        | Standard st  -> error loc "this standard I/O procedure requires one or more actual parameters" 
        | Analysis _   -> error loc "this procedure requires an actual parameter"
        | Array (_, _) -> error loc "this array requires subscripts" 
        | Field (_, _) -> error loc "Record fields expect one actual parameter"
        | Label        -> error loc "expected an expression here, this is a GOTO label"
      )

  (* It is the same story with "dereferences". 
     
     Algol W 'Standard Functions of Analysis' can raise 'Exceptional
     Conditions', so they are represented by functions with
     location parameters for runtime messages. The run-time
     libary functions for these are prefixed with "alw_" to
     prevent them name-clashing with C math library
     functions. *)
  | Tree.Dereference (loc, id, actuals) as dereference ->
      ( match (get loc scope id) with
        | Array (etype, ndims) ->
            designator Lvalue scope dereference
        | Procedure procedure_type  -> 
            procedure_call loc scope id procedure_type actuals
        | Standard stdproc ->
            standard_procedure loc scope stdproc actuals
        | Record (record_class, field_types) ->
            record_allocation_expression loc scope id record_class field_types actuals
        | Field (rtype, class_number) ->
            designator Lvalue scope dereference
        | Analysis t ->
            if List.length actuals = 1 then
              let arg = expression scope (List.hd actuals) in
              { t = t; c = "alw_$($, $)" $ [ code_of_id id; code_of_loc loc; cast loc t arg] }
            else
              error loc "the procedure %s requires one actual parameter" (Table.Id.to_string id)
        | defn -> 
            error loc "this is %s, it cannot be called or dereferenced" (describe_definition defn)
      )

  (* *** Blocks - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*)

  (* C requires declarations in a block to be a specific order, unlike Algol W.

     1. Code to be executed outside the block.  Array bounds
     expressions must be executed outside the block's scope. So there
     need to be two nested C blocks per Algol W block.

     2. Labels declarations. Gnu C "__label__" declarations must
     appear first in the C block. Gnu C nested functions need these
     declarations.

     3. Function prototypes. "auto" prototypes for the C functions
     that will bedelated in the block. If these appear at the
     top here the order of the actaul function definitions later
     on becomes unimportant.

     4. "struct" declarations for record classes. The functions that
     will provide access to record fields will need to see these.

     5. Declararition for variables. All the function defintions will
     need to see these.

     6. Function definitions. These are Gnu C "auto" nested functions.

     7. Assignment statements to initialize simple variables. It is
     particularly important to initialize reference variables to
     NULL, this prevents prevents "dangling pointers" errors going
     uncaught.

     8. The statements. If the last statment is an expression then
     this is an expression block, which is enclosed in ({ })
     brackets in Gnu C.
  *)
  | Tree.BEGIN (_, declarations, block_items) -> 

      (* Collect the block's various declarartions into the block's
         scope.  Note that the identifiers in RECORD headers are added
         to the block's scope first.  They are needed when creating
         REFERENCE simple types, which can happen anywhere in the
         block, including inside the RECORD declarations
         themselves. Otherwise the order this is done in isn't very
         important. *)
      let block = {empty_block with scope = Scope.push scope} in
      let block = add_record_headers block declarations in
      let block = List.fold_left add_declaration block declarations in
      let block = add_label_declarations block block_items in
      let block = add_procedure_functions block in

      (* There should be at least an empty statement in a syntactically correct block. *)
      if block_items = [] then failwith "CodeGen.block_statements: no items" ;
      let statement_items, return_expression = snip_last block_items in
      let c_statements =
        List.map
          ( function
            | Tree.Label (_, id) -> "$:\n" $ [code_of_id id]
            | statement          -> expression_expect Statement block.scope statement )
          statement_items
      in
      let return_value = expression block.scope return_expression in

      { t = return_value.t;
        c = 
          (if return_value.t = Statement then "{\n${\n$$$$$$$$}\n}\n" else "({\n$({\n$$$$$$$$;\n});\n})") $
            [ block.outsidescope;
              block.labels;
              block.structs;
              block.prototypes;
              block.variables;
              block.functions;
              block.initialization;
              Code.concat c_statements;
              return_value.c ]
      }      

  (* *** Code blocks - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* Inline blocks of raw C code. This is an aw2c extension. *)
  | Tree.C_code (loc, tree_opt, code) -> 
      ( match tree_opt with
      | None ->   (* a block of C c statements *)
          { t = Statement; 
            c = "{\n$\n$\n}\n" $ [Code.string (Location.to_line_directive loc); Code.string code] }
      | Some tree ->   (* a C expression, the tree describes its AlgolW simple type *)
          { t = simple scope tree;
            c = "\n$\n$\n" $ [Code.string (Location.to_line_directive loc); Code.string code] }
      )

  | e -> failwith (sprintf "CodeGen expression: this should not be an expression %s\n" (Tree.str e))


(* [expression_expect t scope tree] returns C code for the Algol W
   expression in [tree], which must be of simple type [t]. *)
and expression_expect (t : simple_t) (scope : Scope.t) (tree : Tree.t) : Code.t =
  let ce = expression scope tree in
  if ce.t = t then
    ce.c
  else
    error (Tree.to_loc tree) "expected %s here, got %s" 
      (describe_simple t) 
      (describe_simple ce.t)
      

(* ** Unary operator expressions ---------------------------------------------------------- *)

(* Algol operators are 'overloaded', [unary_expression] and
   [binary_expression] pick out the correct C code operation by examining
   the operator and the types of its arguments. *)

and unary_expression (loc      : Location.t) 
    (scope    : Scope.t) 
    (operator : Tree.t) 
    (atree    : Tree.t) : typed_code_t =

  let {t=ta; c=ca} as a = expression scope atree in

  match operator, ta with

  (* *** Arithmetic expressions. cf .6.3 *)

  (* *** Identity and negation. c.f. 6.3.3.2 *)

  | Tree.IDENTITY, Number(_,_) ->
      a

  | Tree.NEG, Number(_,_) ->
      { t = ta;
        c = "(- $)" $ [ca] }
        
  (* *** Absolute value. c.f. 6.3.2.6 *)

  | Tree.ABS, Number(_,Integer) ->
      { t = ta;
        c = "alw_abs($)" $ [ca] }

  | Tree.ABS, Number(_,Real) ->
      { t = ta;
        c = "alw_fabs($)" $ [ca] }

  | Tree.ABS, Number(precision, Complex) ->
      { t = Number(precision, Real);
        c = "alw_cabs($)" $ [ca] }

  (* *** Precision of arithmetic.  c.f. 6.3.2.7 *)

  | Tree.LONG, Number(_,Integer) ->
      { t = Number(Long,Real);
        c = "((double) $)" $ [ca] }

  | Tree.LONG, Number(_, ((Real|Complex) as domain)) ->
      { t = Number(Long, domain);
        c = ca }

  | Tree.SHORT, Number(_, ((Real|Complex) as domain)) ->
      { t = Number(Short, domain);
        c = ca }

  (* *** Logical expressions. cf .6.3 *)

  | Tree.NOT, Logical ->
      { t = Logical;
        c = "(! $)" $ [ca] }

  (* *** Bit expressions. cf .6.3 *)

  | Tree.NOT, Bits ->
      { t = Bits;
        c = "(~ $)" $ [ca] }

    (* All remaining combinations of unary operators and operand types are erroneous. *)
          
    | op, t ->
        error loc "Incorrect operand type: %s %s" (Tree.str op) (string_of_simple t)


(* ** Binary operator expressions ---------------------------------------------------------------- *)

and binary_expression (loc      : Location.t) 
                      (scope    : Scope.t) 
                      (atree    : Tree.t) 
                      (operator : Tree.t) 
                      (btree    : Tree.t) : typed_code_t =

  (* The general rule for the numeric type compatibility, c.f. 6.*)
  let triplet_rule t1 t2 =
    try
      unifying_type t1 t2
    with Incompatible ->
        error loc "Incorrect operand types: %s %s %s" 
          (string_of_simple t1)
          (Tree.str operator)
          (string_of_simple t2)
  in
      
  (* C code for relation operators c.f. 6.4.1, <equality operator>, <inequality operator> *)
  let c_equality operator = 
    Code.string 
      ( match operator with
        | Tree.EQ -> "=="
        | Tree.NE -> "!="
        | _ -> failwith "?" )
  in
  let c_inequality operator = 
    Code.string 
      ( match operator with
        | Tree.GT -> ">"
        | Tree.GE -> ">="
        | Tree.LT -> "<"
        | Tree.LE -> "<="
        | _ -> failwith "?" )
  in
  
  let cl = code_of_loc loc in
  let {t=ta; c=ca} = expression scope atree in
  let {t=tb; c=cb} = expression scope btree in

  match operator, ta, tb with

    (*** * Arithmetic expressions c.f. 6.3 *)
    (* C's rules for automatic numeric type casting correspond to Algol W's, so we let C handle that. *)

    | Tree.ADD, Number(_,_), Number(_,_) ->
        { t = triplet_rule ta tb; 
          c = "($ + $)" $ [ca; cb] }

    | Tree.SUB, Number(_,_), Number(_,_) ->
        { t = triplet_rule ta tb; 
          c = "($ - $)" $ [ca; cb] }

    (*** * Multiplication and division. c.f. 6.3.2.1 *)
          
    | Tree.MUL, Number(_,_), Number(_,_) ->
        let modified = 
          match triplet_rule ta tb with
          | Number(_,Integer) as t -> t
          | Number(_,domain) -> Number(Long,domain)
          | _ -> failwith "?"
        in
        { t = modified; 
          c = "($ * $)" $ [ca; cb] }

    | Tree.RDIV, Number(_,Integer), Number(_,Integer) ->
        { t = Number(Long,Real); 
          c = "alw_rdiv($, $, $)" $ [cl; ca; cb] }

    | Tree.RDIV, Number(_,_), Number(_,_) ->
        ( let t0 = triplet_rule ta tb in
          match t0 with
          | Number(_,Real) ->
              { t = t0; 
                c = "alw_rdiv($, $, $)" $ [cl; ca; cb] }
          | Number(_,Complex) ->
              { t = t0; 
                c = "alw_cdiv($, $, $)" $ [cl; ca; cb] } 
          | _ -> failwith "real division triplet rule" )

    (*** * Integer division and remainder. c.f. 6.3.2.3, 6.3.2.4 *)

    | Tree.IDIV, Number(_,Integer), Number(_,Integer) ->
        { t = Number(Long,Integer); c = "alw_div($, $, $)" $ [cl; ca; cb] }

    | Tree.REM, Number(_,Integer), Number(_,Integer) ->
        { t = Number(Long,Integer); c = "alw_rem($, $, $)" $ [cl; ca; cb] }

    (*** * Power operator. c.f. 6.3.2.5 *)

    | Tree.PWR, Number(_,(Integer|Real)), Number(_,Integer) ->
        { t = Number(Long,Real); 
          c = "alw_rpwr($, $, $)" $ [cl; ca; cb] }

    | Tree.PWR, Number(_,Complex), Number(_,Integer) ->
        { t = Number(Long,Complex); 
          c = "alw_cpwr($, $, $)" $ [cl; ca; cb] }

    (* Logical expressions. c.f. 6.4 *)

    (*** * Logical operators. These are "shortcut operators" like C has.  cf.6.4.2.2 *)

    | Tree.AND, Logical, Logical -> 
        { t = Logical; 
          c = "($ && $)" $ [ca; cb]  }

    | Tree.OR, Logical, Logical -> 
        { t = Logical; 
          c = "($ || $)" $ [ca; cb]  }

    (* Relations. c.f. 6.4.1. *)

    (* Equality. (See rules about symbols tau_6 and tau_7.) *)
    (* Comparision of LOGICAL values was an ALGOLW extension to Algol W . *)
    (* Note that LOGICAL is represented by zero/non-zero 'int' values in C,
       the comparision to zero ensures that non-zero values are converted to 1 
       (I think.) *)

    | (Tree.EQ | Tree.NE), Bits, Bits ->
        { t = Logical; 
          c = "($ $ $)" $ [ca; c_equality operator; cb] }

    | (Tree.EQ | Tree.NE), String 1, String 1 ->
        { t = Logical; 
          c = "($ $ $) " $ [ca; c_equality operator; cb] }
    | (Tree.EQ | Tree.NE), String 1, String lenb ->
        { t = Logical; 
          c = "(alw_str_cmp_cs($, $, $) $ 0) " $ [ca; cb; code_of_int lenb; c_equality operator] }
    | (Tree.EQ | Tree.NE), String lena, String 1 ->
        { t = Logical; 
          c = "(alw_str_cmp_sc($, $, $) $ 0) " $ [ca; code_of_int lena; cb; c_equality operator] }
    | (Tree.EQ | Tree.NE), String lena, String lenb ->
        { t = Logical; 
          c = "(alw_str_cmp($, $, $, $) $ 0) " $ [ca; code_of_int lena; cb; code_of_int lenb; c_equality operator] }

    | (Tree.EQ | Tree.NE), (Reference(_) | Null), (Reference(_) | Null) ->
        { t = Logical; 
          c = "($ $ $)" $ [ca; c_equality operator; cb] }

    | (Tree.EQ | Tree.NE), Number(_,_), Number(_,_) ->
        { t = Logical; 
          c = "($ $ $)" $ [ca; c_equality operator; cb] }

    | (Tree.EQ | Tree.NE), Logical, Logical ->
        { t = Logical; 
          c = "((($) != 0)  $ (($) != 0))" $ [ca; c_equality operator; cb] }

    (* Inequality. (See rules about symbols tau_8 and tau_9.) *)

    | (Tree.GT  | Tree.GE  | Tree.LT  | Tree.LE), String 1, String 1 ->
        { t = Logical; 
          c = "(alw_str_cmp_cc($, $) $ 0) " $ [ca; cb; c_inequality operator] }
    | (Tree.GT  | Tree.GE  | Tree.LT  | Tree.LE), String 1, String lenb ->
        { t = Logical; 
          c = "(alw_str_cmp_cs($, $, $) $ 0) " 
            $ [ca; cb; code_of_int lenb; c_inequality operator] }
    | (Tree.GT  | Tree.GE  | Tree.LT  | Tree.LE), String lena, String 1 ->
        { t = Logical; 
          c = "(alw_str_cmp_sc($, $, $) $ 0) " 
            $ [ca; code_of_int lena; cb; c_inequality operator] }
    | (Tree.GT  | Tree.GE  | Tree.LT  | Tree.LE), String lena, String lenb ->
        { t = Logical; 
          c = "(alw_str_cmp($, $, $, $) $ 0) " 
            $ [ca; code_of_int lena; cb; code_of_int lenb; c_inequality operator] }

    | (Tree.GT  | Tree.GE  | Tree.LT  | Tree.LE), Number(_,(Real|Integer)), Number(_,(Real|Integer)) ->
        { t = Logical; 
          c = "($ $ $)" $ [ca; c_inequality operator; cb] }

    | (Tree.GT  | Tree.GE  | Tree.LT  | Tree.LE), Logical, Logical ->
        { t = Logical; 
          c = "((($) != 0) $ (($) != 0))" $ [ca; c_inequality operator; cb] }

    (*** * Bit expressions. c.f. 6.5 *)

    | Tree.AND, Bits, Bits -> 
        { t = Bits; 
          c = "($ & $)" $ [ca; cb]  }

    | Tree.OR, Bits, Bits -> 
        { t = Bits; 
          c = "($ | $)" $ [ca; cb]  }

    | Tree.SHL, Bits, Number(_,Integer) -> 
        { t = Bits; 
          c = "alw_shl($, $, $)" $ [cl; ca; cb]  }

    | Tree.SHR, Bits, Number(_,Integer) -> 
        { t = Bits; 
          c = "alw_shr($, $, $)" $ [cl; ca; cb]  }

    (* All remaining combinations of binary operators and operand types are erroneous. *)
          
    | op, t1, t2 ->
        error loc "Incorrect operand types: %s %s %s" (string_of_simple t1) (Tree.str op) (string_of_simple t2)


(* ** Procedure calls ------------------------------------------------------------------------- *)

(*
  1. Declarations for temporary variables and "thunks":
     * variables to hold RESULT or VALUE RESULT parameters;
     * variables to hold String(n) VALUE parameters;
     * a variable to hold the result of the function call if there are RESULT parameters (see 4);
     * thunks for Name parameters;
     * thunks for sliced array parameters;
     * thunks for statments and expressions passed as PROCEDURE parameters.
  2. Code to execute before the procedure call:
     * Assignments of actual parameters to VALUE temporary variables;
     * a call to "alw_trace_procedure_called", if procedure call tracing is on;
  3. The procedure call;
  4. Code to execute after the procedure call:
     * a call to "alw_trace_procedure_exited", if procedure call tracing is on;
     * Assignments to RESULT parameters to back actual parameters;
  5. the result variable (if any)

  "Thunks" are temporary functions used to pass actual parameters into
  procedures, typically they calculate the address of the actual
  parameter's value when called.
*)
and procedure_call (loc : Location.t) 
    (scope : Scope.t) 
    (procedure_id : Table.Id.t) 
    ((return_type, formals) : procedure_t) 
    (actuals : Tree.t list)
    : typed_code_t =

  let parameter_info =
    (* List of (temporary variable name, actual parameter code, formal parameter type)
       for each parameter in the procedure call.*)
    try
      mapi 1 
        (fun i (a, f) -> ("_$_arg$" $ [code_of_id procedure_id; code_of_int i], a, f)) 
        (List.combine actuals formals)
    with Invalid_argument _ -> 
      error loc "%s expects %i actual parameters" (Table.Id.to_string procedure_id) (List.length formals)
  in
  
  let (declarations, value_assignments, parameters, result_assignments) = 
    List.fold_left 
      (add_call_parameter scope)
      ([], [], [], []) 
      parameter_info 
  in
  let declaration_code = Code.rseparate ""   declarations in
  let parameter_code   = Code.rseparate ", " parameters in
  let precall_code = 
    if !Options.trace then 
      "alw_trace_procedure_called($, $);\n$" $ 
        [ code_of_loc loc; 
          c_str_const (Table.Id.to_string procedure_id);
          Code.rseparate "" value_assignments]
    else
      Code.rseparate "" value_assignments
  in
  let postcall_code = 
    if !Options.trace then 
      "$alw_trace_procedure_exited($, $);\n" $ 
        [ Code.rseparate "" result_assignments;
          code_of_loc loc; 
          c_str_const (Table.Id.to_string procedure_id) ]
    else
      Code.rseparate "" result_assignments
  in
  let call_code =
    if declarations = [] && not !Options.trace then
        (* This is an ordinary C function call. *)
      if return_type = Statement then
        "$($);\n" $ [code_of_id procedure_id; parameter_code]
      else
        "$($)" $ [code_of_id procedure_id; parameter_code]
    else if return_type = Statement then
      "{\n$$$($);\n$}\n" 
        $ [ declaration_code; 
            precall_code;
            code_of_id procedure_id; parameter_code; 
            postcall_code ]
    else
      let return_var = "_$_ret" $ [code_of_id procedure_id] in
      "({\n$$ $;\n$$ = $($);\n$$;\n})\n" 
        $ [ declaration_code; 
            ctype return_type; return_var; 
            precall_code;
            return_var; code_of_id procedure_id; parameter_code; 
            postcall_code;
            return_var ]
  in
  { t = return_type; c = call_code }


and add_call_parameter scope (declarations, value_assignments, parameters, result_assignments) (var, actual, formal) =

  let loc = Tree.to_loc actual in

  match formal with

  (* *** value - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* 
     Declaration:  integer procedure f(integer value n); 
     Algol W call: i := f(1)
     C call:       i = f(1);

     Declaration:  procedure p(string(2) value s);
     Algol W call: p("X");
     C call:       { alw_chr _p_arg1[2]; 
                     alw_str_cpy(_p_arg1, 2, "X", 1); 
                     p(_p_arg1); }

     Declaration:  integer procedure f(string(2) value s);
     Algol W call: i := f("X") 
     C call:       i = ({ alw_chr _f_arg1[2]; 
                          alw_str_cpy(_f_arg1, 2, "X", 1); 
                          f(_f_arg1); });
  *)
  | By_value (String n as ftype) when n > 1 ->
      let ftype_var = {t = ftype; c = var} in
      ( declare_simple ftype var :: declarations, 
        assignment_statement loc ftype_var (expression scope actual) :: value_assignments,
        var :: parameters, 
        result_assignments )

  | By_value ftype ->
      ( declarations, 
        value_assignments,
        (cast loc ftype (expression scope actual)) :: parameters, 
        result_assignments )

  (* *** value result - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* 
     Declaration:  integer procedure p(integer value result n);
     Algol W call: p(a(i))
     C call:       { int *_p_arg1; 
                     *_p_arg1 = *a(alw_HERE, i); 
                     p(&_p_arg1); 
                     *a(alw_HERE, i) = *_p_arg1; }
 
     (Note how '*a(alw_HERE, i)', the function to select element 'i' of array 'a', gets called twice -
      once before the procedure is entered and once after. This is supposed to happen: c.f. 5.3.2.2.)

     Declaration:  procedure p(string(2) value result s);
     Algol W call: p(t)
     C call:       { alw_chr s[2]; 
                     alw_str_cpy(s, 2, t, 2);
                     p(s); 
                     alw_str_cpy(t, 2, s, 2); }

     Declaration:  integer procedure f(integer value result _p_arg1);
     Algol W call: i := f(t)
     C call:       i = ({ int _ret_f;
                          int *_f_arg1;
                          *_f_arg1 = 0; 
                          _ret_f := f(&_f_arg1); 
                          t = *_f_arg1; 
                          _ret_f; 
                       });
  *)
  | By_value_result ftype ->
      let ftype_var = {t = ftype; c = var} in
      ( declare_simple ftype var :: declarations, 
        assignment_statement loc ftype_var (expression scope actual) :: value_assignments,
        address_of ftype var :: parameters,
        assignment_statement loc (designator Lvalue scope actual) ftype_var :: result_assignments  )

  (* *** result - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (* The only difference from VALUE RESULT parameters is that the temporary variable is 
     assigned a default value rather than the acutual parameter's value.

     Declaration:  integer procedure p(integer result n); 
     Algol W call: p(x)
     C call:       { int *_p_arg1; 
                     *_p_arg1 = 0; 
                     p(&_p_arg1); 
                     x = *_p_arg1; }
  *)

  | By_result ftype ->
      let ftype_var = {t = ftype; c = var} in
      ( declare_simple ftype var :: declarations, 
        initialize_simple ftype var :: value_assignments,
        address_of ftype var :: parameters,
        assignment_statement loc (designator Lvalue scope actual) ftype_var :: result_assignments  )

  (* *** procedure - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (*
    If the actual parameter is a procedure with a same type as the formal parameter 
    then it is be passed directly:

     Declaration:  procedure p(integer procedure q(integer r)); ...
                   integer procedure f(integer i); ...
     Algol W call: p(f)
     C call:       p(f);

    If formal parameter is of a procedure type type with has no formal parameters then the
    the actual parameter may be a expression. The expression is to be wrapped in a temporary
    "thunk" function:

     Declaration:  procedure p(integer procedure q);
     Algol W call: p(x * 2 + 1)
     C call:       { int _p_arg1() { return x * 2 + 1; }
                     p(_p_arg1); }

    It's the same story with statements:

     Declaration:  procedure p(procedure q);
     Algol W call: p(print(x * 2 + 1))
     C call:       { void _p_arg1() { print(x * 2 + 1); }
                     p(_p_arg1); }
  *)

  | By_procedure (ftype, []) ->
      let use_thunk () =
        let procedure_thunk =
          if ftype = Statement then
            "void $(void) { $ }\n" $ [var; expression_expect Statement scope actual]
          else
            let e = expression scope actual in
            if equal_simple_types ftype e.t then
              "$ $(void) { return $; }\n" $ [ctype ftype; var; cast ~copy:true loc ftype e]
            else
              error loc  "expected %s, this is %s"  (describe_formal formal) (describe_simple e.t)
        in
        ( procedure_thunk :: declarations, 
          value_assignments,
          var :: parameters, 
          result_assignments )
      in
      ( match actual with
      | Tree.Variable (loc, procedure_id) ->
          ( match get loc scope procedure_id with
          | Procedure (t, []) when equal_simple_types t ftype -> 
              ( declarations, 
                value_assignments,
                code_of_id procedure_id :: parameters, 
                result_assignments )
          | _ -> use_thunk()
          )
      | _ -> use_thunk()
      )
  | By_procedure formal_procedure ->
      ( match actual with
      | Tree.Variable (loc, procedure_id) ->
          ( match get loc scope procedure_id with
          | Procedure actual_procedure when equal_procedure_types actual_procedure formal_procedure -> 
              ( declarations, 
                value_assignments,
                code_of_id procedure_id :: parameters, 
                result_assignments )
          | Procedure actual_procedure  -> 
              error loc "expected %s here, this is %s" 
                (describe_procedure formal_procedure)
                (describe_procedure actual_procedure)
          | d ->
              error loc "expected %s here, this is %s" (describe_formal formal) (describe_definition d)
          )
      | _ -> error loc "expected %s here" (describe_formal formal)
      )

  (* *** name - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (*
     Designator actual parameter.

     Declaration:  integer procedure p(integer n);
     Algol W call: p(a(i))
     C call:       { int *_p_arg1(void) { return a(alw_HERE, i); }
                     p(_p_arg1); }

     Expression actual parameter:

     Declaration:  integer procedure p(integer n);
     Algol W call: p(i * 2 + 1)
     C call:       { int _p_arg1_temp;
                     int *_p_arg1(void) { _p_arg1_temp = i * 2 + 1; return &_p_arg1_temp; }
                     p(_p_arg1); }

    If the actual parameter is another name formal parameter with the same type, then
    there is no need to make another thunk, it can be passed directly:

     Declaration:  integer procedure p(integer n);
     Algol W call: p(x)
     C call:       p(x);
  *)

  | By_name ftype ->
      let name_error kind t =
        error loc "%s %s is not compatible with %s name parameter" 
          (describe_simple t) 
          kind
          (describe_simple ftype) 
      in
      let use_thunk () =
        match designator_or_expression Pointer scope actual with
        | Designator dcode ->
            if equal_simple_types ftype dcode.t then
              let designator_thunk = "$ $(void){ return $; }\n" $ [c_pointer_type ftype; var; dcode.c] 
              in
              ( designator_thunk :: declarations, 
                value_assignments,
                var :: parameters, 
                result_assignments )
            else
              name_error "variable" dcode.t
        | Expression ecode ->
            if equal_simple_types ftype ecode.t then
              let temp_var = "$_temp" $ [var] in
              let expression_thunk = 
                "$ $(void){ $; return $; }\n" 
                  $ [ c_pointer_type ftype; var; 
                      assignment_statement loc {t = ftype; c = temp_var} ecode; 
                      address_of ftype temp_var ]
              in
              ( expression_thunk :: declare_simple ftype temp_var :: declarations,
                value_assignments,
                var :: parameters, 
                result_assignments )
            else
              name_error "expression" ecode.t
      in
      let pass_name_variable variable_id =
        let defn = get loc scope variable_id in
        match defn with 
        | Name t ->
            if equal_simple_types ftype t then
              ( declarations, 
                value_assignments,
                code_of_id variable_id :: parameters, 
                result_assignments )
            else
              name_error "name variable" t
        | _ ->
            use_thunk ()
      in
      ( match actual with
      | Tree.Variable (_, id) -> pass_name_variable id
      | _ -> use_thunk ()
      )

  (* *** array - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

  (*   If A is declared as REAL ARRAY(*,*,*,*,*) then:
  
       the actual parameter "A" becomes "A";
       the actual parameter "A(*,*,*,*,*)" becomes "A";
       i.e. in both cases the array element selector thunk is reused;
       
       the actual parameter "A(*,1+2,*,3+4,*)", an array slice,
       becomes a new thunk that calls A's thunk, the stars become the
       thunk's parameters and the expression parameters are supplied
       to A's thunk as constants, the actual parameter becomes this:
      
       ({ 
          /* one constant for each expression subscript: */
          const int sub2 = 1+2;  
          const int sub4 = 3+4; 

          /* The array slice thunk, one parameter for each star subscript: */ 
          double *_array( alw_loc loc, int _sub1, int _sub3, int _sub5 )
          {
            return A( <source-location>, _sub1, _sub2, _sub3, _sub4, sub5 );
          } 
   
          _array;
       })
  *)
  | By_array (ftype, n_dimensions) -> 

      ( match actual with

      | Tree.Variable (loc, id) ->
          ( match get loc scope id with
          | Array (t, adims) when equal_simple_types ftype t && adims = n_dimensions -> 
              ( declarations, 
                value_assignments,
                code_of_id id :: parameters, 
                result_assignments )
          | defn -> 
              error loc "expected %s here, this is %s" (describe_formal formal) (describe_definition defn)
          )

      | Tree.Dereference (loc, array_id, actuals) ->

          let defn = get loc scope array_id in
          ( match defn with
          | Array (t, n_dimensions) when equal_simple_types ftype t -> 
              if List.length actuals <> n_dimensions then
                error loc "expected %s here, this is %s" (describe_formal formal) (describe_definition defn)
              else
                let (_, constant_declarations, parameter_declarations, array_parameters) =
                  List.fold_left
                    ( fun (n , constant_declarations, parameter_declarations, array_parameters) actual ->
                        let parameter = "$_sub$" $ [var; code_of_int n] in 
                        match actual with
                        | Tree.STAR _ ->
                            ( n + 1, 
                              constant_declarations, 
                              ("int $" $ [parameter]) :: parameter_declarations,
                              parameter :: array_parameters )
                        | expr ->
                            let ecode = expression_expect integer scope expr in
                            ( n + 1, 
                              ("const int $ = $;\n" $ [parameter; ecode]) :: constant_declarations, 
                              parameter_declarations,
                              parameter :: array_parameters )
                    )
                    (0, [], [], [])
                    actuals
                in
                let constant_declaration_code  = Code.rseparate ""   constant_declarations in
                let parameter_declaration_code = Code.rseparate ", " parameter_declarations in
                let array_parameter_code       = Code.rseparate ", " array_parameters in
                
                if List.length parameter_declarations = n_dimensions then (* all the subscripts are stars *)
                  ( declarations, 
                    value_assignments,
                    code_of_id array_id :: parameters, 
                    result_assignments )
                else
                  let thunk = 
                    "$ $(alw_loc loc, $) { 
                     return $($, $); 
                    }\n" $ [ c_pointer_type ftype; var; parameter_declaration_code; 
                           code_of_id array_id; code_of_loc loc; array_parameter_code ]
                  in
                  ( thunk :: constant_declaration_code :: declarations,
                    value_assignments,
                    var :: parameters, 
                    result_assignments )
          | _ -> 
              error loc "expected %s here, this is %s" (describe_formal formal) (describe_definition defn)
          )

      | _ -> error loc "expected %s here" (describe_formal formal)
      )


(* ** Record allocation "designators" --------------------------------------------------------- *)

and record_allocation_expression (loc          : Location.t) 
                                 (scope        : Scope.t) 
                                 (record_id    : Table.Id.t) 
                                 (record_class : Class.t) 
                                 (field_types  : simple_t list) 
                                 (actuals      : Tree.t list)  : typed_code_t =
    let n_actuals = List.length actuals in
    let n_fields = List.length field_types in
    if n_actuals <> 0 && n_actuals <> n_fields then
      error loc "%s expects 0 or %d parameters" (Table.Id.to_string record_id) n_fields
    else if n_actuals = 0 then
        { t = Reference (ClassSet.singleton record_class);
          c = "$($, $)" $ [ code_of_id record_id; code_of_loc loc; 
                            Code.separate ", " (List.map default field_types) ] }
    else
      let add_field_parameter (declarations, assignments, parameters, count) (field_t, actual) =
        let loc' = Tree.to_loc actual in
        let e = expression scope actual in
        if e.t = Statement || assignment_compatible field_t e.t then
          match field_t with
          | String n when n > 1 ->
              let var = "_field$" $ [code_of_int (count + 1)] in
              let assignment =
                match e.t with
                | Statement -> initialize_simple field_t var
                | _         -> assignment_statement loc' {t=field_t; c=var} e
              in
              ( declare_simple field_t var :: declarations, 
                assignment :: assignments,
                var :: parameters, 
                count + 1 )
          | _ ->
              let parameter =
                match e.t with
                | Statement -> default field_t
                | _         -> cast loc' field_t e
              in
              ( declarations, 
                assignments,
                parameter :: parameters, 
                count + 1 )
        else
          error loc "expected %s expression here, this is %s" (describe_simple field_t) (describe_simple e.t)
      in
      let declarations, assignments, parameters, count = 
        List.fold_left
          add_field_parameter
          ([], [], [], 0)
          (List.combine field_types actuals)
      in
      if List.length declarations = 0 then
        { t = Reference (ClassSet.singleton record_class);
          c = "$($, $)" $ [ code_of_id record_id; code_of_loc loc; Code.rseparate ", " parameters ] }
      else
        { t = Reference (ClassSet.singleton record_class);
          c = "({ $$$($, $); })" $ [ Code.rseparate "" declarations;
                                     Code.rseparate "" assignments;
                                     code_of_id record_id; code_of_loc loc; 
                                     Code.rseparate ", " parameters ] }



(* ** Standard Input/Output statements ------------------------------------------------------------ *)


(* Input/Output System statements become a block of C function calls,
   one for each actual parameter, based on the parameter's simple
   type. 

   The editing variables are saved in "_editing_state" at the start 
   of an standard procedure statement and restored after. C.f. section 7.9.4.  

   Example:
     WRITE(i, st, r)       comment integer, statement, real;
   becomes
   { alw_Editing_t _editing_state;          
     alw_Editing_save(&_editing_state);     /* save the editing variables */
     alw_iocontrol(<loc>, 2);               /* iocontrol to start a new line */
     alw_write_integer(<loc>, i);           /* write the integer parameter */
     st();                                  /* execute the statement parameter */
     alw_write_real(<loc>, r);              /* write the real parameter */
     alw_Editing_restore(&_editing_state);  /* restore the editing variables */
   }

   The "alw_push_editing" and "alw_pop_editing" save and restore the "editing variables".
   
*)
and standard_procedure (loc : Location.t) (scope : Scope.t) 
                       (stdproc : standard) (actuals : Tree.t list) : typed_code_t =

    let write parameter =
      let ploc = Tree.to_loc parameter in
      let pa = expression scope parameter in
      match pa.t with
      | Statement -> pa.c
      | Number(_, Integer) ->     "alw_write_integer($, $);\n" $ [code_of_loc loc; pa.c]
      | Number(Short, Real) ->    "alw_write_real($, $);\n" $ [code_of_loc loc; pa.c]
      | Number(Long, Real) ->     "alw_write_long_real($, $);\n" $ [code_of_loc loc; pa.c]
      | Number(Short, Complex) -> "alw_write_complex($, $);\n" $ [code_of_loc loc; pa.c]
      | Number(Long, Complex)  -> "alw_write_long_complex($, $);\n" $ [code_of_loc loc; pa.c]
      | Logical ->                "alw_write_logical($, $);\n" $ [code_of_loc loc; pa.c]
      | Bits ->                   "alw_write_bits($, $);\n" $ [code_of_loc loc; pa.c]
      | String 1 ->               "alw_write_char($, $);\n" $ [code_of_loc loc; pa.c]
      | String length ->          "alw_write_string($, $, $);\n" $ [code_of_loc loc; pa.c; code_of_int length]
      | Reference _ ->            "alw_write_reference($, $);\n" $ [code_of_loc loc; pa.c]
      | _ -> error ploc "%s cannot be written" (describe_simple pa.t)
    in

    let writecard parameter =
      let ploc = Tree.to_loc parameter in
      let pa = expression scope parameter in
      match pa.t with
      | Statement -> pa.c
      | String 1 -> 
          "alw_iocontrol($, 2);\nalw_write_char($, $);\n" $ [code_of_loc loc; code_of_loc loc; pa.c]
      | String length -> 
          "alw_iocontrol($, 2);\nalw_write_string($, $, $);\n" $ [code_of_loc loc; code_of_loc loc; pa.c; code_of_int length]
      | t -> error ploc "Expected a statement or string expression, this is %s expression" (describe_simple t)
    in

    let readcard parameter =
      match designator_or_expression Pointer scope parameter with
      | Designator d ->
          ( match d.t with
          | String 1 -> "alw_readcard_char($, $);\n" $ [code_of_loc loc; d.c]
          | String n -> "alw_readcard($, $, $);\n" $ [code_of_loc loc; d.c; code_of_int n]
          | t -> error loc "%s cannot be read by READCARD" (describe_simple t)
          )
      | Expression e ->
          ( match e.t with
          | Statement -> e.c
          | t -> error loc "Expected a designator or statement, this is %s expression" (describe_simple t)
          )
    in

    let read parameter =
      match designator_or_expression Pointer scope parameter with
      | Designator d ->
          ( match d.t with
          | String 1 ->          "alw_read_char($, $);\n" $ [code_of_loc loc; d.c]
          | String length  ->    "alw_read_string($, $, $);\n" $ [code_of_loc loc; d.c; code_of_int length]
          | Number(_,Integer) -> "alw_read_integer($, $);\n" $ [code_of_loc loc; d.c]
          | Number(_,Real) ->    "alw_read_real($, $);\n" $ [code_of_loc loc; d.c]
          | Number(_,Complex) -> "alw_read_complex($, $);\n" $ [code_of_loc loc; d.c]
          | Bits ->              "alw_read_bits($, $);\n" $ [code_of_loc loc; d.c]
          | Logical ->           "alw_read_logical($, $);\n" $ [code_of_loc loc; d.c]
          | t -> error loc "%s cannot be read" (describe_simple t)
          )
      | Expression e ->
          ( match e.t with
          | Statement -> e.c
          | t -> error loc "Expected a designator or statement, this is %s expression" (describe_simple t)
          )
    in

    let iocontrol parameter =
      let e  = expression scope parameter in
      match e.t with
      | Statement -> e.c
      | Number(_, Integer) -> "alw_iocontrol($, $);\n" $ [code_of_loc loc; e.c]
      | t -> 
          let loc = Tree.to_loc parameter in
          let s = describe_simple t in
          error loc "IOCONTROL expects INTEGER or statement actual parameters, this is %s" s
    in

    let io_block f initial final =
        { t = Statement;
          c = "{ alw_Editing_t _editing_state;
                 alw_Editing_save(&_editing_state);
                 $$$
                 alw_Editing_restore(&_editing_state);
               }
              " $ [initial; Code.concat (List.map f actuals); final] }
    in
    match stdproc with
    | Writeon   -> io_block write     Code.empty Code.empty
    | Write     -> io_block write     ("alw_iocontrol($, 2);\n" $ [code_of_loc loc]) Code.empty
    | Writecard -> io_block writecard Code.empty ("alw_iocontrol($, 2);\n" $ [code_of_loc loc])
    | Iocontrol -> io_block iocontrol Code.empty Code.empty
    | Readcard  -> io_block readcard  Code.empty Code.empty
    | Read      -> io_block read      ("alw_iocontrol($, 1);\n" $ [code_of_loc loc]) Code.empty
    | Readon    -> io_block read      Code.empty Code.empty
        


(* * Designators ---------------------------------------------------------------------------------- *)

and designator_or_expression (flavour : designator_t) (scope : Scope.t) (tree : Tree.t) : designator_or_expression_t =
  (* This parses a tree as either a designator or expression, trying
     for a designator first. Name actual parameters need this
     behaviour. *)

  let qualifier exprs_flavour t =
    (* E.g. if we have a pointer variable, but we want an lvalue, we prefix it with "*" *)
    match t, exprs_flavour, flavour with
    | String n, _,       _       when n > 1 -> Code.empty
    | _,        Pointer, Lvalue             -> Code.string "*"
    | _,        Lvalue,  Pointer            -> Code.string "&"
    | _,        _,       _                  -> Code.empty
  in

  match tree with
  | Tree.Variable (loc, id) -> 
      ( match get loc scope id with
      | Variable t -> Designator { t = t; c = "$$"   $ [qualifier Lvalue  t; code_of_id id] }
      | Result t   -> Designator { t = t; c = "$$"   $ [qualifier Pointer t; code_of_id id] }
      | Name t     -> Designator { t = t; c = "$$()" $ [qualifier Pointer t; code_of_id id] }
      | _          -> Expression (expression scope tree)
      )
  | Tree.Dereference (loc, id, actuals) ->
      ( match get loc scope id with
      | Array (etype, ndims) ->
          if List.length actuals <> ndims then
            error loc "Array '%s' requires %i parameter%s" (string_of_id id) ndims (if ndims = 0 then "" else "s") ;
          let cdims = List.map (expression_expect integer scope) actuals in
          Designator 
            { t = etype;
              c = "$$($, $)" $ [qualifier Pointer etype; code_of_id id; code_of_loc loc; Code.separate ", " cdims]
            }
      | Field (field_type, field_class) ->
          let field_name = Table.Id.to_string id in
          if List.length actuals <> 1 then
            error loc "Record field designator %s should have one actual parameter" field_name
          else
            let actual = List.hd actuals in
            let reference = expression scope actual in
            ( match reference.t with
            | Reference class_set when Type.ClassSet.mem field_class class_set ->
                Designator 
                  { t = field_type; 
                    c = "$$($, $)" $ [ qualifier Pointer field_type; code_of_id id; code_of_loc loc; reference.c ] }
            | Reference _ ->
                error (Tree.to_loc actual) "%s can never have the field %s" 
                  (describe_simple reference.t) 
                  field_name
            | _ ->
                error loc "Expecting a reference, this is %s" (describe_simple reference.t)
            )
      | _ -> 
          Expression (expression scope tree)
      )
  | Tree.Substring (loc, desig, index, length) ->
      let src = designator Pointer scope desig in
      let index_code = expression_expect integer scope index in
      ( match src.t with
      | String srclen when length <= srclen ->
          Designator
            { t = String length;
              c = "$alw_str_sub($, $, $, $, $)" 
                $ [ qualifier Pointer (String length);
                    code_of_loc loc; 
                    src.c; code_of_int srclen; 
                    index_code; code_of_int length ] }
      | String _ ->
          error loc "%s can never contain %i character substrings " (describe_simple src.t) length
      | t -> 
          error loc "The substring operator expects a STRING variable on the right, this is %s" (describe_simple t)
      )
  | _ -> 
      Expression (expression scope tree)


and designator (flavour : designator_t) (scope : Scope.t) (tree : Tree.t) : typed_code_t =
  match designator_or_expression flavour scope tree with
  | Designator dcode -> dcode
  | Expression ecode -> error (Tree.to_loc tree) "expected a designator here, this is %s expression"
      (describe_simple ecode.t)


(* * Declarations --------------------------------------------------------------------------------------------- *)

and add_declaration (block : block_t) (decl : Tree.t) : block_t =
  match decl with
  | Tree.Simple (loc, t, ids) -> 
      List.fold_left
        (fun d id ->
           let t = simple block.scope t in
           { d with 
               scope          = set loc d.scope id (Variable t);
               variables      = Code.add d.variables (declare_simple t (code_of_id id));
               initialization = Code.add d.initialization (initialize_simple t (code_of_id id))
           } )
        block
        ids

  (* Array bounds are evaluated outside the block they are in. The
     variables on the same level as the block will not have been
     initialized. *)
  | Tree.ARRAY (loc, t, ids, bounds) -> 
      let elttype = simple block.scope t in
      let cbounds = 
        List.map 
          ( fun (l,u) -> 
              let outside_scope = Scope.pop block.scope in
              ( expression_expect integer outside_scope l, 
                expression_expect integer outside_scope u ) )
          bounds
      in
      List.fold_left (add_array_declaration loc elttype cbounds) block ids

  | Tree.PROCEDURE (loc, t_opt, id, formal_trees, body) -> 
      add_procedure_declaration loc block t_opt id formal_trees body

  | Tree.RECORD (loc, id, fields) -> 
      add_record_declaration loc block id fields

  | Tree.C_code (loc, _, code) ->
      let c_declaration = "\n$\n$\n" $ [Code.string (Location.to_line_directive loc); Code.string code] in
      { block with functions = Code.add block.functions c_declaration }

  | e -> 
      failwith (sprintf "CodeGen.add_declaration: not a declaration: %s\n" (Tree.str e))
  

(* ** Record declarations ---------------------------------------------------------------------- *)

and add_record_declaration (loc : Location.t)
                           (block : block_t) 
                           (record_id : Table.Id.t) 
                           (field_declarations : Tree.t list) : block_t =


  (* The previously recorded class for this record: *)
  let record_class = 
    match get loc block.scope record_id  with
    | Type.Record (c, []) -> c
    | _ -> failwith "add_record_declaration: record_class not previously defined"
  in
  let class_code = code_of_id (Class.to_id record_class) in
  
  (* The types and ids of the record's fields, in order: *)
  let fields : (simple_t * Table.Id.t) list =
    List.fold_left 
      ( fun ds decl ->
          match decl with
          | Tree.Simple (loc, t, ids) -> 
              let t = simple block.scope t in
              let fields = List.map (fun id -> (t, id)) ids in
              ds @ fields
          | d -> error (Tree.to_loc d) "Records may only contain simple variable declarations"
      )
      []
      field_declarations
  in

  (* Redefine the record in the block: add code and field types. *)
  let r = code_of_id record_id in
  let record_struct =
    let field_declarations =
      let declaration (t, id) = declare_simple t (code_of_id id) in
      Code.concat (List.map declaration fields)
    in
    "const char *alw_class_$ = $;
     struct $ {
       const char *_class;
       int _number;
       $
     };
    " $ [r; class_code; r; field_declarations]
  in
  let record_prototype =
    let field_arguments = 
      let argument (t, id) = "$ $" $ [ctype t; code_of_id id] in
      Code.separate ", " (List.map argument fields)
    in
    "struct $ *$(alw_loc loc, $)" $ [r; r; field_arguments]
  in
  let record_function =
    let field_assignments = 
      let assignment (t, id) = 
        assignment_statement loc 
          {t = t; c = "ref->$" $ [code_of_id id]} 
          {t = t; c = code_of_id id} 
      in
      Code.concat (List.map assignment fields)
    in        
    "$ {
       struct $ *ref = (struct $ *)alw_allocate_record(loc, sizeof(struct $));
       ref->_class = $;
       ref->_number = ++alw_record_counter;
       $
       return (void *)ref;
     }
    " $ [ record_prototype;
          r; r; r;
          class_code;
          field_assignments;
        ]
  in
  let block =
    let field_types = List.map fst fields in
    { block with
        scope = Scope.redefine block.scope record_id (Record (record_class, field_types));
        structs = Code.add block.structs record_struct ;
        prototypes = ("$auto $;\n" $ [block.prototypes; record_prototype]);
        functions = Code.add block.functions record_function }
  in

  (* Add the record's fields in the block. *)
  let field_prototype t field_id = 
    "$ $ (alw_loc loc, void *ref)" $ [c_pointer_type t; code_of_id field_id] 
  in
  let field_function t field_id prototype = 
    let pointer = address_of t ("((struct $ *)ref)->$" $ [code_of_id record_id; code_of_id field_id]) in
    "$ {
       alw_ref_field_check(loc, ref, $, $);
       return $;
     }
    " $ [ prototype; 
          class_code; c_str_const (Table.Id.to_string field_id);
          pointer ]
  in
  let block =
    List.fold_left 
      ( fun block' decl ->
          match decl with
          | Tree.Simple (loc, t, field_ids) -> 
              let t = simple block.scope t in
              List.fold_left
                ( fun block'' field_id ->
                    let p = field_prototype t field_id in
                    let f = field_function t field_id p in
                    { block'' with
                        scope = set loc block''.scope field_id (Field (t, record_class));
                        prototypes = ("$auto $;\n" $ [block''.prototypes; p]);
                        functions = Code.add block''.functions f } )
                block'
                field_ids
          | d -> error (Tree.to_loc d) "Records may only contain simple variable declarations" )
      block
      field_declarations
  in
  block


(* This adds empty record declarations to the scope (the records'
   fields are ignored at this stage).  All other declarations,
   including record fields, need to be able look up record identifiers
   to determine reference simple types.  *)
and add_record_headers (block : block_t) (block_items : Tree.t list) : block_t =
  List.fold_left
    (fun d item ->
       match item with
       | Tree.RECORD (loc, id, _) -> 
           { d with scope = set loc d.scope id (Record (Class.create loc id, []))  }
       | _ -> d )
    block
    block_items


(* ** Procedure declarations ---------------------------------------------------------------- *)


and add_procedure_declaration loc block t_opt id formal_trees body =
  let returntype =
    match t_opt with
    | None -> Statement
    | Some t -> simple block.scope t
  in
  let parameters = 
    List.fold_left 
      (add_formal_segment block.scope)
      empty_formal_parameters
      formal_trees
  in
  let header = 
    match body with
    | Tree.Foreign (_, _, _) ->
        "$ $($)" $ [ ctype returntype; 
                     code_of_id id;
                     Code.separate ", " parameters.arguments ]
    | _ ->
        "$ $($)" $ [ctype returntype; code_of_id id; Code.separate ", " parameters.arguments]
  in
  let prototype = 
    match body with
    | Tree.Foreign (loc, language_id, external_reference) ->
        if language_id = Table.Id.create "macro" then
          Code.string ("\n" ^ external_reference ^ "\n")
        else
          let external_reference_alias = 
            if external_reference <> "" && external_reference <> string_of_id id then
              if is_valid_c_identifier external_reference then
                "\n$\n#define $ $\n" 
                  $ [ Code.string (Location.to_line_directive loc);
                      code_of_id id; Code.string external_reference ]
              else
                error loc "the external reference string does not contain a valid C identifier"
            else
              Code.empty
          in
          if language_id = Table.Id.create "auto" then
            "$auto $;\n" $ [external_reference_alias; header]
          else
            "$$;\n" $ [external_reference_alias; header]
    | _ ->
        "auto $;\n" $ [header]
  in
  let proc = 
    { returntype = returntype;
      proc_id    = id;
      proc_loc   = loc;
      parameters = parameters; 
      header     = header; 
      body       = body } (* to be defined later, in the complete block scope *)
  in
  { block with
      scope      = set loc block.scope id (Procedure (returntype, parameters.formal_types));
      prototypes = Code.add block.prototypes prototype;
      procedures = block.procedures @ [proc]
  }

  
and add_procedure_functions (block : block_t) : block_t =
  let add_function (block : block_t) (procedure : procedure_header_t) : block_t =
    match procedure.body with 
    | Tree.Foreign (_, _, _) ->  (* These don't need functions. *)
        block
    | _ ->
        let function_code = 
          let loc = Tree.to_loc procedure.body in
          let procedure_parameter_scope = procedure.parameters.procedure_locals :: block.scope in
          let procedure_body_scope = Scope.push procedure_parameter_scope in
          let trace = 
            if !Options.trace then
              "alw_trace_procedure_entered($, $);\n" $ 
                [ code_of_loc procedure.proc_loc; 
                  c_str_const (Table.Id.to_string procedure.proc_id)]
            else
              Code.empty
          in
          let body = expression procedure_body_scope procedure.body in
          if body.t <> Statement then 
            "$ {\n$return $;\n }\n" $ [procedure.header; trace; cast ~copy:true loc procedure.returntype body] 
          else if procedure.returntype = Statement then
            "$ {\n$$ }\n" $ [procedure.header; trace; body.c]
          else
            error loc "this procedure should return %s, but this is a statement" 
              (describe_simple procedure.returntype)
        in
        { block with functions = Code.add block.functions function_code }
  in
  List.fold_left add_function block block.procedures


and add_formal_segment (scope : Scope.t) (formals : formal_parameters_t) (segment : Tree.t) : formal_parameters_t =

  let pointer_qualifier t =
    match t with
    | String _ -> Code.empty
    | _        -> Code.string "*"
  in

  match segment with
  | Tree.VALUE_formal (loc, t, ids) ->
      let t = simple scope t in
      List.fold_left 
        ( fun d id -> 
            { procedure_locals = set_local loc d.procedure_locals id (Variable t);
              formal_types    = d.formal_types @ [By_value t];
              arguments       = d.arguments @ ["$ $" $ [ctype t; code_of_id id]]
            } )
        formals
        ids

  | Tree.VALUE_RESULT_formal (loc, t, ids) ->
      let t = simple scope t in
      List.fold_left 
        ( fun d id -> 
            let r = code_of_id id in
            { procedure_locals = set_local loc d.procedure_locals id (Result t);
              formal_types    = d.formal_types @ [By_value_result t];
              arguments       = d.arguments @ ["$ $" $ [c_pointer_type t; r]];
            } )
        formals
        ids

  | Tree.RESULT_formal (loc, t, ids) ->
      let t = simple scope t in
      List.fold_left 
        ( fun d id -> 
            let r = code_of_id id in
            { procedure_locals = set_local loc d.procedure_locals id (Result t);
              formal_types    = d.formal_types @ [By_result t];
              arguments       = d.arguments @ ["$ $" $ [c_pointer_type t; r]];
            } )
        formals
        ids

  | Tree.PROCEDURE_formal (loc, t_opt, ids, formal_trees) ->
      let t = 
        match t_opt with
        | None -> Statement
        | Some t -> simple scope t 
      in
      let fs =  (* the formal parameters to this formal procedure *)
        List.fold_left 
          (add_formal_segment scope)
          empty_formal_parameters
          formal_trees
      in
      List.fold_left 
        ( fun d id -> 
            { procedure_locals = set_local loc d.procedure_locals id (Procedure (t, fs.formal_types));
              formal_types     = d.formal_types @ [By_procedure (t, fs.formal_types)];
              arguments        = d.arguments @ ["$ ($$)($)" $ [ ctype t; pointer_qualifier t; code_of_id id; 
                                                                Code.separate ", " fs.arguments ] ]
            } )
        formals
        ids

  | Tree.Name_formal (loc, t, ids) ->
      let t = simple scope t in
      List.fold_left 
        ( fun d id -> 
            { procedure_locals = set_local loc d.procedure_locals id (Name t);
              formal_types     = d.formal_types @ [By_name t];
              arguments        = d.arguments @ [ "$ ($$)(void)" 
                                                   $ [c_pointer_type t; pointer_qualifier t; code_of_id id] ]
            } )
        formals
        ids

  | Tree.ARRAY_formal (loc, t, ids, ndims) ->
      let t = simple scope t in
      List.fold_left 
        ( fun d id -> 
            assert (ndims > 0);
            let cformal = 
              let rec array_args = 
                function
                | 1 -> "int"
                | n -> "int," ^ array_args (n - 1)
              in 
              "$ $(alw_loc,$)" $ [c_pointer_type t; code_of_id id; Code.string (array_args ndims)]
            in
            { procedure_locals = set_local loc d.procedure_locals id (Array (t, ndims));
              formal_types     = d.formal_types @ [By_array (t, ndims)];
              arguments        = d.arguments @ [cformal];
            } )
        formals
        ids

  | e -> 
      failwith (sprintf "CodeGen.add_formal_segment: this is not a formal: %s\n" (Tree.str e))


(* ** Label declarations ------------------------------------------------------------------- *)

(* These become Gnu C local label declarations. They need to be the first
   things declared in a C block. *)

and add_label_declarations (block : block_t) (block_items : Tree.t list) : block_t =
  List.fold_left
    (fun d item ->
       match item with
       | Tree.Label (loc, label) -> 
           { d with 
               scope = set loc d.scope label Label;
               labels = Code.add d.labels ("__label__ $;\n" $ [code_of_id label])
           }
       | _ -> d )
    block
    block_items


(* ** Array declarations ---------------------------------------------------------------------  *)

(* Arrays need all sorts of special temporary variables and initializations
   because they are not very like C arrays: they can be multidimensional and
   can have non-zero lower bounds. *)

and add_array_declaration 
    (loc : Location.t)
    (elttype : simple_t) 
    (bounds : (Code.t * Code.t) list) 
    (block : block_t)
    (id : Table.Id.t) 
    : block_t =

  let array_id = code_of_id id in
  let ndims = List.length bounds in

  let initialize_bounds =
    let initialize_bound dim (cl, cu) =
      "const int _$_lwb$ = $, _$_upb$ = $;\n" $ [ array_id; code_of_int dim; cl; array_id; code_of_int dim; cu] 
    in
    Code.concat (mapi 1 initialize_bound bounds)
  in
  
  let array_variable =
    let dimensions =
      Code.concat
        ( mapn 1 ndims
            (fun i -> "[_$_upb$ - _$_lwb$ + 1]" $ [array_id; code_of_int i; array_id; code_of_int i]) )
    in
    match elttype with
    | String n when n > 1 -> "alw_chr _$_array$[$];\n" $ [array_id; dimensions; code_of_int n]
    | t ->                   "$ _$_array$;\n" $ [ctype t; array_id; dimensions]
  in
  
  let header = 
    "$ $(alw_loc loc, $)" $
      [ c_pointer_type elttype;
        array_id;
        Code.separate ", " (mapn 1 ndims (fun i -> "int _sub$" $ [code_of_int i])) ]
  in
  
  let subscripts = 
    Code.concat (mapn 1 ndims (fun i -> "[_sub$ - _$_lwb$]" $ [code_of_int i; array_id; code_of_int i])) 
  in

  let designator_function =
    let subscript_range_check dim = 
      "alw_array_range_check($, $);" $ [array_id; code_of_int dim] 
    in
    let element_ptr = address_of elttype ("_$_array$" $ [array_id; subscripts]) in
    "$ 
     {
       $
       return $;
     }
    " $ [ header; 
          Code.concat (mapn 1 ndims subscript_range_check); 
          element_ptr ]
  in
  
  let array_bounds_check =
    let check dim = 
      "alw_array_bounds_check($, $, $);" $ 
        [array_id; code_of_loc loc; code_of_int dim] in
    Code.concat (mapn 1 ndims check)
  in

  let array_initialization =
    let for_loop_var n = "_sub$" $ [code_of_int n] in
    let for_loop n = 
      let cn = code_of_int n in
      "for (_sub$ = _$_lwb$; _sub$ <= _$_upb$; ++_sub$)" $ 
        [cn; array_id;cn; cn; array_id;cn; cn]
    in
    let element = "_$_array$" $ [array_id; subscripts] in
    "{ 
       int $;
       $
       $ 
       $
     }
    " $ [ Code.separate ", " (mapn 1 ndims for_loop_var);
          array_bounds_check;
          Code.separate "\n" (mapn 1 ndims for_loop);
          initialize_simple elttype element ]
  in
  
  { block with
      scope          = set loc block.scope id (Array (elttype, ndims)); 
      outsidescope   = Code.add block.outsidescope   initialize_bounds;
      variables      = Code.add block.variables      array_variable;
      prototypes     = Code.add block.prototypes     ("auto $;\n" $ [header]);
      functions      = Code.add block.functions      designator_function;
      initialization = Code.add block.initialization array_initialization
  }

(* end *)
