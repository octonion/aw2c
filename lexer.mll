(* lexer.mll -- Algol W lexical analyser 

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

{
  open Parser
  open Lexing

  exception Error of Location.t * string
  let error message pos = raise (Error (Location.of_position pos, message))

  (* Start position of tokens with their own parsers. *)
  let start_pos = ref dummy_pos  

  (* Start position of the current line. *)
  let start_of_line = ref 0

  (* Increment the line position for error messages. (See 'Lexing' module.) *)
  let new_line lexbuf =
    start_of_line := lexbuf.lex_curr_p.pos_cnum ;
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with 
                           pos_bol = lexbuf.lex_curr_p.pos_cnum ; 
                           pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1 } 


  (* Make real numbers from their bits and pieces *)
  let make_real real_part opt_exp_part opt_imaginary_modifier opt_long_modifier =
    let real_part = if (real_part.[0] = '.') then ("0" ^ real_part) else real_part in
    let real_part = if (real_part.[String.length real_part - 1] = '.') then (real_part ^ "0") else real_part in
    let exp_part = match opt_exp_part with None -> "" | Some s -> s in
    match opt_imaginary_modifier, opt_long_modifier with
    | None, None     -> Real (real_part, exp_part)
    | Some _, None   -> Imaginary (real_part, exp_part)
    | None, Some _   -> LongReal (real_part, exp_part)
    | Some _, Some _ -> LongImaginary (real_part, exp_part)

  (* Buffer for the content of string constants. *)
  let string_buffer = Buffer.create 80
  let string_add s = Buffer.add_string string_buffer s

  (* Buffer for the contents of inline C code. *)
  let c_code_buffer = Buffer.create 80
  let c_add s = Buffer.add_string c_code_buffer s
  let c_nesting_level = ref 0

  (* Reserved words. *)
  let tokens_of_reserved_words : Parser.token Table.IdMap.t = 
    List.fold_left
      (fun m (s, t) -> Table.IdMap.add (Table.Id.create s) t m)
      Table.IdMap.empty
      [ ("abs",       ABS);
        ("and",       AND);
        ("array",     ARRAY);
        ("assert",    ASSERT);
        ("begin",     BEGIN);
        ("bits",      BITS);
        ("boolean",   LOGICAL);
        ("case",      CASE);
        ("complex",   COMPLEX);
        ("div",       DIV);
        ("do",        DO);
        ("else",      ELSE);
        ("end",       END);
        ("false",     FALSE);
        ("for",       FOR);
        ("goto",      GOTO);
        ("if",        IF);
        ("integer",   INTEGER);
        ("is",        IS);
        ("logical",   LOGICAL);
        ("long",      LONG);
        ("not",       NOT);
        ("null",      NULL);
        ("of",        OF);
        ("or",        OR);
        ("procedure", PROCEDURE);
        ("real",      REAL);
        ("record",    RECORD);
        ("reference", REFERENCE);
        ("rem",       REM);
        ("result",    RESULT);
        ("shl",       SHL);
        ("short",     SHORT);
        ("shr",       SHR);
        ("step",      STEP);
        ("string",    STRING);
        ("then",      THEN);
        ("true",      TRUE);
        ("until",     UNTIL);
        ("value",     VALUE);
        ("while",     WHILE) ]
      
  let token_to_string = 
    function
      | Identifier x -> Table.Id.to_string x
      | Integer x ->  x
      | Real (r,"") -> r
      | Real (r,e) -> Printf.sprintf "%s'%s" r e
      | Imaginary (r,"") -> Printf.sprintf "%sI" r
      | Imaginary (r,e) -> Printf.sprintf "%s'%sI" r e
      | LongReal (r,"") -> Printf.sprintf "%sL" r
      | LongReal (r,e) -> Printf.sprintf "%s'%sL" r e
      | LongImaginary (r,"") -> Printf.sprintf "%sIL" r
      | LongImaginary (r,e) -> Printf.sprintf "%s'%sIL" r e
      | String x -> Printf.sprintf "%S" x
      | Bits x -> Printf.sprintf "#%s" x
      | C x -> x
      | EOF -> "{eof}"
      | STARSTAR -> "**"
      | STAR -> "*"
      | SLASH -> "/"
      | PLUS -> "+"
      | MINUS -> "-"
      | NOT -> "~"
      | NE -> "~="
      | EQ -> "="
      | LT -> "<"
      | LE -> "<="
      | GT -> ">"
      | GE -> ">="
      | BRA -> "("
      | KET -> ")"
      | COMMA -> ","
      | COLONCOLON -> "::"
      | COLON -> ":"
      | SEMICOLON -> ";"
      | BAR -> "|"
      | ASSIGN -> ":="
      | FULLSTOP -> "."
      | ABS -> "ABS"
      | AND -> "AND"
      | ARRAY -> "ARRAY"
      | ASSERT -> "ASSERT"
      | BEGIN -> "BEGIN"
      | BITS -> "BITS"
      | CASE -> "CASE"
      | COMPLEX -> "COMPLEX"
      | DIV -> "DIV"
      | DO -> "DO"
      | ELSE -> "ELSE"
      | END -> "END"
      | FALSE -> "FALSE"
      | FOR -> "FOR"
      | GOTO -> "GOTO"
      | IF -> "IF"
      | INTEGER -> "INTEGER"
      | IS -> "IS"
      | LOGICAL -> "LOGICAL"
      | LONG -> "LONG"
      | LONG_COMPLEX -> "LONG COMPLEX"
      | LONG_REAL -> "LONG REAL"
      | NULL -> "NULL"
      | OF -> "OF"
      | OR -> "OR"
      | PROCEDURE -> "PROCEDURE"
      | REAL -> "REAL"
      | RECORD -> "RECORD"
      | REFERENCE -> "REFERENCE"
      | REM -> "REM"
      | RESULT -> "RESULT"
      | SHL -> "SHL"
      | SHORT -> "SHORT"
      | SHR -> "SHR"
      | STEP -> "STEP"
      | STRING -> "STRING"
      | THEN -> "THEN"
      | TRUE -> "TRUE"
      | UNTIL -> "UNTIL"
      | VALUE -> "VALUE"
      | WHILE -> "WHILE"

  (* Testing can be added here. *)
  let return_token t = 
     (* print_endline (token_to_string t) ; *)
     t
}

let integer_number = ['0'-'9']+
let unscaled_real = integer_number '.' integer_number | '.' integer_number | integer_number '.'
let exponent = ['+' '-']? integer_number
let tenpower = '\''

let nl = '\n' | "\r\n" | "\n\r" | '\r'

(* The not sign can be '~' (aw2c extension), a latin-1 or UTF-8 not sign, or the keyword "NOT" *)
let not_sign = '~' | '\172' | "\194\172" | ['N' 'n']['O' 'o']['T' 't']

rule token = parse

(* Whitespace *)
| [' ' '\t'] { token lexbuf }
| nl         { new_line lexbuf ; token lexbuf }

(* Symbols *)
| ':' [' ' '\t']* '=' { return_token ASSIGN }
| ':' [' ' '\t']* ':' { return_token COLONCOLON }
| '*' [' ' '\t']* '*' { return_token STARSTAR }
| not_sign [' ' '\t']* '=' { return_token NE }
| '<' [' ' '\t']* '=' { return_token LE }
| '>' [' ' '\t']* '=' { return_token GE }
| not_sign { return_token NOT }
| ')'     { return_token KET }
| '('     { return_token BRA }
| '+'     { return_token PLUS }
| '*'     { return_token STAR }
| '-'     { return_token MINUS }
| ','     { return_token COMMA }
| '/'     { return_token SLASH }
| '<'     { return_token LT }
| '='     { return_token EQ }
| ';'     { return_token SEMICOLON }
| ':'     { return_token COLON }
| '|'     { return_token BAR }
| '>'     { return_token GT }

(* Numeric constant. *)    

| (unscaled_real as r) (tenpower (exponent as e))? (['I' 'i'] as i)? (['L' 'l'] as l)?
    { return_token (make_real r e i l) }

| tenpower (exponent as e) (['I' 'i'] as i)? (['L' 'l'] as l)?
    { return_token (make_real "1.0" (Some e) i l) }

| (integer_number as r) (tenpower (exponent as e)) (['I' 'i'] as i)? (['L' 'l'] as l)?
    { return_token (make_real (r ^ ".0") (Some e) i l) }

| (integer_number as r) ['I' 'i'] (['L' 'l'] as l)?
    { return_token (make_real (r ^ ".0") None (Some "I") l) }

| (integer_number as r) ['L' 'l']
    { return_token (make_real (r ^ ".0") None None (Some "L")) }

| integer_number as s
    { return_token (Integer s) }

(* Hexadecimal BITS constant. *)    
| '#' (['0'-'9' 'A'-'F' 'a'-'f']+ as s)
    { return_token (Bits (String.uppercase s)) }

(* The start of a string constant. *)    
| '"'  
    { start_pos := lexbuf.lex_start_p ;
      Buffer.clear string_buffer ; 
      string_constant lexbuf 
    }

(* The start of a comment. *)
| ['C' 'c']['O' 'o']['M' 'm']['M' 'm']['E' 'e']['N' 'n']['T' 't']  
    { start_pos := lexbuf.lex_start_p ;
      algolw_comment false lexbuf 
    }

| '%'
    { start_pos := lexbuf.lex_start_p ;
      algolw_comment true lexbuf 
    }

(* Reserved words with embedded whitespace. *)
| ['G' 'g']['O' 'o']
  [' ' '\t']*
  ['T' 't']['O' 'o']
    { return_token GOTO }
| ['L' 'l']['O' 'o']['N' 'n']['G' 'g']
  [' ' '\t']+
  ['R' 'r']['E' 'e']['A' 'a']['L' 'l']
      { return_token LONG_REAL }
| ['L' 'l']['O' 'o']['N' 'n']['G' 'g']
  [' ' '\t']+
  ['C' 'c']['O' 'o']['M' 'm']['P' 'p']['L' 'l']['E' 'e']['X' 'x']
    { return_token LONG_COMPLEX }

(* Reserved words and identifiers. *)
| ['A'-'Z' 'a'-'z'] ['A'-'Z' '_' 'a'-'z' '0'-'9']* as word
    { let id = Table.Id.create word in
      try
        return_token (Table.IdMap.find id tokens_of_reserved_words)
      with Not_found ->
        return_token (Identifier id)
    }

(* End-of-file. A fullstop marks the end of the program: anything after it is ignored. *)
| eof { return_token EOF }
| '.' { return_token FULLSTOP }

(* Inline C code. (An AW2C extension.) *)
| '{' 
    { start_pos := lexbuf.lex_start_p ;
      Buffer.clear c_code_buffer ; 
      c_nesting_level := 0 ;
      c_code lexbuf
    }

(* Compiler directive for a long comment, for commenting out sections of code. (An AW2C extension.) *)
| '@' ['A' 'a']['W' 'w'] '2' ['C' 'c'] '_'
      ['T' 't']['E' 'e']['X' 'x']['T' 't'] 
      [' ' '\t']* nl
    { if lexbuf.lex_start_p.pos_cnum = !start_of_line then
        ( start_pos := lexbuf.lex_start_p ;
          new_line lexbuf ; 
          algolw_long_comment lexbuf  )
      else
        let s = lexeme lexbuf in
        let directive = String.sub s 0 (String.length s - 1) in
        error (Printf.sprintf "Misplaced directive \"%s\"" directive) lexbuf.lex_start_p
    }

(* Other compiler directives.  AW2C ignores these. *)
| '@' [^ '\r' '\n']* nl
    { if lexbuf.lex_start_p.pos_cnum = !start_of_line then
        ( new_line lexbuf ; token lexbuf ) (* ignore *)
      else
        let s = lexeme lexbuf in
        let directive = String.sub s 0 (String.length s - 1) in
        error (Printf.sprintf "Misplaced directive \"%s\"" directive) lexbuf.lex_start_p
    }

| _ as c  { error (Printf.sprintf "Unexpected character %C" c) lexbuf.lex_start_p }


(* Comment.  Returns nothing, behaves like whitespace. Short comments may end on a '%' *)
and algolw_comment short = parse
| ';'   { lexbuf.lex_start_p <- !start_pos ; token lexbuf }
| '%'   { if short then (lexbuf.lex_start_p <- !start_pos ; token lexbuf) else algolw_comment short lexbuf }
| nl    { new_line lexbuf ; algolw_comment short lexbuf }
| eof   { error "This comment is not closed with a semicolon" !start_pos }
| _     { algolw_comment short lexbuf }


(* Long comment.  Returns nothing, behaves like whitespace. *)
and algolw_long_comment = parse
| '@' ['A' 'a']['W' 'w'] '2' ['C' 'c'] '_'
      ['C' 'c']['O' 'o']['D' 'd']['E' 'e'] 
      [' ' '\t']* nl
    { if lexbuf.lex_start_p.pos_cnum = !start_of_line then
        (new_line lexbuf ; lexbuf.lex_start_p <- !start_pos ; token lexbuf)  (* return *)
      else
        (new_line lexbuf ; algolw_long_comment lexbuf)  (* ignore *) }
| '@' ['A' 'a']['W' 'w'] '2' ['C' 'c'] '_'
      ['T' 't']['E' 'e']['X' 'x']['T' 't'] 
      [' ' '\t']* nl
    { if lexbuf.lex_start_p.pos_cnum = !start_of_line then
        error "This @AW2C_TEXT directive is not closed with an @AW2C_CODE directive" !start_pos
      else
        (new_line lexbuf ; algolw_long_comment lexbuf) (* ignore *) }
| eof   
    { error "This @AW2C_TEXT directive is not closed with an @AW2C_CODE directive" !start_pos }
| nl    
    { new_line lexbuf ; algolw_long_comment lexbuf }
| _
    { algolw_long_comment lexbuf }


(* String constant. Returns the contents of the string. *)
and string_constant = parse
| "\"\"" { string_add "\"" ; string_constant lexbuf }  (* A doubled quote is an escape for a quote. *)
| "\""   { return_token (String (Buffer.contents string_buffer)) }
| eof    { error "This string is not closed with a double quote" !start_pos }
| nl     { error "This string is not closed with a double quote" !start_pos }
| _      { let s = lexeme lexbuf in
           let c = s.[0] in
           if c >= ' ' && c <= '~' || c >= '\xA1' && c <= '\xFF' then
             (* The 'Algol W Language Description' allows only allows
                the basic set of EDCDIC printable characters inside
                string constants [4.2.2., Appendix A]. But as an
                extension Aw2c allows every Latin-1/IBM4047 printable
                character. *)
             ( string_add s ; string_constant lexbuf )
           else
             error "This string contains a non-printing character code (is it UTF-8 encoded?)" !start_pos
}


and c_code = parse
| '}'    { 
           if !c_nesting_level = 0 then
             ( lexbuf.lex_start_p <- !start_pos ;
               return_token (C (Buffer.contents c_code_buffer)) )
           else 
             ( c_add "}" ;
               decr c_nesting_level ; 
               c_code lexbuf )
         }
| '{'    { c_add "{" ; 
           incr c_nesting_level ; 
           c_code lexbuf 
         }
| "/*"   { c_add "/*" ; c_comment lexbuf }
| "*/"   { error "There is a C comment without an opening '/*' in here" !start_pos }
| "\""   { c_add "\"" ; c_string lexbuf }
| nl     { c_add "\n" ; new_line lexbuf ; c_code lexbuf }
| eof    { error "This C block is not closed with a '}'" !start_pos }
| _      { c_add (lexeme lexbuf); c_code lexbuf }

and c_string = parse
| nl     { error "There is a C string without a closing '\"' in here" !start_pos }
| "\""   { c_add "\"" ; c_code lexbuf }
| "\\\"" { c_add "\\\"" ; c_string lexbuf }
| eof    { error "There is a C string without a closing '\"' in here" !start_pos }
| _      { c_add (lexeme lexbuf) ; c_string lexbuf }

and c_comment = parse
| "*/"   { c_add "*/" ; c_code lexbuf }
| "/*"   { error "There is a nested C comment in here" !start_pos }
| nl     { c_add "\n" ; new_line lexbuf ; c_comment lexbuf }
| eof    { error "There is a C comment without a closing '*/' in here" !start_pos }
| _      { c_add (lexeme lexbuf); c_comment lexbuf }

(* end *)
