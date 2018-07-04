(* a2wc.ml -- minimal compiler 

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


let message = "
AW2C Algol W to Gnu C compiler.

Command line: 

    aw2c [<algol-source-file>] [-o <gnuc-output-file>]
    aw2c --version

The standard input and output are used by default.
"
;;


let version () =
  Printf.printf "Release: %s\n" Version.release ;
  exit 0
;;

let error (loc : Location.t) (message : string) : unit =
  Printf.fprintf stderr "%s %s\n" (Location.to_string loc) message ;
  exit 1
;;


let compile (source : string) (input : in_channel) (output : out_channel) : unit =
  Location.set_filename source ;
  let lexbuf = Lexing.from_channel input in
  try
    let tree = Parser.program Lexer.token lexbuf in
    let code = CodeGen.entry_point tree in 
    Code.output_code output code;
  with
  | Lexer.Error (loc, message) -> error loc message
  | Parsing.Parse_error -> error (Location.of_position (Lexing.lexeme_start_p lexbuf)) "Syntax error"
  | CodeGen.Error (loc, message) -> error loc message
;;    


let main () : unit =
  let source = ref "<stdin>" in
  let input  = ref stdin in
  let output = ref stdout in
  let open_input pathname =
    if !input != stdin then
      raise (Arg.Bad "aw2c expects only one source file")
    else
      ( source := pathname ;
        try
          input := open_in pathname
        with Sys_error _ ->
          raise (Arg.Bad (Printf.sprintf "aw2c cannot open source file '%s'" pathname)) )
  in
  let open_output pathname =
    if !output != stdout then
      raise (Arg.Bad "aw2c expects only one output file")
    else
      try
        output := open_out pathname
      with Sys_error _ ->
          raise (Arg.Bad (Printf.sprintf "aw2c cannot create output file '%s'" pathname))
  in
  let options = 
    [ ("-o", Arg.String open_output, "Path name for the C code output");
      ("--output", Arg.String open_output, "Path name for the C code output");
      ("-v", Arg.Unit version, "Version number");
      ("--version", Arg.Unit version, "Version number");
      ("--trace", Arg.Set Options.trace, "Insert procedure call tracing code");
    ] 
  in
  Arg.parse options open_input message ;
  compile !source !input !output ;
  close_in !input ;
  close_out !output
;;

main ()


(* end *)  
