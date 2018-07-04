(* testprograms.ml -- script to test lots of little Algol W programs. 

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

open Printf ;;

exception Syntax_error of int ;;

let nfails = ref 0 ;;
let halt_on_error = ref false ;;

let strip_whitespace s =
  let rec loop = function
    | -1 -> ""
    | i when s.[i] = ' ' -> loop (i - 1)
    | i when s.[i] = '\t' -> loop (i - 1)
    | i -> (String.sub s 0 (i + 1))
  in
  loop (String.length s - 1)
;;

let read_whole_file filename =
  if Sys.file_exists filename then
    let chan = open_in filename in
    let lines = ref [] in 
    ( try
        while true do
          lines := (strip_whitespace (input_line chan) ^ "\n") :: !lines
        done
      with End_of_file ->
        close_in chan 
    );
    let s = String.concat "" (List.rev !lines) in
    if s <> "" && s.[String.length s - 1] <> '\n' then
      s ^ "\n"
    else
      s
  else 
    ""
;;

let write_whole_file filename contents =
  let chan = open_out filename in
  output_string chan contents;
  close_out chan
;;

let starts s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  l2 <= l1 && String.sub s1 0 l2 = s2
;;  

let input_test chan =
  let compile = ref "" in
  let stdout = ref "" in
  let stdin = ref "" in
  let stderr = ref "" in
  let exitcode = ref 0 in
  let line = ref "" in
  let linenum = ref 0 in
  let text = Buffer.create 1024 in
  let eatlines eof_okay =
    Buffer.clear text;
    try 
      line := strip_whitespace (input_line chan);
      incr linenum;
      while not (starts !line "----") do
        Buffer.add_string text !line;
        Buffer.add_char text '\n';
        line := strip_whitespace (input_line chan);
        incr linenum;
      done;
      Buffer.contents text
    with End_of_file ->
      if eof_okay then
        Buffer.contents text
      else
        raise (Syntax_error !linenum)
  in
  let eatcode () =
    try
      Scanf.sscanf (eatlines false) " %i " (fun i -> i);
    with Failure _->
      raise (Syntax_error !linenum)
  in
  ignore (eatlines true);
  if starts !line "----" then
    begin
      if starts !line "----stdin" then
        stdin := eatlines false;
      if starts !line "----compile" then
        compile := eatlines false;
      if starts !line "----stdout" then
        stdout := eatlines false;
      if starts !line "----stderr" then 
        stderr := eatlines false;
      if starts !line "----exitcode" then 
        exitcode := eatcode () ;
      if not (starts !line "----end") then raise (Syntax_error !linenum);
    end;
  (!compile, !stdin, !stdout, !stderr, !exitcode)
;;  

let run_commands cs =
  let rec loop =
    function
    | [] -> 0
    | c :: cs' ->
        let exitcode = Sys.command c in
        if exitcode = 0 then
          loop cs'
        else
          exitcode
  in loop cs
;;

let run_test filename compile stdin stdout stderr exitcode =
  print_endline filename;
  let s = String.sub filename 0 (String.index filename '.') in
  let compilation_exitcode = 
    run_commands
      [ "rm -f /tmp/a.out /tmp/compile /tmp/stderr /tmp/stdin /tmp/stdout";
        sprintf "./aw2c %s.alw -o %s.c 2>/tmp/compile" s s;
        sprintf "gcc -I. %s.c libalw.a -lgc -lm -o /tmp/a.out 2>/tmp/compile" s ]
  in
  let () = write_whole_file "/tmp/stdin" stdin in
  let exitcode' = 
    if compilation_exitcode = 0 then
      Sys.command "/tmp/a.out </tmp/stdin >/tmp/stdout 2>/tmp/stderr"
    else
      0  (* i.e. try to ignore it *)
  in
  let compile' = read_whole_file "/tmp/compile" in
  let stderr'  = read_whole_file "/tmp/stderr"  in
  let stdout'  = read_whole_file "/tmp/stdout"  in
  if compile'  <> compile || 
    stderr'   <> stderr  || 
    stdout'   <> stdout  ||
    exitcode' <> exitcode 
  then
    begin
      if compile' <> compile then
        printf "*** Got this compiler message:\n%s\nExpected:\n%s\n" compile' compile ;
      if stdout' <> stdout then
        printf "*** Got this output on stdout:\n%s\nExpected:\n%s\n" stdout' stdout ;
      if stderr' <> stderr then
        printf "*** Got this output on stderr:\n%s\nExpected:\n%s\n" stderr' stderr ;
      if exitcode' <> exitcode then
        printf "*** Got exit code %i, expected %i\n" exitcode' exitcode ;
      incr nfails ;
      if !halt_on_error then exit 1
    end
;;

let test_file filename =
  let chan = open_in filename in
  let compile, stdin, stdout, stderr, exitcode = 
    try 
      input_test chan
    with Syntax_error linenum ->
      begin
        close_in chan ;
        fprintf stderr "File %S, line %i: test file syntax error\n" filename linenum ;
        exit 1 
      end
  in
  close_in chan ;
  run_test filename compile stdin stdout stderr exitcode
;;

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-h" then
      halt_on_error := true
    else
      test_file Sys.argv.(i)
  done ;
  if !nfails = 0 then
    printf "testprograms: All Algol W test programs passed!\n"
  else
    begin
      printf "testprograms: %i test failed.\n" !nfails ;
      exit 1
    end
;;

(* end *)
