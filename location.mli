(* location.mli 

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

(* Lexing.position has a file name field, but it is not used, hence the set_filename function *)

type t
val set_filename : string -> unit
val create : string -> int -> int -> t
val of_position : Lexing.position -> t  (* uses filename from set_filename *)
val to_string : t -> string
val to_line_directive : t -> string
val contents : t -> string * int * int

(* end *)
