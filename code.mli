(* code.ml 

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
(** Quickly concatenatable scraps of C code text. *)

(** If this is raised it indicates a bug in the program. *)
exception TemplateError of string

(** A scrap of C code. *)
type t 

val empty : t

(** [of_string s] converts the  string [s] to a scrap. *)
val string : string -> t

(** [separate seperator scrap_list] concatenates a list of C code scraps,
    using the string [separator] as a separator. *)
val separate : string -> t list -> t


(** [separate seperator scrap_list] concatenates a list of C code scraps,
    in reverse order, using the string [separator] as a separator. *)
val rseparate : string -> t list -> t

val concat : t list -> t
val add : t -> t -> t

(** [template template scrap_list] makes a C code scrap, replacing [$]
    signs in [template] with C code scraps from [scrap_list] 
    @raises TemplateError if the number of [$]s and code scraps don't match *)
val template : string -> t list -> t

(** [output_code channel scrap] outputs [scrap] to [channel] as a string. *)
val output_code : out_channel -> t -> unit

(** [is_empty scrap] returns true if [scrap] represents an empty string. *)
val is_empty : t -> bool

(* end *)
