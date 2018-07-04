(* dynArray.mli 

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
(** A dynamically extensible array. New elements are added by pushing
    them onto the end. *)

(* An array of element of the type ['a]. *)
type 'a t = {
  mutable length : int;
  mutable arr : 'a array;
  filler : 'a
} ;;


(** [create len fill] returns an array containing [len] elements of the value [fill]. *)
val create : int-> 'a -> 'a t

(** [get array index] gets an array element by index. *)
val get : 'a t -> int -> 'a

(** [push array element] pushs a new element onto the end of the array. *)
val push : 'a t -> 'a -> unit

(** [length array] returns the number of elements in the array. *)
val length : 'a t -> int

(** Removes all elements from the array. *)
val clear : 'a t -> unit

(* end *)

