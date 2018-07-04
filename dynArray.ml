(* dynArray.ml 

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


let min_initial_size = 20 ;;


(** [create len fill] returns an array containing [len] elements of the value [fill]. *)
let create (len : int) (fill : 'a) : 'a t =
  assert (len >= 0) ;
  { length = len;
    arr = Array.create (max min_initial_size (len * 2)) fill;
    filler = fill }
;;

(** [get array index] gets an array element by index. *)
let get (a : 'a t) (i : int) : 'a =
  assert (i >= 0 &&  i < a.length) ;
  Array.get a.arr i
;;


(** [push array element] pushs a new element onto the end of the array. *)
let push (a : 'a t) (x : 'a) : unit =
  let len = Array.length a.arr in
  if len = a.length then
    a.arr <- Array.append a.arr (Array.create len a.filler) ;
  Array.set a.arr a.length x ;
  a.length <- a.length + 1
;;


(** [length array] returns the number of elements in the array. *)
let length (a : 'a t) : int =
  a.length
;;


(** Removes all elements from the array. *)
let clear (a : 'a t) : unit =
  a.length <- 0;
  a.arr <- Array.create min_initial_size a.filler
;;

(* end *)

