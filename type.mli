(* type.mli 

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

module ClassSet : Set.S with type elt = Class.t

(* Definitions for identifiers in the scope: *)

type definition_t =
  | Variable of simple_t                   (* Simple-type variables, VALUE parameter variables. *)
  | Array of simple_t * int                (* ARRAY variables. *)
  | Name of simple_t                       (* Name parameter variables. *)
  | Result of simple_t                     (* RESULT or VALUE RESULT parameter variables. *)
  | Control                                (* FOR statment control variables. *)
  | Procedure of procedure_t               (* Procedures, PROCEDURE parameter variables. *)
  | Standard of standard                   (* Varadic Standard I/O procedures. *)
  | Analysis of simple_t                   (* 'Standard Functions of Analysis' need runtime checks *)
  | Record of Class.t * simple_t list      (* RECORD classes. *)
  | Field of simple_t * Class.t            (* RECORD field selectors. *)
  | Label                                  (* GOTO labels. *)

and procedure_t = simple_t * formal_t list

and formal_t =
  | By_name of simple_t
  | By_value of simple_t
  | By_result of simple_t
  | By_value_result of simple_t
  | By_procedure of procedure_t
  | By_array of simple_t * int

and simple_t =
  | Number of precision * domain
  | Logical
  | Bits
  | String of int
  | Reference of ClassSet.t
  | Null                     (* Null is a specical Reference type compatible with all others. *)
  | Statement

and  domain =
  | Integer
  | Real
  | Complex

and precision =
  | Short
  | Long

and standard =
  | Write
  | Writeon
  | Writecard
  | Read
  | Readon
  | Readcard
  | Iocontrol


val integer : simple_t  (* equal to Number(Long, Integer) *)


(** [assignment_compatible dest src] returns [true] if a value of simple type
    [src] can assigned to a designator of simple type [dest]. *)
val assignment_compatible : simple_t -> simple_t -> bool


(** [equal_simple_types t0 t1] returns [true] are the same simple type.  All
    references have the same simple type in the eyes of Algol W, until
    run-time, but references with no classes in common are clearly
    never equal.
*)
val equal_simple_types : simple_t -> simple_t -> bool


(** [equal_procedure_types p0 p1] returns [true] if two procedures
    have the type. Their return types and all their formal parameter
    types must match by [equal_simple_types] rules.
*)
val equal_procedure_types : procedure_t -> procedure_t -> bool


(** [unifying_type t1 t2] returns the simple type general
    enough to hold values of both type [t1] and [t2].  

    E.g. INTEGER and REAL have the mutually compatible type REAL
    because INTEGERs can be cast to REAL.

    @raises Incompatible if there is no such type. *)
val unifying_type : simple_t -> simple_t -> simple_t

exception Incompatible


(** These pretty print type information for error messages: *)

val describe_definition : definition_t -> string

val describe_simple : simple_t -> string

val describe_procedure : procedure_t -> string

val string_of_simple : simple_t -> string

val describe_formal : formal_t -> string

val describe_standard : standard -> string


(* end *)
