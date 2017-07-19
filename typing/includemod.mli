(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the module language *)

open Typedtree
open Types
open Format

val modtypes:
  loc:Location.t -> Env.t ->
  module_type -> module_type -> module_coercion

val signatures: Env.t -> signature -> signature -> module_coercion

val compunit:
      Env.t -> string -> signature -> string -> signature -> module_coercion

val type_declarations:
  loc:Location.t -> Env.t ->
  Ident.t -> type_declaration -> type_declaration -> unit

val compose_coercions: module_coercion -> module_coercion -> module_coercion
val coerce_position: module_coercion -> int -> int * module_coercion
val print_coercion: formatter -> module_coercion -> unit

(*
   Get a real path by which objects may be accessed.
   _value_ -> treat last part as a value, do not modify it
   _module -> treat last part as a module, possibly change it too if an alias

   Unrolls aliases until a present one is found. May raise if some module is unavailable.

   The latter variants are to be used only if no location is available.
*)
val realize_module_path: loc:Location.t -> env:Env.t -> Path.t -> Path.t
val realize_value_path: loc:Location.t -> env:Env.t -> Path.t -> Path.t

val realize_module_path_no_location: env:Env.t -> Path.t -> Path.t
val realize_value_path_no_location: env:Env.t -> Path.t -> Path.t

type symptom =
    Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of Ident.t * type_declaration
        * type_declaration * Includecore.type_mismatch list
  | Extension_constructors of
      Ident.t * extension_constructor * extension_constructor
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t * class_type_declaration * class_type_declaration *
      Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * class_declaration * class_declaration *
      Ctype.class_match_failure list
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
    Module of Ident.t | Modtype of Ident.t | Arg of Ident.t | Body of Ident.t
type error = pos list * Env.t * symptom

exception Error of error list

val report_error: formatter -> error list -> unit
val expand_module_alias: Env.t -> pos list -> Path.t -> Types.module_type
