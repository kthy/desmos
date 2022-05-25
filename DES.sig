(* Licensed under the Academic Free License version 2.1, <https://spdx.org/licenses/AFL-2.1.html> *)
(* SPDX-FileCopyrightText: © 2003 Kristian Thy <thy@42.dk>                                        *)
(* SPDX-License-Identifier: AFL-2.1                                                               *)

signature DES =
    sig
        exception  NotBinary                                          (* digit is not binary                          *)

        val str2bin  : string -> int list                             (* string to binary list conversion             *) (* hidden *)
        val bin2str  : int list -> string                             (* binary list to string conversion             *) (* hidden *)
        val XOR      : int * int -> int                               (* single digit XOR                 (* infix *) *) (* hidden *)
        val xor      : int list * int list -> int list                (* binary list XOR                  (* infix *) *) (* hidden *)
        val ls       : 'a list * int -> 'a list                       (* left shift n places                          *) (* hidden *)
        val keys     : 'a list -> 'a list list                        (* subkey generator                             *) (* hidden *)
        val ip       : 'a list -> 'a list                             (* initial permutation                          *) (* hidden *)
        val ip'      : 'a list -> 'a list                             (* reverse init. perm.                          *) (* hidden *)
        val E        : int list -> int list                           (* expansion permutation                        *) (* hidden *)
        val P        : int list -> int list                           (* permutation function                         *) (* hidden *)
        val F        : int list * int list -> int list                (* the mysterious F                             *) (* hidden *)

        val ECBenc   : string -> string -> string                     (* Electronic Code Book encryption              *)
        val ECBdec   : string -> string -> string                     (* Electronic Code Book decryption              *)
        val CBCenc   : string -> string -> string                     (* Cipher Block Chaining encryption             *)
        val CBCdec   : string -> string -> string                     (* Cipher Block Chaining decryption             *)
        val TrDESenc : (string * string * string) -> string -> string (* Triple-DES Cipher Block Chaining encryption  *)
        val TrDESdec : (string * string * string) -> string -> string (* Triple-DES Cipher Block Chaining decryption  *)
    end;
