open Utils
open Annotations
open Syntax

module F = Format
module L = Location
module A = Annotations
module S = Syntax
module E = Expr
module P = Prog
module W = Wsize
module T = Type





(*

use dune to load dependencies (https://dune.readthedocs.io/en/stable/toplevel-integration.html#)

#use_output "dune ocaml top-module foo.ml";;



to debug the loading of DynLibs on macos:
- https://craftware.xyz/tips/Dynamic-libraries-loading.html
- https://github.com/ocaml/graphics/issues/33
*)




type full_module = FM of string * mod_param list
 and full_ident = { fi_mod: full_module; fi_ident: string }
 and mod_param = MPC of int | MPP of int | MPS of full_ident

let fident m i = { fi_mod=m; fi_ident=i }

(* param_names, global_names, fn_names *)
type module_sig = { ms_params: string list
                  ; ms_globals: string list
                  ; ms_fns: string list
                  }

type px = | P1 of int | P2 of int | P3 of int

let rec split_ps (l1,l2,l3) = function
 | [] -> (l1,l2,l3)
 | P1 i :: xs -> split_ps (i::l1, l2, l3) xs
 | P2 i :: xs -> split_ps (l1, i::l3, l3) xs
 | P3 i :: xs -> split_ps (l1, l2, i::l3) xs



(*
MODULO:

params: [typ] 
deps: [ qual option, fullModule

*)


(* REDUCED CONSTRAINTS *)
(* bn_util: [],[],[]*)
let nbu0 = FM ("bn_util", [])

(* bn_base: [int],[],[] *)
let nbb0 = FM ("bn_base", [MPP 0])
let nbb_nbu1 = FM ("bn_util", [])

(* bn_rnd: [int],[],[] *)
let nbr0 = FM ("bn_rnd", [MPP 0])
let nbr_nbb1 = FM ("bn_base", [MPP 0])

(* bn_exp: [int,int],[u64[MPP 0]],[([u64[MPP 0],u64[MPP 0]],[u64[MPP 0]]), ([u64[MPP 0],u64[MPP 0]],[u64[MPP 0]]] *)
(* nlimbs; nlimbsexp; exp0; mulU, sqrU *)
let nbe0 = FM ("bn_exp", [MPP 0; MPP 1; MPP 2; MPP 3; MPP 4])
let nbe_nbb1 = FM ("bn_base", [MPP 0])

(* fp_base: [int],[u64[MPP 0],u64[MPP 0],u64[MPP 0],u64[MPP 0]],[([u64[MPP 0],u64[MPP 0]],[u64[MPP 0]])] *)
(* nlimbs; P; mP; exp0; pm2; red *)
let fpb = FM ("fp_base", [MPP 0; MPP 1; MPP 2; MPP 3; MPP 4; MPP 5])
let fpb_nbb1 = FM ("bn_base", [MPP 0])
let fpb_rnd2 = FM ("bn_rnd", [MPP 0])
let fpb_rnd1_nbb1 = FM ("bn_base", [MPP 0])
let fpb_nbe3 = FM ("bn_exp", [MPP 0; MPP 0; MPP 1; MPS (fident fpb "mulU"); MPS (fident fpb "sqrU")])
let fpb_nbe_nbb = FM ("bn_base", [MPP 0])


(* fp_mont: [int],[u64[MPP 0],u64[MPP 0],u64,u64[MPP 0],u64[MPP 0]],u64[MPP 0]],[] *)
(* nlimbs, P, mP, P0i, exp0, pm2, RM *)
let fpm = FM ("fp_mont", [MPP 0; MPP 1; MPP 2; MPP 3; MPP 4; MPP 5; MPP 6])
let fpm_nbb = FM ("bn_base", [MPP 0])
let fpm_fpb = FM ("fp_base", [MPP 0; MPP 1; MPP 2; MPP 4; MPP 5; MPS (fident fpm "redM")])
let fpm_fpb_nbb = FM ("bn_base", [MPP 0])
let fpm_fpb_rnd = FM ("bn_rnd", [MPP 0])
let fpm_fpb_rnd_nbb = FM ("bn_base", [MPP 0])
let fpm_fpb_nbe = FM ("bn_exp", [MPP 0; MPP 0; MPP 4; MPS (fident fpm_fpb "mulU"); MPS { fi_mod=fpm_fpb; fi_ident="sqrU" }])
let fpm_fpb_nbe_nbb = FM ("bn_base", [MPP 0])


let eg_modp = FM ("modp2048", [])
(*let fpm_FPM = FM ("fp_mont", [MPC 12; MPP 1; MPP 2; MPP 3; MPP 4; MPP 5; MPP 6])*)


