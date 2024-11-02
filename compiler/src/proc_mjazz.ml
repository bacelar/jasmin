(* -------------------------------------------------------------------- *)
open Utils
module Path = BatPathGen.OfString
module F = Format
module L = Location
module A = Annotations
module S = Syntax
module E = Expr
module P = Prog
module W = Wsize
module T = Type

module SyntaxPrinter = Syntax_printer

type qualident = A.symbol

(* -------------------------------------------------------------------- *)


(* each file is implicitly a module *)
let fmodule_name from (fname: A.symbol) : A.symbol =
  let _, mname, _ = Path.split (Path.of_string fname) in
  match from with
  | None -> mname
  | Some libname -> (L.unloc libname) ^ mname


(* -------------------------------------------------------------------- *)

type typattern = TPBool | TPInt | TPWord | TPArray

type mjazzerror =
  | UnknownVar          of A.symbol
  | UnknownFun          of A.symbol
  | InvalidType         of P.pty * typattern
  | TypeMismatch        of P.pty pair
  | StringError         of string

exception MjazzError of L.t * mjazzerror

let string_error fmt =
  let buf  = Buffer.create 127 in
  let bfmt = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun bfmt ->
      Format.pp_print_flush bfmt ();
      (StringError (Buffer.contents buf)))
    bfmt fmt

let mjazzerror ~loc (code : mjazzerror) =
  MjazzError (loc, code)

let rs_mjazzerror ~loc (code : mjazzerror) =
  raise (mjazzerror ~loc code)

let pp_typat fmt = function
  | TPBool  -> F.fprintf fmt "bool"
  | TPInt   -> F.fprintf fmt "int"
  | TPWord  -> F.fprintf fmt "word (u8, u16, u32, u64)"
  | TPArray -> F.fprintf fmt "array"

let pp_mjazzerror fmt (code : mjazzerror) =
  match code with
  | UnknownVar x ->
      F.fprintf fmt "unknown variable: `%s'" x

  | UnknownFun x ->
      F.fprintf fmt "unknown function: `%s'" x

  | InvalidType (ty, p) ->
    F.fprintf fmt "the expression has type %a instead of %a"
       Printer.pp_ptype ty pp_typat p

  | TypeMismatch (t1,t2) ->
    F.fprintf fmt
      "the expression has type %a instead of %a"
      Printer.pp_ptype t1 Printer.pp_ptype t2

  | StringError s ->
    F.fprintf fmt "%s" s

(* -------------------------------------------------------------------- *)

type qualifier = A.symbol
type module_name = A.symbol
type module_ctxt = module_name list
type fromkey = A.symbol L.located option
type filename = String.t

let add_from idir from_map (name, filename) = 
  let p = Path.of_string filename in 
  let ap = 
    if Path.is_absolute p then p
    else Path.concat idir p in  
  begin match Map.find name from_map with
  | ap' -> 
    if ap <> ap' then 
      hierror ~loc:Lnone ~kind:"compilation" "cannot bind %s with %s it is already bound to %s"
        name (Path.to_string ap) (Path.to_string ap')
  | exception Not_found -> ()
  end;
  Map.add name ap from_map

type fmodule_info =
  { fmod_ast  : Syntax.pprogram
  ; fmod_deps : (module_ctxt * fromkey * L.t option * filename * qualifier option) list
  }
type fmenv =
  { fm_env : (A.symbol, fmodule_info) Map.t
  ; fm_topsort : A.symbol list
  }
let empty_fmenv = { fm_env = Map.empty; fm_topsort = [] }

let pp_fmenv fmt env =
  let pp_entry fmt modname =
    F.fprintf fmt "\n//=============  %s  =============\n" modname;
    F.fprintf fmt "%a \n"
      (Syntax_printer.pp_prog) (Map.find modname env.fm_env).fmod_ast in
  F.fprintf fmt "@[<v 2>fm_topsort:@,%a@]\n"
               (Utils.pp_list ", " F.pp_print_string) env.fm_topsort;
  F.fprintf fmt "%a" (Utils.pp_list "\n" pp_entry) (env.fm_topsort)

(*
from, fname, Some (mctxt::modname)

require "F" => open F
require "F" as X => (nothing)
*)
let rec collect_deps mctxt deps = function
  | [] -> deps, []
  | x::xs ->
      match L.unloc x with
      | S.Prequire (from, fnames, None) ->
          let deps, rest = collect_deps mctxt deps xs
          in List.fold_left (fun l name -> (mctxt, from, Some (L.loc name), L.unloc name, None)::l) deps fnames
             , List.fold_left (fun l name -> L.mk_loc (L.loc x) (S.POpen (L.mk_loc L._dummy (fmodule_name from (L.unloc name))))::l) rest fnames
      | S.Prequire (from, name::[], qual) ->
          collect_deps mctxt ((mctxt, from, Some (L.loc name), L.unloc name, Option.map L.unloc qual)::deps) xs
      | S.Prequire (from, name::_, qual) ->
          hierror ~loc:(Lone (L.loc name)) ~kind:"syntax" 
            "(mjazz) unexpected multiple files with qualifier: \"%s\"" 
            (L.unloc name)
      | S.PModule (name, qual, body) ->
          let inner_deps, inner_rest = collect_deps (L.unloc name :: mctxt) deps body
          in let deps, rest = collect_deps mctxt inner_deps xs
          in deps, L.mk_loc (L.loc x) (S.PModule (name, qual, inner_rest))::rest
      | S.PNamespace (name, _) ->
          hierror ~loc:(Lone (L.loc name)) ~kind:"syntax" 
            "unexpected namespace in mjazz-mode: \"%s\"" 
            (L.unloc name)
      | _ -> let deps, rest = collect_deps mctxt deps xs
             in deps, (x::rest)



(* pass0:
    - load & parse dependencies
    - detect circular dependencies
    - topological sorting of fmodules
    - move requires upfront:
      + [in A] requires "F" ==> open F; (toplevel: requires "F" as F)
      + [in A] requires "F" as X ==> alias A::X = F (toplevel: requires "F" as F)
*)

let add_require ast (mctxt, from, loc, fname, qual) =
  let loc = match loc with None -> L._dummy | Some loc -> loc in
  let modname = L.mk_loc loc (fmodule_name from fname) in
  match qual with
  | None -> L.mk_loc loc (S.Prequire (from, [L.mk_loc loc fname], Some modname))::ast
  | Some qual -> L.mk_loc loc (S.Prequire (from, [L.mk_loc loc fname], Some modname))::ast

let rec visit_file from_map visited env (mctxt, from, loc, fname, qual) =
  let modname = fmodule_name from fname in
  let ploc = match loc with None -> Lnone | Some l -> Lone l in
  let p = Path.of_string fname in
  let current_dir =
    match from with
    | None -> snd (List.hd visited)
    | Some name -> 
        if Path.is_absolute p then 
          hierror ~loc:ploc ~kind:"typing" 
            "cannot use absolute path in from %s require \"%s\"" 
            (L.unloc name) fname;
        try Map.find (L.unloc name) from_map 
        with Not_found ->
          rs_mjazzerror ~loc:(L.loc name) (string_error "unknown name %s" (L.unloc name)) in
        let p = if Path.is_absolute p then p
                else Path.concat current_dir p in
        let p = Path.normalize_in_tree p in
        let ap = if Path.is_absolute p then p
                 else (* ?deadcode? *) Path.concat (snd (BatList.last visited)) p in
        let ap = Path.normalize_in_tree ap in
        (if Option.is_some (List.find_opt (fun x -> String.equal modname (fst x)) visited)
         then hierror ~loc:ploc ~kind:"dependencies"
                      "circular dependency detected on module \"%s\"" 
                      modname);
        match Map.find_opt modname env.fm_env with
        | Some _ ->
            if !Glob_options.debug then Printf.eprintf "reusing AST for \"%s\" \n%!" modname;
            env
        | None -> let ast = Parseio.parse_program ~name:fname in
                  let ast = try BatFile.with_file_in fname ast
                            with Sys_error(err) ->
                              let loc = Option.map_default (fun l -> Lone l) Lnone loc in
                              hierror ~loc ~kind:"typing" "error reading file %S (%s)" fname err
                            in let deps, ast = collect_deps [modname] [] ast
                            in let ast = List.fold_left add_require ast deps
                            in let env = { env with fm_env = Map.add modname { fmod_ast = ast; fmod_deps = deps } env.fm_env}
                            in let env = List.fold_left (visit_file from_map ((modname, ap)::visited)) env deps
                  in {env with fm_topsort = modname::env.fm_topsort}




(* pass1: Name resolution (following the topological ordering)
    - move requires to the top fo file (top-level dependencies)
(A::B, Some "Libjbn", Some loc, filename, Some qual)
from Libjbn require filename as modname
((mctxt, from, Some (L.loc name), L.unloc name, qual)::deps)

    - manage namespaces and visibility:
      + handle open clauses
      + resolve symbols:
        * non-qualified:
          - search in the own symtable
          - lookup in the visible symtable
        * qualified:
          - lookup at the module aliases
          - resolve the unqualified symbol in the module

Env:
 - key: symbol name (fully-qualified)
 - value: ???

Modules Map:
 - key: module name (fully-qualified)
 - value:
   + mod_ctxt: module name (fully-qualified)
   + symbolMap: key: symbol_name, value: unit(type, etc.)
   + vsymbolMap: key: symbol_name, value: module_name // visible symbols
   + 

*)

(**
type module_info =
  { mod_ctxt: A.symbol                        (* module parent *)
  ; mod_parms: A.symbol list                  (* module parameters *)
  ; mod_body: Syntax.pprogram                 (* module body *)
  ; mod_symbtab : (A.symbol, unit) Map.t      (* symbol table *)
  ; mod_vsymbtab : (A.symbol, A.symbol) Map.t (* symbol -> module *)
  ; mod_aliases : (A.symbol, A.symbol) Map.t  (* alias -> module *)
  }
**)

(**
let add_param pp symbt =
  let param = L.unloc pp.S.ppa_name in 
  match Map.find_opt global symbt with
  | None -> Map.add global () symbt
  | Some _ -> hierror ~loc:(Lone (L.loc pg.S.ppa_name)) ~kind:"typing" "param %s already defined" global

let add_global pg symbt =
  let global = L.unloc pg.S.pgd_name in 
  match Map.find_opt global symbt with
  | None -> Map.add global () symbt
  | Some _ -> hierror ~loc:(Lone (L.loc pg.S.pgd_name)) ~kind:"typing" "symbol %s already defined" global
  
let add_fundef fd loc symbt =
  let fname = L.unloc fd.S.pdf_name in
  match Map.find_opt fname symbt with
  | None -> Map.add fname () symbt
  | Some _ -> hierror ~loc:(Lone (L.loc pg.S.pdf_name)) ~kind:"typing" "function %s already defined" fname

let rec resolve_item ctxt lnames env = function

type modtable = 
  { mtab_env : (A.symbol, module_info) Map.t
  ; mtab_vis : (A.symbol, A.symbol) Map.t
  }

**)
