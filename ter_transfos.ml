open Ter_iterateurs
open Ter_identite
open Ter_arith_to_call
open Ter_no_submod
open Ter_chk_spec
open NEW_make_graph
open NEW_graph_to_java

let id prog = 
	let module IdParam : tRef = struct
		type r = unit
		let creerRef _ = ()
	end 
	in let module Trans = Identite(IdParam)
	in let module Apply_transfo = Transformation(Trans) 
		in Apply_transfo.transform_spec prog 

let noSub prog =
	let module Trans = Tfr_no_submodule
	in let module Apply_transfo = Transformation(Trans) 
		in (Apply_transfo.transform_spec prog) 

let addCall prog =
	let module Trans = Tfr_arith_to_call
	in let module Apply_transfo = Transformation(Trans) 
	in Apply_transfo.transform_spec prog

let check prog =
	let module Applic = Apl_chk_spec
	in let module Apply_transfo = Application(Applic) 
		in Apply_transfo.apply_spec prog

let mk_graph prog =
	let module Applic = NEW_make_graph
	in let module Apply_transfo = Application(Applic) 
	in  let p = Apply_transfo.apply_spec prog
		in p.gr

let graph_to_java g = to_java g;;