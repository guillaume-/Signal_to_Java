open Ms_syntax_tree
open SyntaxTree
open Ms_identifier
open Ter_exception
open Ter_iterateurs
open Ter_identite_p
open NEW_struc_graph

type ref = {
	spec: specification;
	gr: graph;
}

	module MbParam : tRef with type r = ref = struct
	type r = ref
	let creerRef s = 
		{
			spec = s;
			gr = {
				g_enums = [];
				g_procedures = [];
				g_vars = [];
				g_kons = [];
				g_tasks = [];
			};
		}
	end

	include Identite(MbParam)

	let rec exp_to_kons = function
		SignalAtom(id) -> K_V(id)
		| ClockPlus(e1, e2) -> K_Bin_op(exp_to_kons e1, "^+", exp_to_kons e2)
		| ClockMinus(e1, e2) -> K_Bin_op(exp_to_kons e1, "^-", exp_to_kons e2)
		| ClockTimes(e1, e2) -> K_Bin_op(exp_to_kons e1, "^*", exp_to_kons e2)
		| _ -> raise (Clock_Error("^+, ^-, ^* used to set clock but with incompatible operations"))

	let rec exp_to_exp = function
		IntegerConstant(i) -> C(string_of_int i)
		| EnumVariantAtom(id) -> Enum(id)
		| SignalAtom(id) -> V(id)
		| WhenNotAtom(id) -> Una_op("when not", V(id))
		| NotAtom(id) -> Una_op("not", V(id))
		| WhenAtom(id) -> Una_op("when", V(id))
		| When(e1, e2) -> Bin_op(exp_to_exp e1, "when", exp_to_exp e2)
		| EqualityAtom(e1, e2) -> Bin_op(exp_to_exp e1, "==", exp_to_exp e2)
		| Delay(e1, e2) -> Bin_op(exp_to_exp e1, "$1 init ", exp_to_exp e2)
		| Default(e1, e2) -> Bin_op(exp_to_exp e1, "default", exp_to_exp e2)
		| AndExp(e1, e2) -> Bin_op(exp_to_exp e1, "&&", exp_to_exp e2)
		| OrExp(e1, e2) -> Bin_op(exp_to_exp e1, "||", exp_to_exp e2)
		| Plus(e1, e2) -> Bin_op(exp_to_exp e1, "+", exp_to_exp e2)
		| Minus(e1, e2) -> Bin_op(exp_to_exp e1, "-", exp_to_exp e2)
		| Times(e1, e2) -> Bin_op(exp_to_exp e1, "*", exp_to_exp e2)
		| FunctionCall(id, sigL) -> Call(id, List.map (exp_to_exp) sigL)
		| _ -> raise (Clock_Error("^+, ^-, ^* used to set clock but with incompatible operations"))

	let apl_proced_decla param pn pi po = {
		param with
		gr = {
			param.gr with
			g_procedures = {
				proc_in_types = pi;
				proc_name = pn;
				proc_out_type = po;
			}::param.gr.g_procedures
		}
	}

	let apl_typed_var_set param (ttn: Ms_identifier.Identifier.t) (vs: Ms_identifier.IdentifierSet.t) = 
		if(List.exists (fun x -> x = ttn) ["Integer"; "integer"; "int"; "Boolean"; "boolean"; "bool"; "Event"; "event"])then
			param
		else {
		param with
		gr = {
			param.gr with
			g_enums = {
				e_name = ttn;
				e_values = IdentifierSet.elements vs;
			}::param.gr.g_enums
		}
	}

	let apl_sig_decla param sn st (sd: Ms_syntax_tree.SyntaxTree.direction) = match(sd)with
	|Input -> {
		param with
		gr = {
			param.gr with
			g_vars = {
				v_type = st;
				v_name = sn;
				v_dir = D_in;
			}::param.gr.g_vars;
			g_tasks = {
				t_id = ("T"^sn);
				t_do = {
					port_name = sn;
					port_delays = [];
					port_assign = None;
					port_ins = [];
					port_outs = [];
					port_sequence = None;
				}
			}::param.gr.g_tasks;
		}
	}
	|Output -> {
		param with
		gr = {
			param.gr with
			g_vars = {
				v_type = st;
				v_name = sn;
				v_dir = D_out;
			}::param.gr.g_vars
		}
	}
	|Local -> {
		param with
		gr = {
			param.gr with
			g_vars = {
				v_type = st;
				v_name = sn;
				v_dir = D_local;
			}::param.gr.g_vars
		}
	}

	let scons_k_to_string = function
		ClockEquality -> "^="
		| ClockLeq -> "<="
		| ClockLess -> "<"
		| ClockWhen -> "when"
		| ClockWhenNot -> "when not"
		| ClockExclusive -> "#="
	

	let apl_sconstr param (ck: Ms_syntax_tree.SyntaxTree.sconstraint_kind) (lsn: Ms_identifier.Identifier.t) (rsn: Ms_identifier.Identifier.t) = {
		spec = param.spec;
		gr = {
			param.gr with
			g_kons = K_Bin_op(K_V(lsn), scons_k_to_string ck, K_V(rsn))::param.gr.g_kons
		};
	}

	let apl_assign param asn ae = match(ae)with
		ClockPlus(_)
		| ClockMinus(_)
		| ClockTimes(_) -> {
			spec = param.spec;
			gr = {
				param.gr with
				g_kons = K_Bin_op(K_V(asn), "=", exp_to_kons ae)::param.gr.g_kons;
			}
		};
		| _ -> {
			spec = param.spec;
			gr = {
				param.gr with
				g_tasks = {
					t_id = ("T"^asn);
					t_do = {
						port_name = asn;
						port_assign = Some(exp_to_exp ae);
						port_ins = [];
						port_outs = [];
						port_delays = [];
						port_sequence = None;
					}
				}::param.gr.g_tasks;
			}
		}

	let add_pins g =
		let rec find_pins tL = function
			V(s) -> if(List.exists (fun t -> t.t_do.port_name = s) tL)then
					[(List.find (fun t -> t.t_do.port_name = s) tL).t_do]
					else []
			| Bin_op(V(s), "$1 init ", e) -> find_pins tL e
			| Bin_op(e1, s, e2) -> (find_pins tL e1)@(find_pins tL e2)
			| Una_op(s, e) -> find_pins tL e
			| Call(s, eL) -> forAll_e tL eL
			| _ -> []
		and forAll_e tL = function
			[] -> []
			|e::l -> (find_pins tL e)@(forAll_e tL l)
		in let i_add_pins tL = function
			None -> []
			| Some(e) -> find_pins tL e
		in let p_add_pins tL p = {
			p with
			port_ins = i_add_pins tL p.port_assign
		}
		in let t_add_pins tL t = {
			t with
			t_do = p_add_pins tL t.t_do
		} in {
		g with
		g_tasks = List.map (t_add_pins g.g_tasks) g.g_tasks
	}

	let add_pouts g =
		let rec forAll_t name = function
			[] -> []
			|t::l -> (if(List.exists (fun p -> p.port_name = name) t.t_do.port_ins)then [t.t_do] else [])@(forAll_t name l)
		in let p_add_pouts tL p = {
			p with
			port_outs = forAll_t p.port_name tL
		}
		in let t_add_pouts tL t = {
			t with
			t_do = p_add_pouts tL t.t_do
		} in {
		g with
		g_tasks = List.map (t_add_pouts g.g_tasks) g.g_tasks
	}

	let rule_delays g =
		let rec find_delays vars = function
			|Bin_op(V(s), "$1 init ", e) -> 
				if(List.exists (fun v -> v.v_name = s) vars)then
					let tmp = List.find (fun v -> v.v_name = s) vars
					in [{
						d_name = s;
						d_type = tmp.v_type;
						d_value = e;
					}]
				else []
			|Bin_op(e1, s, e2) -> (find_delays vars e1)@(find_delays vars e2)
			|Una_op(s, e) -> find_delays vars e
			|Call(s, eL) -> run_assign vars eL
			|_ -> []
		and run_assign vars = function
			[] -> []
			|e::l -> (find_delays vars e)@(run_assign vars l)
		in let p_rule_delays vars p = match(p.port_assign)with
			None -> p
			|Some(a) -> {
				p with
				port_delays = find_delays vars a
		} in let rec t_rule_delays vars = function
			[] -> []
			|t::l -> {
				t with
				t_do = (p_rule_delays vars t.t_do)
			}::(t_rule_delays vars l)
		in {
		g with
		g_tasks = t_rule_delays g.g_vars g.g_tasks
	}

	let apl_spec param pl tdl pdl = {
		param with
		gr = rule_delays (add_pouts (add_pins param.gr));
	}