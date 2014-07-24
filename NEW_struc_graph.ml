type direction = D_in | D_out | D_local

type variable = {
	v_type : string;
	v_name : string;
	v_dir : direction;
}

type expression =
	V of string (* pour les variables, peut valoir true or false dans un when *)
	| C of string (* pour les constantes entières *)
	| B of string (* pour les booléens *)
	| Enum of string (* pour les valeurs énumérées *)
	| Bin_op of expression*string*expression
	| Una_op of string*expression
	| Call of string*(expression list)

type konstraint =
	K_V of string
	| K_Bin_op of konstraint*string*konstraint
	| K_Una_op of string*konstraint

type delay = {
	d_name : string;
	d_type : string;
	d_value : expression;
}

type port = {
	port_name : string;
	port_delays : delay list;
	port_assign : expression option; (* None if direction = in *)
	port_ins : port list;
	port_outs : port list;
	port_sequence : port option;
}

type task = {
	t_id : string;
	t_do : port;
}

type enum = {
	e_name : string;
	e_values : string list;
}

type procedure = {
	proc_in_types : string list;
	proc_name : string;
	proc_out_type : string;
}

type graph = {
	g_enums : enum list;
	g_procedures : procedure list;
	g_vars : variable list;
	g_kons : konstraint list;
	g_tasks : task list;
}