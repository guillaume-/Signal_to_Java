open NEW_struc_graph
open Printf
open Unix


(**********************************************************
**********************CONSTANTS****************************
**********************************************************)

let str_of_Ter_runnable =
"package thread;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import main.Main;
import data.GlobalData;
public abstract class Ter_Runnable implements Runnable{
\tprivate int num;
\tprivate int NB_D;
\tprotected GlobalData data;
\tprotected int nb_d;
\tprotected List<Ter_Runnable>tasks;
\tprotected ExecutorService e;

\tpublic Ter_Runnable(int n, GlobalData gd, int nb_dependances, ExecutorService exe){
\t\tnum = n;
\t\tdata = gd;
\t\tNB_D = nb_d = nb_dependances;
\t\ttasks = new ArrayList<Ter_Runnable>();
\t\te = exe;
\t}

\tpublic void add_task(Ter_Runnable t){
\t\ttasks.add(t);
\t}

\tpublic abstract boolean compute();

\tpublic abstract boolean present();

\t@Override
\tpublic void run(){
\t\tSystem.out.println(this);
\t\t\tif(compute())
\t\t\t\tfor(Ter_Runnable r : tasks){
\t\t\t\t\tr.try_execute();
\t\t\t\t}
\t\t\tMain.c.dec();
\t}

\tpublic synchronized void try_execute(){
\t\tnb_d--;
\t\tif(nb_d==0){
\t\t\tMain.c.inc1();
\t\t\te.execute(this);
\t\t}
\t\t
\t}

\tpublic int id(){
\t\treturn num;
\t}

\tpublic int nb_dependances(){
\t\treturn nb_d;
\t}

\tpublic void reset(){
\t\tnb_d = NB_D;
\t}
}
";;

let str_of_Uncoded_function =
"package exceptions;
public class Uncoded_function extends Exception{
\tprivate static final long serialVersionUID = 9212106058488647222L;
}
";;

let str_of_Signal =
"package data;
import thread.Ter_Runnable;
public abstract class Signal<Type>{
\tprotected Type t;

\tpublic Signal(){}

\tpublic void setT(Type value){
\t\tt = value;
\t}

\tpublic void setT(Signal<Type> s){
\t\tt = s.t;
\t}
\t
\tpublic Type getT(){
\t\treturn t;
\t}

\tprotected String get_res(String line, String name, Ter_Runnable th){
\t\tString res = \"\";
\t\tint ind = 0;
\t\tint k;
\t\tif(line==null)
\t\t\treturn \"*\";
\t\t/*forme du fichier txt :
\t\t espace nom espace = espace valeur; espace nom2 ...
\t\t  pour nouvelles données
\t\t*/
\t\tfor(int i=1; (ind==0) && (line.length()-5)>=i; i++){
\t\t\tk = i;
\t\t\tif(line.charAt(k-1)==' ')
\t\t\t\tfor(int j=0; line.charAt(k)==name.charAt(j); j++){
\t\t\t\t\tif((name.length() == (j+1)) && (line.charAt(k+1)==' ')){
\t\t\t\t\t\tind=k+4;
\t\t\t\t\t\tbreak;
\t\t\t\t\t}
\t\t\t\t\tk++;
\t\t\t\t}
\t\t}
\t\tif(ind!=0){
\t\t\tfor(k=ind; line.charAt(k) != ';'; k++)
\t\t\t\tres += line.charAt(k);
\t\t\treturn res;
\t\t}else
\t\t\treturn \"*\";
\t}

\t/* Pour line = \" name = Test; e = 8;\" et name = \"e\"
\t * t = 8
\t */
\tpublic abstract boolean read_name(String line, String name, Ter_Runnable th) throws InterruptedException;

\t@Override
\tpublic String toString(){
\t\tif(t!=null)
\t\t\treturn t.toString();
\t\telse
\t\t\treturn \"*\";
\t}
\t
\tpublic void reset() {
\t\tt = null;\t
\t}
}
";;

let str_of_Signal_Int =
"package data;
import thread.Ter_Runnable;
public class Signal_Int extends Signal<Integer>{

\tpublic Signal_Int(){}

\tpublic Signal_Int(int init){
\t\tt = init;
\t}

\t/* Pour line = \" name = Test; e = 8;\" et name = \"e\"
\t * t = 8
\t */
\t@Override
\tpublic boolean read_name(String line, String name, Ter_Runnable th){
\t\tString s = get_res(line, name, th);
\t\tif(s.equals(\"*\")){
\t\t\tt = null;
\t\t\treturn false;
\t\t}
\t\telse{
\t\t\tt = Integer.parseInt(s);
\t\t\treturn true;
\t\t}
\t}
}
";;

let str_of_Signal_Bool =
"package data;
import thread.Ter_Runnable;
public class Signal_Bool extends Signal<Boolean>{

\tpublic Signal_Bool(){}

\tpublic Signal_Bool(boolean init){
\t\tt = init;
\t}

\t/* Pour line = \" name = Test; e = 8;\" et name = \"e\"
\t* t = 8
\t*/
\t@Override
\tpublic boolean read_name(String line, String name, Ter_Runnable th){
\t\tString s = get_res(line, name, th);
\t\tif(s.equals(\"*\")){
\t\t\tt = null;
\t\t\treturn false;
\t\t}
\t\telse{
\t\t\tt = Boolean.parseBoolean(s);
\t\t\treturn true;
\t\t}
\t}
}
";;

let str_of_Count =
"package data;
public class Count{
\tpublic int c = 0;

\tpublic synchronized void inc1(){
\t\tc++;
\t}

\tpublic synchronized void dec(){
\t\tc--;
\t\tif(c==0)
\t\t\tnotify();
\t}

\tpublic synchronized void await(){
\t\twhile(c!=0)
\t\t\ttry{
\t\t\t\twait();
\t\t\t}catch(InterruptedException e){}
\t}

}
";;

(**********************************************************
************************TOOLS******************************
**********************************************************)

let parse_type = function
	|"int" |"integer" |"Integer" -> "Int"
	|"bool" |"boolean" |"Boolean" -> "Bool"
	|x -> x

let parse_type_2 = function
	|"int" |"integer" |"Integer" -> "int"
	|"bool" |"boolean" |"Boolean" -> "boolean"
	|x -> x

(**********************************************************
************************DATA*******************************
**********************************************************)

let create_Count () =
	let file = open_out ("src/data/Count.java")
	in fprintf file "%s" str_of_Count;
	close_out file

let create_Signal () =
	let file = open_out ("src/data/Signal.java")
	in fprintf file "%s" str_of_Signal;
	close_out file

let create_Signal_Bool () =
	let file = open_out ("src/data/Signal_Bool.java")
	in fprintf file "%s" str_of_Signal_Bool;
	close_out file

let create_Signal_Int () =
	let file = open_out ("src/data/Signal_Int.java")
	in fprintf file "%s" str_of_Signal_Int;
	close_out file

let str_of_Const g =
"package data;
public final class Constantes {
\tpublic static final int NB_THREADS = 4;
\tpublic static final int NB_TASKS ="
^(string_of_int (List.length g.g_tasks))
^";\n}\n"

let create_Constantes g =
	let file = open_out ("src/data/Constantes.java")
	in fprintf file "%s" (str_of_Const g);
	close_out file

let rec str_of_variables =
	let str v =
		let t = parse_type v.v_type
		in "\tpublic Signal_"^t^" "^v.v_name^"0 = new Signal_"^t^"();\n"
	in function 
	[] -> ""
	|v::l -> (str v)^(str_of_variables l)

let rec str_of_str_vars =
	let str v = " "^v.v_name^" = \"+"^v.v_name^"0+\";"
	in function
	[] -> ""
	|v::l -> (str v)^(str_of_str_vars l)

let rec str_of_reset_variables =
	let str v = "\t\t"^v.v_name^"0.reset();\n"
	in function
	[] -> ""
	|v::l -> (str v)^(str_of_reset_variables l)

let str_of_Global g =
"package data;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import thread.Ter_Runnable;\n"
^(if(g.g_enums != [])then "import usable.Usable.*;\n" else "")
^"public class GlobalData{
	private InputStream ips; 
	private InputStreamReader ipsr;
	private BufferedReader br;
	private FileWriter fw;
	public String line;
	public List<Ter_Runnable> tasks = new ArrayList<Ter_Runnable>();\n"
^(str_of_variables g.g_vars)
^"\n	public GlobalData(){
			try{
				ips = new FileInputStream(\"src/In\");
				ipsr = new InputStreamReader(ips);
				br = new BufferedReader(ipsr);
				line = br.readLine();
				fw = new FileWriter(new File(\"src/Out\"));
			}catch(IOException e){}
	}

	public void add_runnable(Ter_Runnable t){
		tasks.add(t);
	}

	public boolean isPresent(String clock){
		String res = \"\";
		int ind = 0;
		int k;
		if(line==null)
			return false;
		/*forme du fichier txt :
		 espace nom espace = espace valeur; espace nom2 ...
		 \\n pour nouvelles données
		*/
		for(int i=1; (ind==0) && (line.length()-5)>=i; i++){
			k = i;
			if(line.charAt(k-1)==' ')
				for(int j=0; line.charAt(k)==clock.charAt(j); j++){
					if((clock.length() == (j+1)) && (line.charAt(k+1)==' ')){
						ind=k+4;
						break;
					}
					k++;
				}
		}
		if(ind!=0){
			for(k=ind; line.charAt(k) != ';'; k++)
				res += line.charAt(k);
			if(res.equals(\"*\"))
				return false;
			return Boolean.parseBoolean(res);
		}else
			return false;
	}

	public void new_line(String s){
		try{
			line = br.readLine();
			s += \""
^(str_of_str_vars g.g_vars)
^"\";\n			s += \"\\n\";
			fw.write(s);
			if(line==null)
				fw.close();
		}catch(IOException e){}
		for(Ter_Runnable r : tasks)
			r.reset();\n"
^(str_of_reset_variables g.g_vars)
^"\t}\n}\n"

let create_GlobalData g =
	let file = open_out ("src/data/GlobalData.java")
	in fprintf file "%s" (str_of_Global g);
	close_out file

let create_data g = 
	create_Constantes g;
	create_Count ();
	create_GlobalData g;
	create_Signal_Bool ();
	create_Signal_Int ();
	create_Signal ()

(**********************************************************
**********************EXCEPTIONS***************************
**********************************************************)

let create_exceptions () =
	let file = open_out ("src/exceptions/Uncoded_function.java")
	in fprintf file "%s" str_of_Uncoded_function;
	close_out file

(**********************************************************
************************MAIN*******************************
**********************************************************)

let rec str_of_Main_Tasks i =
	let str t i = "\t\tTer_Runnable "^t.t_do.port_name^"0 = new "
					^t.t_id^"("^(string_of_int i)^", data, "
					^(string_of_int (List.length t.t_do.port_ins))
					^", execute);\n"
	in function
	[] -> ""
	|t::l -> (str t i)^(str_of_Main_Tasks (i+1) l)

let rec str_of_Main_Depends =
	let str t = 
		let rec str_p_outs t = function
			[] -> ""
			|p::l ->("\t\t"^t.t_do.port_name^"0.add_task("^p.port_name^"0);\n")
					^(str_p_outs t l)
		in str_p_outs t t.t_do.port_outs
	in function
	[] -> ""
	|t::l -> (str t)^(str_of_Main_Depends l)

let rec str_of_Main_data_add = function
	[] -> ""
	|v::l -> "\t\tdata.add_runnable("^v.v_name^"0);\n"^(str_of_Main_data_add l)

let str_of_main g =
"package main;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import data.Constantes;
import data.Count;
import data.GlobalData;
import thread.*;
public class Main{
	public static Count c = new Count();
	public static void main(String[]args){
		GlobalData data = new GlobalData();
		ExecutorService execute = Executors.newFixedThreadPool(Constantes.NB_THREADS);\n"
^(str_of_Main_Tasks 0 g.g_tasks)
^(str_of_Main_Depends g.g_tasks)
^(str_of_Main_data_add g.g_vars)
^"		for(String s =\"\"; data.line!=null; s=\"\"){
			System.out.println(\"Line : \"+data.line);
			for(Ter_Runnable r : data.tasks){
				if(r.nb_dependances()==0){
					c.inc1();
					execute.execute(r);
				}
			}
			c.await();
			data.new_line(s);
		}
		execute.shutdown();
	}
}\n"

let create_main g =
	let str g = str_of_main g
	in let file = open_out ("src/main/Main.java")
	in fprintf file "%s" (str g);
	close_out file

(**********************************************************
***********************THREAD******************************
**********************************************************)

let create_Ter_runnable () =
	let file = open_out ("src/thread/Ter_Runnable.java")
	in fprintf file "%s" str_of_Ter_runnable;
	close_out file

let str_of_e = 
	let rec str_of_Exp res = function
	V("true") -> "true", res
	|V("false") -> "false", res
	|V(s)-> "data."^s^"0.getT()", res
	|C(s)|B(s)|Enum(s) -> s, res
	|Bin_op(e1, "when", e2) ->  let tmp1 = (str_of_Exp res e1) in
								let tmp2 = (str_of_Exp (snd(tmp1)) e2) in
								fst(tmp1), ("\t\tif("^(fst(tmp2))^"){\n\t")::snd(tmp2)
	|Una_op("when not", V(id)) -> "", ("\t\tif(!data."^id^".getT()){\n\t")::res
	|Una_op("when", V(id)) -> "", ("\t\tif(data."^id^".getT()){\n\t")::res
	|Bin_op(V(s), "$1 init ", e) -> (s^"1.getT()", res)
	|Bin_op(C(s), "default", e2)|Bin_op(B(s), "default", e2)|Bin_op(Enum(s), "default", e2)
	-> s, res
	|Bin_op(V("true"), "default", e2) -> "true", res
	|Bin_op(V("false"), "default", e2) -> "false", res
	|Bin_op(V(s), "default", e2)->let tmp1 = (str_of_Exp res (V(s))) in
								let tmp2 = (str_of_Exp (snd(tmp1)) e2) in
								"data."^s^"0.present() ? "^fst(tmp1)^" : "^fst(tmp2)^")", snd(tmp2)
	|Bin_op(e1, s, e2)->let tmp1 = (str_of_Exp res e1) in
						let tmp2 = (str_of_Exp (snd(tmp1)) e2) in
						fst(tmp1)^" "^s^" "^fst(tmp2), snd(tmp2)
	|Una_op("not", V(id)) -> ("!("^id^")", res)
	|Call(s, eL) -> let rec f res = function
					[] -> "", res
					|e::[] -> str_of_Exp res e
					|e::l -> let tmp1 = (str_of_Exp res e) in
							 let tmp2 = (f (snd(tmp1)) l) in
							 (fst(tmp1)^", "^fst(tmp2)), snd(tmp2)
					in let tmp = f res eL
					in "Usable."^s^"("^fst(tmp)^")", snd(tmp)
	|_ -> "", res
	in str_of_Exp []

let rec str_snd = function
	[] -> ""
	|s::l -> s^(str_snd l)

let rec str_snd_end tname = function
	[] -> ""
	|s::l -> "\n\t\t}else{\n\t\t\tSystem.out.println(\""^tname^" : fin\");\n\t\t\treturn false;\n\t\t}"

let str_exp tname pname e =
		let res = str_of_e e in
		(str_snd (snd(res)))
^"		data."^pname^"0.setT("^(fst(res))^");
		System.out.println(\""^tname^" : fin\");
		return true;"
^str_snd_end tname (snd(res))

let str_expression tname pname = function
	None -> "\t\tboolean b = data."^pname^"0.read_name(data.line, \""^pname^"\", this);
\t\tSystem.out.println(\""^tname^" : fin\");
\t\treturn b;"
	|Some(e) -> str_exp tname pname e

let rec str_delays_init = function
	[] -> ""
	|d::l ->"\tSignal_"^(parse_type d.d_type)^" "^d.d_name^"1 = null;\n"^(str_delays_init l)

let rec str_delays = function
	[] -> ""
	|d::l ->let res = str_of_e d.d_value in
			(str_snd (snd(res)))
			^"\t\tif("^d.d_name^"1==null)\n\t\t\t"
			^d.d_name^" = new Signal_"^(parse_type d.d_type)^"("^fst(res)^");\n"
			^(str_delays l)

let rec exp_contains_call = function
	V(_)|C(_)|B(_) -> false
	|Enum(_) -> false
	|Bin_op(e1, _, e2) -> (exp_contains_call e1) || (exp_contains_call e2)
	|Una_op(_, e) -> exp_contains_call e
	|Call(_) -> true

let import_usable t = match(t.t_do.port_assign)with
	None -> ""
	|Some(x) ->	if(exp_contains_call x)then "import usable.Usable;\n"
				else ""

let rec exp_contains_enum =
	let rec rule_calls = function
		[] -> false
		|e::l -> (exp_contains_enum e)||(rule_calls l)
	in function
	V(_)|C(_)|B(_) -> false
	|Enum(_) -> true
	|Bin_op(e1, _, e2) -> (exp_contains_enum e1) || (exp_contains_enum e2)
	|Una_op(_, e) -> exp_contains_enum e
	|Call(_, eL) -> rule_calls eL


let import_usable_types t = match(t.t_do.port_assign)with
	None -> ""
	|Some(x) ->	if(exp_contains_enum x)then "import usable.Usable.*;\n"
				else ""

let str_of_delays = function
	[] -> ""
	|d::l -> "\t\t"^d.d_name^"1 = data."^d.d_name^"0;\n"

let newTask g t =
	let name = t.t_id
	in let delays = t.t_do.port_delays
	in let str g t ="package thread;
import java.util.concurrent.ExecutorService;
import data.*;\n"
^(import_usable t)
^(import_usable_types t)
^"public class "^name^" extends Ter_Runnable{\n"
^(str_delays_init t.t_do.port_delays)^"
\tpublic "^name^"(int num, GlobalData gd, int nbDependances, ExecutorService e){
\t\tsuper(num, gd, nbDependances, e);
\t}\n
\t@Override
\tpublic boolean compute(){
\t\tif(! present())
\t\t\treturn false;
\t\tSystem.out.println(\""^name^" : début\");\n"
^(str_delays delays)
^(str_expression name t.t_do.port_name t.t_do.port_assign)
^"\n\t}\n"
^(
if(List.length delays)>0
then "\n	@Override
	public void reset(){
		super.reset();
"^(str_of_delays delays)
^"	}\n"
else ""
)
^(if(t.t_do.port_assign = None)
then "\n	@Override
	public boolean present(){
		return true;
	}\n}\n"
else "\n	@Override
	public boolean present(){
		return data.isPresent(\"^"^t.t_do.port_name^"\");
	}\n}\n")
	in let file = open_out ("src/thread/"^name^".java")
	in fprintf file "%s" (str g t);
	close_out file

let rec create_tasks g = function
	[] -> ()
	|t::l -> (newTask g t); (create_tasks g l)

let create_thread g =
	create_Ter_runnable ();
	create_tasks g g.g_tasks

(**********************************************************
***********************USABLE******************************
**********************************************************)

let rec str_enums =
	let create_Enum e =
		let str e =
"package data;
import thread.Ter_Runnable;
import usable.Usable."^e^";
public class Signal_"^e^" extends Signal<"^e^">{
	@Override
	public boolean read_name(String line, String name, Ter_Runnable th){
		String s = get_res(line, name, th);
		if(s.equals(\"*\")){
			t = null;
			return false;
		}
		else{
			t = "^e^".valueOf(s);
			return true;
		}
	}
}\n"
		in let file = open_out ("src/data/Signal_"^e^".java")
		in fprintf file "%s" (str e);
		close_out file
	in let rec str_enum = function
		[] -> ""
		|e::[] -> e
		|e::l -> e^", "^(str_enum l)
	in function
	[] -> ""
	|e::l -> (create_Enum e.e_name); "\tpublic enum "^e.e_name^"{"^(str_enum e.e_values)^"}\n"

let rec str_procedures =
	let rec str_p i = function
		[] -> ""
		|e::[] -> (parse_type_2 e)^" i"^(string_of_int i)
		|e::l -> ((parse_type_2 e)^" i"^(string_of_int i)^", ")^(str_p (i+1) l)
	in function
	[] -> ""
	|p::l -> match(p.proc_name)with
	 "add" -> "\tpublic static int add(int i1, int i2){\n\t\treturn(i1+i2);\n\t}\n"
	|"mul" -> "\tpublic static int mul(int i1, int i2){\n\t\treturn(i1*i2);\n\t}\n"
	|"sub" -> "\tpublic static int sub(int i1, int i2){\n\t\treturn(i1-i2);\n\t}\n"
	|_ -> "\tpublic static "^(parse_type_2 p.proc_out_type)^" "^p.proc_name^"("^(str_p 0 p.proc_in_types)^")throws Uncoded_function{
		throw new Uncoded_function();
	}\n"

let str_of_Usable g =
"package usable;
import exceptions.Uncoded_function;
public final class Usable{\n"
^str_enums g.g_enums
^str_procedures g.g_procedures
^"}\n"

let create_usable g =
	let str g = str_of_Usable g
	in let file = open_out ("src/usable/Usable.java")
	in fprintf file "%s" (str g);
	close_out file

(*
***********************************************************
***********************************************************
***********************TO_JAVA*****************************
***********************************************************
***********************************************************
*)

let create_env () =
	let file = open_out ("src/In")
	in fprintf file "%s" "";
	close_out file;
	let file = open_out ("src/Out")
	in fprintf file "%s" "";
	close_out file

let to_java (g: graph) =
	(if(Sys.file_exists "src")then
		if(Unix.fork () = 0)then
			Unix.execvp "rm" [|"rm"; "-r"; "src"|]
		else ignore (wait ())
	else ());
	Unix.mkdir "src" 0o777;
	Unix.mkdir "src/data" 0o777;
	Unix.mkdir "src/exceptions" 0o777;
	Unix.mkdir "src/main" 0o777;
	Unix.mkdir "src/thread" 0o777;
	Unix.mkdir "src/usable" 0o777;
	create_data g;
	create_exceptions ();
	create_main g;
	create_thread g;
	create_usable g;
	create_env ();
;;