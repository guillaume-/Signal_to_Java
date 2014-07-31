type integer;
type boolean = enum (true, false);
type test = enum (un, deux, trois);

process P = (? integer a; boolean b; ! integer plop, u, v, w, x, y, z;)(
|x := a when b
|y := x +1
|z := a +3
|^z := ^a
|u := (y $1 init 0)
|v := u default 0
|submodule Exemple(w)(u)
|plop := 1 when b
|plop := 2 when not b
|)where
	process Exemple = (? integer i; ! integer o;)(
		|o := i + 1
	|)end;
end;