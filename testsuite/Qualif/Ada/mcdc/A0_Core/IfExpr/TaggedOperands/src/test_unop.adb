with Support; use Support;

with Ops, Sys; use Ops, Sys;

procedure Test_Unop is
   X : Boolean := False;
   Op_Not : Op_Not_T;
begin
   Assert (not Process (Op => Op_Not, A => True, B => X));
end;

--# sys.adb
--  /umon/ l+ ## 0
--  /bmon/ l- ## s-

--%opts: --trace-mode=bin
--  /unop/ l! ## d!
--  /binop/ l! ## d!

--%opts: --trace-mode=src
--  /unop/ l! ## dF-
--  /binop/ l! ## dT-
