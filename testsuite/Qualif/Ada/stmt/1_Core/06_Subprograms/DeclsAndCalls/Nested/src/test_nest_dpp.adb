with Nest, Support; use Nest, Support;

-- Direct call to Package Procedure

procedure Test_Nest_DPP is
   Cd : aliased Cdata;
begin
   Check (Lfun => False, Lproc => False,
          Pfun => False, Pproc => True,
          Indirect => False, Cd => Cd'Access);
   Assert (Cd.Nops = 1);
end;

--# nest.adb
--  /check/ l+ ## 0
--  /lfun/  l- ## s-
--  /lfi/   l- ## s-
--  /lfd/   l- ## s-
--  /lproc/ l- ## s-
--  /lpi/   l- ## s-
--  /lpd/   l- ## s-
--  /pfun/  l- ## s-
--  /pfi/   l- ## s-
--  /pfd/   l- ## s-
--  /pproc/ l+ ## 0
--  /ppi/   l- ## s-
--  /ppd/   l+ ## 0
--  /indirect/ l- ## s-

