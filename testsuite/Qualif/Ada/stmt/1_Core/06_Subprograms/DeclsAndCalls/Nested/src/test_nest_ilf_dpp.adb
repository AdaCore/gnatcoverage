with Nest, Support; use Nest, Support;

-- Indirect call to Local Function
-- Direct call to Package Procedure

procedure Test_Nest_ILF_DPP is
   Cd : aliased Cdata;
begin
   Check (Lfun => True, Lproc => False,
          Pfun => False, Pproc => False,
          Indirect => True, Cd => Cd'Access);
   Check (Lfun => False, Lproc => False,
          Pfun => False, Pproc => True,
          Indirect => True, Cd => Cd'Access);
   Assert (Cd.Nops = 2);
end;

--# nest.adb
--  /check/ l+ ## 0

--  /lfun/  l+ ## 0
--  /lfi/   l+ ## 0
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

--  /indirect/ l+ ## 0

