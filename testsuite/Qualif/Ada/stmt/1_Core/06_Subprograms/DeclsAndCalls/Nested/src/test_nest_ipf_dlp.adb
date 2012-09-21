with Nest, Support; use Nest, Support;

-- Indirect call to Package Function
-- Direct call to Local Procedure

procedure Test_Nest_IPF_DLP is
   Cd : aliased Cdata;
begin
   Check (Lfun => False, Lproc => False,
          Pfun => True, Pproc => False,
          Indirect => True, Cd => Cd'Access);
   Check (Lfun => False, Lproc => True,
          Pfun => False, Pproc => False,
          Indirect => False, Cd => Cd'Access);
   Assert (Cd.Nops = 2);
end;

--# nest.adb
--  /check/ l+ ## 0

--  /lfun/  l- ## s-
--  /lfi/   l- ## s-
--  /lfd/   l- ## s-

--  /lproc/ l+ ## 0
--  /lpi/   l- ## s-
--  /lpd/   l+ ## 0

--  /pfun/  l+ ## 0
--  /pfi/   l+ ## 0
--  /pfd/   l- ## s-

--  /pproc/ l- ## s-
--  /ppi/   l- ## s-
--  /ppd/   l- ## s-

--  /indirect/ l+ ## 0

