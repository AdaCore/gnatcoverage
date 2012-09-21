with Nest, Support; use Nest, Support;

-- Indirect call to Package Function

procedure Test_Nest_IPF is
   Cd : aliased Cdata;
begin
   Check (Lfun => False, Lproc => False,
          Pfun => True, Pproc => False,
          Indirect => True, Cd => Cd'Access);
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

--  /pfun/  l+ ## 0
--  /pfi/   l+ ## 0
--  /pfd/   l- ## s-

--  /pproc/ l- ## s-
--  /ppi/   l- ## s-
--  /ppd/   l- ## s-

--  /indirect/ l+ ## 0

