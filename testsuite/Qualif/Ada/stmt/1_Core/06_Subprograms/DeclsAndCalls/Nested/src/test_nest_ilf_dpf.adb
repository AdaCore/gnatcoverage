with Nest, Support; use Nest, Support;

-- Indirect call to Local Function
-- Direct call to Package Function

procedure Test_Nest_ILF_DPF is
   Cd : aliased Cdata;
begin
   Check (Lfun => True, Lproc => False,
          Pfun => False, Pproc => False,
          Indirect => True, Cd => Cd'Access);
   Check (Lfun => False, Lproc => False,
          Pfun => True, Pproc => False,
          Indirect => False, Cd => Cd'Access);
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
--  /pfun/  l+ ## 0
--  /pfi/   l- ## s-
--  /pfd/   l+ ## 0
--  /pproc/ l- ## s-
--  /ppi/   l- ## s-
--  /ppd/   l- ## s-
--  /indirect/ l+ ## 0

