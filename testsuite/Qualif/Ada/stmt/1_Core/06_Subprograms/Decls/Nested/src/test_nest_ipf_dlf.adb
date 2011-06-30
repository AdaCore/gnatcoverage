with Nest, Support; use Nest, Support;

-- Indirect call to Package Function
-- Direct call to Local Function

procedure Test_Nest_IPF_DLF is
   Cd : aliased Cdata;
begin
   Check (Lfun => False, Lproc => False,
          Pfun => True, Pproc => False,
          Indirect => True, Cd => Cd'Access);
   Check (Lfun => True, Lproc => False,
          Pfun => False, Pproc => False,
          Indirect => False, Cd => Cd'Access);
   Assert (Cd.Nops = 2);
end;

--# nest.adb
--  /check/ l+ 0

--  /lfun/  l+ 0
--  /lfi/   l- s-
--  /lfd/   l+ 0

--  /lproc/ l- s-
--  /lpi/   l- s-
--  /lpd/   l- s-

--  /pfun/  l+ 0
--  /pfi/   l+ 0
--  /pfd/   l- s-

--  /pproc/ l- s-
--  /ppi/   l- s-
--  /ppd/   l- s-

--  /indirect/ l+ 0

