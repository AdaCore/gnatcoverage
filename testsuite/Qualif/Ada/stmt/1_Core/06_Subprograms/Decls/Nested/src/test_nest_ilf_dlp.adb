with Nest, Support; use Nest, Support;

-- Indirect call to Local Function
-- Direct call to Local Procedure

procedure Test_Nest_ILF_DLP is
   Cd : aliased Cdata;
begin
   Check (Lfun => True, Lproc => False,
          Pfun => False, Pproc => False,
          Indirect => True, Cd => Cd'Access);
   Check (Lfun => False, Lproc => True,
          Pfun => False, Pproc => False,
          Indirect => False, Cd => Cd'Access);
   Assert (Cd.Nops = 2);
end;

--# nest.adb
--  /check/ l+ 0
--  /lfun/  l+ 0
--  /lfi/   l+ 0
--  /lfd/   l- s-
--  /lproc/ l+ 0
--  /lpi/   l- s-
--  /lpd/   l+ 0
--  /pfun/  l- s-
--  /pfi/   l- s-
--  /pfd/   l- s-
--  /pproc/ l- s-
--  /ppi/   l- s-
--  /ppd/   l- s-
--  /indirect/ l+ 0

