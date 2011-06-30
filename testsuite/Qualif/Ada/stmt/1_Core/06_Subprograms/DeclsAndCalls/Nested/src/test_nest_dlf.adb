with Nest, Support; use Nest, Support;

-- Direct call to Local Function

procedure Test_Nest_DLF is
   Cd : aliased Cdata;
begin
   Check (Lfun => True, Lproc => False,
          Pfun => False, Pproc => False,
          Indirect => False, Cd => Cd'Access);
   Assert (Cd.Nops = 1);
end;

--# nest.adb
--  /check/ l+ 0
--  /lfun/  l+ 0
--  /lfi/   l- s-
--  /lfd/   l+ 0
--  /lproc/ l- s-
--  /lpi/   l- s-
--  /lpd/   l- s-
--  /pfun/  l- s-
--  /pfi/   l- s-
--  /pfd/   l- s-
--  /pproc/ l- s-
--  /ppi/   l- s-
--  /ppd/   l- s-
--  /indirect/ l- s-

