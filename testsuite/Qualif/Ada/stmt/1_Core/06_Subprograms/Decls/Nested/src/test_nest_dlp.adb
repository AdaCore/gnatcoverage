with Nest, Support; use Nest, Support;

-- Direct call to Local Procedure

procedure Test_Nest_DLP is
   Cd : aliased Cdata;
begin
   Check (Lfun => False, Lproc => True,
          Pfun => False, Pproc => False,
          Indirect => False, Cd => Cd'Access);
   Assert (Cd.Nops = 1);
end;

--# nest.adb
--  /check/ l+ 0
--  /lfun/  l- s-
--  /lfi/   l- s-
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
--  /indirect/ l- s-

