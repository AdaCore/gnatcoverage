package body Example is

   -------
   -- P --
   -------

   procedure P (V : Integer; V2 : Root) is
   begin
      null; -- # root-p
   end P;

   procedure P (V : Integer; V2 : Child) is
   begin
      null; -- # child-p
   end P;

   --------
   -- P2 --
   --------

   procedure P2 (V : Integer; V2 : Child) is
   begin
      null; -- # not-primitive-p2
   end P2;

end Example;
