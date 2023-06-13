package body Pkg is

   task body T is
   begin
      null; -- # stmt
   end T;

   protected body P is
   end P;

   procedure Run is
      task ST; -- # stmt
      task body ST is
      begin
         null; -- # stmt
      end ST;

      protected SP is -- # stmt
      end SP;

      protected body SP is
      end SP;

      T_Instance : T; -- # stmt
      P_Instance : P; -- # stmt
   begin
      null; -- # stmt
   end Run;

end Pkg;
