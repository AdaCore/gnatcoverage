package Subprogram_Pack is
   function Fun1 (I : Integer) return Integer;
   function Fun2 (I : Integer) return Integer;

   procedure Proc1 (I : in out Integer);
   procedure Proc2 (J : out Integer; I : Integer := Fun2 (1));

   type Rec is record
      I : Integer := Fun1 (1);
   end record;

   type Access_To_Proc is access procedure (I : in out Integer);

   procedure Proc3 (I : in out Integer);
   procedure Proc4 (I : in out Integer);

end Subprogram_Pack;
