package Null_Statements is
   procedure Null_Proc_1 (I : Integer);
   procedure Null_Proc_2 (B : Boolean);
   --  Null procedures, they have bodies with a single null statements each

   procedure Adjust
     (Res : in out Integer;
      I, J : Integer);

   function Adjust (Val : Integer; I, J : Integer) return Integer;
   --  These two routines put null statement into an alternative of a case
   --  statement

   procedure Set_Max (Res : out Integer; I, J : Integer);
   procedure Set_Min (Res : out Integer; I, J : Integer);
   --  These two routines put (labeled) null statement as the last statement of
   --  statement sequence

end Null_Statements;

