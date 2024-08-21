pragma Ada_2012;

procedure Access_To_Subp                                        -- # fun
is
   type Point is record                                        -- # decl
      X, Y : Float;                                            -- # dstmt
   end record;                                                 -- # dstmt

   function Set (X, Y : Float) return Point is (X, Y);         -- # stmt

   function Reset (X, Y : Float) return Point is (0.0, 0.0);   -- # stmt

   type Pt_Acc is access function (X, Y : Float) return Point; -- # decl

   function Run_Callback (Callback : Pt_Acc) return Point is   -- # fun
   begin
      return Callback (1.0, 2.00);                             -- # call_1
   end;

   Set_Acc   : constant Pt_Acc := Set'Access;                  -- # decl
   Reset_Acc : constant Pt_Acc := Reset'Access;                -- # decl

   Dispatch_Table : constant array (1 .. 2) of Pt_Acc :=       -- # decl
     (Set_Acc, Reset_Acc);                                     -- # dstmt

   --  LIMITATION: Selected component with a call expression for a prefix
   --  F : Float := Run_Callback (Set_Acc).X;
   P : Point := Run_Callback (Set_Acc);                        -- # call_2
begin
   P := Run_Callback (Set_Acc);                                -- # call_3
   P := Set_Acc.all (1.0, 2.0);                                -- # call_4
   P := Dispatch_Table (2) (1.0, 3.0);                         -- # call_5
end Access_To_Subp;
