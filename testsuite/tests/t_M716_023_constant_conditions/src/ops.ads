pragma Ada_2005;

package Ops is
   subtype Cond_Index is Natural range 1 .. 4;
   type Cond_Array is array (Cond_Index) of Boolean;
   Cond_1 : constant Cond_Array := (True, False, True, False);
   Cond_2 : constant Cond_Array := (True, True, False, False);

   type Cond_Array_Option (Available : Boolean) is record
      case Available is
         when False =>
            null;
         when True =>
            Cond : Cond_Array;
      end case;
   end record;

   Cond_Array_Unavailable : aliased Cond_Array_Option :=
     (Available => False);
   Cond_Array_Available   : aliased Cond_Array_Option :=
     (Available => True, Cond => Cond_1);
   Cond_Array_Current     : access Cond_Array_Option :=
      Cond_Array_Available'Access;

   function Eval (Idx : Cond_Index) return Boolean;
end Ops;
