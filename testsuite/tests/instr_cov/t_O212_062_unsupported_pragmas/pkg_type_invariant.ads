pragma Ada_2012;
pragma Assertion_Policy (Check);

package Pkg_Type_Invariant is

   type Bool_Record is private;

   function Make_Bool_Record (B : Boolean) return Bool_Record;

   function Get_Value (R : Bool_Record) return Boolean;
private
   -- Type invariant as pragma
   type Bool_Record is
      record
         B : Boolean := True;
      end record;
   pragma Type_Invariant (Bool_Record, Check => B);

   function Make_Bool_Record (B : Boolean) return Bool_Record is
      ((B => B));

   function Get_Value (R : Bool_Record) return Boolean is (R.B);
end Pkg_Type_Invariant;
