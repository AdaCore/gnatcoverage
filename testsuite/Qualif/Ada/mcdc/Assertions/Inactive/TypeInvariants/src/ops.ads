pragma Ada_2012;
pragma Assertion_Policy (Invariant => Disable);

package Ops is
   
   type T_Pair (Checked : Boolean := True) is tagged limited private
     with Type_Invariant'Class => Valid(T_Pair) or else Unchecked(T_Pair);
   
   procedure Set (P : in out T_Pair'Class; X, Y : Integer);
   function Valid (P : T_Pair) return Boolean is (True); -- # fn
   function Unchecked (P : T_Pair) return Boolean is (not P.Checked); -- # fn
   
   type T_Double is new T_Pair with private;
   
   function Valid (D : T_Double) return Boolean;
   function Bad_Set (D : T_Double) return Boolean;

private
   
   type T_Pair (Checked : Boolean := True) is tagged limited record
      X, Y : Integer := 0;
      Is_Set : Boolean := False;
   end record;

   type T_Double is new T_Pair with null record with
     Type_Invariant => T_Double.X > 0 and then T_Double.Y > 0;
   
end;
