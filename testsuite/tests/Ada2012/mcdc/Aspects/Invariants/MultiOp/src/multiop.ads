pragma Ada_2012;
pragma Assertion_Policy (Type_Invariant => Check);

-- exercise a multi-operand invariant (complex expr)

package Multiop is
   
   type Int (LB, UB : Integer) is private;
   
   procedure Set (I : in out Int; V : Integer);
   
private
   
   type Int (LB, UB : Integer) is record
      Value : Integer := LB;
   end record with
     Type_Invariant => (Int.Value >= Int.LB and then Int.Value <= Int.UB); -- # eval
   
end;
