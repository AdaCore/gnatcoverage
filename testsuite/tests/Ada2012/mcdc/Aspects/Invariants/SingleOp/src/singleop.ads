pragma Ada_2012;
pragma Assertion_Policy (Invariant => Check);

-- exercise a mono-operand invariant (simple expr)

package SingleOp is
   
   type Int (UB : Integer) is private;
   
   procedure Set (I : in out Int; V : Integer);

private
   
   type Int (UB : Integer) is record
      Value : Integer := UB;
   end record with
     Invariant => (Int.Value <= Int.UB); -- # eval
   
end;
