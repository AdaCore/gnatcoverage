with Support;

package body Actuators is

   type Update_Policy is (Fast, Safe);

   procedure Increment
     (A : in out Actuator; Value : Integer; Policy : Update_Policy);
   pragma Inline (Increment);

   procedure Increment
     (A : in out Actuator; Value : Integer; Policy : Update_Policy) is
   begin
      if Policy = Safe and then A.Value + Value > A.Safety then -- # check
         return;                                                -- # punt
      else
         A.Value := A.Value + Value;         -- # update
      end if;
   end;

   procedure Fast_Increment
     (A : in out Actuator; Value : Integer) is
   begin
      Increment (A, Value, Policy => Fast); -- # call
   end;

end;


