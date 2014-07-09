with Environ; use Environ;

package body Monitor is
   
   procedure Compare_With_Initial_State_Now is
      Current_Environ_State : State := Environ.Probe; -- # compare
   begin
      if Match (Current_Environ_State, Initial_Environ_State) then -- # compare
         N_Matches := N_Matches + 1;                               -- # match
      end if;
   end;
         
end;
