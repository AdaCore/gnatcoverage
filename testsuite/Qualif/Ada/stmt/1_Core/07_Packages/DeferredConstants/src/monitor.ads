with Environ;

package Monitor is
   
   Initial_Environ_State : constant Environ.State; -- # decl
   
   procedure Compare_With_Initial_State_Now;
   N_Matches : Natural := 0;
   
private
   Initial_Environ_State : constant Environ.State := Environ.Probe; -- # decl
end;
