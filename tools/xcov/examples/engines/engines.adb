
package body Engines is

   --------------
   -- State_Of --
   --------------
   function State_Of (E : Engine) return State is
   begin
      if E.P >= Stable_P and then E.T >= Stable_T then
         return Critical;
      elsif E.P >= Stable_P or else E.T >= Stable_T then
         return Alarming;
      else
         return Stable;
      end if;
   end;
end;
