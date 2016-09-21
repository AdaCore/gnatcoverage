pragma Ada_12;

with Support; use Support;

package body Ops is
   
   pragma Assertion_Policy (Static_Predicate => Disable);

   function Wpos (D : T_Day) return T_Wpos is
   begin
      if D in Early_Day then  -- # test_early
         return Early; -- # early
      elsif D > Thursday then -- # test_late
         return Late; -- # late
      else
         return Mid; -- # mid
      end if;
   end;
   
   procedure Check_Early (D : T_Day) is
      ED : Early_Day := D; -- # check_early
   begin
      Assert (Wpos (ED) = Early); -- # check_early
   end;
   
   procedure Check_Late (D : T_Day) is
      ED : Late_Day := D; -- # check_late
   begin
      Assert (Wpos (ED) = Late); -- # check_late
   end;
   
   procedure Check_Mid (D : T_Day) is
      ED : Mid_Day := D; -- # check_mid
   begin
      Assert (Wpos (ED) = Mid); -- # check_mid
   end;
   
end;
