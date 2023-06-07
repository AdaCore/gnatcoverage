with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Log;

procedure Test_Main is

   abc  : constant Unbounded_String := To_Unbounded_String ("abc");
   def  : constant Unbounded_String := To_Unbounded_String ("def");
   ghi  : constant Unbounded_String := To_Unbounded_String ("ghi");
   bool : constant Unbounded_String := To_Unbounded_String ("bool");

   function Foo (Var : Unbounded_String) return Boolean is
   begin
      if Var = To_Unbounded_String ("abc")        -- # evalA
        or else Var = To_Unbounded_String ("def") -- # evalB
        or else Var = To_Unbounded_String ("ghi") -- # evalC
      then
         return True;  -- # dtrue
      else
         return False; -- # dfalse
      end if;
   end Foo;
   
   
begin
   if Foo (abc) then -- # outer_test_xok
      Log ("ok");    -- # outer_ok
   else
      Log ("err");   -- # outer_err_xok
   end if;

   if Foo (def) then -- # outer_test_xok
      Log ("ok");    -- # outer_ok
   else
      Log ("err");   -- # outer_err
   end if;

   if Foo (ghi) then -- # outer_test_xok
      Log ("ok");    -- # outer_ok
   else
      Log ("err");   -- # outer_err
   end if;

   if Foo (bool) then -- # outer_test_xfail
      Log ("err");    -- # outer_err
   else
      Log ("ok");     -- # outer_ok
   end if;
end;

--  Self test, originating from customer, mcdc complete ...

--# test_main.adb
-- /evalA/  l+ ## 0
-- /evalB/  l+ ## 0
-- /evalC/  l+ ## 0
-- /dtrue/  l+ ## 0
-- /dfalse/ l+ ## 0

-- /outer_test_xok/   s=>l+, dmu=>l! ## s=>0, dmu=>dF-
-- /outer_test_xfail/ s=>l+, dmu=>l! ## s=>0, dmu=>dT-
-- /outer_err/  l- ## s-
-- /outer_ok/   l+ ## 0

