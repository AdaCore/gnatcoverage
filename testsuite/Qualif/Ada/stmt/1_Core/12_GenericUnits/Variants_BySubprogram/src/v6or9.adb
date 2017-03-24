with Vops, Support; use Support;

package body V6or9 is
   
   procedure Check (V6inc, V6mult, V9inc, V9mult : Boolean) is
      package V6 is new Vops (Size => 6);  -- # i:V6
      V6i : V6.Vector_Type := (others => 2);
      V6m : V6.Vector_Type := (others => 2);
      
      package V9 is new Vops (Size => 9);    -- # i:V9
      V9i : V9.Vector_Type := (others => 3);
      V9m : V9.Vector_Type := (others => 3);
   begin
      if V6inc then -- # stmt
         V6.Inc (V6i, 1);    -- # 6i
         Assert (V6i(1) = 3); -- # 6i
      end if;
      if V6mult then -- # stmt
         V6.Mult (V6m, 2);   -- # 6m
         Assert (V6m(1) = 4); -- # 6m
      end if;
      if V9inc then -- # stmt
         V9.Inc (V9i, 1);    -- # 9i
         Assert (V9i(1) = 4); -- # 9i
      end if;
      if V9mult then -- # stmt
         V9.Mult (V9m, 2);   -- # 9m
         Assert (V9m(1) = 6); -- # 9m
      end if;      
   end;
end;

