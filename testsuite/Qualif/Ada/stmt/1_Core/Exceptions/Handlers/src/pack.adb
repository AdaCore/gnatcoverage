package body Pack is
   pragma Unsuppress (All_Checks);

   -----------
   -- Proc1 --
   -----------

   procedure Proc1 (I : in out My_Int) is
   begin
      if I = My_Int'Last then        -- # proc1
         raise My_Exception;         -- # raise_user_defined_proc1
      end if;

      I := I + 10;                   -- # no_raise_proc1
   exception
      when others  =>
         I := 0;                     -- # raise_proc1
   end Proc1;

   ----------
   -- Fun1 --
   ---------

   function Fun1 (I : My_Int) return My_Int is
      Res : My_Int := I;             -- # fun1
   begin

      if Res = My_Int'Last then      -- # fun1
         raise My_Exception;         -- # raise_user_defined_fun1
      end if;

      Res := Res + 10;               -- # no_raise_user_defined_fun1

      return Res;                    -- # no_raise_fun1
   exception
      when others =>
         return 0;                   -- # raise_fun1
   end Fun1;

   -----------
   -- Proc2 --
   -----------

   procedure Proc2 (I : in out My_Int) is
   begin
      if I = My_Int'Last then        -- # proc2
         raise My_Exception;         -- # raise_user_defined_proc2
      end if;

      I := I + 1;                    -- # no_user_defined_raise_proc2

      if I < 0 then                  -- # no_user_defined_raise_proc2
         I := I - 50;                -- # in_if_proc2
      end if;

      I := I / 2;                    -- # no_predefined_raise_proc2

   exception
      when My_Exception =>
         I := 1;                     -- # raise_user_defined_proc2
      when Constraint_Error =>
         I := -1;                    -- # raise_predefined_proc2
   end Proc2;

   ----------
   -- Fun2 --
   ---------

   function Fun2 (I : My_Int) return My_Int is
      Res : My_Int := I;             -- # fun2
   begin
      begin
         if I = My_Int'First then    -- # fun2
            raise My_Exception;      -- # raise_user_defined_fun2
         end if;

         Res := Res - 1;             -- # no_user_defined_raise_fun2
         Res := Res * I;             -- # no_user_defined_raise_fun2
         Res := Res / 2;             -- # no_predefined_raise_fun2
      exception
         when My_Exception =>
            return 1;                -- # raise_user_defined_fun2
         when Constraint_Error =>
            return -1;               -- # raise_predefined_fun2
      end;

      return Res;                    -- # no_raise_fun2
   end Fun2;

   -----------
   -- Proc3 --
   -----------

   procedure Proc3 (I : in out My_Int) is
   begin
      I := I + 1;                    -- # proc3
      I := I / 2;                    -- # no_predefined_raise_proc3

      if I = 0 then                  -- # no_predefined_raise_proc3
         raise My_Exception;         -- # raise_user_defined_proc3
      end if;

      I := (I + 10) / I;             -- # no_raise_proc3
   exception
      when Constraint_Error =>
         I := -1;                    -- # raise_predefined_proc3
      when others =>
         I := 1;                     -- # raise_user_defined_proc3
   end Proc3;

   ----------
   -- Fun3 --
   ---------

   function Fun3 (I : My_Int) return My_Int is
      Res : My_Int := I;             -- # fun3
   begin
      Res := Res + 1;                -- # fun3

      if Res = 0 then                -- # no_predefined_raise_fun3
         raise My_Exception;         -- # raise_user_defined_fun3
      end if;

      Res := (Res + 10) / Res;       -- # no_raise_fun3

      return Res;                    -- # no_raise_fun3
   exception
      when Constraint_Error =>
         return -1;                   -- # raise_predefined_fun3
      when others =>
         return 1;                    -- # raise_user_defined_fun3
   end Fun3;

   -----------
   -- Proc4 --
   -----------

   procedure Proc4 (I : in out My_Int) is
   begin
      I := I + 1;                    -- # proc4
      I := I / 2;                    -- # no_predefined_raise_proc4

      if I = 0 then                  -- # no_predefined_raise_proc4
         raise My_Exception;         -- # raise_user_defined_proc4
      end if;

      I := (I + 10) / I;             -- # no_raise_proc4
   exception
      when My_Exception =>
         I := -1;                    -- # raise_user_defined_proc4
      when others =>
         I := 1;                     -- # raise_predefined_proc4
   end Proc4;

   ----------
   -- Fun4 --
   ---------

   function Fun4 (I : My_Int) return My_Int is
      Res : My_Int := I;             -- # fun4
   begin
      Res := Res + 1;                -- # fun4

      if Res = 0 then                -- # no_predefined_raise_fun4
         raise My_Exception;         -- # raise_user_defined_fun4
      end if;

      Res := (Res + 10) / Res;       -- # no_raise_fun4

      return Res;                    -- # no_raise_fun4
   exception
      when My_Exception =>
         return -1;                   -- # raise_user_defined_fun4
      when others =>
         return 1;                    -- # raise_predefined_fun4
   end Fun4;

end Pack;
