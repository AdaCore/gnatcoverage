with Parent;        use Parent;
with Parent.Child1; use Parent.Child1;
with Parent.Child2; use Parent.Child2;
procedure Check_Class_Wide_Condition
  (Check_Var :     Integer;
   Result    : out Integer)
is
   type Access_T_Class is access T'Class; -- # dcl
   Var : Access_T_Class;                  -- # dcl

begin
   case Check_Var is                      -- # case
      when 1 =>
         Var := new T1_T'(C1 => 1);       -- # var_1
      when 2 =>
         Var := new T2_T'(C1 => 1);       -- # var_2
      when 3 =>
         Var := new T1_T'(C1 => 101);     -- # var_3
      when others =>
         null;                            -- # var_others
   end case;

   if Valid_C (Var.all) then              -- # eval
      Result := 10;                       -- # true
   else
      Result := 20;                       -- # false
   end if;

exception
   when Constraint_Error =>
      Result := 0;                        -- # exc
end Check_Class_Wide_Condition;
