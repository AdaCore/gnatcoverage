with Parent;        use Parent;
with Parent.Child1; use Parent.Child1;
with Parent.Child2; use Parent.Child2;
procedure Check_Dynamic_Binding
  (Check_Var :     Integer;
   Result    : out Integer)
is
   type Access_T_Class is access T'Class;
   Var : Access_T_Class;

begin
   case Check_Var is
      when 1 =>
         Var := new T1_T'(C1 => 1);
      when 2 =>
         Var := new T2_T'(C1 => 1);
      when 3 =>
         Var := new T2_T'(C1 => 20);
      when 4 =>
         Var := new T2_T'(C1 => 101);
      when others =>
         null;
   end case;

   Result := Compute (Var.all);

end Check_Dynamic_Binding;
