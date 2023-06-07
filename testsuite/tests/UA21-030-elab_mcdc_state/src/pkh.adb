package body Pkh is

   function Bar (X : Boolean) return Boolean is

      package Inner_Bar is
         Res_Val : Boolean := X and then C; -- # elab_fun
      end Inner_Bar;

   begin
      return Inner_Bar.Res_Val;
   end Bar;
end Pkh;