--  TODO: Type_Pkg needed to avoid problems with type conversion in the main
--  when instrumenting for MC/DC (U706-036)

with Type_Pkg; use Type_Pkg;

procedure Main is

   procedure Regular_And_Or (L, R : Typ; Res_And, Res_Or : out Typ);

   procedure Regular_And_Or_Use_Pkg (L, R : Typ; Res_And, Res_Or: out Typ);

   procedure Overloaded_And_Or (L, R : Typ; Res_And, Res_Or : out Typ);

   procedure Derived_And_Or (L, R : Derived; Res_And, Res_Or : out Derived);

   package Pkg is
      function "and" (Left, Right : Typ) return Typ;
      function "or" (Left, Right : Typ) return Typ;
   end Pkg;

   package body Pkg is
      function "and" (Left, Right : Typ) return Typ is
      begin
         if Left then
            return Right;
         end if;
         return False;
      end "and";

      function "or" (Left, Right : Typ) return Typ is
      begin
         if Left then
            return True;
         else
            return Right;
         end if;
      end "or";
   end Pkg;

   procedure Regular_And_Or (L, R : Typ; Res_And, Res_Or : out Typ) is
   begin
      Res_And := L and R;  --  Should be instrumented for MCDC
      Res_Or := L or R;    --  Same
   end Regular_And_Or;

   procedure Regular_And_Or_Use_Pkg (L, R : Typ; Res_And, Res_Or: out Typ) is
      use Pkg;
   begin
      --  Use clause visibility cannot hide declarations happening within the
      --  Standard Ada package, so the following should resolve to the
      --  Standard.Boolean operators, which need to be instrumented for MC/DC.

      Res_And := L and R;
      Res_Or := L or R;
   end Regular_And_Or_Use_Pkg;

   procedure Overloaded_And_Or (L, R : Typ; Res_And, Res_Or : out Typ) is
      function "and" (Left, Right : Typ) return Typ is
      begin
         if Left then
            return Right;
         else
            return False;
         end if;
      end "and";

      function "or" (Left, Right : Typ) return Typ is
      begin
         if Left then
            return True;
         else
            return Right;
         end if;
      end "or";

   begin
      --  Here there is no use clause visibility involved, so the operators
      --  used are the ones defined above, and should not be instrumented for
      --  MCDC.

      Res_And := L and R;
      Res_Or := L or R;
   end Overloaded_And_Or;

   procedure Derived_And_Or (L, R : Derived; Res_And, Res_Or : out Derived) is
   begin
      Res_And := L and R;
      Res_Or := L or R;
   end Derived_And_Or;

   True_Var : Boolean := True;
   pragma Volatile (True_Var);

   False_Var : Boolean := False;
   pragma Volatile (False_Var);

   True_Var_Der : Derived := True;
   pragma Volatile (True_Var_Der);

   False_Var_Der : Derived := False;
   pragma Volatile (False_Var_Der);

   Res_And, Res_Or : Typ;
   pragma Volatile (Res_And);
   pragma Volatile (Res_Or);

   Res_And_Der, Res_Or_Der : Derived;
   pragma Volatile (Res_And_Der);
   pragma Volatile (Res_Or_Der);

begin

   Regular_And_Or (True_Var, True_Var, Res_And, Res_Or);
   Regular_And_Or (False_Var, False_Var, Res_And, Res_Or);

   Regular_And_Or_Use_Pkg (True_Var, True_Var, Res_And, Res_Or);
   Regular_And_Or_Use_Pkg (False_Var, False_Var, Res_And, Res_Or);

   Overloaded_And_Or (True_Var, True_Var, Res_And, Res_Or);
   Overloaded_And_Or (False_Var, False_Var, Res_And, Res_Or);

   Derived_And_Or (True_Var_Der, True_Var_Der, Res_And_Der, Res_Or_Der);
   Derived_And_Or (False_Var_Der, False_Var_Der, Res_And_Der, Res_Or_Der);

   --  These do not actively test anything, but are used to make sure the
   --  procedures "and" and "or" in package Pkg are marked as covered,
   --  so coverage violation only happen in lines where the operators are
   --  called.

   Res_And := Pkg."and" (True_Var, True_Var);
   Res_And := Pkg."and" (False_Var, False_Var);

   Res_Or := Pkg."or" (True_Var, True_Var);
   Res_Or := Pkg."or" (False_Var, False_Var);

end Main;
