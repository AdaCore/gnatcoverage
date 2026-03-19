package body Stacks_G is

   procedure Push (V : Value; S : in out Stack) is
   begin
      S.Store (S.N_Values + 1) := V;   -- # push
      S.N_Values := S.N_Values + 1;    -- # push
   end;

   procedure Pop (V : out Value; S : in out Stack) is
   begin
      V := S.Store (S.N_Values);       -- # pop
      S.N_Values := S.N_Values - 1;    -- # pop
   end;

   function N_Values (S : in Stack) return Natural is
   begin
      return S.N_Values;               -- # n_values
   end;

   function Default_Stack return Stack is
   begin
      return Default_Stack_Var;        -- # default_stack
   end Default_Stack;

begin
   Push (Init_Val, Default_Stack_Var); -- # elab
end Stacks_G;
