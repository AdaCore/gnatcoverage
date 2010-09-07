--  Library-level generic package, contains elaboration code in the body
generic
   type Value is private;
   Init_Val : Value;

package Stacks_G is

   type Stack (Size : Natural) is private;

   procedure Push (V : Value; S : in out Stack);
   procedure Pop (V : out Value; S : in out Stack);

   function N_Values (S : in Stack) return Natural;

   function Default_Stack return Stack;

private
   type Value_Array is array (Natural range <>) of Value; -- # elab
   type Stack (Size : Natural) is record                  -- # elab
      Store : Value_Array (1 .. Size);                    -- # elab
      N_Values : Natural := 0;                            -- # elab
   end record;

   Default_Stack_Var : Stack (10);                        -- # elab
end Stacks_G;
