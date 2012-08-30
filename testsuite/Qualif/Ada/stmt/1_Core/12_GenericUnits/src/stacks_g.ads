--  Library-level generic package, contains elaboration code in the body
generic
   type Value is private;
   Init_Val : Value;

package Stacks_G is

   type Stack is private;

   procedure Push (V : Value; S : in out Stack);
   procedure Pop (V : out Value; S : in out Stack);

   function N_Values (S : in Stack) return Natural;

   function Default_Stack return Stack;

private
   --  We use a small static size here to prevent dragging dependencies to
   --  memcpy, malloc or secondary stack services, irrelevant to the purpose
   --  of the testcase family we serve.
   
   type Value_Array is array (1 .. 5) of Value; -- # elab
   
   type Stack is record           -- # elab
      Store : Value_Array;        -- # elab
      N_Values : Natural := 0;    -- # elab
   end record;

   Default_Stack_Var : Stack;     -- # elab
end Stacks_G;
