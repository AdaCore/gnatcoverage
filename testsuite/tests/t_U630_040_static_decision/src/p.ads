pragma Ada_2012;

package P is
   subtype Priority is Integer range 10 .. 20;

   FUNNY_PRIO : constant Integer := 999;

   USE_FUNNY_PRIO : constant Boolean := False;
   --  constant boolean to guard out of range computation

   Base_Prio : Priority :=
     (if USE_FUNNY_PRIO then FUNNY_PRIO - 1 else Priority'First); -- # eval

   --  When instrumenting, the tool needs to prevent a Witness call on
   --  the static USE_FUNNY_PRIO. Such a call would make the if-expr control
   --  non-static, which would get the compiler to evaluate both possible
   --  values and error out on FUNNY_PRIO - 1 with:
   --
   --   error: value not in range of type "Priority" defined at line 4
   --   error: static expression fails Constraint_Check

   --  And likewise for mcdc conditions:

   FORCE_BASE_PRIO : constant Boolean := False;

   function Prio_Val return Priority is
      (if USE_FUNNY_PRIO and then not FORCE_BASE_PRIO -- # complex
       then FUNNY_PRIO
       else Base_Prio);
end P;
