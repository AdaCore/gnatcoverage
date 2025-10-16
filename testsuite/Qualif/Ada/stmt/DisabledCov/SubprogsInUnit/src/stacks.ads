package Stacks is

   type Varray is array (Natural range <>) of Integer;

   type Stack (Size : Natural) is record
      Vcount : Natural := 0;
      Values : Varray (1 .. Size);
      Ecount : Natural := 0;
   end record;

   type Op_Kind is (Push, Pop);

   procedure Push (S : in out Stack; Value : Integer);
   procedure Pop  (S : in out Stack; Value : out Integer);

   function  Errcount (S : in Stack) return Natural;
end;
