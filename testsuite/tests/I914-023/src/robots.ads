
package Robots is
   type Opmode is (Cautious, Dumb);
   type Command is (Nop, Rotate, Step);

   type Robot is record
      Mode : Opmode := Cautious;
      Exec : Integer := 0;
   end record;

   procedure Run (R : in out Robot; C : Command);
end;
