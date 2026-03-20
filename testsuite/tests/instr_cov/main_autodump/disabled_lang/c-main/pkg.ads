with Interfaces.C; use Interfaces.C;

package Pkg is

   function Fact (N : int) return int;
   pragma Export (C, Fact, "fact");

   procedure Check (Cond : int);
   pragma Export (C, Check, "check");
   --  This is akin to "assert", with an int arg and not
   --  conflicting with the libsupport exposed one.

end Pkg;
