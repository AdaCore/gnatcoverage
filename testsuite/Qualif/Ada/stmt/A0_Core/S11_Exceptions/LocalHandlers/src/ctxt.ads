
package Ctxt is
   --  Prevent compilation warnings on some versions of ctxt.adb
   --  where X is accessed within generic bodies:
   pragma Elaborate_Body;

   X : Integer;
   R0 : Integer range 0 .. 0 := 0;
   I1 : Integer := 1;

   Handled : Integer := 0;
   procedure Run (Iraise, Xraise : Boolean);
end;
