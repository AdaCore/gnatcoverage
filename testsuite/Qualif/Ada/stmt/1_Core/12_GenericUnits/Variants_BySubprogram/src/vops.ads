--  This can be instantiated multiple times e.g. for different vector sizes,
--  and different instances may call different sets of subprograms.

generic
   Size : in Integer;
package Vops is
   type Vector_Type is array (1 .. Size) of Integer;
   
   procedure Inc (V : in out Vector_Type; Amount : Integer);
   procedure Mult (V : in out Vector_Type; Amount : Integer);
end;
