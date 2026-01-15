pragma Ada_2012;

with Ghost_Ops;
package Ops is
   procedure Bump (X : in out Integer)
     with Pre => Ghost_Ops.Bumpable (X);
end;
