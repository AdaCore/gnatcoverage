pragma Ada_2012;

package Ghost_Ops
  with Ghost
is
   function Bumpable (X : Integer) return Boolean;
end;
