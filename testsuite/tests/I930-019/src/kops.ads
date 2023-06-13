
generic
   K : Integer;
package Kops is
   procedure Inc (X : in out Integer);
   pragma Inline (Inc);
end;

