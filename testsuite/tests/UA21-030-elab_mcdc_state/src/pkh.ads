package Pkh is
   A : constant Boolean := True;

   B : constant Boolean := False;

   C : Boolean := A and then B;  -- elab_spec

   function Bar (X : Boolean) return Boolean;

end Pkh;
pragma Preelaborate (Pkh);
