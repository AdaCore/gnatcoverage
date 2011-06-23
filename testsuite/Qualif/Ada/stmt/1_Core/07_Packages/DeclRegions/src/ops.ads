
package Ops is
   
   -- This package features a private and an internal subprogram (operations),
   -- with a single API to call them:
   
   type Opdata is record
      Np, Ni : Integer := 0;  -- Number of calls to each
   end record;
      
   procedure Call_Ops (Pop, Iop : Boolean; Opd : in out Opdata);
   
private
   
   -- The private op declaration, within the private part of the package spec
   
   procedure Op_Private (Opd : in out Opdata);
   
end;
