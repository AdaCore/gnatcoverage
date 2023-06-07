with Ada.Finalization;

package Pkg is

   type Ctrl_Type is new Ada.Finalization.Controlled with null record;

   procedure Finalize (Self : in out Ctrl_Type);
end Pkg;