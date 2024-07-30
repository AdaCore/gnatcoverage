package Pkg is

   type Bool_Acc is access all Boolean;

   procedure Swap (X, Y : Bool_Acc);

end Pkg;
