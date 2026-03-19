package Vars is

   N : Integer := 12; -- to be used for VLA sizing

   procedure Raise_PE;

   procedure Post_Raise;
   N_Post_Raise : Integer := 0;
end;
