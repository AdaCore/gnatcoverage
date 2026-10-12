pragma Flare_0_1;

package Pkg is
   procedure Proc_Finally (Early_Return : Boolean);
   --  Finally is after the begin of the procedure

   procedure Decl_Finally (Very_Early_Return, Early_Return : Boolean);
   --  Finally is after the begin of a declare block

   procedure Begin_Finally (Very_Early_Return, Early_Return : Boolean);
   --  Finally is after the begin of a begin block
end Pkg;
