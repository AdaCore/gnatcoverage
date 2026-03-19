pragma Ada_2012;

package body Package_A is

   The_Instance : aliased T_A;

   NA_Intance : T_NA_A := The_Instance'Access;

   function NA_A return T_NA_A is (NA_Intance);

   overriding procedure Init (Obj : in out T_A)
   is
   begin
      Obj.Variable := 15;
   end Init;

   overriding procedure Run (Obj : in out T_A)
   is
   begin
      Obj.Enumeration := DEF;
   end Run;

   overriding function Get_Variable (Obj : in T_A) return Integer is (1);

end Package_A;
