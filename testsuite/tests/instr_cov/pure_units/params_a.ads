pragma Ada_2012;

package Params_A is
   Param_1 : constant Boolean := False with Export, External_Name => "param_1";
   Param_2 : constant Boolean := False with Export, External_Name => "param_2";
end Params_A;
