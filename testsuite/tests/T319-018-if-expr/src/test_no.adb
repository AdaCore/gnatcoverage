pragma Ada_2005;

with Pkg; use Pkg;
with Proc;

procedure Test_No is
begin
   if Null_Out_Data_Port.Image = "" then
      Proc (Simple_In_Data_Port'(Incoming_Signal_Is_Null => False));
   end if;
end Test_No;

--# proc.adb
--  /uncond/    l- ## s-
--  /decision/  l- ## 0
--  /out-true/  l- ## 0
--  /out-false/ l- ## 0
