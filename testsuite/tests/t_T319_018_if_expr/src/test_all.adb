with Pkg; use Pkg;
with Proc;

procedure Test_All is
begin
   Proc (Simple_In_Data_Port'(Incoming_Signal_Is_Null => False));
   Proc (Simple_In_Data_Port'(Incoming_Signal_Is_Null => True));
end Test_All;

--# proc.adb
--  /uncond/    l+ ## 0
--  /decision/  l+ ## 0
--  /out-true/  l+ ## 0
--  /out-false/ l+ ## 0
