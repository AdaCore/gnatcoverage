with Pkg; use Pkg;
with Proc;

procedure Test_False is
begin
   Proc (Simple_In_Data_Port'(Incoming_Signal_Is_Null => False));
end Test_False;

--# proc.adb
--  /uncond/    l+ ## 0
--  /decision/  l! ## d!
--  /out-true/  l+ ## 0
--  /out-false/ l+ ## 0
--
-- %opts: --trace-mode=src
-- =/decision/ l! ## dF-
