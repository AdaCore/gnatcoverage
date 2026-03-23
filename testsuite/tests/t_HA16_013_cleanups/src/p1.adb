with Vars;
procedure P1 is
   S : string (1 .. Vars.N);
   pragma Volatile (S);
begin
   Vars.Raise_PE;   -- # call_raise
   Vars.Post_Raise; -- # post_raise
end;
