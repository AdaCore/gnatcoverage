with Vars;
procedure P2 is
begin
   declare
      S : string (1 .. Vars.N);
      pragma Volatile (S);
   begin
      Vars.Raise_PE;   -- # call_raise
      Vars.Post_Raise; -- # post_raise
   end;
end;
