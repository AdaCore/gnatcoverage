with Vars, P1, P2, Assert;

procedure Test_Blobs is
begin
   begin
      P1;
      Assert (Vars.N_Post_Raise = 0);
   exception
      when Program_Error => null;
   end;
   
   begin
      P2;
      Assert (Vars.N_Post_Raise = 0); 
   exception
      when Program_Error => null;
   end;
     
end;

--# p1.adb p2.adb vars.adb
--  /call_raise/  l+ ## 0
--  /post_raise/  l- ## s-
