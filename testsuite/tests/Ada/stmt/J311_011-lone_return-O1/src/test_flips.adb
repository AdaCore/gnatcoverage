with Flips, Support; use Flips, Support;

procedure Test_Flips is
   B : Boolean := True;
   X : Integer := 5;
begin
   Flip (B);
   Assert (B = False);

   Flip (X);
   Assert (X = -5);
end;

--# flips.adb
--  /flipb/  l+ ## 0
--  /flipx/  l+ ## 0
