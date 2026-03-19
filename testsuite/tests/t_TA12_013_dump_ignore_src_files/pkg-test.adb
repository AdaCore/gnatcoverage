with Ada.Text_IO; use Ada.Text_IO;

separate (pkg) procedure Test is
   procedure Sep;
   procedure Sep is separate;
begin
   Put_Line ("This test procedure is useless!");
   Sep;
end Test;
