with Ada.Text_IO; use Ada.Text_IO;

procedure Characterize (X : Integer) is
   Ximg : constant String := Integer'Image (X); -- # decl
begin
   if X > 0 then                          -- # first-cond
      Put_Line (Ximg & " is Positive");   -- # first-stmt
   elsif X < 0 then                       -- # second-cond
      Put_Line (Ximg & " is Negative");   -- # second-stmt
   else
      Put_Line (Ximg & " is Null");       -- # third-stmt
   end if;
end;
