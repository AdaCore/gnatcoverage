with Ada.Text_IO; use Ada.Text_IO;

procedure Exemptions (Debug_Mode : Boolean; I : Integer) is
begin
   pragma Annotate (Xcov, Exempt_On, "Exempted"); -- # ex
   if Debug_Mode and then I < 0 then              -- # ex_if_neg
      Put_Line ("I is negative.");                -- # ex_error
   elsif Debug_Mode then                          -- # ex_if_debug
      Put_Line ("No error.");                     -- # ex_no_error
   end if;                                        -- # ex
   pragma Annotate (Xcov, Exempt_Off);            -- # ex
end Exemptions;
