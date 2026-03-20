with ADA.TEXT_IO;

package body PKG_UNDER_TEST is

   procedure PROC_UNDER_TEST (A : in Integer) is
   begin
      if (A > 10) then
         ADA.TEXT_IO.PUT_LINE ("A is over 10");
      else
         ADA.TEXT_IO.PUT_LINE ("A is under 10");
      end if;
   end PROC_UNDER_TEST;


   procedure PKG_TEST is separate;

end PKG_UNDER_TEST;
