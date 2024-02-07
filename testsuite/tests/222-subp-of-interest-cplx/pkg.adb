pragma Ada_2012;

package body Pkg is

   function Zero return Integer is
   begin
      return 0;
   end Zero;

   function One return Integer is
   begin
     return (if String'("this is a very long decision"
                    & "that spans multiple lines")'Length = 1
      then 2
      else 1);
   end One;

   function Two return Integer is
   begin
      return 2;
   end Two;

end Pkg;
