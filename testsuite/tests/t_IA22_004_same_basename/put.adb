with ada.text_io;

package body put is
   procedure putchar (c : integer) is
   begin
     ada.text_io.put (Character'val (C));
   end putchar;
end put;
