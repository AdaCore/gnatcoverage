
package body Mystreams is
   
   procedure Sint_Read
     (S : not null access Root_Stream_Type'Class;
      Item : out Sint) is
   begin
      case Sint_Mode is -- # read_test
         when Double => Item.value := Latch * 2; -- # read_double
         when Copy => Item.Value := Latch;         -- # read_copy
      end case;
   end;
                      
   procedure Sint_Write 
     (S : not null access Root_Stream_Type'Class;
      Item : in Sint) is
   begin
      case Sint_Mode is -- # write_test
         when Double => Latch := Item.value * 2;  -- # write_double
         when Copy => Latch := Item.Value;        -- # write_copy
      end case;
   end;
   
   --
   
   overriding procedure Read
     (Port   : in out Port_T;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      null; -- # pread
   end Read;
   
   overriding procedure Write
     (Port   : in out Port_T;
      Buffer : Stream_Element_Array)
   is
   begin
      null; -- # pwrite
   end Write;

end;
