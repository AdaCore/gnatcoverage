with Ada.Streams; use Ada.Streams;

package Mystreams is
   
   type Port_T is new Root_Stream_Type with null record;
   
   overriding procedure Read
     (Port   : in out Port_T;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   
   overriding procedure Write
     (Port   : in out Port_T;
      Buffer : Stream_Element_Array);
     
   --
   
   type Sint_Mode_Type is (Copy, Double);
   --  When reading or writing integers from/into a stream, whether
   --  we should read/fetch the base values untouched or multiply them
   --  by two;
   
   Sint_Mode : Sint_Mode_Type := Copy;
   
   type Sint is record
      Value : Integer;
   end record;
   
   procedure Sint_Read
     (S : not null access Root_Stream_Type'Class;
      Item : out Sint);
                      
   procedure Sint_Write 
     (S : not null access Root_Stream_Type'Class;
      Item : in Sint);
   
   for Sint'Read use Sint_Read;
   for Sint'Write use Sint_Write;
   
   Latch : Integer := 0;
   
end;
