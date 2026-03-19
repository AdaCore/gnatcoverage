pragma Ada_2005;

package body Pkg is

   function Null_Out_Data_Port return Out_Data_Port'Class is
   begin
      return Out_Data_Port'Class (Simple_Out_Data_Port'(null record));
   end Null_Out_Data_Port;

   overriding function Image (Out_P : Simple_Out_Data_Port) return String is
   begin
     return "foo";
   end Image;

   overriding function Incoming_Signal
     (I : Simple_In_Data_Port) return Signal'Class
   is
   begin
      return Signal'Class
        (Simple_Signal'(Is_Null => I.Incoming_Signal_Is_Null));
   end Incoming_Signal;

   function Src_Port (S : Simple_Signal) return Out_Port'Class is
   begin
      return Out_Port'Class (Simple_Out_Data_Port'(null record));
   end Src_Port;

end Pkg;
