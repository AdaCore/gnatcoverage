pragma Ada_2012;

package Pkg is

   -----------------
   --  Interfaces --
   -----------------

   type Signal;

   type Out_Port is interface;
   type Out_Data_Port is interface and Out_Port;
   function Image (Out_P : Out_Data_Port) return String is abstract;

   type In_Port is interface;
   function Incoming_Signal (I : In_Port) return Signal'Class is abstract;

   type In_Data_Port is interface and In_Port;

   type Signal is interface;
   function Is_Not_Null (S : Signal) return Boolean is abstract;
   function Src_Port (S : Signal) return Out_Port'Class is abstract;

   function Null_Out_Data_Port return Out_Data_Port'Class;

   ---------------------
   -- Implementations --
   ---------------------

   type Simple_Out_Data_Port is new Out_Data_Port with null record;
   overriding function Image (Out_P : Simple_Out_Data_Port) return String;

   type Simple_In_Data_Port is new In_Data_Port with record
      Incoming_Signal_Is_Null : Boolean;
   end record;
   overriding function Incoming_Signal
     (I : Simple_In_Data_Port) return Signal'Class;

   type Simple_Signal is new Signal with record
      Is_Null : Boolean;
   end record;
   function Is_Not_Null (S : Simple_Signal) return Boolean is (not S.Is_Null);
   function Src_Port (S : Simple_Signal) return Out_Port'Class;
end Pkg;
