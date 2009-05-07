pragma Warnings (Off);
with System.Bb.Peripherals;
with System.Bb.Peripherals.Registers; use System.Bb.Peripherals.Registers;
pragma Warnings (On);

package body Uart is
   procedure Put (C : Character) renames System.Bb.Peripherals.Console_Send;
   
   procedure New_Line is
   begin
      Put (ASCII.CR);
      Put (ASCII.LF);
   end New_Line;
   
   procedure Put_Line (Str : String) is
   begin
      Put (Str);
      New_Line;
   end Put_Line;
   
   procedure Put (Str : String) is
   begin
      for I in Str'Range loop
	 Put (Str (I));
      end loop;
   end Put;

   procedure Get (C : out Character) is
      V : Uart_Data_Register;
   begin
      while not UART_1_Status.DR loop
	 null;
      end loop;
      V := UART_1_Data;
      C := V.RTD;
   end Get;
end Uart;
