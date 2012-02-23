------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

separate (Gdbtrace)
package body Gdb_MI_IO is
   Initialized      : Boolean := False;
   Time_Initialized : Time;
   To_Gdb, From_Gdb : File_Descriptor;
   Logging_On       : Boolean;
   Log_File         : File_Type;
   package Duration_IO is new Fixed_IO (Duration);
   use Duration_IO;

   procedure Initialize_MI
     (Gdb_In  : File_Descriptor;
      Gdb_Out : File_Descriptor;
      Log     : Boolean := False) is
   begin
      To_Gdb     := Gdb_In;
      From_Gdb   := Gdb_Out;
      Logging_On := Log;
      if Logging_On then
         Create (Log_File, Name => "rgdb.log");
      end if;
      Initialized := True;
      Time_Initialized := Clock;
   end Initialize_MI;

   function Get_MI_Output_Record return MI_Record_T is
      Return_Record : MI_Record_T;
      L : Natural;
   begin
      --  Not considering 'token's for now ???

      L := Get_Gdb_Line;
      Return_Record.Length := L;
      if L < 1 then
         Return_Record.Record_Type := Unknown_Record;
      elsif Gdb_Output_Buffer (1) = '~' then
         Return_Record.Record_Type := Console_Stream_Output;
      elsif Gdb_Output_Buffer (1) = '@' then
         Return_Record.Record_Type := Target_Stream_Output;
      elsif Gdb_Output_Buffer (1) = '&' then
         Return_Record.Record_Type := Log_Stream_Output;
      elsif Gdb_Output_Buffer (1) = '*' then
         Return_Record.Record_Type := Exec_Async_Record;
      elsif Gdb_Output_Buffer (1) = '+' then
         Return_Record.Record_Type := Status_Async_Record;
      elsif Gdb_Output_Buffer (1) = '=' then
         Return_Record.Record_Type := Notify_Async_Record;
      elsif Gdb_Output_Buffer (1) = '^' then
         Return_Record.Record_Type := Result_Record;
         if    L >=  5 and then Gdb_Output_Buffer (2 ..  5) = "done" then
            Return_Record.Result_Class := Done_Result;
         elsif L >=  8 and then Gdb_Output_Buffer (2 ..  8) = "running" then
            Return_Record.Result_Class := Running_Result;
         elsif L >= 10 and then Gdb_Output_Buffer (2 .. 10) = "connected" then
            Return_Record.Result_Class := Connected_Result;
         elsif L >=  6 and then Gdb_Output_Buffer (2 ..  6) = "error" then
            Return_Record.Result_Class := Error_Result;
         elsif L >=  5 and then Gdb_Output_Buffer (2 ..  5) = "exit" then
            Return_Record.Result_Class := Exit_Result;
         else
            Return_Record.Record_Type := Error_Record;
            Return_Record.Almost_Type := Result_Record;
         end if;
      elsif Gdb_Output_Buffer (1) = '(' then
         if L = 6 and Gdb_Output_Buffer (2 .. 6) = "gdb) " then
            Return_Record.Record_Type := Prompt_Record;
         else
            Return_Record.Record_Type := Unknown_Record;
         end if;
      end if;
      return Return_Record;
   end Get_MI_Output_Record;

   function Get_Gdb_Line return Natural is
      B_Idx  : Natural;
      N_Read : Integer;
      C : Character;
      procedure Log_Gdb_Output;

      procedure Log_Gdb_Output is
         S : constant String := Gdb_Output_Buffer (1 .. B_Idx);
      begin
         if Logging_On then
            Put (Log_File, Clock - Time_Initialized, Fore => 5, Aft => 2);
            Put (Log_File, " <-- ");
            Put (Log_File, S);
            New_Line (Log_File);
         end if;
      end Log_Gdb_Output;

   begin
      if not Initialized then
         Put_Line (Standard_Error,
                   "Gdb_Line_IO not initialized when Gdb_Get_Line called.");
         OS_Exit (1);
      end if;

      B_Idx := 1;
      loop
         N_Read := Read (From_Gdb, Gdb_Output_Buffer (B_Idx)'Address, 1);
         if N_Read /= 1 then
            Put_Line (Standard_Error, "Error reading gdb output.");
            OS_Exit (1);
         end if;
         if Gdb_Output_Buffer (B_Idx) = ASCII.LF then
            B_Idx := B_Idx - 1;
            Log_Gdb_Output;
            return B_Idx;
         end if;
         if B_Idx = Gdb_Output_Buffer'Last then
            Log_Gdb_Output;
            Put_Line (Standard_Error, "Gdb_Output_Buffer overflow");
            loop
               N_Read := Read (From_Gdb, C'Address, 1);
               if N_Read /= 1 then
                  Put_Line (Standard_Error, "Error reading gdb output.");
                  OS_Exit (1);
               end if;
               exit when C = ASCII.LF;
            end loop;
            return B_Idx;
         else
            B_Idx := B_Idx + 1;
         end if;
      end loop;
   end Get_Gdb_Line;

   procedure Put_Gdb_Line (S : String) is
      C : Character := ASCII.LF;
      N : Integer;
      pragma Unreferenced (N);
   begin
      if not Initialized then
         Put_Line (Standard_Error,
                   "Gdb_Line_IO not initialized when Put_Gdb__Line called.");
         OS_Exit (1);
      end if;
      if Logging_On then
         Put (Log_File, Clock - Time_Initialized, Fore => 5, Aft => 2);
         Put (Log_File, " --> ");
         Put (Log_File, S);
         New_Line (Log_File);
      end if;
      N := Write (To_Gdb, S'Address, S'Length);
      N := Write (To_Gdb, C'Address, 1);
   end Put_Gdb_Line;

end Gdb_MI_IO;
