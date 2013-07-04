------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2013-2013, AdaCore                     --
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

with Outputs; use Outputs;

with Ada.Unchecked_Conversion;

with Qemu_Traces;
with Interfaces;

with Traces_Files; use Traces_Files;
with GNAT.Strings;

package body Convert is

   procedure Set_Trace_Source (Arg : String) is
   begin
      if Arg = "iSystem-5634" then
         Trace_Source := Isystem_5634;
      else
         Fatal_Error (Arg & " is not a known Trace Source.");
      end if;
   end Set_Trace_Source;

   procedure Run_Convert (Exe_Name : String_Access;
                          Output   : String_Access;
                          Histmap  : String_Access;
                          Tag      : String_Access) is
      Prg                : String_Access;
      Opts               : String_List_Access;
      Success            : Boolean;
      Trace_Output       : String_Access;
      Trace_Arg          : String_Access;
      Trigger_Start_ID   : String_Access;
      Trigger_Start_Addr : String_Access;
      Trigger_Stop_ID    : String_Access;
      J, K               : Positive;
   begin
      if Exe_Name = null then
         Fatal_Error ("No executable provided for convert.");
      elsif Trace_Source = Unspecified then
         Fatal_Error ("No trace source provided for convert.");
      elsif Input_Arg = null then
         Fatal_Error ("No input provided for convert.");
      elsif Trace_Source = Isystem_5634 then

         --  This Trace Source requires (for now at least) the use of
         --  --hw-trigger_traces. Here we check that it is present,
         --  and do very minimal processing -- just breaking it up
         --  into 3 strings separated by the first 2 commas seen.
         --  The driver called for doing the conversion does further
         --  further checking of the validity of the components.

         if HW_Trigger_Arg = null then
            Fatal_Error ("Specified trace-source needs HW trigger argument.");
         end if;
         J := HW_Trigger_Arg'First;
         loop
            if J >= HW_Trigger_Arg'Last then
               Fatal_Error ("Invalid hw-trigger-traces syntax.");
            end if;
            exit when HW_Trigger_Arg.all (J) = ',';
            J := J + 1;
         end loop;
         Trigger_Start_ID :=
           new String'(HW_Trigger_Arg.all (HW_Trigger_Arg'First .. J - 1));
         if Trigger_Start_ID'Length = 0 then
            Fatal_Error ("Missing START_ID in hw-trigger-traces.");
         end if;
         J := J + 1;
         K := J;
         loop
            if J >= HW_Trigger_Arg'Last then
               Fatal_Error ("Invalid hw-trigger-traces syntax.");
            end if;
            exit when HW_Trigger_Arg.all (J) = ',';
            J := J + 1;
         end loop;
         Trigger_Start_Addr := new String'(HW_Trigger_Arg.all (K .. J - 1));
         if Trigger_Start_Addr'Length = 0 then
            Fatal_Error ("Missing START_ADDR in hw-trigger-traces.");
         end if;
         J := J + 1;
         K := J;
         loop
            exit when J > HW_Trigger_Arg'Last;
            J := J + 1;
         end loop;
         Trigger_Stop_ID := new String'(HW_Trigger_Arg.all (K .. J - 1));
         if Trigger_Stop_ID'Length = 0 then
            Fatal_Error ("Missing STOP_ID in hw-trigger-traces.");
         end if;

         if Output = null then
            Trace_Output := new String'(Exe_Name.all & ".trace");
         else
            Trace_Output := Output;
         end if;

         --  Create the trace file

         declare
            use Qemu_Traces;
            use Interfaces;
            Trace_File : Trace_File_Type;
            Date_Info  : Trace_Info_Date;
            Date       : constant OS_Time := Current_Time;
            subtype String_8 is String (1 .. 8);
            function Date_Info_To_Str is new Ada.Unchecked_Conversion
              (Trace_Info_Date, String_8);
         begin
            Create_Trace_File (Info, Trace_File);
            Date_Info :=
              Trace_Info_Date'(Year  => Unsigned_16 (GM_Year (Date)),
                               Month => Unsigned_8  (GM_Month (Date)),
                               Day   => Unsigned_8  (GM_Day (Date)),
                               Hour  => Unsigned_8  (GM_Hour (Date)),
                               Min   => Unsigned_8  (GM_Minute (Date)),
                               Sec   => Unsigned_8  (GM_Second (Date)),
                               Pad   => 0);
            Append_Info (Trace_File, Date_Time, Date_Info_To_Str (Date_Info));
            Append_Info (Trace_File, Exec_File_Name, Exe_Name.all);

            if GNAT.Strings."/=" (Tag, null) then
               Append_Info (Trace_File, User_Data, Tag.all);
            end if;

            Write_Trace_File (Trace_Output.all, Trace_File);
            Free (Trace_File);
         end;

         if Histmap = null then
            Trace_Arg := Trace_Output;
         else
            Trace_Arg :=
              new String'("histmap=" & Histmap.all & ',' & Trace_Output.all);
         end if;

         Prg := Locate_Exec_On_Path ("../libexec/gnatcoverage/isys_drv");
         if Prg = null then
            Fatal_Error ("Could not find convert program.");
         end if;
         Opts := new String_List'(1 => new String'("5634"),
                                  2 => Exe_Name,
                                  3 => Trace_Arg,
                                  4 => Input_Arg,
                                  5 => Trigger_Start_ID,
                                  6 => Trigger_Start_Addr,
                                  7 => Trigger_Stop_ID
                                 );
         Spawn (Prg.all, Opts.all, Success);
      end if;
   end Run_Convert;

end Convert;
