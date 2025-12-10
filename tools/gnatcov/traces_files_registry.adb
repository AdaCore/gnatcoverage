------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Containers.Generic_Sort;
with Ada.Containers.Hashed_Sets;

with Interfaces.C;

with Checkpoints; use Checkpoints;
with Outputs;
with Qemu_Traces; use Qemu_Traces;

package body Traces_Files_Registry is

   use type Unbounded_String;

   package Traces_Files_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Trace_File_Element_Acc,
        Hash                => Hash,
        Equivalent_Elements => Equivalent);

   Files : Traces_Files_Sets.Set;

   procedure Sort (V : in out Traces_Files_Vectors.Vector);
   --  Sort elements in V according to the "<" predicate

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : Trace_File_Element_Acc) return Boolean is
   begin
      return
        Left.Filename = Right.Filename
        and then Left.Kind = Right.Kind
        and then Left.Context = Right.Context
        and then Left.Program_Name = Right.Program_Name
        and then Left.Time = Right.Time
        and then Left.User_Data = Right.User_Data;
   end Equivalent;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Trace_File_Element_Acc) return Boolean is
   begin
      if Left.Filename < Right.Filename then
         return True;
      elsif Left.Filename > Right.Filename then
         return False;
      end if;

      if Left.Kind < Right.Kind then
         return True;
      elsif Left.Kind > Right.Kind then
         return False;
      end if;

      if Left.Context < Right.Context then
         return True;
      elsif Left.Context > Right.Context then
         return False;
      end if;

      if Left.Program_Name < Right.Program_Name then
         return True;
      elsif Left.Program_Name > Right.Program_Name then
         return False;
      end if;

      if Left.Time < Right.Time then
         return True;
      elsif Left.Time > Right.Time then
         return False;
      end if;

      if Left.User_Data < Right.User_Data then
         return True;
      elsif Left.User_Data > Right.User_Data then
         return False;
      end if;

      return False;
   end "<";

   ----------
   -- Hash --
   ----------

   function Hash
     (Element : Trace_File_Element_Acc) return Ada.Containers.Hash_Type
   is
      use type Ada.Containers.Hash_Type;
   begin
      return
        Strings.Hash (Element.Filename)
        + Trace_File_Kind'Pos (Element.Kind)
        + Strings.Hash (Element.Context)
        + Strings.Hash (Element.Program_Name)
        + Strings.Hash (Element.Time)
        + Strings.Hash (Element.User_Data);
   end Hash;

   -------------------------------
   -- Create_Trace_File_Element --
   -------------------------------

   function Create_Trace_File_Element
     (Filename : String; Kind : Trace_File_Kind) return Trace_File_Element_Acc
   is
   begin
      return Result : constant Trace_File_Element_Acc := new Trace_File_Element
      do
         Result.Filename := +Filename;
         Result.Kind := Kind;
         Result.Context := Null_Unbounded_String;
      end return;
   end Create_Trace_File_Element;

   ------------------------------
   -- Update_From_Binary_Trace --
   ------------------------------

   procedure Update_From_Binary_Trace
     (Element : in out Trace_File_Element; File : Trace_File_Type) is
   begin
      Element.Program_Name := +Get_Info (File, Exec_File_Name);
      Element.Time := +Format_Date_Info (Get_Info (File, Date_Time));
      Element.User_Data := +Get_Info (File, User_Data);
   end Update_From_Binary_Trace;

   ------------------------------
   -- Update_From_Source_Trace --
   ------------------------------

   procedure Update_From_Source_Trace
     (Element : in out Trace_File_Element;
      Kind    : Traces_Source.Supported_Info_Kind;
      Data    : String)
   is
      use Traces_Source;
   begin
      case Kind is
         when Traces_Source.Info_End =>
            --  As this special kind is used only to encode the end of a trace
            --  entry sequence in the file format, we don't expect it to show
            --  up at execution.

            raise Program_Error;

         when Info_Program_Name      =>
            Element.Program_Name := +Data;

         when Info_Exec_Date         =>
            declare
               use Ada.Calendar.Conversions;
               use Interfaces;
               use Interfaces.C;

               --  We first read the stamp as an unsigned_64 then craft a
               --  "long" value, which the time interpretation services
               --  expect. Going through unsigned_64 is useful to "support"
               --  bogus input values that would overflow a long, occasionally
               --  observed on non-native systems where the time services are
               --  misconfigured.

               Timestamp : Unsigned_64 := 0;
            begin
               --  Turn Data (little-endian 64-bit Unix timestamp) into
               --  Timestamp.

               if Data'Length /= 8 then
                  Outputs.Fatal_Error
                    (+Element.Filename & "invalid execution date format");
               end if;
               for I in reverse Data'Range loop
                  Timestamp := 2 ** 8 * Timestamp + Character'Pos (Data (I));
               end loop;

               --  We can now split this timestamp it in UTC and turn this into
               --  the Trace_Info_Date structure to finally use
               --  Format_Date_Info for our internal data structures.

               declare
                  Info_Date           : Trace_Info_Date;
                  Info_Date_As_String : String (1 .. Trace_Info_Date'Size / 8)
                  with Import, Address => Info_Date'Address;

                  Year, Month, Day, Hour, Minute, Second : int;

                  --  Convert the timestamp we got (Unsigned_64) to the
                  --  corresponding Unix API type (long long). In case we get
                  --  buggy (too large) values, silently cap them.

                  Max_Timestamp  : constant Unsigned_64 :=
                    Unsigned_64 (long_long'Last);
                  Unix_Timestamp : constant long_long :=
                    long_long (Unsigned_64'Min (Timestamp, Max_Timestamp));
               begin
                  To_Struct_Tm
                    (To_Ada_Time_64 (Unix_Timestamp),
                     Year,
                     Month,
                     Day,
                     Hour,
                     Minute,
                     Second);
                  Info_Date.Year := Unsigned_16 (1900 + Year);
                  Info_Date.Month := Unsigned_8 (1 + Month);
                  Info_Date.Day := Unsigned_8 (Day);
                  Info_Date.Hour := Unsigned_8 (Hour);
                  Info_Date.Min := Unsigned_8 (Minute);
                  Info_Date.Sec := Unsigned_8 (int'Min (Second, 59));
                  Element.Time := +Format_Date_Info (Info_Date_As_String);
               end;
            end;

         when Info_User_Data         =>
            Element.User_Data := +Data;
      end case;
   end Update_From_Source_Trace;

   ---------------------
   -- Add_Traces_File --
   ---------------------

   procedure Add_Traces_File (TF : in out Trace_File_Element_Acc) is
      Dummy_Cur : Traces_Files_Sets.Cursor;
      Inserted  : Boolean;
   begin
      Files.Insert (TF, Dummy_Cur, Inserted);
      if not Inserted then
         Free (TF);
      end if;
   end Add_Traces_File;

   ----------
   -- Sort --
   ----------

   procedure Sort (V : in out Traces_Files_Vectors.Vector) is
      function Before (Left, Right : Positive) return Boolean
      is (V (Left) < V (Right));

      procedure Swap (Left, Right : Positive);
      --  Swap elements at positions Left and Right in V

      procedure Sort_Helper is new
        Ada.Containers.Generic_Sort (Positive, Before, Swap);

      ----------
      -- Swap --
      ----------

      procedure Swap (Left, Right : Positive) is
         Temp : constant Trace_File_Element_Acc := V (Left);
      begin
         V (Left) := V (Right);
         V (Right) := Temp;
      end Swap;

      --  Start of processing for Sort

   begin
      Sort_Helper (V.First_Index, V.Last_Index);
   end Sort;

   -------------------------
   -- Sorted_Traces_Files --
   -------------------------

   function Sorted_Traces_Files return Traces_Files_Vectors.Vector is
   begin
      return Result : Traces_Files_Vectors.Vector do
         for Cur in Files.Iterate loop
            Result.Append (Traces_Files_Sets.Element (Cur));
         end loop;
         Sort (Result);
      end return;
   end Sorted_Traces_Files;

   -----------------------------
   -- Iterate_On_Traces_Files --
   -----------------------------

   procedure Iterate_On_Traces_Files
     (Callback : access procedure (Element : Trace_File_Element))
   is
      Files : constant Traces_Files_Vectors.Vector := Sorted_Traces_Files;
   begin
      for F of Files loop
         Callback.all (F.all);
      end loop;
   end Iterate_On_Traces_Files;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (CSS     : access Checkpoints.Checkpoint_Save_State;
      Context : access Coverage.Context)
   is
      This_Context : constant Unbounded_String :=
        +Coverage.To_String (Context.all);
   begin
      for TF of Files loop
         CSS.Write (TF.Filename);

         declare
            --  If this trace file does not come from a checkpoint (TF.Context
            --  is empty), then this context is the original one where it has
            --  actually been processed: record in in its infos.

            TF_Context : constant Unbounded_String :=
              (if TF.Context = "" then This_Context else TF.Context);
         begin
            CSS.Write_U8 (Trace_File_Kind'Pos (TF.Kind));
            CSS.Write (TF_Context);
            CSS.Write (TF.Program_Name);
            CSS.Write (TF.Time);
            CSS.Write (TF.User_Data);
         end;
      end loop;

      --  Mark end of list with empty string

      CSS.Write_Unbounded ("");
   end Checkpoint_Save;

   ----------------------
   -- Checkpoint_Clear --
   ----------------------

   procedure Checkpoint_Clear is
   begin
      Files.Clear;
   end Checkpoint_Clear;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (CLS : in out Checkpoints.Checkpoint_Load_State)
   is
   begin
      loop
         declare
            Name    : constant Unbounded_String := CLS.Read_Unbounded_String;
            CP_File : Trace_File_Element_Acc;
         begin
            exit when Name = "";
            CP_File := new Trace_File_Element;
            CP_File.Filename := Name;
            CP_File.Kind := Trace_File_Kind'Val (CLS.Read_U8);
            CLS.Read (CP_File.Context);
            CLS.Read (CP_File.Program_Name);
            CLS.Read (CP_File.Time);
            CLS.Read (CP_File.User_Data);

            Add_Traces_File (CP_File);
         end;
      end loop;
   end Checkpoint_Load;

end Traces_Files_Registry;
