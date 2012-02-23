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

with GNAT.Expect;      use GNAT.Expect;
with Text_IO;          use Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Interfaces;       use Interfaces;
with Ada.Calendar;     use Ada.Calendar;
with Ada.Unchecked_Deallocation;

with Traces_Dbase; use Traces_Dbase;
with Traces;       use Traces;
with Qemu_Traces;  use Qemu_Traces;
with Traces_Files; use Traces_Files;

--  Gdbtrace implements a gdb-based mechanism for creating trace information
--  from execution of a program on a remote target.
--
--  It has been developed with execution on a bare board, and loading/
--  execution via gdb's remote serial protocol in mind. It is PowerPC
--  target specific, and deals only with code that follows the branch
--  instructions in the executable (no interrupts or exceptions are
--  handled).
--
--  The design in influenced by the great overhead of running the target
--  programs under gdb control, when the outcome of every branch needs
--  to be detected and recorded at run time.
--
--  The program preprocesses the target executable, and creates an
--  array of structures which represent a block of code with only one
--  branch instrucion in it, at the end of the block.  The basic loop
--  involved in running the program under gdb control consists of
--  finding which block the PC is in, setting a temporary HW breakpoint
--  in that branch, run to the break, single step, record which way
--  the branch went, and repeat.

procedure Gdbtrace is

   Executable_Name_Ptr    : String_Access;
   Remote_Target_Name_Ptr : String_Access;
   Spawn_Timeout          : Duration;

   procedure Process_Command_Line;
   --  Process_Command_Line reads the gdbtrace command line and sets
   --  the values of the variables above. It causes the program to
   --  exit with an error code if the command line is invalid. For
   --  the Usage string as well as default values of optional parameters
   --  see the body of the procedure.

   Failure_Exit_Code : constant Integer := 1;

   procedure Process_Command_Line is
      Usage : constant String
        := "usage: gdbtrace [--spawn-timeout=Secs] remote-targ-name prog-name";
      --  The first required argument is the string that should complete
      --  the "target remote" command to gdb. The second is the filename
      --  of the target executable file.

      Current_Arg : Positive;
      S : String_Access;

      function Prefix_Cmp
        (Pref : String; S_Ptr : String_Access) return Boolean;
      --  Returns True iff the String pointed to by Ptr is a prefix
      --  of S.
      function Prefix_Cmp
        (Pref : String; S_Ptr : String_Access) return Boolean is
      begin
         if Pref'Length > S_Ptr'Length then
            return False;
         end if;
         return Pref = S_Ptr (S_Ptr'First .. S_Ptr'First + Pref'Length - 1);
      end Prefix_Cmp;

   begin
      Spawn_Timeout := 12.0;

      if Argument_Count < 2 then
         Put_Line (Standard_Error, "Too few arguments.");
         Put_Line (Standard_Error, Usage);
         OS_Exit (Failure_Exit_Code);
      end if;

      Current_Arg := 1;
      loop
         if Current_Arg + 1 > Argument_Count then
            Put_Line (Standard_Error, "Too few arguments.");
            Put_Line (Standard_Error, Usage);
            OS_Exit (Failure_Exit_Code);
         end if;
         S := new String'(Argument (Current_Arg));
         if Prefix_Cmp ("--", S) then
            if Prefix_Cmp ("--spawn-timeout=", S) then
               null;
            else
               Put_Line (Standard_Error, "Invalid argument: " & S.all);
               Put_Line (Standard_Error, Usage);
               OS_Exit (Failure_Exit_Code);
            end if;
         else
            exit;
         end if;
         Free (S);
         Current_Arg := Current_Arg + 1;
      end loop;
      Remote_Target_Name_Ptr := S;
      Executable_Name_Ptr := new String'(Argument (Current_Arg + 1));
   end Process_Command_Line;

   --  The Exec_Info package contains the data structures for representing
   --  blocks of code, and the procedure for extracting that information
   --  from a target exectuable file.

   package Exec_Info is

      type Code_Block_T is record
         Start                  : Unsigned_32;
         N_Insns                : Positive;
      end record;
      --  The type Code_Block_T is used to represent a blocksof code in
      --  the target program being executed. A block of consists of
      --  a sequence of zero or more non-branch instructions, followed
      --  by a single branch. The member Start contains the starting
      --  address of the block, and N_Insns contains a count of number
      --  of instructions in the block (including the branch).

      type Code_Block_Array_T is array (Positive range <>) of Code_Block_T;
      type Code_Block_Array_Ptr_T is access Code_Block_Array_T;

      procedure Process_Exec
        (Exe_Filename : String;
         CBA_Ptr      : out Code_Block_Array_Ptr_T;
         Exit_Addr    : out Unsigned_32);
      --  Before the program is loaded and run, it is processed by the
      --  Process_Exec procedure, resulting in an array of Code_Block_T
      --  elements, which together cover all of the executable code in
      --  the program.
      --
      --  The address which tells gdbtrace to stop execution (i.e., when
      --  the PC contains this address after a branch) is also returned.
      --  Currently, this is hardcoded as the value of the symbol 'abort' ???
      --
      --  Process_Exec will raise Program_Error for error conditions including
      --  inability to process executable file, no code blocks found, and
      --  the designated stop symbol not found.
   end Exec_Info;
   package body Exec_Info is separate;
   use Exec_Info;

   CBA        : Code_Block_Array_Ptr_T;
   PC_Val     : Unsigned_32;
   First_Addr : Unsigned_32;
   Last_Addr  : Unsigned_32;
   --  CBA contains the code block array calculated by Process_Exec.
   --  The other variables are used in the main loop of the program
   --  to keep track of the PC and identify the code block that contains
   --  the PC at any time.

   --  The capacity to keep track of some data about the history of
   --  the execution of the target program.... may or may not be
   --  needed depending up the functionality of the QEMU trace
   --  interface, which is not understood at this point. ??? ...
   --  or different data than that represented by the declarations
   --  below, may be needed.

   type Block_History_T is record
      Branch_Was_Taken_Start : Unsigned_32;
      Branch_Not_Taken_Start : Unsigned_32;
   end record;

   type Block_History_Array_T is array (Positive range <>) of Block_History_T;
   type Block_History_Array_Ptr_T is access Block_History_Array_T;
   Block_History : Block_History_Array_Ptr_T;
   pragma Unreferenced (Block_History);
   --  The variable Block_History points to an array of Block_History_T
   --  records, and parallels the Code_Block_Array caluculated and
   --  returned by Exec_Info.Process_Exec. During the execution of the
   --  target program, each Block_History_T element in the array, keeps
   --  track of data about instructions in the corresponding Code_Block.
   --
   --  When the value in Branch_Was_Taken_Start component is outside the
   --  range of addresses of the corresponding block, then the target program
   --  has not executed instructions within the block that resulted in
   --  the terminating branch instruction being taken. If the value
   --  of Branch_Was_Taken_Start is an address in the block, then at
   --  some time during program execution, the block was entered at
   --  Branch_Was_Taken_Start, the instructions up to the branch at the
   --  end of the block were executed and the branch was taken. The value
   --  is the lowest address in the block for which that happened.
   --  Branch_Not_Taken_Start has the same function except it tracks
   --  executions within the block that result in the final branch
   --  not being taken.

   Tbase : Traces_Base;
   Trace_Filename_Ptr : String_Access;
   Pc_B, Pc_E : Pc_Type := 0;
   Tfile : Trace_File_Type;
   At_Start : Boolean;
   Op : Unsigned_8;

   --  A package for using the MI interface to interact with gdb could
   --  be useful. Currently (until further research), this small package
   --  is defined here to meet gdbtrace's requirements for interacting
   --  with gdb using the MI interface.

   package Gdb_MI_IO is
      Gdb_Output_Buffer : String (1 .. 4096);
      --  Make into something that grows as needed ???

      type MI_Record_Type_T is (Result_Record,         Exec_Async_Record,
                                Status_Async_Record,   Notify_Async_Record,
                                Console_Stream_Output, Target_Stream_Output,
                                Log_Stream_Output,     Prompt_Record,
                                Unknown_Record,        Error_Record);
      --  MI_Record_Type represent the types of records returned from
      --  gdb via the MI interface.
      --
      --  Result_Record records start with the '^' character followed by
      --   the name of the result-class.
      --  Exec_Async_Record records start with a '*', followed by an
      --   async class name.
      --  Status_Async_Record records start with a '+', followed by an
      --   async class name.
      --  Notify_Async_Record records start with a '=', followed by an
      --   async class name.
      --  Records consisting of "(gdb) " are of type Prompt_Record;
      --  When a record's type is not recognized it is of type Unknown_Reocrd;
      --  When a record looks like a known type (e.g. by the first character),
      --   but is malformed (e.g. unknown result class) it is classified
      --   as an Error_Record.

      type Result_Class_T is (Done_Result,  Running_Result, Connected_Result,
                              Error_Result, Exit_Result);

      type Async_Class_T is (Stopped_Async, Running_Async, Other_Async);
      pragma Unreferenced (Stopped_Async);
      pragma Unreferenced (Running_Async);
      pragma Unreferenced (Other_Async);

      type MI_Record_T is record
         Record_Type  : MI_Record_Type_T;
         Almost_Type  : MI_Record_Type_T;
         Result_Class : Result_Class_T;
         Async_Class  : Async_Class_T;
         Length       : Natural;
      end record;
      --  Get_MI_Output_Record (below) returns values of this type. It
      --  contains information about a gdb mi output record.
      --  - Almost_Type is filled in when Record_Type is Error_Record,
      --    It contains that type indicated by the first charaacter (before
      --    a problem was found).
      --  - Result_Class is filled in when Record_Type is Result_Record.
      --  - Async_Class isn't yet used.
      --  - Length is the length of the record (which can be found in
      --    Gdb_Output_Buffer after Get_MI_Output_Record returns).

      procedure Initialize_MI
        (Gdb_In  : File_Descriptor;
         Gdb_Out : File_Descriptor;
         Log     : Boolean := False);

      function Get_MI_Output_Record return MI_Record_T;

      function  Get_Gdb_Line return Natural;
      --  Gets the string output by GDB MI, placing it in Gdb_Output_Buffer.
      --  This is used by Get_MI_Output_Rcord (so maybe should be made
      --  internal???).

      procedure Put_Gdb_Line (S : String);
      --  Sends a gdb command over MI.
   end Gdb_MI_IO;
   package body Gdb_MI_IO is separate;
   use Gdb_MI_IO;

   MI_Output : MI_Record_T;

   GDB_Process : Process_Descriptor;
   --  The Process_Descriptor type comes from GNAT.Expect. The Expect
   --  package is only used to spawn gdb and retrieving the file
   --  descriptors for communicating with gdb.
   --  Handy interface for that functionality without all the
   --  stuff of Expect???

   procedure Command_Gdb (Command : String; T : Duration; Err_Msg : String);
   --  Send Command to gdb. Set the watchdog timer to timeout after T,
   --  changing any previous deadline. If T is 0, then the watchdog timer
   --  is turned off. The error message output by the watchdog timer is
   --  given by Err_Msg.

   function Get_Hval (S  : String) return Unsigned_32;
   --  Used to read a hex value from gdb output.

   procedure Addr_Str_In_C (Addr : Unsigned_32; S : in out String);
   --  Create a string representation of Addr in a format that can
   --  be passed to gdb.

   C_Addr : String (1 .. 10);

   function Get_Hval (S  : String) return Unsigned_32 is
      V  : Unsigned_32 := 0;
      HD : Unsigned_32;
      J  : Natural;

      Bad_Hex_Char : constant Unsigned_32 := 16;

      function Hex_Digit (C : Character) return Unsigned_32;
      --  Returns a value in 0 .. 15 unless C is not a valid
      --  hex digit, in which case Bad_Hex_Char is returned;

      function Hex_Digit (C : Character) return Unsigned_32 is
      begin
         if C in '0' .. '9' then
            return Unsigned_32 (Character'Pos (C) - Character'Pos ('0'));
         elsif C in 'a' .. 'f' then
            return Unsigned_32 (Character'Pos (C) - Character'Pos ('a') + 10);
         elsif C in 'A' .. 'F' then
            return Unsigned_32 (Character'Pos (C) - Character'Pos ('A') + 10);
         end if;
         return Bad_Hex_Char;
      end Hex_Digit;
   begin
      J := S'First;
      loop
         exit when J > S'Last;
         HD := Hex_Digit (S (J));
         exit when HD = Bad_Hex_Char;
         V := V * 16 + HD;
         J := J + 1;
      end loop;
      return V;
   end Get_Hval;

   procedure Addr_Str_In_C (Addr : Unsigned_32; S : in out String) is
      J    : Positive;
      A    : Unsigned_32 := Addr;
      Digs : constant String := ("0123456789abcdef");
   begin
      J := S'Last;
      loop
         if A = 0 then
            while J > S'First + 1 loop
               S (J) := '0';
               J := J - 1;
            end loop;
            S (S'First + 1) := 'x';
            S (S'First)     := '0';
            exit;
         end if;
         S (J) := Digs (Integer (A mod 16) + 1);
         A := A / 16;
         J := J - 1;
      end loop;
   end Addr_Str_In_C;

   Stop_Addr : Unsigned_32;

   task Timeout;
   --  This is a simple watchdog timer. At regular intervals it checks
   --  to see if it is supposed to be turned on. If so, it compares
   --  Clock time to a timeout time, and if the timeout time has been
   --  passed, it prints out an error message and force the program
   --  to exit with an error code.

   type Timeout_Err_Msg is access String;
   type Wd_Info is record
      T  : Time;
      M  : Timeout_Err_Msg;
      On : Boolean;
   end record;
   --  An object of this type contains the data used by Timeout task.
   --  It is shared between Timeout and other tasks via the protected
   --  type below.

   protected Wd_Timer is
      procedure Set_Timeout (D : Duration; E : String);
      procedure Clear_Timeout;
      function  Get_Timeout return Wd_Info;
   private
      Timer : Wd_Info := (Clock, null, False);
   end Wd_Timer;
   protected body Wd_Timer is
      procedure Set_Timeout (D : Duration; E : String) is
         procedure Free is new Ada.Unchecked_Deallocation
           (String, Timeout_Err_Msg);
      begin
         if Timer.M = null then
            if E'Length /= 0 then
               Timer.M := new String'(E);
            end if;
         elsif Timer.M.all /= E then
            Free (Timer.M);
            if E'Length /= 0 then
               Timer.M := new String'(E);
            end if;
         end if;

         Timer.T := Clock + D;
         Timer.On := True;
      end Set_Timeout;

      procedure Clear_Timeout is
      begin
         Timer.On := False;
      end Clear_Timeout;

      function  Get_Timeout return Wd_Info is
      begin
         return Timer;
      end Get_Timeout;
   end Wd_Timer;

   task body Timeout is
      T : Wd_Info;
   begin
      loop
         delay 2.0;
         T := Wd_Timer.Get_Timeout;
         if T.On and then Clock > T.T then
            Put_Line (T.M.all);
            OS_Exit (1);
         end if;
      end loop;
   end Timeout;

   procedure Command_Gdb (Command : String; T : Duration; Err_Msg : String) is
   begin
      Put_Gdb_Line (Command);
      if T = 0.0 then
         Wd_Timer.Clear_Timeout;
      else
         Wd_Timer.Set_Timeout (T, Err_Msg);
      end if;
   end Command_Gdb;

begin

   Process_Command_Line;

   Trace_Filename_Ptr := new String'(Executable_Name_Ptr.all & ".trace");
   Init_Base (Tbase);

   Process_Exec (Executable_Name_Ptr.all, CBA, Stop_Addr);

   Non_Blocking_Spawn
     (GDB_Process,
      "powerpc-elf-gdb",
      (1 => new String'("--quiet"),
       2 => new String'("--interpreter=mi"),
       3 => Executable_Name_Ptr));

   Initialize_MI (Get_Input_Fd  (GDB_Process),
                  Get_Output_Fd (GDB_Process),
                  True);

   Wd_Timer.Set_Timeout
     (Spawn_Timeout, "Timeout waiting for gdb prompt following spawn.");
   loop
      MI_Output := Get_MI_Output_Record;
      exit when MI_Output.Record_Type /= Console_Stream_Output;
   end loop;
   if MI_Output.Record_Type /= Prompt_Record then
      Put_Line (Standard_Error,
                "Unexpected output after spawn.");
      OS_Exit (Failure_Exit_Code);
   end if;

   Command_Gdb ("-target-select remote " & Remote_Target_Name_Ptr.all, 10.0,
               "Timeout following ""-target-select remote""");
   loop
      MI_Output := Get_MI_Output_Record;
      exit when MI_Output.Record_Type = Prompt_Record;
   end loop;

   Command_Gdb ("-target-download", 25.0,
               "Timeout waiting for target download");
   loop
      MI_Output := Get_MI_Output_Record;
      exit when MI_Output.Record_Type = Result_Record and then
        MI_Output.Result_Class = Done_Result;
   end loop;
   loop
      MI_Output := Get_MI_Output_Record;
      exit when MI_Output.Record_Type = Prompt_Record;
   end loop;

   Command_Gdb ("-var-create Program_Counter * $pc", 8.0,
               "Timeout creating Program_Counter variable.");
   loop
      MI_Output := Get_MI_Output_Record;
      exit when MI_Output.Record_Type = Result_Record and then
        MI_Output.Result_Class = Done_Result;
   end loop;
   loop
      MI_Output := Get_MI_Output_Record;
      exit when MI_Output.Record_Type = Prompt_Record;
   end loop;

   At_Start := True;
   loop
      Command_Gdb ("-var-evaluate-expression Program_Counter", 8.0,
                  "Timeout Evaluating Program_Counter");
      loop
         MI_Output := Get_MI_Output_Record;
         exit when MI_Output.Record_Type = Result_Record and then
           MI_Output.Result_Class = Done_Result;
      end loop;
      if Gdb_Output_Buffer (6 .. 15) /= ",value=""0x" then
         Put_Line (Standard_Error,
                   "Unexpected output after -var_evaluate_expression.");
         OS_Exit (1);
      end if;
      PC_Val := Get_Hval (Gdb_Output_Buffer (16 .. 24));
      if not At_Start then
         if PC_Val = Unsigned_32 (Pc_E) + 1 then
            Op := Trace_Op_Br1 or Trace_Op_Block;
         else
            Op := Trace_Op_Br0 or Trace_Op_Block;
         end if;
         Add_Entry (Tbase, Pc_B, Pc_E, Op);
      else
         At_Start := False;
      end if;

      exit when PC_Val = Stop_Addr;

      for CB_Index in CBA'Range loop
         First_Addr := CBA (CB_Index).Start;
         Last_Addr
           := First_Addr + Unsigned_32 (CBA (CB_Index).N_Insns - 1) * 4;
         exit when PC_Val >= First_Addr and then PC_Val <= Last_Addr;
         if CB_Index = CBA'Last then
            Put_Line (Standard_Error, "Couldn't find code block.");
            OS_Exit (1);
         end if;
      end loop;
      Pc_B := Pc_Type (PC_Val);
      Pc_E := Pc_Type (Last_Addr + 3);

      loop
         MI_Output := Get_MI_Output_Record;
         exit when MI_Output.Record_Type = Prompt_Record;
      end loop;

      if Last_Addr > PC_Val then
         Addr_Str_In_C (Last_Addr, C_Addr);
         Command_Gdb ("-break-insert -t -h *" & C_Addr, 8.0,
                     "Timeout inserting breakpoint.");
         loop
            MI_Output := Get_MI_Output_Record;
            exit when MI_Output.Record_Type = Result_Record and then
              MI_Output.Result_Class = Done_Result;
         end loop;
         loop
            MI_Output := Get_MI_Output_Record;
            exit when MI_Output.Record_Type = Prompt_Record;
         end loop;
         Command_Gdb ("-exec-continue", 30.0,
                     "Timeout after continue.");
         loop
            MI_Output := Get_MI_Output_Record;
            exit when MI_Output.Record_Type = Result_Record and then
              MI_Output.Result_Class = Running_Result;
         end loop;
         loop
            MI_Output := Get_MI_Output_Record;
            exit when MI_Output.Record_Type = Prompt_Record;
         end loop;
         loop
            MI_Output := Get_MI_Output_Record;
            exit when MI_Output.Record_Type = Exec_Async_Record and then
              Gdb_Output_Buffer (2 .. 8) = "stopped";
         end loop;
         loop
            MI_Output := Get_MI_Output_Record;
            exit when MI_Output.Record_Type = Prompt_Record;
         end loop;
      end if;

      Command_Gdb ("-exec-step-instruction", 10.0,
                  "Timeout following Step Instruction.");
      loop
         MI_Output := Get_MI_Output_Record;
         exit when MI_Output.Record_Type = Result_Record and then
           MI_Output.Result_Class = Running_Result;
      end loop;
      loop
         MI_Output := Get_MI_Output_Record;
         exit when MI_Output.Record_Type = Exec_Async_Record and then
           Gdb_Output_Buffer (2 .. 8) = "stopped";
      end loop;
      loop
         MI_Output := Get_MI_Output_Record;
         exit when MI_Output.Record_Type = Prompt_Record;
      end loop;
      Command_Gdb ("-var-update Program_Counter", 10.0,
                  "Timeout updating Program_Counter");
      loop
         MI_Output := Get_MI_Output_Record;
         exit when MI_Output.Record_Type = Prompt_Record;
      end loop;
   end loop;
   Wd_Timer.Clear_Timeout;

   Put_Gdb_Line ("-gdb-exit");
   Traces.Machine := 20;
   Create_Trace_File (Flat, Tfile);
   Append_Info (Tfile, Exec_File_Name, Executable_Name_Ptr.all);

   Write_Trace_File (Trace_Filename_Ptr.all, Tfile, Tbase);

end Gdbtrace;
