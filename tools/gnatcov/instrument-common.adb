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

with Ada.Containers; use Ada.Containers;
with Ada.Directories;

with GPR2;
with GPR2.Containers;
with GPR2.Build.Command_Line;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;

with Coverage;
with Diagnostics;
with JSON;    use JSON;
with Outputs; use Outputs;
with SCOs;
with Paths;   use Paths;
with Project; use Project;

package body Instrument.Common is

   function Buffer_Symbol
     (Instrumented_Unit : Unbounded_String; Buffer_Name : String)
      return String;
   --  Helper for Statement_Buffer_Symbol and Decision_Buffer_Symbol. Return
   --  the name of the symbol for the entity that contains the address of a
   --  coverage buffer for Instrumented_Unit.

   function Next_Bit (Last_Bit : in out Any_Bit_Id) return Any_Bit_Id;
   --  Convenience function to allocate a new coverage buffer bit: increment
   --  Last_Bit and return the new Last_Bit.

   -------------------
   -- Buffer_Symbol --
   -------------------

   function Buffer_Symbol
     (Instrumented_Unit : Unbounded_String; Buffer_Name : String) return String
   is
      Slug : constant String := Filename_Slug (+Instrumented_Unit);
   begin
      return "xcov__buf_" & Buffer_Name & "__" & Slug;
   end Buffer_Symbol;

   -----------------------------
   -- Statement_Buffer_Symbol --
   -----------------------------

   function Statement_Buffer_Symbol
     (Instrumented_Unit : Unbounded_String) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "stmt");
   end Statement_Buffer_Symbol;

   ----------------------------
   -- Decision_Buffer_Symbol --
   ----------------------------

   function Decision_Buffer_Symbol
     (Instrumented_Unit : Unbounded_String) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "dc");
   end Decision_Buffer_Symbol;

   ------------------------
   -- MCDC_Buffer_Symbol --
   ------------------------

   function MCDC_Buffer_Symbol
     (Instrumented_Unit : Unbounded_String) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "mcdc");
   end MCDC_Buffer_Symbol;

   Dump_Pattern  : constant Pattern_Matcher :=
     Compile (Dump_Procedure_Symbol ("", Manual => True));
   Reset_Pattern : constant Pattern_Matcher :=
     Compile (Reset_Procedure_Symbol (Ada_Identifier_Vectors.Empty_Vector));
   --  Precomputed patterns to be used as helpers for
   --  Is_Manual_Indication_Procedure_Symbol.

   -------------------------------------------
   -- Is_Manual_Indication_Procedure_Symbol --
   -------------------------------------------

   function Is_Manual_Indication_Procedure_Symbol
     (Symbol : String) return Boolean is
   begin
      return
        Match (Dump_Pattern, Symbol) or else Match (Reset_Pattern, Symbol);
   end Is_Manual_Indication_Procedure_Symbol;

   -----------------------
   -- Unit_Buffers_Name --
   -----------------------

   function Unit_Buffers_Name (Unit : Compilation_Unit) return String is
      Slug : constant String :=
        (case Unit.Language is
           when Unit_Based_Language =>
             Qualified_Name_Slug (To_Qualified_Name (+Unit.Unit_Name)),
           when File_Based_Language => Filename_Slug (+Unit.Unit_Name));
   begin
      return To_Symbol_Name (Sys_Buffers) & "_" & Slug & "_buffers";
   end Unit_Buffers_Name;

   ------------------------
   -- Project_Output_Dir --
   ------------------------

   function Project_Output_Dir
     (Project : GPR2.Project.View.Object; In_Extending : Boolean := True)
      return GNATCOLL.VFS.Virtual_File
   is
      Storage_Project : constant GPR2.Project.View.Object :=
        (if In_Extending then Most_Extending (Project) else Project);
      --  Actual project that will host instrumented sources: even when
      --  we instrument an extended project, the resulting instrumented
      --  sources must go to the ultimate extending project's object
      --  directory. This is similar to the object directory that hosts
      --  object files when GPRbuild processes a project that is
      --  extended.
   begin
      if Storage_Project.Kind not in GPR2.With_Object_Dir_Kind then
         return GNATCOLL.VFS.No_File;
      end if;

      declare
         Obj_Dir  : constant GPR2.Path_Name.Object :=
           Storage_Project.Object_Directory;
         Prj_Name : constant String :=
           Ada.Characters.Handling.To_Lower (String (Storage_Project.Name));
      begin
         return
           Obj_Dir.Compose (GPR2.Simple_Name (Prj_Name & "-gnatcov-instr"))
             .Virtual_File;
      end;
   end Project_Output_Dir;

   ------------------------
   -- Format_Fingerprint --
   ------------------------

   function Format_Fingerprint
     (Fingerprint : SC_Obligations.Fingerprint_Type; Opening, Closing : String)
      return String
   is
      Result : Unbounded_String;
      First  : Boolean := True;
   begin
      Append (Result, Opening);
      for Byte of Fingerprint loop
         if First then
            First := False;
         else
            Append (Result, ", ");
         end if;
         Append (Result, Strings.Img (Integer (Byte)));
      end loop;
      Append (Result, Closing);
      return +Result;
   end Format_Fingerprint;

   --------------
   -- Next_Bit --
   --------------

   function Next_Bit (Last_Bit : in out Any_Bit_Id) return Any_Bit_Id is
   begin
      Last_Bit := Last_Bit + 1;
      return Last_Bit;
   end Next_Bit;

   ----------------------------
   -- Allocate_Statement_Bit --
   ----------------------------

   function Allocate_Statement_Bit
     (Unit_Bits : in out Allocated_Bits; LL_S_SCO : Nat) return Any_Bit_Id
   is
      Bit : constant Any_Bit_Id := Next_Bit (Unit_Bits.Last_Statement_Bit);
   begin
      Unit_Bits.Statement_Bits.Append (Statement_Bit_Ids'(LL_S_SCO, Bit));
      return Bit;
   end Allocate_Statement_Bit;

   ----------------------------
   -- Allocate_Decision_Bits --
   ----------------------------

   function Allocate_Decision_Bits
     (Unit_Bits      : in out Allocated_Bits;
      Decision_Sloc  : Source_Location;
      LL_D_SCO       : Nat;
      State_Variable : Unbounded_String;
      Path_Count     : Natural) return Decision_Bit_Ids is
   begin
      return Result : Decision_Bit_Ids do
         Result.LL_D_SCO := LL_D_SCO;

         --  Allocate one bit per outcome

         for Outcome in Boolean loop
            Result.Outcome_Bits (Outcome) :=
              Next_Bit (Unit_Bits.Last_Outcome_Bit);
         end loop;

         --  If appropriate, allocate path bits for MC/DC: one bit per path in
         --  the decision.

         if (Coverage.MCDC_Coverage_Enabled
             or else Coverage.Assertion_Condition_Coverage_Enabled)
           and then State_Variable /= ""
           and then Path_Count > 0
           and then Path_Count < Get_Path_Count_Limit
         then
            Result.Path_Bits_Base := Unit_Bits.Last_Path_Bit + 1;
            Unit_Bits.Last_Path_Bit :=
              Unit_Bits.Last_Path_Bit + Bit_Id (Path_Count);
         else
            Result.Path_Bits_Base := No_Bit_Id;
         end if;

         --  Warn if the number of paths exceeded the limit

         if Path_Count > Get_Path_Count_Limit
           and then Coverage.MCDC_Coverage_Enabled
         then
            Diagnostics.Report
              (Decision_Sloc,
               "Number of distinct paths in the decision exceeds the limit"
               & " ("
               & Img (SC_Obligations.Get_Path_Count_Limit)
               & ")."
               & " MC/DC coverage for this decision will be left undetermined"
               & " in coverage reports. Use option --path-count-limit to"
               & " adjust the limit if the default value is too low.",
               Diagnostics.Warning);
         end if;

         Unit_Bits.Decision_Bits.Append (Result);
      end return;
   end Allocate_Decision_Bits;

   ----------------------
   -- Create_Unit_Bits --
   ----------------------

   function Create_Unit_Bits
     (Allocated_Bits : in out Allocated_Bits_Vectors.Vector;
      SFI            : Valid_Source_File_Index) return Positive is
   begin
      Allocated_Bits.Append (Common.Allocated_Bits'(SFI => SFI, others => <>));
      return Allocated_Bits.Last_Index;
   end Create_Unit_Bits;

   ------------------------
   -- Import_Annotations --
   ------------------------

   procedure Import_Annotations
     (UIC : in out Unit_Inst_Context; Created_Units : Created_Unit_Maps.Map)
   is
      ALI_Annotations : ALI_Annotation_Maps.Map;
   begin
      for Couple of UIC.Annotations loop
         ALI_Annotations.Insert
           (Key => Couple.Sloc, New_Item => Couple.Annotation);
      end loop;
      Set_Annotations (ALI_Annotations);
   end Import_Annotations;

   -------------------------------------
   -- Import_Non_Instrumented_LL_SCOs --
   -------------------------------------

   procedure Import_Non_Instrumented_LL_SCOs
     (UIC : Unit_Inst_Context; SCO_Map : LL_HL_SCO_Map) is
   begin
      for LL_SCO of UIC.Non_Instr_LL_SCOs loop
         declare
            Remapped_SCO : constant SCO_Id := SCO_Map (Nat (LL_SCO));
         begin
            case Kind (Remapped_SCO) is
               when Statement         =>
                  Set_Stmt_SCO_Non_Instr (Remapped_SCO);

               when Decision          =>
                  Set_Decision_SCO_Non_Instr (Remapped_SCO);

               when Condition         =>
                  Set_Decision_SCO_Non_Instr_For_MCDC
                    (Enclosing_Decision (Remapped_SCO));

               when Fun_Call_SCO_Kind =>
                  Set_Fun_Call_SCO_Non_Instr (Remapped_SCO);

               when Guarded_Expr      =>
                  Set_GExpr_SCO_Non_Instr (Remapped_SCO);

               when others            =>
                  null;
            end case;
         end;
      end loop;
   end Import_Non_Instrumented_LL_SCOs;

   -----------------
   -- Append_Unit --
   -----------------

   procedure Append_Unit (SFI : Source_File_Index) is
   begin
      SCOs.SCO_Unit_Table.Append
        ((File_Name  => new String'(Get_Full_Name (SFI)),
          File_Index => SFI,
          Dep_Num    => 1,
          From       => SCOs.SCO_Table.Last + 1,
          To         => SCOs.SCO_Table.Last));
   end Append_Unit;

   ----------------
   -- Append_SCO --
   ----------------

   procedure Append_SCO
     (C1, C2             : Character;
      From, To           : Local_Source_Location;
      SFI                : Source_File_Index;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
      use type SCOs.SCO_Unit_Index;
   begin
      --  If needed, append a new entry to the SCO_Unit_Table (if we
      --  are entering a new file when instrumenting a compilation unit,
      --  which can happen with #included files for instance).

      if SCOs.SCO_Unit_Table.Last = 0
        or else
          SCOs.SCO_Unit_Table.Table (SCOs.SCO_Unit_Table.Last).File_Index
          /= SFI
      then
         Append_Unit (SFI);
         if SCOs_Trace.Is_Active then
            SCOs_Trace.Trace ("SFI=" & Img (Integer (SFI)));
         end if;
      end if;

      if SCOs_Trace.Is_Active then
         SCOs_Trace.Trace
           ("Add:"
            & " C1="
            & C1
            & ", C2="
            & C2
            & " at "
            & Image (Local_Source_Location_Range'(From, To))
            & (if Last then ", last" else "")
            & (if Pragma_Aspect_Name /= Namet.No_Name
               then
                 ", pragma_aspect_name="
                 & Namet.Get_Name_String (Pragma_Aspect_Name)
               else ""));
      end if;

      --  Append a new SCO to the low level SCO table

      SCOs.SCO_Table.Append
        ((From               =>
            (Logical_Line_Number (From.Line), Column_Number (From.Column)),
          To                 =>
            (Logical_Line_Number (To.Line), Column_Number (To.Column)),
          C1                 => C1,
          C2                 => C2,
          Last               => Last,
          Pragma_Sloc        => Types.No_Location,
          Pragma_Aspect_Name => Pragma_Aspect_Name));

      --  Then, extend the SCO_Unit_Table with the newly created SCO

      SCOs.SCO_Unit_Table.Table (SCOs.SCO_Unit_Table.Last).To :=
        SCOs.SCO_Table.Last;
   end Append_SCO;

   ------------------
   -- Remap_Blocks --
   ------------------

   procedure Remap_Blocks
     (Blocks : in out SCO_Id_Vector_Vector; SCO_Map : LL_HL_SCO_Map)
   is
      Result : SCO_Id_Vector_Vector;
   begin
      for Block of Blocks loop
         declare
            Result_Block : SCO_Id_Vectors.Vector;
         begin
            for LL_SCO of Block loop
               Result_Block.Append (SCO_Map (Nat (LL_SCO)));
            end loop;
            Result.Append (Result_Block);
         end;
      end loop;
      Blocks := Result;
   end Remap_Blocks;

   --------------------------
   -- Instrumentation_File --
   --------------------------

   function Instrumentation_File
     (Prj : Prj_Desc; File : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.VFS.Virtual_File
   is
      --  Determine the directory in which to create the instrumented source
      --  file.
      --
      --  By default, instrumented source files must go to Prj.Output_Dir, but
      --  we must first take into account special cases from
      --  Prj.Special_Output_Dirs.

      Special_Output_Dir : constant String_Maps.Cursor :=
        Prj.Special_Output_Dirs.Find (+File.Display_Base_Name);
      Output_Dir         : constant Virtual_File :=
        (if String_Maps.Has_Element (Special_Output_Dir)
         then Create (+(+String_Maps.Element (Special_Output_Dir)))
         else Prj.Output_Dir);
   begin
      return GNATCOLL.VFS.Join (Output_Dir, GNATCOLL.VFS.Base_Name (File));
   end Instrumentation_File;

   --------------------------
   -- Is_Instrumented_File --
   --------------------------

   function Is_Instrumented_File
     (Prj : Prj_Desc; File : GNATCOLL.VFS.Virtual_File) return Boolean is
   begin
      return GNATCOLL.VFS.Dir (File) = Prj.Output_Dir;
   end Is_Instrumented_File;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File
     (Prj : in out Prj_Desc; File : in out Text_Files.File_Type; Name : String)
   is
      File_VF : constant Virtual_File :=
        Instrumentation_File (Prj, Create (+Name));
   begin
      File.Create (File_VF.Display_Full_Name);
      Prj.Instr_Artifacts.Include (File_VF);
   end Create_File;

   -----------------
   -- Add_Options --
   -----------------

   procedure Add_Options
     (Args          : in out String_Vectors.Vector;
      Options       : Analysis_Options;
      Pass_Builtins : Boolean := True;
      Preprocessed  : Boolean := False)
   is

      procedure Add_Macro_Switches (Macros : Macro_Set);
      --  Add the given macro switches to Args

      ------------------------
      -- Add_Macro_Switches --
      ------------------------

      procedure Add_Macro_Switches (Macros : Macro_Set) is
      begin
         for M of Macros loop
            if M.Define then
               Args.Append ("-D" & M.Name & M.Args & "=" & M.Value);
            else
               Args.Append ("-U" & M.Name);
            end if;
         end loop;
      end Add_Macro_Switches;

   begin
      for Dir of Options.PP_Search_Path loop
         Args.Append (+"-I");
         Args.Append (Dir);
      end loop;

      --  If the file was already preprocessed, do not pass macro command line
      --  switches. Since preprocessed code can contain names of defined
      --  macros, passing macro arguments for the parsing step could trigger
      --  other expansions, and thus feed the parser with unexpected code.

      if not Preprocessed then

         --  Add builtin macros before macros from command line switches, as
         --  the latter should have precedence over builtins and thus must
         --  come last in Args.

         if Pass_Builtins then
            Add_Macro_Switches (Options.Builtin_Macros);
         end if;
         Add_Macro_Switches (Options.PP_Macros);
         for Included of Options.Include_Files loop
            Args.Append (+"-include");
            Args.Append (Included);
         end loop;
      end if;

      --  Add other compiler switches as they may also influence both the
      --  configuration of the preprocessor, and the parsing of the file. A
      --  non-exhaustive list includes undefining macros through -U switches,
      --  using -std to change the C++ standard in use, -fno-rtti to prevent
      --  inclusion of runtime type information etc.

      Args.Append (Options.Compiler_Switches);

      --  Add -m32 if needed

      if Options.Clang_Needs_M32 then
         Args.Append (+"-m32");
      end if;
   end Add_Options;

   ----------------------------
   -- Parse_Macro_Definition --
   ----------------------------

   procedure Parse_Macro_Definition
     (Str : String; Parsed_Def : out Macro_Definition; Success : out Boolean)
   is
      Matches : Match_Array (0 .. 3);
   begin
      Match (Macro_Def_Regexp, Str, Matches);
      if Matches (0) = No_Match then
         Success := False;
      else
         Success := True;
         Parsed_Def.Name := +Str (Matches (1).First .. Matches (1).Last);
         if Matches (2) /= No_Match then
            Parsed_Def.Args := +Str (Matches (2).First .. Matches (2).Last);
         end if;
         Parsed_Def.Value := +Str (Matches (3).First .. Matches (3).Last);
      end if;
   end Parse_Macro_Definition;

   ------------------------------------
   -- Parse_Cmdline_Macro_Definition --
   ------------------------------------

   procedure Parse_Cmdline_Macro_Definition
     (Str : String; Parsed_Def : out Macro_Definition; Success : out Boolean)
   is
      Matches : Match_Array (0 .. 4);
   begin
      Match (Macro_Cmdline_Regexp, Str, Matches);
      if Matches (0) = No_Match then
         Success := False;
      else
         Success := True;
         Parsed_Def.Name := +Str (Matches (1).First .. Matches (1).Last);
         if Matches (2) /= No_Match then
            Parsed_Def.Args := +Str (Matches (2).First .. Matches (2).Last);
         end if;

         --  Command line macros can have part of their value before the
         --  assignment, e.g. "-DA(b)b" is equivalent to "#define A(b) b 1".

         if Matches (3) /= No_Match then
            Append
              (Parsed_Def.Value,
               Str (Matches (3).First .. Matches (3).Last) & " ");
         end if;

         --  If no value is given, then it is implicitly 1

         if Matches (4) /= No_Match then
            Append
              (Parsed_Def.Value, Str (Matches (4).First .. Matches (4).Last));
         else
            Append (Parsed_Def.Value, " 1");
         end if;
      end if;
   end Parse_Cmdline_Macro_Definition;

   ----------------------
   -- Import_From_Args --
   ----------------------

   procedure Import_From_Args
     (Self : in out Analysis_Options; Args : String_Vectors.Vector)
   is
      I    : Natural := Args.First_Index;
      Last : constant Integer := Args.Last_Index;

      function Read_With_Argument
        (Arg : String; Option_Name : Character; Value : out Unbounded_String)
         return Boolean;
      --  Assuming that Arg starts with "-X" where X is Option_Name, try to
      --  fetch the value for this option. If we managed to get one, return
      --  True and set Value to it. Return False otherwise.

      ------------------------
      -- Read_With_Argument --
      ------------------------

      function Read_With_Argument
        (Arg : String; Option_Name : Character; Value : out Unbounded_String)
         return Boolean
      is
         Prefix : constant String := "-" & Option_Name;
      begin
         if Arg = Prefix then

            --  Option and value are two separate arguments (-O VALUE)

            I := I + 1;
            if I <= Last then
               Value := Args (I);
               return True;
            end if;

         elsif Has_Prefix (Arg, Prefix) then

            --  Option and value are combined in a single argument (-OVALUE)

            Value := +Arg (Arg'First + Prefix'Length .. Arg'Last);
            return True;
         end if;

         return False;
      end Read_With_Argument;

      --  Start of processing for Import_From_Args

   begin
      Self.Raw_Switches.Append_Vector (Args);

      while I <= Last loop
         declare
            A     : constant String := +Args (I);
            Value : Unbounded_String;
         begin

            --  Process arguments we manage to handle, silently discard unknown
            --  ones.
            --
            --  TODO??? In order to avoid surprising situations for users (for
            --  instance typos in command line arguments), maybe we should emit
            --  a warning for unknown arguments. However, given that this
            --  procedure is called at least once per instrumented source file,
            --  we would need to avoid emitting duplicate warnings.

            if Read_With_Argument (A, 'I', Value) then
               Self.PP_Search_Path.Append (Value);

            elsif Read_With_Argument (A, 'D', Value) then
               declare
                  Macro_Def : Macro_Definition (Define => True);
                  Success   : Boolean;
               begin
                  Parse_Cmdline_Macro_Definition
                    (Str        => +Value,
                     Parsed_Def => Macro_Def,
                     Success    => Success);
                  if Success then
                     Self.PP_Macros.Include (Macro_Def);
                  else
                     Warn
                       ("Failed to parse command-line macro definition: "
                        & (+Value));
                  end if;
               end;

            elsif Read_With_Argument (A, 'U', Value) then
               Self.PP_Macros.Include ((Define => False, Name => Value));
            --  Account for all the switches that can influence the file
            --  preprocessing.

            elsif Has_Prefix (A, "-std")
              or else Has_Prefix (A, "-fno-rtti")
              or else Has_Prefix (A, "-fno-exceptions")

              --  All the warning switches can influence the preprocessing
              --  through the use of the __has_warning macro, e.g.
              --  #if __has_warning("-Wimplicit-fallthrough")

              or else Has_Prefix (A, "-W")
            then
               Self.Compiler_Switches.Append (+A);

            elsif A in "-include" | "--include" then
               Self.Include_Files.Append (Args (I + 1));
               I := I + 1;

            elsif Has_Prefix (A, "--include=") then
               Self.Include_Files.Append (+A (11 .. A'Last));

            elsif Has_Prefix (A, "-include") then
               Self.Include_Files.Append (+A (9 .. A'Last));

            elsif A = "-m32" then
               Self.Compiler_Switches.Append (+A);

            end if;

            I := I + 1;
         end;
      end loop;
   end Import_From_Args;

   ------------------------
   -- Is_Disabled_Region --
   ------------------------

   function Is_Disabled_Region
     (UIC : Unit_Inst_Context; Sloc : Source_Location) return Boolean is
   begin
      for Disabled_Region of UIC.Disable_Cov_Regions loop
         if In_Range (Sloc, Disabled_Region) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Disabled_Region;

   -------------------------------
   -- Populate_Ext_Disabled_Cov --
   -------------------------------

   procedure Populate_Ext_Disabled_Cov
     (UIC    : in out Unit_Inst_Context;
      Annots : Instr_Annotation_Map;
      SFI    : Source_File_Index)
   is
      use Instr_Annotation_Maps;
      Cur : Instr_Annotation_Maps.Cursor := Annots.First;
   begin
      while Has_Element (Cur) loop

         declare
            Off_Annot : constant Instr_Annotation := Element (Cur);
            Off_Sloc  : constant Local_Source_Location := Key (Cur);
            On_Annot  : Instr_Annotation;
         begin

            --  First comes the Cov_Off annotation

            pragma Assert (Off_Annot.Kind = Cov_Off);

            UIC.Annotations.Append
              (Annotation_Couple'
                 ((Source_File => SFI, L => Off_Sloc),
                  (Kind    => Cov_Off,
                   Message =>
                     (if Length (Off_Annot.Justification) /= 0
                      then new String'(+Off_Annot.Justification)
                      else null),
                   others  => <>)));

            --  Then the Cov_On annotation

            Next (Cur);
            pragma Assert (Has_Element (Cur));
            On_Annot := Element (Cur);
            pragma Assert (On_Annot.Kind = Cov_On);

            UIC.Annotations.Append
              (Annotation_Couple'
                 ((Source_File => SFI, L => Key (Cur)),
                  (Kind => Cov_On, others => <>)));

            --  And the region annotation

            UIC.Disable_Cov_Regions.Append
              (Source_Location_Range'
                 (SFI, (First_Sloc => Off_Sloc, Last_Sloc => Key (Cur))));
         end;
         Next (Cur);
      end loop;
   end Populate_Ext_Disabled_Cov;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   function Emit_Buffers_List_Unit
     (Self           : Language_Instrumenter;
      Buffer_Symbols : String_Sets.Set;
      Prj            : in out Prj_Desc) return Compilation_Unit
   is
      pragma Unreferenced (Prj);
   begin
      return No_Compilation_Unit;
   end Emit_Buffers_List_Unit;

   --------------
   -- Has_Main --
   --------------

   function Has_Main
     (Self     : in out Language_Instrumenter;
      Filename : GNATCOLL.VFS.Virtual_File;
      Prj      : in out Prj_Desc) return Boolean
   is
      pragma Unreferenced (Prj);
   begin
      return False;
   end Has_Main;

   --------------------------------
   -- Replace_Manual_Indications --
   --------------------------------

   procedure Replace_Manual_Indications
     (Self                 : in out Language_Instrumenter;
      Prj                  : in out Prj_Desc;
      Source               : Virtual_File;
      Has_Dump_Indication  : out Boolean;
      Has_Reset_Indication : out Boolean) is
   begin
      raise Program_Error;
   end Replace_Manual_Indications;

   ---------------------
   -- Dependency_File --
   ---------------------

   function Dependency_File
     (Prj : Prj_Desc; Filename : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return
        Instrumentation_File
          (Prj, Create (+(Filename.Display_Base_Name & ".d")));
   end Dependency_File;

   -----------------------------
   -- Instrumented_Files_File --
   -----------------------------

   function Instrumented_Files_File
     (Prj : Prj_Desc; Filename : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return
        Instrumentation_File
          (Prj, Create (+(Filename.Display_Base_Name & ".instr_files")));
   end Instrumented_Files_File;

   -------------------------------------
   -- Files_Instrumentation_Info_File --
   -------------------------------------

   function Files_Instrumentation_Info_File
     (Prj : Prj_Desc; Unit_Name : String) return GNATCOLL.VFS.Virtual_File is
   begin
      return
        Instrumentation_File
          (Prj,
           GNATCOLL.VFS.Create
             (+(Ada.Directories.Simple_Name (Unit_Name) & ".json")));
   end Files_Instrumentation_Info_File;

   ------------------------
   -- Load_Naming_Scheme --
   ------------------------

   function Load_Naming_Scheme
     (Prj : GPR2.Project.View.Object) return Naming_Scheme_Desc
   is
      --  Load the naming scheme from project attributes

      package R renames GPR2.Project.Registry.Attribute;

      NS : Naming_Scheme_Desc;
   begin
      for Lang in Some_Language loop
         if Builtin_Support (Lang)
           and then Prj.Language_Ids.Contains (To_Language_Id (Lang))
         then
            declare
               Body_Suffix : constant String :=
                 Project.Source_Suffix (Lang, GPR2.S_Body, Prj);
               Spec_Suffix : constant String :=
                 Project.Source_Suffix (Lang, GPR2.S_Spec, Prj);
            begin
               NS.Body_Suffix (Lang) := +Body_Suffix;
               NS.Spec_Suffix (Lang) := +Spec_Suffix;
            end;
         end if;
      end loop;
      NS.Dot_Replacement :=
        +Prj.Attribute (Name => R.Naming.Dot_Replacement).Value.Text;
      NS.Casing :=
        Casing_From_String
          (Prj.Attribute (Name => R.Naming.Casing).Value.Text,
           "project " & String (Prj.Name));
      return NS;
   end Load_Naming_Scheme;

   ------------------
   -- SID_Filename --
   ------------------

   function SID_Filename
     (Main_Part_Src : GPR2.Build.Source.Object; In_Library_Dir : Boolean)
      return String
   is
      --  Determine in which project we will put this SID file, and the
      --  basename for the SID file to create. Mimic how GNAT creates ALI
      --  files: use the project of the main source of the library unit, start
      --  from the basename of that source file, replace the last extension
      --  with ".sid". Also make sure to use the most extending project in the
      --  hierarchy, which is where GPRbuild puts ALI/object files.

      SID_Basename : Unbounded_String;

      Prj              : GPR2.Project.View.Object renames
        Main_Part_Src.Owning_View;
      Output_Directory : constant GPR2.Path_Name.Object :=
        (if In_Library_Dir and then Prj.Is_Library
         then Prj.Library_Ali_Directory
         else Prj.Object_Directory);

   begin
      case Language_Kind (To_Language (Main_Part_Src.Language)) is
         when Unit_Based_Language =>
            SID_Basename :=
              +(String (Main_Part_Src.Path_Name.Base_Name) & ".sid");

         when File_Based_Language =>

            --  TODO (eng/toolchain/gnat#603)??? Ada.Directories.Simple_Name
            --  fails in edge cases. Use GNATCOLL.VFS instead for more reliable
            --  results.

            SID_Basename :=
              +Main_Part_Src.Path_Name.Virtual_File.Display_Base_Name & ".sid";
      end case;

      return String (Output_Directory.Value) / (+SID_Basename);
   end SID_Filename;

   ------------------------------
   -- Compilation_Unit_Options --
   ------------------------------

   function Compilation_Unit_Options
     (IC   : Inst_Context;
      Prj  : Prj_Desc;
      Lang : Src_Supported_Language;
      Src  : GPR2.Build.Source.Object) return Command_Line_Args
   is
      use GPR2.Build.Command_Line;
      Result : Command_Line_Args;
   begin
      --  Depending on the language, pass the right set of options

      if Lang = Switches.Ada_Language then
         Result.Append
           (Create ("--ada-default-charset=" & (+IC.Ada_Default_Charset)));
         Result.Append (Create ("--gnatem", Mode => Ignore));
         Result.Append (Create (Arg => +IC.Mapping_File, Mode => Ignore));
         Result.Append (Create ("--config-pragmas-mapping", Mode => Ignore));
         Result.Append
           (Create (Arg => +IC.Config_Pragmas_Mapping, Mode => Ignore));
         Result.Append (Create ("--ada-preprocessor-data", Mode => Ignore));
         Result.Append (Create (+IC.Ada_Preprocessor_Data_File));
      end if;

      --  Pass the list of sources of interest, to e.g. skip the
      --  instrumentation of the spec / body / subunit for an Ada unit if
      --  it was requested through a --ignored-source-files switch.

      Result.Append (Create ("--files", Mode => Ignore));
      Result.Append
        (Create
           (Arg  => "@" & (+IC.Sources_Of_Interest_Response_File),
            Mode => Ignore));

      --  Pass the right language

      Result.Append (Create ("--lang=" & Image (Lang)));

      --  Pass the project description options

      Result.Append (Command_Line_Args'(Unparse (Prj, Src, Lang)));
      return Result;
   end Compilation_Unit_Options;

   -----------------------
   -- Load_From_Project --
   -----------------------

   function Load_From_Project (Prj : GPR2.Project.View.Object) return Prj_Desc
   is
      package R renames GPR2.Project.Registry.Attribute;

      Languages : constant GPR2.Containers.Language_Set := Prj.Language_Ids;
      Result    : Prj_Desc;
   begin
      Result.Prj_Name := To_Qualified_Name (String (Prj.Name));

      --  Load the naming scheme from project attributes

      declare
         NS : Naming_Scheme_Desc renames Result.Naming_Scheme;
      begin
         for Lang in Some_Language loop
            if Builtin_Support (Lang)
              and then Languages.Contains (To_Language_Id (Lang))
            then
               declare
                  Body_Suffix : constant String :=
                    Project.Source_Suffix (Lang, GPR2.S_Body, Prj);
                  Spec_Suffix : constant String :=
                    Project.Source_Suffix (Lang, GPR2.S_Spec, Prj);
               begin
                  NS.Body_Suffix (Lang) := +Body_Suffix;
                  NS.Spec_Suffix (Lang) := +Spec_Suffix;
               end;
            end if;
         end loop;
         NS.Dot_Replacement :=
           +Prj.Attribute (Name => R.Naming.Dot_Replacement).Value.Text;
         NS.Casing :=
           Casing_From_String
             (Prj.Attribute (Name => R.Naming.Casing).Value.Text,
              "project " & String (Prj.Name));
      end;

      --  Register the source directories of the project tree

      for Lang in C_Family_Language loop
         for Dir_Path of Prj.Include_Path (To_Language_Id (Lang)) loop
            Result.Search_Paths (Lang).Append
              (+("-I" & String (Dir_Path.Name)));
         end loop;
      end loop;

      --  Load the set of compiler switches for languages requiring it

      for Lang in C_Family_Language loop
         if Languages.Contains (To_Language_Id (Lang)) then
            declare
               Lang_Index : constant GPR2.Project.Attribute_Index.Object :=
                 GPR2.Project.Attribute_Index.Create (Image (Lang));

               package R renames GPR2.Project.Registry.Attribute;

               Options         : Analysis_Options;
               Switches        : GPR2.Project.Attribute.Object;
               Compiler_Driver : constant GPR2.Project.Attribute.Object :=
                 Prj.Attribute
                   (Name => R.Compiler.Driver, Index => Lang_Index);

            begin
               --  Get the compiler switches from the project file. When
               --  registering a compilation unit for instrumentation, we also
               --  fill the compilation unit specific switches that will
               --  override the project defaults, if there are any (see
               --  Add_Instrumented_Unit).
               --
               --  Language specific switches can be specified through the
               --  Compiler.Switches or the Compiler.Default_Switches
               --  attribute, the former being prioritized over the latter.

               Switches :=
                 Prj.Attribute
                   (Name => R.Compiler.Switches, Index => Lang_Index);

               if not Switches.Is_Defined then
                  Switches :=
                    Prj.Attribute
                      (Name  => R.Compiler.Default_Switches,
                       Index => Lang_Index);
               end if;

               --  If we manage to find appropriate switches, convert them to a
               --  string vector import the switches.

               if Switches.Is_Defined then
                  declare
                     Args : String_Vectors.Vector;
                  begin
                     for S of Switches.Values loop
                        Args.Append (+S.Text);
                     end loop;
                     Import_From_Args (Options, Args);
                  end;
               end if;

               Add_Options
                 (Result.Compiler_Options (Lang),
                  Options,
                  Pass_Builtins => False);

               --  If we could not find a C compiler, stay silent at this
               --  point: the C instrumenter will complain if needed.

               Result.Compiler_Driver (Lang) :=
                 +(if Compiler_Driver.Is_Defined
                   then Compiler_Driver.Value.Text
                   else "");
            end;
         end if;
      end loop;

      return Result;
   end Load_From_Project;

   --------------------------------
   -- Get_Or_Create_Project_Info --
   --------------------------------

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context; Project : GPR2.Project.View.Object)
      return Project_Info_Access
   is
      use Project_Info_Maps;

      --  Look for an existing Project_Info record corresponding to Project

      Project_Name : constant Unbounded_String := +String (Project.Name);
      Position     : constant Cursor :=
        Context.Project_Info_Map.Find (Project_Name);
   begin
      if Has_Element (Position) then
         return Element (Position);

      else
         --  The requested Project_Info record does not exist yet. Create it,
         --  register it and return it.

         declare
            Result : constant Project_Info_Access :=
              new Project_Info'
                (Project          => Project,
                 Externally_Built => Project.Is_Externally_Built,
                 Desc             => Load_From_Project (Project));
         begin
            Result.Desc.Output_Dir := Project_Output_Dir (Project);
            if Project.Is_Library then
               Result.Desc.Lib_Dir := Project.Library_Directory.Virtual_File;
            end if;
            Context.Project_Info_Map.Insert (Project_Name, Result);
            return Result;
         end;
      end if;
   end Get_Or_Create_Project_Info;

   -------------------------------
   -- Instrumentation_Artifacts --
   -------------------------------

   function Instrumentation_Artifacts
     (Main_Part_Src : GPR2.Build.Source.Object; Prj : Prj_Desc)
      return File_Sets.Set
   is
      Result : File_Sets.Set;

      Unit_Instr_Info_File : constant Virtual_File :=
        Files_Instrumentation_Info_File
          (Prj, Project.Get_Unit_Name (Main_Part_Src));
   begin
      Result.Insert (Unit_Instr_Info_File);
      if not Unit_Instr_Info_File.Is_Regular_File then
         return File_Sets.Empty_Set;
      end if;

      --  Source instrumentation artifacts generated by the source
      --  instrumentation command are to be found in the JSON.
      --
      --  Other instrumentation artifacts are stored in the project
      --  description.

      declare
         Unit_Instr_Info_JSON : constant JSON_Value :=
           JSON.Read_File (Unit_Instr_Info_File.Display_Full_Name);
         Artifacts            : constant JSON_Array :=
           Unit_Instr_Info_JSON.Get ("artifacts");
      begin
         for Art of Artifacts loop
            Result.Insert (Join (Prj.Output_Dir, +Art.Get));
         end loop;
      end;
      for Art of Prj.Instr_Artifacts loop
         Result.Insert (Art);
      end loop;
      return Result;
   end Instrumentation_Artifacts;

begin
   Sys_Prefix.Append (To_Unbounded_String ("GCVRT"));

   Sys_Buffers.Append (To_Unbounded_String ("GNATcov_RTS"));
   Sys_Buffers.Append (To_Unbounded_String ("Buffers"));

   Statement_Buffer_Name.Append (To_Unbounded_String ("Statement_Buffer"));
   Decision_Buffer_Name.Append (To_Unbounded_String ("Decision_Buffer"));
   MCDC_Buffer_Name.Append (To_Unbounded_String ("MCDC_Buffer"));

   Witness_Dummy_Type_Name := Sys_Buffers;
   Witness_Dummy_Type_Name.Append (To_Unbounded_String ("Witness_Dummy_Type"));

end Instrument.Common;
