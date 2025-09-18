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

with GPR2.Path_Name;

with Coverage;
with Diagnostics;
with Outputs; use Outputs;
with Paths;   use Paths;
with SCOs;

package body Instrument.Common is

   function Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Part; Buffer_Name : String)
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
     (Instrumented_Unit : Compilation_Unit_Part; Buffer_Name : String)
      return String
   is
      Slug : constant String := Instrumented_Unit_Slug (Instrumented_Unit);
   begin
      return "xcov__buf_" & Buffer_Name & "__" & Slug;
   end Buffer_Symbol;

   -----------------------------
   -- Statement_Buffer_Symbol --
   -----------------------------

   function Statement_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Part) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "stmt");
   end Statement_Buffer_Symbol;

   ----------------------------
   -- Decision_Buffer_Symbol --
   ----------------------------

   function Decision_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Part) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "dc");
   end Decision_Buffer_Symbol;

   ------------------------
   -- MCDC_Buffer_Symbol --
   ------------------------

   function MCDC_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Part) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "mcdc");
   end MCDC_Buffer_Symbol;

   Dump_Pattern  : constant Pattern_Matcher :=
     Compile
       (Dump_Procedure_Symbol
          ((File_Based_Language, Null_Unbounded_String), Manual => True));
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
     (Project : GPR2.Project.View.Object) return String is
   begin
      if Project.Kind not in GPR2.With_Object_Dir_Kind then
         return "";
      end if;

      declare
         Obj_Dir  : constant GPR2.Path_Name.Object := Project.Object_Directory;
         Prj_Name : constant String :=
           Ada.Characters.Handling.To_Lower (String (Project.Name));
      begin
         return
           String
             (Obj_Dir.Compose (GPR2.Simple_Name (Prj_Name & "-gnatcov-instr"))
                .Value);
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
        or else SCOs.SCO_Unit_Table.Table (SCOs.SCO_Unit_Table.Last).File_Index
                /= SFI
      then
         Append_Unit (SFI);
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

   --------------
   -- New_File --
   --------------

   function New_File (Prj : Prj_Desc; Name : String) return String is
      Base_Filename   : constant String := Ada.Directories.Simple_Name (Name);
      Output_Filename : constant String := (+Prj.Output_Dir) / Base_Filename;
   begin
      return Output_Filename;
   end New_File;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File
     (Prj : Prj_Desc; File : in out Text_Files.File_Type; Name : String)
   is
      Filename : constant String := New_File (Prj, Name);
   begin
      File.Create (Filename);
   end Create_File;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename
     (Prj : Prj_Desc; CU_Name : Compilation_Unit_Part) return String is
   begin
      case CU_Name.Language_Kind is
         when Unit_Based_Language =>

            --  It is tempting here to fetch the answer from the best source of
            --  truth: the GPR library for the given project. Unfortunately
            --  this is not always possible: this function may be called in a
            --  subprocess for the parallel instrumentation, and no GPR project
            --  is loaded in this context.
            --
            --  Fortunately, this function is called only to create new sources
            --  (i.e. not instrumented sources, but extra helpers): all we have
            --  to do in principle is to follow the general purpose naming
            --  scheme directives (dot replacement and body/spec sufifxes), as
            --  project files are very unlikely to contain naming exceptions
            --  for extra helpers, which are sources that do not exist before
            --  instrumentation.

            declare
               --  Do the same assumption as GPR2: the only unit-based language
               --  is Ada.

               Language : constant Src_Supported_Language := Ada_Language;

               NS       : Naming_Scheme_Desc renames Prj.Naming_Scheme;
               Filename : Unbounded_String;
            begin
               for Id of CU_Name.Unit loop
                  if Filename /= "" then
                     Append (Filename, NS.Dot_Replacement);
                  end if;
                  case NS.Casing is
                     when Lowercase =>
                        Append (Filename, To_Lower (To_String (Id)));

                     when Uppercase =>
                        Append (Filename, To_Upper (To_String (Id)));

                     when Mixedcase =>
                        Append (Filename, To_String (Id));
                  end case;
               end loop;

               case CU_Name.Part is
                  when GPR2.S_Body | GPR2.S_Separate =>
                     Append (Filename, NS.Body_Suffix (Language));

                  when GPR2.S_Spec                   =>
                     Append (Filename, NS.Spec_Suffix (Language));
               end case;
               return +Filename;
            end;

         when File_Based_Language =>
            return +CU_Name.Filename;
      end case;
   end To_Filename;

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
