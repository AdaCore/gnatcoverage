------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Text_IO;

with Interfaces; use Interfaces;

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GPR2.Build.Source;
with GPR2.Project.View;

with TOML;

with Stable_Sloc;            use Stable_Sloc;
with Stable_Sloc.TOML_Utils; use Stable_Sloc.TOML_Utils;

with Coverage_Options;  use Coverage_Options;
with Command_Line;      use Command_Line;
with Files_Table;       use Files_Table;
with Hex_Images;        use Hex_Images;
with Instrument;        use Instrument;
with Instrument.Common; use Instrument.Common;
with Outputs;           use Outputs;
with Paths;             use Paths;
with Project;           use Project;
with Switches;          use Switches;

package body SS_Annotations is
   use type Unbounded_String;

   Xcov_Namespace : constant Ada_Qualified_Name := To_Qualified_Name ("xcov");
   --  Common prefix for all stable_sloc purposes that gnatcov recognizes

   Exemption_Namespace : constant Ada_Qualified_Name :=
     Xcov_Namespace & To_Qualified_Name ("exempt");
   --  Common prefix for all exemption related annotations

   Exempt_On_Purpose     : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("on");
   Exempt_Off_Purpose    : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("off");
   Exempt_Region_Purpose : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("region");

   Buffers_Namespace : constant Ada_Qualified_Name :=
     Xcov_Namespace & To_Qualified_Name ("buffers");
   --  Common prefix for all buffer manipulation related annotations

   Buffers_Dump_Purpose  : constant Ada_Qualified_Name :=
     Buffers_Namespace & To_Qualified_Name ("dump");
   Buffers_Reset_Purpose : constant Ada_Qualified_Name :=
     Buffers_Namespace & To_Qualified_Name ("reset");

   Coverage_Namespace : constant Ada_Qualified_Name :=
     Xcov_Namespace & To_Qualified_Name ("cov");
   --  Common prefix to instruct gnatcov to enable or disable coverage on a
   --  given location

   Cov_Off_Purpose : constant Ada_Qualified_Name :=
     Coverage_Namespace & To_Qualified_Name ("off");
   Cov_On_Purpose  : constant Ada_Qualified_Name :=
     Coverage_Namespace & To_Qualified_Name ("on");

   Valid_Annotation_DB : Stable_Sloc.Entry_DB;
   --  Copy of the Ext_Annotation_DB, but filtering out the entries with
   --  invalid annotations. This DB should be used when matching to avoid
   --  searching for annotations which we won't be able to import anyways.
   --
   --  We still need to keep the Ext_Annotation_DB at hand in case we are
   --  writing back the annotations to file, in order not to delete the user's
   --  data.

   function Annotation_Kind
     (Annot : TOML.TOML_Value) return Any_Annotation_Kind;
   --  Convert the purpose string in Match.Annotation into one of the
   --  recognized annotation kinds, based on the purpose strings defined above.
   --
   --  Return Unknown if there is no "purpose" field in the annotation, or if
   --  it is empty or unknown.

   function Annotation_Kind (Str : String) return Any_Annotation_Kind;
   --  Try to interpret Str as an annotation kind, return Unknown if Str does
   --  not match with any valid annotation kind.

   function Purpose (Kind : Any_Annotation_Kind) return Ada_Qualified_Name;
   --  Return the qualified name to be used as purpose for the given annotation
   --  kind.

   procedure Report_Failed (Match : Match_Result)
   with Pre => not Match.Success;
   --  Report the diagnostics for Match. Consider all failed matches as stale
   --  annotations that need to be re-generated.

   function "+"
     (Sloc : TOML.Source_Location) return Slocs.Local_Source_Location
   is (Line => Sloc.Line, Column => Sloc.Column)
   with Unreferenced;

   generic
      type Expected_Annot_Kind is (<>);
      with
        function Kind
          (Match_Res : TOML.TOML_Value) return Expected_Annot_Kind'Base;
      with
        function Convert
          (Kind      : Expected_Annot_Kind;
           Match_Res : Match_Result;
           Success   : out Boolean) return Instrument.Common.Instr_Annotation;
      Purpose_Prefix : String;
   function Generic_Get_Annotations
     (Filename : String) return Instrument.Common.Instr_Annotation_Map;
   --  Match the annotations on File for the entries for which the purpose
   --  starts with Purpose_Prefix. Warn and discard the match results that
   --  either failed, or for which the purpose does not lie in
   --  Expected_Annot_Kind after conversion through Kind.
   --  Otherwise, each valid annotation is converted and associated to its
   --  starting location in the returned map.

   subtype Buffer_Annotation_Kind is
     Src_Annotation_Kind range Dump_Buffers .. Reset_Buffers;

   function Convert_Buffer_Annotation
     (Kind      : Buffer_Annotation_Kind;
      Match_Res : Match_Result;
      Success   : out Boolean) return Instrument.Common.Instr_Annotation;
   --  Convert Match_Res to a buffer annotation, assuming the annotation in
   --  Match Res is of kind Kind.
   --
   --  Print a warning and set Success to False if there are errors
   --  interpreting the TOML annotation.

   function Get_Or_Error
     (Str : String; Sw : String) return Slocs.Local_Source_Location;
   --  Parse a local source location from Str, or exit with a fatal error if
   --  parsing fails. Sw represents the switch name for which Str represents
   --  the value.
   --
   --  An empty Str value is interpreted as a missing switch value.
   --
   --  This function will reject source locations with a 0 for the line or the
   --  column.

   function Guess_Lang (File : Virtual_File) return Any_Language;
   --  Try to guess the language of file based on its extension.

   procedure Validate_Annotation
     (Identifier : Unbounded_String; Entr : Entry_View);
   --  Helper for the above procedure, validate a single entry, if it is ok
   --  then copy it in Valid_Annotation_DB, otherwise emit a warning.

   procedure Check_New_Entry_Diags (Diags : Load_Diagnostic_Arr);
   --  Check if Diags is empty, and if not error out while displaying the
   --  diagnostics.

   ---------------------
   -- Annotation_Kind --
   ---------------------

   function Annotation_Kind
     (Annot : TOML.TOML_Value) return Any_Annotation_Kind
   is
      Purpose : constant Ada_Qualified_Name :=
        To_Qualified_Name (+Get_Or_Null (Annot, "purpose"));

   begin
      if Purpose.Is_Empty
        or else Purpose.Element (1) /= Xcov_Namespace.Last_Element
        or else Purpose.Last_Index < 2
      then
         return Unknown;
      end if;

      if Purpose.Element (2) = Exemption_Namespace.Last_Element then

         --  Convert exemption annotation kind

         if Purpose.Last_Index < 3 then
            return Unknown;
         elsif Purpose.Element (3) = Exempt_On_Purpose.Last_Element then
            return Exempt_On;
         elsif Purpose.Element (3) = Exempt_Off_Purpose.Last_Element then
            return Exempt_Off;
         elsif Purpose.Element (3) = Exempt_Region_Purpose.Last_Element then
            return Exempt_Region;
         end if;
      elsif Purpose.Element (2) = Buffers_Namespace.Last_Element then

         --  Convert buffer annotation kind

         if Purpose.Last_Index < 3 then
            return Unknown;
         elsif Purpose.Element (3) = Buffers_Dump_Purpose.Last_Element then
            return Dump_Buffers;
         elsif Purpose.Element (3) = Buffers_Reset_Purpose.Last_Element then
            return Reset_Buffers;
         end if;

      elsif Purpose.Element (2) = Coverage_Namespace.Last_Element then
         if Purpose.Last_Index < 3 then
            return Unknown;
         elsif Purpose.Element (3) = Cov_Off_Purpose.Last_Element then
            return Cov_Off;
         elsif Purpose.Element (3) = Cov_On_Purpose.Last_Element then
            return Cov_On;
         end if;
      end if;

      return Unknown;
   end Annotation_Kind;

   function Annotation_Kind (Str : String) return Any_Annotation_Kind is
   begin
      return Any_Annotation_Kind'Value (Str);
   exception
      when Constraint_Error =>
         return Unknown;
   end Annotation_Kind;

   -------------
   -- Purpose --
   -------------

   function Purpose (Kind : Any_Annotation_Kind) return Ada_Qualified_Name is
   begin
      case Kind is
         when Exempt_Region =>
            return Exempt_Region_Purpose;

         when Exempt_On     =>
            return Exempt_On_Purpose;

         when Exempt_Off    =>
            return Exempt_Off_Purpose;

         when Dump_Buffers  =>
            return Buffers_Dump_Purpose;

         when Reset_Buffers =>
            return Buffers_Reset_Purpose;

         when Cov_Off       =>
            return Cov_Off_Purpose;

         when Cov_On        =>
            return Cov_On_Purpose;

         when Unknown       =>
            return Ada_Identifier_Vectors.Empty_Vector;
      end case;
   end Purpose;

   -------------------
   -- Report_Failed --
   -------------------

   procedure Report_Failed (Match : Match_Result) is
   begin
      Warn
        ("Stale annotation for "
         & Match.File.Display_Base_Name
         & ". id:"
         & (+Match.Identifier)
         & "; reason: "
         & (+Match.Diagnostic));
   end Report_Failed;

   ------------------
   -- Get_Or_Error --
   ------------------

   function Get_Or_Error
     (Str : String; Sw : String) return Slocs.Local_Source_Location
   is
      Res : Slocs.Local_Source_Location;
   begin
      if Str'Length = 0 then
         Fatal_Error ("Missing " & Sw & " on the command line");
      end if;
      Res := Slocs.Value (Str);
      if Res.Line = 0 then
         Fatal_Error ("Line number in argument to " & Sw & " should not be 0");
      end if;
      if Res.Column = 0 then
         Fatal_Error
           ("Column number in argument to " & Sw & " should not be 0");
      end if;
      return Res;
   exception
      when Exc : Constraint_Error =>
         Fatal_Error
           ("Wrong value for " & Sw & ": " & Switches.Exception_Info (Exc));
   end Get_Or_Error;

   --------------------------
   -- Load_Ext_Annotations --
   --------------------------

   procedure Load_Ext_Annotations (Annotation_File : Unbounded_String) is
      Load_Diags : constant Load_Diagnostic_Arr :=
        Load_Entries
          (GNATCOLL.VFS.Create (+US.To_String (Annotation_File)),
           DB => Ext_Annotation_DB);
   begin
      Ext_Annotation_Trace.Trace
        ("Loading external annotations from " & (+Annotation_File));
      for Diag of Load_Diags loop
         Warn (Format_Diagnostic (Diag));
      end loop;
   end Load_Ext_Annotations;

   --------------------------------
   -- Import_External_Exemptions --
   --------------------------------

   procedure Import_External_Exemptions
     (FI : Source_File_Index; Filter : Boolean := False)
   is
      File            : Virtual_File;
      Matches         : Match_Result_Vec;
      Justification   : Unbounded_String;
      Kind            : Any_Annotation_Kind;
      New_Annotations : ALI_Annotation_Maps.Map;
   begin
      --  Exit early if there are no external annotations.
      --  Validate them if needed.

      if Is_Empty (Valid_Annotation_DB) then
         return;
      end if;

      --  Match the entries on FI

      File := Create (+Get_Full_Name (FI, Or_Simple => True));
      Matches :=
        Match_Entries
          ((1 => File),
           Valid_Annotation_DB,
           Purpose_Prefix => To_Ada (Exemption_Namespace));

      --  Process each match result

      for Match of Matches loop
         Kind := Annotation_Kind (Match.Annotation);

         if not Match.Success then
            Report_Failed (Match);
         else
            --  Exempt_Region will insert an Exempt_On / Exempt_Off couple of
            --  annotations.

            if Kind in Exempt_Region | Exempt_On then
               Justification :=
                 TOML_Utils.Get_Or_Null (Match.Annotation, "justification");
               if Justification = Null_Unbounded_String then
                  Warn
                    (Slocs.Image (To_Sloc (Match.Location.Start_Sloc, FI))
                     & ": Missing or empty justification for external"
                     & " exemption annotation """
                     & (+Match.Identifier)
                     & """");
               else
                  declare
                     use ALI_Annotation_Maps;
                     Annot : constant ALI_Annotation :=
                       (Kind    => Exempt_On,
                        Message => new String'(+Justification),
                        others  => <>);

                     Sloc           : constant Slocs.Source_Location :=
                       To_Sloc (Match.Location.Start_Sloc, FI);
                     Cur            : Cursor := Get_Annotation (Sloc);
                     Existing_Annot : ALI_Annotation;
                  begin
                     if not Has_Element (Cur) then
                        Cur := New_Annotations.Find (Sloc);
                     end if;

                     if Has_Element (Cur) then
                        Existing_Annot := Element (Cur);

                        --  Do not warn if the annotation is of the same
                        --  kind and identical message, as this could simply
                        --  be a case of external annotations passed both
                        --  during instrumentation and coverage report
                        --  computation.

                        if Existing_Annot.Kind /= Annot.Kind
                          or else Existing_Annot.Message.all
                                  /= Annot.Message.all
                        then
                           Warn
                             (Slocs.Image (Sloc)
                              & ": Conflicting annotations for this line,"
                              & " ignoring the external annotation """
                              & (+Match.Identifier)
                              & """");
                        end if;
                     else
                        if Filter then
                           declare
                              SCO : constant SCO_Id :=
                                Sloc_Intersects_SCO (Sloc);
                           begin
                              if SCO /= No_SCO_Id then
                                 Warn
                                   ("Exemption annotation at "
                                    & Slocs.Image (Sloc)
                                    & " intersects a coverage obligation ("
                                    & Image (SCO, True)
                                    & "), ignoring it");
                              else
                                 New_Annotations.Insert (Sloc, Annot);
                              end if;
                           end;
                        else
                           New_Annotations.Insert (Sloc, Annot);
                        end if;
                     end if;
                  end;
               end if;
            end if;

            if Kind in Exempt_Region | Exempt_Off then
               declare
                  use ALI_Annotation_Maps;
                  Annot : constant ALI_Annotation :=
                    (Kind    => Exempt_Off,
                     Message => new String'(+Justification),
                     others  => <>);

                  Sloc           : constant Slocs.Source_Location :=
                    To_Sloc (Match.Location.End_Sloc, FI);
                  Cur            : Cursor := Get_Annotation (Sloc);
                  Existing_Annot : ALI_Annotation;
               begin
                  --  Also check if the new annotations don't already contain
                  --  an annotation for this sloc.

                  if not Has_Element (Cur) then
                     Cur := New_Annotations.Find (Sloc);
                  end if;

                  if Has_Element (Cur) then
                     Existing_Annot := Element (Cur);

                     --  Same for Exempt_Off, except the message isn't
                     --  relevant here.

                     if Existing_Annot.Kind /= Annot.Kind then
                        Warn
                          (Slocs.Image (Sloc)
                           & ": Conflicting annotations for this line,"
                           & " ignoring the external annotation """
                           & (+Match.Identifier)
                           & """");
                     end if;
                  else
                     New_Annotations.Insert (Sloc, Annot);
                  end if;
               end;
            end if;
         end if;
      end loop;
      Set_Annotations (New_Annotations);
   end Import_External_Exemptions;

   -----------------------------
   -- Generic_Get_Annotations --
   -----------------------------

   function Generic_Get_Annotations
     (Filename : String) return Instrument.Common.Instr_Annotation_Map
   is
      VF         : constant Virtual_File := Create (+Filename);
      Matches    : Match_Result_Vec;
      Annot_Kind : Expected_Annot_Kind'Base;
      Result     : Instr_Annotation_Map;
   begin
      --  Exit early if there are no external annotations

      if Is_Empty (Valid_Annotation_DB) then
         return Instr_Annotation_Maps.Empty_Map;
      end if;

      Matches :=
        Match_Entries ((1 => VF), Valid_Annotation_DB, Purpose_Prefix);

      --  Process each annotation result

      for Match of Matches loop
         if not Match.Success then
            Report_Failed (Match);
            goto Continue;
         end if;

         Annot_Kind := Kind (Match.Annotation);
         if Annot_Kind not in Expected_Annot_Kind then
            Warn
              ("Unexpected or unknown annotation kind for annotation """
               & (+Match.Identifier)
               & """: "
               & (+Get_Or_Null (Match.Annotation, "purpose")));
            goto Continue;
         end if;
         declare
            use TOML;
            Sloc    : constant Slocs.Local_Source_Location :=
              +Match.Location.Start_Sloc;
            Success : Boolean;
            Cur     : Instr_Annotation_Maps.Cursor;
            Annot   : Instr_Annotation := Convert (Annot_Kind, Match, Success);
         begin
            if not Success then
               goto Continue;
            end if;

            Annot.Insert_After := False;
            if Match.Annotation.Has ("insert_after") then
               if Match.Annotation.Get ("insert_after").Kind /= TOML_Boolean
               then
                  Warn
                    ("Invalid type for ""insert_after"" field in annotation"
                     & """"
                     & (+Match.Identifier)
                     & """, should be"
                     & " TOML_BOOLEAN.");
                  Success := False;
               else
                  Annot.Insert_After :=
                    Match.Annotation.Get ("insert_after").As_Boolean;
               end if;
            end if;

            Result.Insert (Sloc, Annot, Cur, Success);

            --  Tolerate duplicate annotations if they are the same

            if not Success and then Result.Reference (Cur) /= Annot then
               Warn
                 (Ada.Directories.Simple_Name (Filename)
                  & ":"
                  & Slocs.Image (Sloc)
                  & ": Conflicting annotations for this line, ignoring the"
                  & " external annotation """
                  & (+Match.Identifier)
                  & """");
            end if;

            Ext_Annotation_Trace.Trace
              ("Found instrumentation annotation for "
               & Slocs.Image (Sloc)
               & ": "
               & Annot.Kind'Image);

         end;
         <<Continue>>
      end loop;
      return Result;
   end Generic_Get_Annotations;

   -------------------------------
   -- Convert_Buffer_Annotation --
   -------------------------------

   function Convert_Buffer_Annotation
     (Kind      : Buffer_Annotation_Kind;
      Match_Res : Match_Result;
      Success   : out Boolean) return Instrument.Common.Instr_Annotation
   is
      use TOML;
      New_Annotation : Instr_Annotation (Kind);
   begin
      Success := True;
      case New_Annotation.Kind is
         when Dump_Buffers  =>
            New_Annotation.Trace_Prefix :=
              Get_Or_Null (Match_Res.Annotation, "trace_prefix");

         when Reset_Buffers =>
            null;

         when others        =>
            raise Program_Error with "Unreachable";
      end case;

      return New_Annotation;
   end Convert_Buffer_Annotation;

   ----------------------------
   -- Get_Buffer_Annotations --
   ----------------------------

   function Get_Buffer_Annotations_Internal is new
     Generic_Get_Annotations
       (Expected_Annot_Kind => Buffer_Annotation_Kind,
        Kind                => Annotation_Kind,
        Convert             => Convert_Buffer_Annotation,
        Purpose_Prefix      => To_Ada (Buffers_Namespace));

   function Get_Buffer_Annotations
     (Filename : String) return Instr_Annotation_Map
   is (Get_Buffer_Annotations_Internal (Filename));

   ----------------------------------
   -- Get_Disabled_Cov_Annotations --
   ----------------------------------

   function Get_Disabled_Cov_Annotations
     (Filename : String) return Instr_Annotation_Map
   is
      use Instr_Annotation_Maps;
      SFI : constant Source_File_Index :=
        Get_Index_From_Full_Name (Filename, Source_File);

      subtype Cov_Annotation_Kind is
        Src_Annotation_Kind range Cov_On .. Cov_Off;

      function Convert_Cov_Annotation
        (Kind      : Cov_Annotation_Kind;
         Match_Res : Match_Result;
         Success   : out Boolean) return Instrument.Common.Instr_Annotation;
      --  Convert Match_Res to a Cov_On/Cov_Off annotation, assuming the
      --  annotation in Match Res is of kind Kind.
      --
      --  Print a warning and set Success to False if there are errors
      --  interpreting the TOML annotation.

      function Get_Disabled_Cov_Intl is new
        Generic_Get_Annotations
          (Expected_Annot_Kind => Cov_Annotation_Kind,
           Kind                => Annotation_Kind,
           Convert             => Convert_Cov_Annotation,
           Purpose_Prefix      => To_Ada (Coverage_Namespace));

      ----------------------------
      -- Convert_Cov_Annotation --
      ----------------------------

      function Convert_Cov_Annotation
        (Kind      : Cov_Annotation_Kind;
         Match_Res : Match_Result;
         Success   : out Boolean) return Instrument.Common.Instr_Annotation
      is
         use TOML;
         New_Annotation : Instr_Annotation (Kind);
      begin
         Success := True;
         case New_Annotation.Kind is
            when Cov_Off =>
               New_Annotation.Justification :=
                 Get_Or_Null (Match_Res.Annotation, "justification");
               if New_Annotation.Justification = Null_Unbounded_String then
                  Warn
                    (Slocs.Image (To_Sloc (Match_Res.Location.Start_Sloc, SFI))
                     & ": Missing or empty justification for external"
                     & " disabled coverage region annotation """
                     & (+Match_Res.Identifier)
                     & """");
               end if;

            when Cov_On  =>
               null;

            when others  =>
               raise Program_Error with "Unreachable";

         end case;

         --  Filter the annotation if it conflicts with a pre-existing one
         --  which is not identical.

         if Success then
            declare
               use ALI_Annotation_Maps;
               Cur : constant ALI_Annotation_Maps.Cursor :=
                 Get_Annotation
                   ((Source_File => SFI, L => +Match_Res.Location.Start_Sloc));
            begin
               if Has_Element (Cur) then
                  if Element (Cur).Kind /= Kind then
                     Success := False;
                  elsif Kind = Cov_Off then
                     Success :=
                       (if Element (Cur).Message in null
                        then US.Length (New_Annotation.Justification) = 0
                        else
                          Element (Cur).Message.all
                          = (+New_Annotation.Justification));
                  end if;

                  if not Success then
                     Warn
                       (Ada.Directories.Simple_Name (Filename)
                        & ":"
                        & Image (Match_Res.Location.Start_Sloc)
                        & ": Conflicting annotations for this line, ignoring"
                        & " the external annotation """
                        & (+Match_Res.Identifier)
                        & """");
                  end if;

               end if;
            end;
         end if;

         return New_Annotation;
      end Convert_Cov_Annotation;

      Res : Instr_Annotation_Map := Get_Disabled_Cov_Intl (Filename);
      Cur : Instr_Annotation_Maps.Cursor := Res.First;
      Aux : Instr_Annotation_Maps.Cursor;

      Expected_Kind, Next_Expected_Kind, Tmp : Src_Annotation_Kind;

      --  Start of processing for Get_Disabled_Cov_Annotations

   begin
      --  Filter out any annotations that do not come in pairs, and ensure
      --  the map starts with an Cov_Off annotation.

      if Has_Element (Cur) and then Element (Cur).Kind = Cov_On then
         Warn
           (Ada.Directories.Simple_Name (Filename)
            & ": "
            & Slocs.Image (Key (Cur))
            & ": external Cov_On annotation with no previous Cov_Off"
            & " annotation, ignoring it.");
         Aux := Cur;
         Next (Cur);
         Res.Delete (Aux);
      end if;

      Expected_Kind := Cov_Off;
      Next_Expected_Kind := Cov_On;

      while Has_Element (Cur) loop
         pragma Assert (Element (Cur).Kind = Expected_Kind);
         Aux := Next (Cur);
         if (if Has_Element (Aux)
             then Element (Aux).Kind /= Next_Expected_Kind
             else Element (Cur).Kind = Cov_Off)
         then
            Warn
              (Ada.Directories.Simple_Name (Filename)
               & ": "
               & Slocs.Image (Key (Cur))
               & ": external "
               & Expected_Kind'Image
               & " annotation with no subsequent "
               & Next_Expected_Kind'Image
               & " annotation, ignoring it.");
            Res.Delete (Cur);
         else
            Tmp := Expected_Kind;
            Expected_Kind := Next_Expected_Kind;
            Next_Expected_Kind := Tmp;
         end if;
         Cur := Aux;
      end loop;

      return Res;
   end Get_Disabled_Cov_Annotations;

   --------------------
   -- Add_Annotation --
   --------------------

   procedure Add_Annotation (Args : Command_Line.Parser.Parsed_Arguments) is
      use TOML;

      Annot_Kind    : Any_Annotation_Kind;
      Start_Sloc    : Slocs.Local_Source_Location;
      End_Sloc      : Slocs.Local_Source_Location;
      Target_File   : Virtual_File;
      Output_File   : Virtual_File;
      Language      : Any_Language := All_Languages;
      Justification : Unbounded_String;

      function "+" (Opt : Command_Line.String_Options) return Unbounded_String
      is (Parser.Value_Or_Null (Args.String_Args (Opt)));

      New_Annot_DB  : Entry_DB;
      Entry_Purpose : Ada_Qualified_Name;
      Entry_Id      : Unbounded_String := +Opt_Annotation_Id;
      Annotation    : constant TOML_Value := Create_Table;
      SS_Backend    : Unbounded_String := +Opt_SS_Backend;
      File_Prefix   : Unbounded_String := +Opt_Source_Root;
      Source        : GPR2.Build.Source.Object;

   begin
      --  First, determine the kind of annotation we'll be generating

      Annot_Kind := Annotation_Kind (US.To_String (+Opt_Annotation_Kind));

      --  We always need a valid filename to generate an annotation. More than
      --  one is ambiguous however, so only consider the last one as an input.

      if Annot_Kind = Unknown then
         Fatal_Error
           ("Invalid annotation kind (--kind): "
            & (+(+Opt_Annotation_Kind))
            & ASCII.LF
            & "Must be one of "
            & Coverage_Options.Annotation_Kind_Options);
      end if;

      if Args.Remaining_Args.Is_Empty then
         Fatal_Error
           ("Missing FILE to which the annotation should apply on the command"
            & " line");
      end if;

      Target_File := Create (+(+Args.Remaining_Args.Last_Element));

      if Is_Project_Loaded then
         declare
            Normalized : constant Virtual_File :=
              Create (Target_File.Full_Name, Normalize => True);
         begin
            Source := Project.Lookup_Source (Normalized.Display_Full_Name);
         end;
         if not Source.Is_Defined then
            Fatal_Error
              (Target_File.Display_Full_Name
               & ": no such file in the project");
         end if;
      end if;

      if not Target_File.Is_Regular_File then
         Fatal_Error (Target_File.Display_Full_Name & ": no such file");
      end if;

      --  Require the -o/--output switch to be present

      if not Args.String_Args (Opt_Output).Present then
         Fatal_Error ("Missing --output switch");
      else
         Output_File := Create (+US.To_String ((+Opt_Output)));
      end if;

      --  Validate the arguments depending on the requested annotation kind

      case Annot_Kind is
         when Exempt_Region        =>
            Start_Sloc :=
              Get_Or_Error (+(+Opt_Start_Location), "--start-location");
            End_Sloc := Get_Or_Error (+(+Opt_End_Location), "--end-location");

            if not Args.String_Args (Opt_Justification).Present then
               Warn
                 ("--justification missing for an --kind="
                  & Kind_Image (Annot_Kind)
                  & " annotation");
            end if;
            Justification :=
              Parser.Value_Or_Null (Args.String_Args (Opt_Justification));

         when Exempt_On | Cov_Off  =>

            --  Accept either the --location or --start-location switches

            if Args.String_Args (Opt_Location).Present then
               Start_Sloc := Get_Or_Error (+(+Opt_Location), "--location");
            else
               Start_Sloc :=
                 Get_Or_Error (+(+Opt_Start_Location), "--start-location");
            end if;

            if not Args.String_Args (Opt_Justification).Present then
               Warn
                 ("--justification missing for a --kind="
                  & Kind_Image (Annot_Kind)
                  & " annotation");
            end if;
            Justification :=
              Parser.Value_Or_Null (Args.String_Args (Opt_Justification));
            End_Sloc := Start_Sloc;

         when Exempt_Off .. Cov_On =>

            --  Accept either the --location or --start-location switches

            if Args.String_Args (Opt_Location).Present then
               Start_Sloc := Get_Or_Error (+(+Opt_Location), "--location");
            else
               Start_Sloc :=
                 Get_Or_Error (+(+Opt_Start_Location), "--start-location");
            end if;
            End_Sloc := Start_Sloc;

         when Unknown              =>
            raise Program_Error with "Unreachable";
      end case;

      --  Generate the annotation in isolation and report if there are any
      --  issues.
      --
      --  First, generate an entry identifier if not specified, from the
      --  annotation kind and original source location range.

      if US.Length (Entry_Id) = 0 then
         Entry_Id := +Kind_Image (Annot_Kind);
         Entry_Id :=
           Entry_Id
           & "-"
           & Hex_Image
               (Unsigned_32
                  (Ada.Strings.Hash
                     (Target_File.Display_Full_Name
                      & Slocs.Image (Start_Sloc)
                      & Slocs.Image (End_Sloc))));
      end if;

      Entry_Purpose := Purpose (Annot_Kind);
      Annotation.Set ("purpose", Create_String (To_Ada (Entry_Purpose)));

      --  Add annotation kind specific fields

      case Annot_Kind is
         when Exempt_On | Exempt_Region | Cov_Off =>
            Annotation.Set ("justification", Create_String (Justification));

         when Dump_Buffers | Reset_Buffers        =>
            Annotation.Set
              ("insert_after",
               Create_Boolean (Args.Bool_Args (Opt_Annotate_After)));

            if Annot_Kind = Dump_Buffers
              and then Args.String_Args (Opt_Dump_Filename_Prefix).Present
            then
               Annotation.Set
                 ("trace_prefix",
                  Create_String
                    (Args.String_Args (Opt_Dump_Filename_Prefix).Value));
            end if;

         when Exempt_Off | Cov_On                 =>
            null;

         when Unknown                             =>
            raise Program_Error with "Unreachable";
      end case;

      --  Determine the backend to be used depending on the language, if not
      --  specified on the command line.

      if US.Length (SS_Backend) = 0 then

         --  If we have a project loaded, use it to determine the file language

         if Is_Project_Loaded then
            Language := To_Language_Or_All (Source.Language);
         end if;

         --  If this failed or we have no project at hand, revert to simple
         --  heuristics.

         if Language = All_Languages then
            Language := Guess_Lang (Target_File);
         end if;

         case Language is
            when Ada_Language              =>
               SS_Backend := +"lal_context";

            when C_Language | CPP_Language =>
               SS_Backend := +"clang_context";

            when All_Languages             =>

               --  This should not hit but maybe with rust coverage we may
               --  hit this?

               SS_Backend := +"absolute";
         end case;
      end if;

      --  Compute a File prefix if there isn't one already specified, and we
      --  have a project at hand.

      if US.Length (File_Prefix) = 0 and then Is_Project_Loaded then
         case To_Language_Or_All (Source.Language) is
            when Ada_Language =>

               --  Ada source files are guaranteed to be unique in a project,
               --  so use the directory name as file prefix to end-up with only
               --  the base name.

               File_Prefix := US.To_Unbounded_String (+Target_File.Dir_Name);

            when others       =>
               --  For other sources, check if the source is unique in the
               --  tree, if so, do the same thing.

               declare
                  Count    : Natural := 0;
                  Basename : constant GPR2.Simple_Name :=
                    Source.Path_Name.Simple_Name;
                  Prj_Dir  : constant Virtual_File :=
                    Create (+String (Source.Owning_View.Dir_Name.Value));
                  Rel_Path : constant String :=
                    +Create (Relative_Path (Target_File, Prj_Dir)).Dir_Name;
               begin
                  for View of Project.Project loop
                     declare
                        S : constant GPR2.Build.Source.Object :=
                          View.Source (Basename);
                     begin
                        if S.Is_Defined then
                           Count := Count + 1;
                        end if;
                     end;
                  end loop;

                  --  If the basename is ambiguous, use the relative path from
                  --  the project to the file, if it has no relative path
                  --  components (./ or ..).

                  if Count > 1 then
                     if Has_Relative_Component (Rel_Path) then
                        Warn
                          ("Could not generate adequate file prefix from"
                           & " project, use --source-root if necessary.");
                     else
                        File_Prefix := US.To_Unbounded_String (Rel_Path);
                     end if;
                  else
                     File_Prefix :=
                       US.To_Unbounded_String (+Target_File.Dir_Name);
                  end if;
               end;
         end case;
      end if;

      declare
         Target_Span : constant Sloc_Span := (+Start_Sloc, +End_Sloc);
         Diags       : constant Load_Diagnostic_Arr :=
           Add_Or_Update_Entry
             (DB          => New_Annot_DB,
              Identifier  => Entry_Id,
              Annotation  => Annotation,
              Kind        => SS_Backend,
              File        => Target_File,
              Span        => Target_Span,
              File_Prefix => File_Prefix);
      begin
         --  Do not fallback if we have an explicit backend specified on
         --  command line, or if the backend is already "absolute"

         if Diags'Length /= 0
           and then SS_Backend /= "absolute"
           and then not Args.String_Args (Opt_SS_Backend).Present
         then
            --  Try again, but with the "absolute" backend. If that fails do
            --  not log a warning about the first attempt.

            declare
               Msg       : Unbounded_String :=
                 +("Could not create an auto-relocating annotation for "
                   & Target_File.Display_Full_Name
                   & ":"
                   & Image (Target_Span)
                   & ", creating an absolute location annotation instead.");
               Abs_Diags : constant Load_Diagnostic_Arr :=
                 Add_Or_Update_Entry
                   (DB          => New_Annot_DB,
                    Identifier  => Entry_Id,
                    Annotation  => Annotation,
                    Kind        => +"absolute",
                    File        => Target_File,
                    Span        => Target_Span,
                    File_Prefix => File_Prefix);
            begin
               Check_New_Entry_Diags (Abs_Diags);

               if Ext_Annotation_Trace.Is_Active then
                  Msg := Msg & (ASCII.LF & "Error was: ");

                  for Diag of Diags loop
                     Msg := Msg & (ASCII.LF & Format_Diagnostic (Diag));
                  end loop;
               end if;
               Warn (+Msg);
            end;
         else
            Check_New_Entry_Diags (Diags);
         end if;

      end;

      --  Check if there already is an entry with the same identifier in the
      --  loaded entries. If so, either error out if we cannot overwrite it.

      declare
         Entr : constant Entry_View :=
           Query_Entry (Ext_Annotation_DB, Entry_Id);
      begin
         if Entr /= No_Entry_View and then not Args.Bool_Args (Opt_Force) then
            Outputs.Fatal_Error
              ("Annotation with identifier "
               & (+Entry_Id)
               & "already exists. Use -f to overwrite it");
         end if;
      end;

      --  Now check whether there is an entry matching the same region, for the
      --  same purpose. If so, warn about it so the user doesn't get a surprise
      --  at coverage time.

      declare
         Matches : constant Match_Result_Vec :=
           Match_Entries
             ((1 => Target_File), Valid_Annotation_DB, To_Ada (Entry_Purpose));
      begin
         for Match of Matches loop
            if Match.Success and then Match.Location = (+Start_Sloc, +End_Sloc)
            then
               Outputs.Warn
                 ("Pre-existing annotation with identifier "
                  & (+Match.Identifier)
                  & "matches the same region.");
            end if;
         end loop;
      end;

      --  Finally, import the new entry and write everything to disk

      Replace_Entry (Ext_Annotation_DB, New_Annot_DB, Entry_Id, Entry_Id);
      Write_Entries (Ext_Annotation_DB, Output_File);
   end Add_Annotation;

   procedure Delete_Annotation (Args : Command_Line.Parser.Parsed_Arguments) is
      Output_File : Virtual_File;
      Identifier  : Unbounded_String;
   begin
      --  Require an external annotation file. They have already been loaded
      --  if present, but we still need to check.

      if Args.String_List_Args (Opt_Ext_Annotations).Is_Empty then
         Fatal_Error ("missing --external-annotations switch");
      end if;

      --  Require a file in which to store the amended entries

      if not Args.String_Args (Opt_Output).Present then
         Fatal_Error ("missing --output switch");
      else
         Output_File :=
           Create (+US.To_String (Args.String_Args (Opt_Output).Value));
      end if;

      --  Require an entry identifier

      if not Args.String_Args (Opt_Annotation_Id).Present then
         Fatal_Error ("Missing --annotation-id switch");
      else
         Identifier := Args.String_Args (Opt_Annotation_Id).Value;
      end if;

      --  Check wether there actually is an entry associated with Identifier

      if not Switches.Force then
         declare
            Entr : constant Entry_View :=
              Query_Entry (Ext_Annotation_DB, Identifier);
         begin
            if Entr = No_Entry_View then
               Fatal_Error
                 ("No annotation associated with identifier """
                  & (+Identifier)
                  & """");
            end if;
         end;
      end if;

      --  Delete the entry and write the remaining entries back to file

      Delete_Entry (Ext_Annotation_DB, Identifier);
      Write_Entries (Ext_Annotation_DB, Output_File);
   end Delete_Annotation;

   procedure Show_Annotations (Args : Command_Line.Parser.Parsed_Arguments) is
      Purpose_Filter : Unbounded_String;
      Match_Results  : Match_Result_Vec;
   begin
      --  Require an external annotation file. They have already been loaded
      --  if present, but we still need to check.

      if Args.String_List_Args (Opt_Ext_Annotations).Is_Empty then
         Fatal_Error ("missing --external-annotations switch");
      end if;

      --  Require either a project or some files on the command line

      if not Project.Is_Project_Loaded and then Args.Remaining_Args.Is_Empty
      then
         Fatal_Error ("Missing -P switch or positional FILES");
      end if;

      --  Check the annotation purpose if specified

      if Args.String_Args (Opt_Annotation_Kind).Present then
         declare
            Annot_Kind_Str : constant String :=
              +Args.String_Args (Opt_Annotation_Kind).Value;
            Annot_Kind     : constant Any_Annotation_Kind :=
              Annotation_Kind (Annot_Kind_Str);
         begin
            if Annot_Kind in Unknown then
               Fatal_Error
                 ("Unknown annotation kind (--kind): """
                  & Annot_Kind_Str
                  & """, must be one of "
                  & Coverage_Options.Annotation_Kind_Options);
            else
               Purpose_Filter := +To_Ada (Purpose (Annot_Kind));
            end if;
         end;
      else
         Purpose_Filter := +To_Ada (Xcov_Namespace);
      end if;

      --  Build the file list. Use all the project source files if no files
      --  have been explicitly requested on the command line

      if Args.Remaining_Args.Is_Empty then
         declare
            Source_Files : String_Vectors.Vector;
            Files        : File_Array_Access;
            I            : Positive := 1;

            procedure Add_File
              (Project : GPR2.Project.View.Object;
               File    : GPR2.Build.Source.Object);
            --  Callabck for Enumerate_Sources: append File to Source_Files

            --------------
            -- Add_File --
            --------------

            procedure Add_File
              (Project : GPR2.Project.View.Object;
               File    : GPR2.Build.Source.Object)
            is
               pragma Unreferenced (Project);
            begin
               Source_Files.Append (+String (File.Path_Name.Value));
            end Add_File;
         begin
            Project.Enumerate_Sources (Add_File'Access, All_Languages);
            Files := new File_Array (1 .. Positive (Source_Files.Length));
            for F of Source_Files loop
               Files.all (I) := Create (+(+F));
               I := I + 1;
            end loop;
            Match_Results :=
              Match_Entries (Files.all, Ext_Annotation_DB, +Purpose_Filter);
            GNATCOLL.VFS.Unchecked_Free (Files);
         end;
      else
         declare
            Files : File_Array (1 .. Positive (Args.Remaining_Args.Length));
         begin
            for I in 1 .. Files'Last loop
               Files (I) :=
                 Create (+(US.To_String (Args.Remaining_Args.Element (I))));
            end loop;
            Match_Results :=
              Match_Entries (Files, Valid_Annotation_DB, +Purpose_Filter);
         end;
      end if;

      --  Post-process the match results and display the annotations

      Sort (Match_Results);
      declare
         Current_File : Virtual_File;
      begin
         for Match of Match_Results loop
            if Match.File /= Current_File then
               Current_File := Match.File;
               if Current_File /= No_File then
                  Ada.Text_IO.New_Line;
               end if;
               Ada.Text_IO.Put_Line (Current_File.Display_Base_Name & ":");
            end if;
            declare
               Annot_Kind : constant Any_Annotation_Kind :=
                 Annotation_Kind (Match.Annotation);
            begin
               if Match.Success then
                  Ada.Text_IO.Put ("- " & Image (Match.Location) & "; ");
               else
                  Ada.Text_IO.Put ("- STALE ANNOTATION; ");
               end if;

               Ada.Text_IO.Put
                 ("id: "
                  & (+Match.Identifier)
                  & "; kind: "
                  & Kind_Image (Annot_Kind));

               case Annot_Kind is
                  when Exempt_On | Exempt_Region | Cov_Off =>
                     Ada.Text_IO.Put
                       ("; Justification: "
                        & (+Get_Or_Null (Match.Annotation, "justification")));

                  when Dump_Buffers | Reset_Buffers        =>
                     Ada.Text_IO.Put
                       ("; annotate after statement: "
                        & Boolean'Image
                            (Get_Or_Default
                               (Match.Annotation, "insert_after", False)));
                     if Annot_Kind = Dump_Buffers
                       and then Match.Annotation.Has ("trace_prefix")
                     then
                        Ada.Text_IO.Put
                          ("; trace filename prefix: "
                           & (+Get_Or_Null
                                 (Match.Annotation, "trace_prefix")));
                     end if;

                  when Unknown | Exempt_Off | Cov_On       =>
                     null;
               end case;
               if not Match.Success then
                  Ada.Text_IO.Put ("; diagnostic: " & (+Match.Diagnostic));
               end if;
            end;
            Ada.Text_IO.New_Line;
         end loop;
      end;

   end Show_Annotations;

   ----------------
   -- Guess_Lang --
   ----------------

   function Guess_Lang (File : Virtual_File) return Any_Language is
      Ext : constant String := +File.File_Extension;
   begin
      if Ext = ".h" or else Ext = ".c" then
         return C_Language;
      elsif Ext = ".hh"
        or else Ext = ".cc"
        or else Ext = ".hpp"
        or else Ext = ".cpp"
      then
         return CPP_Language;
      elsif Ext = ".adb"
        or else Ext = ".ads"
        or else File.Has_Suffix (".1.ada")
        or else File.Has_Suffix (".2.ada")
      then
         return Ada_Language;
      else
         return All_Languages;
      end if;
   end Guess_Lang;

   --------------------------
   -- Validate_Annotations --
   --------------------------

   procedure Validate_Annotations is
   begin
      Clear_DB (Valid_Annotation_DB);
      Iterate_Entries (Ext_Annotation_DB, Validate_Annotation'Access);
   end Validate_Annotations;

   procedure Validate_Annotation
     (Identifier : Unbounded_String; Entr : Entry_View)
   is
      All_Ok : Boolean := True;
      --  Wether all relevant annotations are valid. We reject the entry as
      --  soon as one of the annotations is invalid.

      Some_Relevant : Boolean := False;
      --  Wether some of the annotations are relevant to gnatcov (purpose
      --  starts with xcov.)

   begin
      if Entr.Annotations.Length = 0 then
         Warn
           ("Entry """
            & (+Identifier)
            & """ has no annotations, it will be ignored.");
         return;
      end if;
      --  Check each annotation associated with the entry

      for I in 1 .. Entr.Annotations.Length loop
         declare
            use TOML;
            Annot      : constant TOML_Value := Entr.Annotations.Item (I);
            Annot_Kind : constant Any_Annotation_Kind :=
              Annotation_Kind (Annot);
         begin
            case Annot_Kind is
               when Unknown                       =>

                  --  Only warn about unknown annotations that start with
                  --  "xcov."

                  if Has_Prefix (+Get_Or_Null (Annot, "purpose"), "xcov.") then
                     Warn
                       ("Entry """
                        & (+Identifier)
                        & """ has an unknown annotation kind, it will be"
                        & " ignored.");
                     All_Ok := False;
                  end if;

               when Exempt_On | Exempt_Region     =>
                  Some_Relevant := True;
                  if Get_Or_Null (Annot, "justification")
                    = Null_Unbounded_String
                  then
                     Warn
                       ("Missing or empty justification for external"
                        & " exemption annotation """
                        & (+Identifier)
                        & """, it will be ignored.");
                     All_Ok := False;
                  end if;

               when Dump_Buffers | Reset_Buffers  =>
                  Some_Relevant := True;
                  if Annot.Has ("insert_after")
                    and then Annot.Get ("insert_after").Kind /= TOML_Boolean
                  then
                     Warn
                       ("Wrong type for ""insert_after"" flag in entry """
                        & (+Identifier)
                        & """, it will be ignored.");
                     All_Ok := False;
                  end if;
                  if Annot_Kind in Dump_Buffers
                    and then Annot.Has ("trace_prefix")
                    and then Annot.Get ("trace_prefix").Kind /= TOML_String
                  then
                     Warn
                       ("Wrong type for ""trace_prefix"" in entry """
                        & (+Identifier)
                        & """, it will be ignored.");
                     All_Ok := False;
                  end if;

               when Exempt_Off | Cov_Off | Cov_On =>
                  Some_Relevant := True;

            end case;
         end;
      end loop;
      if All_Ok and then Some_Relevant then
         Replace_Entry
           (Valid_Annotation_DB, Ext_Annotation_DB, Identifier, Identifier);
      end if;
   end Validate_Annotation;

   ---------------------------
   -- Check_New_Entry_Diags --
   ---------------------------

   procedure Check_New_Entry_Diags (Diags : Load_Diagnostic_Arr) is
   begin
      if Diags'Length /= 0 then
         Outputs.Error ("Error while generating annotation:");
         for Diag of Diags loop
            Outputs.Error (Format_Diagnostic (Diag));
         end loop;
         raise Xcov_Exit_Exc;
      end if;
   end Check_New_Entry_Diags;

end SS_Annotations;
