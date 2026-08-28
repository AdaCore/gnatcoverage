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

with GNATCOLL.JSON;
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
with Switches_GPR;      use Switches_GPR;

package body SS_Annotations is
   use type Unbounded_String;

   Xcov_Namespace : constant Ada_Qualified_Name := To_Qualified_Name ("xcov");
   --  Common prefix for all stable_sloc purposes that gnatcov recognizes

   Exemption_Namespace : constant Ada_Qualified_Name :=
     Xcov_Namespace & To_Qualified_Name ("exempt");
   --  Common prefix for all exemption related annotations

   Exempt_On_Purpose                  : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("on");
   Exempt_Off_Purpose                 : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("off");
   Exempt_Region_Purpose              : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("region");
   Exempt_Decision_Outcome_Purpose    : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("decision_outcome");
   Exempt_Decision_Condition_Purpose  : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("decision_condition");
   Exempt_Full_Decision_Purpose       : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("full_decision");
   Manual_Decision_Evaluation_Purpose : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("manual_decision_evaluation");
   Exempt_Branch_Purpose              : constant Ada_Qualified_Name :=
     Exemption_Namespace & To_Qualified_Name ("branch");

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

   procedure Require_Annotation_File
     (Args : Command_Line.Parser.Parsed_Arguments);
   --  Stop with an error unless an external annotation file is designated,
   --  either on the command line or by the project

   function Output_File_Of
     (Args : Command_Line.Parser.Parsed_Arguments) return Virtual_File;
   --  File that delete-annotation writes to: --output when passed, else the
   --  first file the project designates. Stop with an error when neither
   --  designates one.

   function Annotation_Output_For
     (Args : Command_Line.Parser.Parsed_Arguments; Source : Virtual_File)
      return Virtual_File;
   --  File that add-annotation writes to: --output when passed, else the
   --  Coverage'External_Annotations of the project owning Source. Stop with an
   --  error when neither designates one.

   procedure Print_Annotations_Text (Results : Match_Result_Vec);
   --  Print Results for a human reader, grouped by file

   procedure Print_Annotations_JSON
     (Args    : Command_Line.Parser.Parsed_Arguments;
      Results : Match_Result_Vec;
      Code    : String;
      Message : String);
   --  Print Results as a JSON object, for a machine reader such as an IDE.
   --
   --  The fields mirror what the text form shows rather than how annotations
   --  are stored, so that consumers depend on the annotation model rather than
   --  on the layout of the annotation file.
   --
   --  Code says whether there is anything to report, and Message carries the
   --  matching diagnostic for a human reader. See Show_Annotations for the
   --  codes and what they mean.

   No_Annotation_File_Error : constant String :=
     "no external annotation file: pass --external-annotations, or designate"
     & " one through the Coverage'External_Annotations project attribute";
   --  Diagnostic for a command that needs an annotation file when nothing
   --  designates one

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

   procedure Validate_Annotation
     (Identifier : Unbounded_String; Entr : Entry_View);
   --  Helper for the above procedure, validate a single entry, if it is ok
   --  then copy it in Valid_Annotation_DB, otherwise emit a warning.

   procedure Check_New_Entry_Diags (Diags : Load_Diagnostic_Arr);
   --  Check if Diags is empty, and if not error out while displaying the
   --  diagnostics.

   function Default_Identifier
     (Kind : Any_Annotation_Kind; File : Virtual_File; Span : Sloc_Span)
      return Unbounded_String;
   --  Return the identifier to use for the annotation of the given Kind
   --  covering Span in File, when the user did not provide one explicitly.

   function To_TOML
     (Kind         : Any_Annotation_Kind;
      Annot        : ALI_Annotation;
      Insert_After : Boolean := False) return TOML.TOML_Value
   with Pre => Kind /= Unknown;
   --  Return the TOML table describing an annotation of the given Kind, taking
   --  all kind-specific fields from Annot.
   --
   --  Annot's own kind is expected to be consistent with Kind, with the
   --  exception of Kind = Exempt_Region, for which Annot is expected to be an
   --  Exempt_On annotation: ALI_Annotation designates a single source
   --  location, so it cannot represent a whole region on its own.
   --
   --  Insert_After is relevant for buffer annotations only, and conveys
   --  whether the generated code should go after the designated location
   --  rather than before it.

   procedure Create_Entry
     (DB               : in out Entry_DB;
      Identifier       : Unbounded_String;
      Annotation       : TOML.TOML_Value;
      File             : Virtual_File;
      Span             : Sloc_Span;
      Source           : GPR2.Build.Source.Object;
      Backend          : Unbounded_String := Null_Unbounded_String;
      File_Prefix      : Unbounded_String := Null_Unbounded_String;
      Explicit_Backend : Boolean := False;
      Language         : Any_Language := All_Languages);
   --  Add to DB the entry named Identifier, described by Annotation, and
   --  designating Span in File.
   --
   --  Backend is the Stable_Sloc matcher kind to use. If empty, it is deduced
   --  from the language of File: "lal_context" for Ada, "clang_context" for
   --  C/C++, using Source to determine that language when a project is
   --  loaded, and falling back on the file extension otherwise. Unless
   --  Explicit_Backend is set, failing to create such a self-relocating entry
   --  is not fatal: we retry with the "absolute" backend and warn about it.
   --
   --  File_Prefix is the prefix to strip from File's name in the created
   --  entry. If empty and a project is loaded, compute it so that the entry
   --  designates File through the shortest unambiguous name.

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
         elsif Purpose.Element (3)
           = Exempt_Decision_Outcome_Purpose.Last_Element
         then
            return Exempt_Decision_Outcome;
         elsif Purpose.Element (3)
           = Exempt_Decision_Condition_Purpose.Last_Element
         then
            return Exempt_Decision_Condition;
         elsif Purpose.Element (3) = Exempt_Full_Decision_Purpose.Last_Element
         then
            return Exempt_Full_Decision;
         elsif Purpose.Element (3)
           = Manual_Decision_Evaluation_Purpose.Last_Element
         then
            return Manual_Decision_Evaluation;
         elsif Purpose.Element (3) = Exempt_Branch_Purpose.Last_Element then
            return Exempt_Branch;
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

   ---------------------
   -- Annotation_Kind --
   ---------------------

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
         when Exempt_Region              =>
            return Exempt_Region_Purpose;

         when Exempt_On                  =>
            return Exempt_On_Purpose;

         when Exempt_Off                 =>
            return Exempt_Off_Purpose;

         when Exempt_Decision_Outcome    =>
            return Exempt_Decision_Outcome_Purpose;

         when Exempt_Decision_Condition  =>
            return Exempt_Decision_Condition_Purpose;

         when Exempt_Full_Decision       =>
            return Exempt_Full_Decision_Purpose;

         when Manual_Decision_Evaluation =>
            return Manual_Decision_Evaluation_Purpose;

         when Exempt_Branch              =>
            return Exempt_Branch_Purpose;

         when Dump_Buffers               =>
            return Buffers_Dump_Purpose;

         when Reset_Buffers              =>
            return Buffers_Reset_Purpose;

         when Cov_Off                    =>
            return Cov_Off_Purpose;

         when Cov_On                     =>
            return Cov_On_Purpose;

         when Unknown                    =>
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
   begin
      --  A file the project designates but that does not exist yet is not an
      --  error: that is what a project looks like before its first annotation
      --  is created. A file named on the command line is loaded whatever its
      --  state, since the user asked for that one by name, and a missing one
      --  is then reported as such.

      if Annotations_From_Project
        and then not Ada.Directories.Exists (+Annotation_File)
      then
         return;
      end if;

      declare
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
      end;
   end Load_Ext_Annotations;

   --------------------------------
   -- Import_External_Exemptions --
   --------------------------------

   procedure Import_External_Exemptions
     (FI : Source_File_Index; Filter : Boolean := False)
   is
      File            : Virtual_File;
      Matches         : Match_Result_Vec;
      New_Annotations : ALI_Annotation_Maps.Map;
      New_Exemptions  : Exemption_Request_Maps.Map;

      procedure Process (Match : Match_Result; Kind : Any_Annotation_Kind);
      --  Insert to New_Annotations an annotation of the given kind for a match
      --  result, or emit a warning if it must be discarded.

      -------------
      -- Process --
      -------------

      procedure Process (Match : Match_Result; Kind : Any_Annotation_Kind) is
         use ALI_Annotation_Maps;
         use TOML;

         Details : constant TOML_Value := Match.Annotation;

         Justification : constant Unbounded_String :=
           TOML_Utils.Get_Or_Null (Details, "justification");
         Annot         : ALI_Annotation (Kind);
         Sloc          : constant Slocs.Source_Location :=
           To_Sloc
             ((if Kind = Exempt_On
               then Match.Location.Start_Sloc
               else Match.Location.End_Sloc),
              FI);
         Req           : Exemption_Request;
         Field         : TOML_Value;

         --  Start of processing for Process
      begin
         case Kind is
            when Exempt_Region .. Exempt_Off =>

               if Kind = Exempt_On then
                  Annot.Justification := Justification;
                  Annot.Violation_Count := 0;
                  Annot.Undetermined_Cov_Count := 0;
               end if;

               --  For region-based annotations, check if the new annotations
               --  don't already contain an annotation for this sloc. Fine
               --  grained annotations have their own detection mechanism.

               declare
                  Cur            : Cursor := Get_Annotation (Sloc, Exemption);
                  Existing_Annot : ALI_Annotation;
               begin
                  if not Has_Element (Cur) then
                     Cur := New_Annotations.Find (Sloc);
                  end if;
                  if Has_Element (Cur) then
                     Existing_Annot := Element (Cur);

                     --  Do not warn if the annotation is of the same kind and
                     --  identical message, as this could simply be a case of
                     --  external annotations passed both during
                     --  instrumentation and coverage report computation.
                     --
                     --  Do not check the message for Exempt_Off, as messages
                     --  are irrelevant for them.

                     if Existing_Annot.Kind /= Annot.Kind
                       or else
                         (Kind = Exempt_On
                          and then
                            Existing_Annot.Justification
                            /= Annot.Justification)
                     then
                        Warn
                          (Slocs.Image (Sloc)
                           & ": Conflicting annotations for this line,"
                           & " ignoring the external annotation """
                           & (+Match.Identifier)
                           & """");
                     end if;
                     return;
                  end if;

                  if Kind = Exempt_On and then Filter then
                     declare
                        SCO : constant SCO_Id := Sloc_Intersects_SCO (Sloc);
                     begin
                        if SCO /= No_SCO_Id then
                           Warn
                             ("Exemption annotation at "
                              & Slocs.Image (Sloc)
                              & " intersects a coverage obligation ("
                              & Image (SCO, True)
                              & "), ignoring it");
                           return;
                        end if;
                     end;
                  end if;
               end;

            when Exempt_Decision_Outcome     =>
               Req := (Kind => Decision_Outcome, Sloc => Sloc, others => <>);

               Field := Details.Get ("outcome");
               Req.Outcome := Field.As_Boolean;

               Field := Details.Get_Or_Null ("decision");
               Req.Decision_Offset :=
                 (if Field.Is_Null then 0 else Natural (Field.As_Integer));

            when Exempt_Decision_Condition   =>
               Req := (Kind => Decision_Condition, Sloc => Sloc, others => <>);

               Field := Details.Get ("condition");
               Req.Condition := Condition_Index (Field.As_Integer);

               Field := Details.Get_Or_Null ("decision");
               Req.Decision_Offset :=
                 (if Field.Is_Null then 0 else Natural (Field.As_Integer));

            when Exempt_Full_Decision        =>
               Req := (Kind => Full_Decision, Sloc => Sloc, others => <>);

               Field := Details.Get_Or_Null ("decision");
               Req.Decision_Offset :=
                 (if Field.Is_Null then 0 else Natural (Field.As_Integer));

            when Manual_Decision_Evaluation  =>
               Req :=
                 (Kind   => Manual_Decision_Evaluation,
                  Sloc   => Sloc,
                  others => <>);

               Field := Details.Get_Or_Null ("decision");
               Req.Decision_Offset :=
                 (if Field.Is_Null then 0 else Natural (Field.As_Integer));

               Field := Details.Get ("values");
               Req.Condition_Values.Clear;
               for I in 1 .. Field.Length loop
                  Req.Condition_Values.Append
                    (To_Tristate (Field.Item (I).As_Boolean));
               end loop;

            when Exempt_Branch               =>
               Req := (Kind => Branch, Sloc => Sloc);

            when others                      =>
               raise Program_Error with "unreachable code";
         end case;

         case Kind is
            when Fine_Grained_Annotation_Kind =>
               Insert_Fine_Grained_Exemption
                 (New_Exemptions, Req, Justification);

            when Exempt_On | Exempt_Off       =>
               New_Annotations.Insert (Sloc, Annot);

            when others                       =>
               raise Program_Error with "unreachable code";
         end case;
      end Process;

      --  Start of processing for Import_External_Exemptions

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
         if Match.Success then
            case Annotation_Kind (Match.Annotation) is
               when Exempt_On                    =>
                  Process (Match, Exempt_On);

               when Exempt_Off                   =>
                  Process (Match, Exempt_Off);

               when Exempt_Region                =>

                  --  Exempt_Region will insert an Exempt_On / Exempt_Off
                  --  couple of annotations.

                  Process (Match, Exempt_On);
                  Process (Match, Exempt_Off);

               when Fine_Grained_Annotation_Kind =>
                  Process (Match, Annotation_Kind (Match.Annotation));

               when others                       =>
                  null;
            end case;
         else
            Report_Failed (Match);
         end if;
      end loop;
      Set_Annotations (New_Annotations);
      Set_Fine_Grained_Exemptions (New_Exemptions);
   end Import_External_Exemptions;

   ------------------------------------
   -- Import_All_External_Exemptions --
   ------------------------------------

   procedure Import_All_External_Exemptions is
      procedure Process (SFI : Source_File_Index);

      -------------
      -- Process --
      -------------

      procedure Process (SFI : Source_File_Index) is
         FI : constant File_Info_Access := Get_File (SFI);
      begin
         if FI.Kind = Source_File and then FI.Ignore_Status /= Always then
            Import_External_Exemptions (SFI);
         end if;
      end Process;

      --  Start of processing for Import_All_External_Exemptions
   begin
      Files_Table_Iterate (Process'Access);
   end Import_All_External_Exemptions;

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
                   ((Source_File => SFI, L => +Match_Res.Location.Start_Sloc),
                    Disable_Coverage);
            begin
               if Has_Element (Cur) then
                  if Element (Cur).Kind /= Kind then
                     Success := False;
                  elsif Kind = Cov_Off then
                     Success :=
                       Element (Cur).Justification
                       = New_Annotation.Justification;
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

   ------------------------
   -- Default_Identifier --
   ------------------------

   function Default_Identifier
     (Kind : Any_Annotation_Kind; File : Virtual_File; Span : Sloc_Span)
      return Unbounded_String
   is (+Kind_Image (Kind)
       & "-"
       & Hex_Image
           (Unsigned_32
              (Ada.Strings.Hash
                 (File.Display_Full_Name
                  & Slocs.Image (+Span.Start_Sloc)
                  & Slocs.Image (+Span.End_Sloc)))));

   -------------
   -- To_TOML --
   -------------

   function To_TOML
     (Kind         : Any_Annotation_Kind;
      Annot        : ALI_Annotation;
      Insert_After : Boolean := False) return TOML.TOML_Value
   is
      use TOML;

      Result : constant TOML_Value := Create_Table;

      function Decision_Offset return TOML_Value
      is (Create_Integer (Any_Integer (Annot.Exemption_Req.Decision_Offset)));
   begin
      Result.Set ("purpose", Create_String (To_Ada (Purpose (Kind))));

      case Kind is
         when Exempt_On
            | Exempt_Region
            | Fine_Grained_Annotation_Kind
            | Cov_Off                      =>
            Result.Set ("justification", Create_String (Annot.Justification));

            case Kind is
               when Exempt_Decision_Outcome    =>
                  Result.Set
                    ("outcome", Create_Boolean (Annot.Exemption_Req.Outcome));
                  Result.Set ("decision", Decision_Offset);

               when Exempt_Decision_Condition  =>
                  Result.Set
                    ("condition",
                     Create_Integer
                       (Any_Integer (Annot.Exemption_Req.Condition)));
                  Result.Set ("decision", Decision_Offset);

               when Exempt_Full_Decision       =>
                  Result.Set ("decision", Decision_Offset);

               when Manual_Decision_Evaluation =>
                  declare
                     Values : constant TOML_Value := Create_Array;
                  begin
                     for V of Annot.Exemption_Req.Condition_Values loop

                        --  Guard against Unknown tristates: they have no
                        --  boolean counterpart, and To_Boolean would raise.

                        Values.Append
                          (Create_Boolean
                             (V in Known_Tristate and then To_Boolean (V)));
                     end loop;
                     Result.Set ("values", Values);
                  end;
                  Result.Set ("decision", Decision_Offset);

               when others                     =>
                  null;
            end case;

         when Dump_Buffers | Reset_Buffers =>
            Result.Set ("insert_after", Create_Boolean (Insert_After));

            if Kind = Dump_Buffers and then Annot.Prefix /= "" then
               Result.Set ("trace_prefix", Create_String (Annot.Prefix));
            end if;

         when Exempt_Off | Cov_On          =>
            null;

         when Unknown                      =>
            raise Program_Error with "Unreachable";
      end case;

      return Result;
   end To_TOML;

   ------------------
   -- Create_Entry --
   ------------------

   procedure Create_Entry
     (DB               : in out Entry_DB;
      Identifier       : Unbounded_String;
      Annotation       : TOML.TOML_Value;
      File             : Virtual_File;
      Span             : Sloc_Span;
      Source           : GPR2.Build.Source.Object;
      Backend          : Unbounded_String := Null_Unbounded_String;
      File_Prefix      : Unbounded_String := Null_Unbounded_String;
      Explicit_Backend : Boolean := False;
      Language         : Any_Language := All_Languages)
   is
      SS_Backend  : Unbounded_String := Backend;
      Prefix      : Unbounded_String := File_Prefix;
      Actual_Lang : Any_Language := Language;
   begin
      --  Determine the backend to be used depending on the language, if not
      --  specified by the caller.

      if US.Length (SS_Backend) = 0 then

         --  If the caller did not tell us and we have a project loaded, use it
         --  to determine the file language.

         if Actual_Lang = All_Languages
           and then Is_Project_Loaded
           and then Source.Is_Defined
         then
            Actual_Lang := To_Language_Or_All (Source.Language);
         end if;

         --  If this failed or we have no project at hand, revert to simple
         --  heuristics.

         if Actual_Lang = All_Languages then
            Actual_Lang := Guess_Language (File);
         end if;

         case Actual_Lang is
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

      --  Compute a file prefix if there isn't one already specified, and we
      --  have a project at hand.

      if US.Length (Prefix) = 0
        and then Is_Project_Loaded
        and then Source.Is_Defined
      then
         case To_Language_Or_All (Source.Language) is
            when Ada_Language =>

               --  Ada source files are guaranteed to be unique in a project,
               --  so use the directory name as file prefix to end-up with only
               --  the base name.

               Prefix := US.To_Unbounded_String (+File.Dir_Name);

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
                    +Create (Relative_Path (File, Prj_Dir)).Dir_Name;
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
                        Prefix := US.To_Unbounded_String (Rel_Path);
                     end if;
                  else
                     Prefix := US.To_Unbounded_String (+File.Dir_Name);
                  end if;
               end;
         end case;
      end if;

      declare
         Diags : constant Load_Diagnostic_Arr :=
           Add_Or_Update_Entry
             (DB          => DB,
              Identifier  => Identifier,
              Annotation  => Annotation,
              Kind        => SS_Backend,
              File        => File,
              Span        => Span,
              File_Prefix => Prefix);
      begin
         --  Do not fallback if we have an explicit backend specified by the
         --  caller, or if the backend is already "absolute"

         if Diags'Length /= 0
           and then SS_Backend /= "absolute"
           and then not Explicit_Backend
         then
            --  Try again, but with the "absolute" backend. If that fails do
            --  not log a warning about the first attempt.

            declare
               Msg       : Unbounded_String :=
                 +("Could not create an auto-relocating annotation for "
                   & File.Display_Full_Name
                   & ":"
                   & Image (Span)
                   & ", creating an absolute location annotation instead.");
               Abs_Diags : constant Load_Diagnostic_Arr :=
                 Add_Or_Update_Entry
                   (DB          => DB,
                    Identifier  => Identifier,
                    Annotation  => Annotation,
                    Kind        => +"absolute",
                    File        => File,
                    Span        => Span,
                    File_Prefix => Prefix);
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
   end Create_Entry;

   ------------------------------
   -- Add_Extracted_Annotation --
   ------------------------------

   procedure Add_Extracted_Annotation
     (DB           : in out Stable_Sloc.Entry_DB;
      Kind         : Any_Annotation_Kind;
      Annot        : ALI_Annotation;
      File         : GNATCOLL.VFS.Virtual_File;
      Span         : Slocs.Local_Source_Location_Range;
      Lang         : Any_Language := All_Languages;
      Insert_After : Boolean := False)
   is
      SS_Span : constant Sloc_Span := (+Span.First_Sloc, +Span.Last_Sloc);
      Source  : GPR2.Build.Source.Object;
   begin
      --  Look up the GPR2 source for File, so that Create_Entry can determine
      --  its language and an adequate file prefix. This is optional: without a
      --  project, Create_Entry falls back on the file extension.

      if Is_Project_Loaded then
         Source :=
           Project.Lookup_Source
             (Create (File.Full_Name, Normalize => True).Display_Full_Name);
      end if;

      Create_Entry
        (DB         => DB,
         Identifier => Default_Identifier (Kind, File, SS_Span),
         Annotation => To_TOML (Kind, Annot, Insert_After),
         File       => File,
         Span       => SS_Span,
         Source     => Source,
         Language   => Lang);
   end Add_Extracted_Annotation;

   -----------------------------
   -- Require_Annotation_File --
   -----------------------------

   procedure Require_Annotation_File
     (Args : Command_Line.Parser.Parsed_Arguments) is
   begin
      if Args.String_List_Args (Opt_Ext_Annotations).Is_Empty then
         Fatal_Error (No_Annotation_File_Error);
      end if;
   end Require_Annotation_File;

   ---------------------------
   -- Annotation_Output_For --
   ---------------------------

   function Annotation_Output_For
     (Args : Command_Line.Parser.Parsed_Arguments; Source : Virtual_File)
      return Virtual_File is
   begin
      --  The command line wins over the project

      if Args.String_Args (Opt_Output).Present then
         return Create (+US.To_String (Args.String_Args (Opt_Output).Value));
      end if;

      if not Project.Is_Project_Loaded then
         Fatal_Error ("Missing --output switch");
      end if;

      --  An annotation belongs with the unit it applies to, so it goes to the
      --  file designated by the project owning that unit rather than to the
      --  root project's.

      declare
         File : constant Unbounded_String :=
           Project.Annotation_File_For (+Source.Full_Name);

         Owner : constant GPR2.Build.Source.Object :=
           Project.Lookup_Source (+Source.Full_Name);
         --  Looked up only to name the project in the diagnostic below
      begin
         if File = Null_Unbounded_String then
            Fatal_Error
              ("the project owning "
               & Source.Display_Base_Name
               & (if Owner.Is_Defined
                  then " (" & String (Owner.Owning_View.Name) & ")"
                  else "")
               & " designates no Coverage'External_Annotations file: add the"
               & " attribute to it, or pass --output");
         end if;
         return Create (+(+File));
      end;
   end Annotation_Output_For;

   --------------------
   -- Output_File_Of --
   --------------------

   function Output_File_Of
     (Args : Command_Line.Parser.Parsed_Arguments) return Virtual_File is
   begin
      if Args.String_Args (Opt_Output).Present then
         return Create (+US.To_String (Args.String_Args (Opt_Output).Value));
      end if;

      --  A project that states where its annotations live also states where
      --  edits to them go, so the first designated file is the default output
      --  of add-annotation and delete-annotation. Only the project is
      --  considered: making --external-annotations imply an output would turn
      --  a read-only switch into an in-place edit.

      if Project.Is_Project_Loaded
        and then Project.External_Annotations /= Null_Unbounded_String
      then
         return Create (+(+Project.External_Annotations));
      end if;

      Fatal_Error
        ("missing --output switch, and the project designates no"
         & " Coverage'External_Annotations file to update");
   end Output_File_Of;

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
      Justification : Unbounded_String;
      Outcome       : Boolean;
      Condition     : Condition_Index;
      Decision      : Natural;
      Values        : Condition_Evaluation_Vectors.Vector;

      function "+" (Opt : Command_Line.String_Options) return Unbounded_String
      is (Parser.Value_Or_Null (Args.String_Args (Opt)));

      function Missing_Switch_Msg (Opt : String_Options) return String
      is (Parser.Option_Name (Arg_Parser, Opt)
          & " missing for a --kind="
          & Kind_Image (Annot_Kind)
          & " annotation");

      function Invalid_Switch_Msg (Opt : String_Options) return String
      is ("Invalid argument for "
          & Parser.Option_Name (Arg_Parser, Opt)
          & ": "
          & (+Args.String_Args (Opt).Value));

      procedure Load_Decision_Offset;
      --  Initialize Decision from --decision

      --------------------------
      -- Load_Decision_Offset --
      --------------------------

      procedure Load_Decision_Offset is
      begin
         if Args.String_Args (Opt_Decision).Present then
            declare
               Decision_Str : constant String :=
                 +Args.String_Args (Opt_Decision).Value;
            begin
               Decision := Natural'Value (Decision_Str);
            exception
               when Constraint_Error =>
                  Fatal_Error (Invalid_Switch_Msg (Opt_Decision));
            end;
         else
            Decision := 0;
         end if;
      end Load_Decision_Offset;

      New_Annot_DB  : Entry_DB;
      Entry_Purpose : Ada_Qualified_Name;
      Entry_Id      : Unbounded_String := +Opt_Annotation_Id;
      SS_Backend    : constant Unbounded_String := +Opt_SS_Backend;
      File_Prefix   : constant Unbounded_String := +Opt_Source_Root;
      Source        : GPR2.Build.Source.Object;

      --  Start of processing for Add_Annotation
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

      --  Determine the file to write the amended annotations to

      Output_File := Annotation_Output_For (Args, Target_File);

      --  Validate the arguments depending on the requested annotation kind

      case Annot_Kind is
         when Exempt_Region                                      =>
            Start_Sloc :=
              Get_Or_Error (+(+Opt_Start_Location), "--start-location");
            End_Sloc := Get_Or_Error (+(+Opt_End_Location), "--end-location");

            if not Args.String_Args (Opt_Justification).Present then
               Warn (Missing_Switch_Msg (Opt_Justification));
            end if;
            Justification :=
              Parser.Value_Or_Null (Args.String_Args (Opt_Justification));

         when Exempt_On
            | Exempt_Decision_Outcome
            | Exempt_Decision_Condition
            | Exempt_Full_Decision
            | Manual_Decision_Evaluation
            | Exempt_Branch
            | Cov_Off                                            =>

            --  Accept either the --location or --start-location switches

            if Args.String_Args (Opt_Location).Present then
               Start_Sloc := Get_Or_Error (+(+Opt_Location), "--location");
            else
               Start_Sloc :=
                 Get_Or_Error (+(+Opt_Start_Location), "--start-location");
            end if;

            if not Args.String_Args (Opt_Justification).Present then
               Warn (Missing_Switch_Msg (Opt_Justification));
            end if;
            Justification :=
              Parser.Value_Or_Null (Args.String_Args (Opt_Justification));
            End_Sloc := Start_Sloc;

            if Annot_Kind = Exempt_Decision_Outcome then
               if Args.String_Args (Opt_Outcome).Present then
                  declare
                     Outcome_Str : constant String :=
                       +Args.String_Args (Opt_Outcome).Value;
                  begin
                     Outcome := Boolean'Value (Outcome_Str);
                  exception
                     when Constraint_Error =>
                        Fatal_Error (Invalid_Switch_Msg (Opt_Outcome));
                  end;
               else
                  Fatal_Error (Missing_Switch_Msg (Opt_Outcome));
               end if;

               Load_Decision_Offset;

            elsif Annot_Kind = Exempt_Decision_Condition then
               if Args.String_Args (Opt_Condition).Present then
                  declare
                     Condition_Str : constant String :=
                       +Args.String_Args (Opt_Condition).Value;
                  begin
                     Condition := Condition_Index'Value (Condition_Str) - 1;
                  exception
                     when Constraint_Error =>
                        Fatal_Error (Invalid_Switch_Msg (Opt_Condition));
                  end;
               else
                  Fatal_Error (Missing_Switch_Msg (Opt_Condition));
               end if;

               Load_Decision_Offset;

            elsif Annot_Kind = Exempt_Full_Decision then
               Load_Decision_Offset;

            elsif Annot_Kind = Manual_Decision_Evaluation then
               if Args.String_Args (Opt_Values).Present then
                  for C of "+" (Args.String_Args (Opt_Values).Value) loop
                     if C = 'T' then
                        Values.Append (SC_Obligations.True);
                     elsif C = 'F' then
                        Values.Append (SC_Obligations.False);
                     else
                        Fatal_Error (Invalid_Switch_Msg (Opt_Values));
                     end if;
                  end loop;
               else
                  Fatal_Error (Missing_Switch_Msg (Opt_Values));
               end if;
               Load_Decision_Offset;
            end if;

         when Exempt_Off | Dump_Buffers | Reset_Buffers | Cov_On =>

            --  Accept either the --location or --start-location switches

            if Args.String_Args (Opt_Location).Present then
               Start_Sloc := Get_Or_Error (+(+Opt_Location), "--location");
            else
               Start_Sloc :=
                 Get_Or_Error (+(+Opt_Start_Location), "--start-location");
            end if;
            End_Sloc := Start_Sloc;

         when Unknown                                            =>
            raise Program_Error with "Unreachable";
      end case;

      --  Generate the annotation in isolation and report if there are any
      --  issues.
      --
      --  Gather the kind-specific details from the command line into an
      --  ALI_Annotation, so that the mapping from annotation details to their
      --  TOML representation lives in a single place (To_TOML), shared with
      --  the extract-annotations command.

      declare
         Target_Span : constant Sloc_Span := (+Start_Sloc, +End_Sloc);

         function Annotation return TOML_Value;
         --  Build the TOML description for the requested annotation

         ----------------
         -- Annotation --
         ----------------

         function Annotation return TOML_Value is

            --  ALI_Annotation designates a single source location, so it
            --  cannot represent a whole region: describe Exempt_Region through
            --  its Exempt_On counterpart, from which To_TOML only needs the
            --  justification.

            Annot_Repr : constant Src_Annotation_Kind :=
              (if Annot_Kind = Exempt_Region then Exempt_On else Annot_Kind);
            Annot      : ALI_Annotation (Annot_Repr);
         begin
            case Annot_Repr is
               when Exempt_On | Exempt_Off | Cov_Off | Cov_On =>
                  Annot.Justification := Justification;

               when Fine_Grained_Annotation_Kind              =>
                  Annot.Justification := Justification;

                  case Fine_Grained_Annotation_Kind'(Annot_Repr) is
                     when Exempt_Decision_Outcome    =>
                        Annot.Exemption_Req :=
                          (Kind            => Decision_Outcome,
                           Sloc            => Slocs.No_Location,
                           Decision_Offset => Decision,
                           Outcome         => Outcome);

                     when Exempt_Decision_Condition  =>
                        Annot.Exemption_Req :=
                          (Kind            => Decision_Condition,
                           Sloc            => Slocs.No_Location,
                           Decision_Offset => Decision,
                           Condition       => Condition);

                     when Exempt_Full_Decision       =>
                        Annot.Exemption_Req :=
                          (Kind            => Full_Decision,
                           Sloc            => Slocs.No_Location,
                           Decision_Offset => Decision);

                     when Manual_Decision_Evaluation =>
                        Annot.Exemption_Req :=
                          (Kind             => Manual_Decision_Evaluation,
                           Sloc             => Slocs.No_Location,
                           Decision_Offset  => Decision,
                           Condition_Values => Values);

                     when Exempt_Branch              =>
                        Annot.Exemption_Req :=
                          (Kind => Branch, Sloc => Slocs.No_Location);
                  end case;

               when Dump_Buffers                              =>
                  if Args.String_Args (Opt_Dump_Filename_Prefix).Present then
                     Annot.Prefix :=
                       Args.String_Args (Opt_Dump_Filename_Prefix).Value;
                  end if;

               when Reset_Buffers                             =>
                  null;
            end case;

            return
              To_TOML
                (Kind         => Annot_Kind,
                 Annot        => Annot,
                 Insert_After => Args.Bool_Args (Opt_Annotate_After));
         end Annotation;

         --  Start of processing for the entry creation

      begin
         --  Generate an entry identifier if not specified, from the annotation
         --  kind and original source location range.

         if US.Length (Entry_Id) = 0 then
            Entry_Id :=
              Default_Identifier (Annot_Kind, Target_File, Target_Span);
         end if;

         Entry_Purpose := Purpose (Annot_Kind);

         Create_Entry
           (DB               => New_Annot_DB,
            Identifier       => Entry_Id,
            Annotation       => Annotation,
            File             => Target_File,
            Span             => Target_Span,
            Source           => Source,
            Backend          => SS_Backend,
            File_Prefix      => File_Prefix,
            Explicit_Backend => Args.String_Args (Opt_SS_Backend).Present);
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
      --  Rewrite only the file the annotations came from. An explicit
      --  --output keeps the older behaviour of writing back everything that
      --  was loaded, which is how several files are deliberately combined.

      Write_Entries
        (Ext_Annotation_DB,
         Output_File,
         Origin =>
           (if Args.String_Args (Opt_Output).Present
            then No_File
            else Output_File));
   end Add_Annotation;

   -----------------------
   -- Delete_Annotation --
   -----------------------

   procedure Delete_Annotation (Args : Command_Line.Parser.Parsed_Arguments) is
      Output_File : Virtual_File;
      Identifier  : Unbounded_String;
   begin
      --  Require an external annotation file. They have already been loaded
      --  if present, but we still need to check.

      Require_Annotation_File (Args);

      --  Require an entry identifier

      if not Args.String_Args (Opt_Annotation_Id).Present then
         Fatal_Error ("Missing --annotation-id switch");
      else
         Identifier := Args.String_Args (Opt_Annotation_Id).Value;
      end if;

      --  Check whether there actually is an entry associated with Identifier,
      --  and determine the file to store the amended entries in: the file the
      --  entry was loaded from, so that deleting an annotation read from
      --  another project rewrites that project's file rather than collapsing
      --  everything into one.

      declare
         Entr : constant Entry_View :=
           Query_Entry (Ext_Annotation_DB, Identifier);
      begin
         if Entr = No_Entry_View then
            if not Switches.Force then
               Fatal_Error
                 ("No annotation associated with identifier " & (+Identifier));
            end if;
            Output_File := Output_File_Of (Args);

         elsif Args.String_Args (Opt_Output).Present then

            --  The command line wins over the project

            Output_File := Output_File_Of (Args);
         else
            Output_File := Entr.Origin;
         end if;
      end;

      --  Delete the entry and write the remaining entries back to file

      Delete_Entry (Ext_Annotation_DB, Identifier);
      --  Rewrite only the file the annotations came from. An explicit
      --  --output keeps the older behaviour of writing back everything that
      --  was loaded, which is how several files are deliberately combined.

      Write_Entries
        (Ext_Annotation_DB,
         Output_File,
         Origin =>
           (if Args.String_Args (Opt_Output).Present
            then No_File
            else Output_File));
   end Delete_Annotation;

   ----------------------
   -- Show_Annotations --
   ----------------------

   procedure Show_Annotations (Args : Command_Line.Parser.Parsed_Arguments) is
      Purpose_Filter : Unbounded_String;
      Match_Results  : Match_Result_Vec;
      As_JSON        : Boolean := False;

      procedure Emit (Code : String; Message : String);
      --  Write the report where --output designates, or to standard output
      --  when it designates nothing. Code and Message say whether there is
      --  anything to report; only the JSON form carries them.

      procedure Fail (Code : String; Message : String)
      with No_Return;
      --  Emit a report saying why there is nothing to show, then stop with
      --  Message as any other fatal error would.
      --
      --  The exit status stays non-zero, so a client keeps using it to tell a
      --  failure from a success and reads Code only to tell the failures
      --  apart. Codes are "not_configured" when nothing designates an
      --  annotation file, and "invalid_command_line" when the invocation
      --  itself is wrong; a successful run reports "ok". Failures found before
      --  the format is known, or outside this command, keep to a diagnostic on
      --  standard error with no report at all.

      ----------
      -- Emit --
      ----------

      procedure Emit (Code : String; Message : String) is
         Report : Ada.Text_IO.File_Type;

         procedure Print;
         --  Print the report in the requested format, to the current output

         procedure Print is
         begin
            if As_JSON then
               Print_Annotations_JSON (Args, Match_Results, Code, Message);
            else
               Print_Annotations_Text (Match_Results);
            end if;
         end Print;

      begin
         if Args.String_Args (Opt_Output).Present then

            --  Both printers write to the current output, so redirecting it
            --  covers either format without them having to know where the
            --  report goes.
            --
            --  A report on its own file is what lets a consumer parse the
            --  JSON: standard output also carries whatever gnatcov has to say,
            --  and a warning landing in the middle of the document is a parse
            --  error rather than a diagnostic.

            Ada.Text_IO.Create
              (Report,
               Ada.Text_IO.Out_File,
               US.To_String (Args.String_Args (Opt_Output).Value));
            Ada.Text_IO.Set_Output (Report);

            Print;

            Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
            Ada.Text_IO.Close (Report);

         else
            Print;
         end if;

      exception
         when others =>

            --  Leaving the output redirected would swallow whatever gnatcov
            --  prints next, the error being propagated included.

            Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
            raise;
      end Emit;

      ----------
      -- Fail --
      ----------

      procedure Fail (Code : String; Message : String) is
      begin
         Emit (Code, Message);
         Fatal_Error (Message);
      end Fail;

   begin
      --  Decode the output format

      if Args.String_Args (Opt_Show_Format).Present then
         declare
            Format : constant String :=
              +Args.String_Args (Opt_Show_Format).Value;
         begin
            if Format = "json" then
               As_JSON := True;
            elsif Format /= "text" then
               Fatal_Error
                 ("Unknown output format (--format): """
                  & Format
                  & """, must be one of text, json");
            end if;
         end;
      end if;

      --  Require an external annotation file. They have already been loaded
      --  if present, but we still need to check.

      if Args.String_List_Args (Opt_Ext_Annotations).Is_Empty then
         Fail ("not_configured", No_Annotation_File_Error);
      end if;

      --  Require either a project or some files on the command line

      if not Project.Is_Project_Loaded and then Args.Remaining_Args.Is_Empty
      then
         Fail
           ("invalid_command_line", "Missing -P switch or positional FILES");
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
               Fail
                 ("invalid_command_line",
                  "Unknown annotation kind (--kind): """
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

      declare
         procedure Get_Matches (Source_Files : String_Vectors.Vector);
         --  Set Match_Results to all DB entries that match the given source
         --  files.

         -----------------
         -- Get_Matches --
         -----------------

         procedure Get_Matches (Source_Files : String_Vectors.Vector) is
            Files : File_Array_Access;
         begin
            Files := new File_Array (1 .. Source_Files.Last_Index);
            for Cur in Source_Files.Iterate loop
               Files.all (String_Vectors.To_Index (Cur)) :=
                 Create (+(+String_Vectors.Element (Cur)));
            end loop;
            Match_Results :=
              Match_Entries (Files.all, Valid_Annotation_DB, +Purpose_Filter);
            GNATCOLL.VFS.Unchecked_Free (Files);
         end Get_Matches;
      begin
         if Args.Remaining_Args.Is_Empty then
            declare
               Source_Files : String_Vectors.Vector;

               procedure Add_File
                 (Project : GPR2.Project.View.Object;
                  File    : GPR2.Build.Source.Object);
               --  Callback for Enumerate_Sources: append File to Source_Files

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
               Get_Matches (Source_Files);
            end;
         else
            Get_Matches (Args.Remaining_Args);
         end if;
      end;

      --  Post-process the match results and display the annotations

      Sort (Match_Results);
      Emit ("ok", "");
   end Show_Annotations;

   ----------------------------
   -- Print_Annotations_Text --
   ----------------------------

   procedure Print_Annotations_Text (Results : Match_Result_Vec) is
      use TOML;
      Current_File : Virtual_File;
   begin
      for Match of Results loop
         if Match.File /= Current_File then
            Current_File := Match.File;
            if Current_File /= No_File then
               Ada.Text_IO.New_Line;
            end if;
            --  Report the full name: a base name does not designate a
            --  file, since several source directories may hold the same
            --  one, and gnatcov is also used without a project, where
            --  there is nothing to make a name relative to.

            Ada.Text_IO.Put_Line (Current_File.Display_Full_Name & ":");
         end if;
         declare
            Annot_Kind : constant Any_Annotation_Kind :=
              Annotation_Kind (Match.Annotation);

            procedure Process_Decision_Offset;
            --  Common helper to dump the "decision" annotation field

            -----------------------------
            -- Process_Decision_Offset --
            -----------------------------

            procedure Process_Decision_Offset is
               Offset : constant TOML_Value :=
                 Match.Annotation.Get_Or_Null ("decision");
            begin
               if not Offset.Is_Null and then Offset.As_Integer /= 0 then
                  Ada.Text_IO.Put
                    ("; Decision:" & Any_Integer'Image (Offset.As_Integer));
               end if;
            end Process_Decision_Offset;

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
               when Exempt_On
                  | Exempt_Region
                  | Exempt_Decision_Outcome
                  | Exempt_Decision_Condition
                  | Exempt_Full_Decision
                  | Manual_Decision_Evaluation
                  | Exempt_Branch
                  | Cov_Off                       =>
                  if Annot_Kind = Exempt_Decision_Outcome then
                     declare
                        Outcome : constant TOML_Value :=
                          Match.Annotation.Get ("outcome");
                     begin
                        Ada.Text_IO.Put
                          ("; Outcome: " & Boolean'Image (Outcome.As_Boolean));
                        Process_Decision_Offset;
                     end;
                  elsif Annot_Kind = Exempt_Decision_Condition then
                     declare
                        Condition : constant TOML_Value :=
                          Match.Annotation.Get ("condition");
                     begin
                        Ada.Text_IO.Put
                          ("; Condition: "
                           & Img (Natural (Condition.As_Integer) + 1));
                        Process_Decision_Offset;
                     end;
                  elsif Annot_Kind = Exempt_Full_Decision then
                     Process_Decision_Offset;
                  elsif Annot_Kind = Manual_Decision_Evaluation then
                     declare
                        Values : constant TOML_Value :=
                          Match.Annotation.Get ("values");
                     begin
                        Ada.Text_IO.Put ("; Values: ");
                        for I in 1 .. Values.Length loop
                           Ada.Text_IO.Put
                             (if Values.Item (I).As_Boolean then 'T' else 'F');
                        end loop;
                     end;
                     Process_Decision_Offset;
                  end if;
                  Ada.Text_IO.Put
                    ("; Justification: "
                     & (+Get_Or_Null (Match.Annotation, "justification")));

               when Dump_Buffers | Reset_Buffers  =>
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
                        & (+Get_Or_Null (Match.Annotation, "trace_prefix")));
                  end if;

               when Unknown | Exempt_Off | Cov_On =>
                  null;
            end case;
            if not Match.Success then
               Ada.Text_IO.Put ("; diagnostic: " & (+Match.Diagnostic));
            end if;
         end;
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Annotations_Text;

   ----------------------------
   -- Print_Annotations_JSON --
   ----------------------------

   procedure Print_Annotations_JSON
     (Args    : Command_Line.Parser.Parsed_Arguments;
      Results : Match_Result_Vec;
      Code    : String;
      Message : String)
   is
      use GNATCOLL.JSON;
      use TOML;

      Root   : constant JSON_Value := Create_Object;
      Files  : JSON_Array := Empty_Array;
      Annots : JSON_Array := Empty_Array;

      function To_JSON (Match : Match_Result) return JSON_Value;
      --  Structured form of one match result

      -------------
      -- To_JSON --
      -------------

      function To_JSON (Match : Match_Result) return JSON_Value is
         Annot_Kind : constant Any_Annotation_Kind :=
           Annotation_Kind (Match.Annotation);
         Res        : constant JSON_Value := Create_Object;

         procedure Set_Decision_Offset;
         --  Common helper to set the "decision" annotation field

         -------------------------
         -- Set_Decision_Offset --
         -------------------------

         procedure Set_Decision_Offset is
            Offset : constant TOML_Value :=
              Match.Annotation.Get_Or_Null ("decision");
         begin
            if not Offset.Is_Null and then Offset.As_Integer /= 0 then
               Res.Set_Field ("decision", Integer (Offset.As_Integer));
            end if;
         end Set_Decision_Offset;

      begin
         Res.Set_Field ("file", Match.File.Display_Full_Name);
         Res.Set_Field ("id", +Match.Identifier);
         Res.Set_Field ("kind", Kind_Image (Annot_Kind));
         Res.Set_Field ("stale", not Match.Success);

         if Match.Success then
            declare
               Loc : constant JSON_Value := Create_Object;
            begin
               Loc.Set_Field ("start_line", Match.Location.Start_Sloc.Line);
               Loc.Set_Field
                 ("start_column", Match.Location.Start_Sloc.Column);
               Loc.Set_Field ("end_line", Match.Location.End_Sloc.Line);
               Loc.Set_Field ("end_column", Match.Location.End_Sloc.Column);
               Res.Set_Field ("location", Loc);
            end;
         else
            Res.Set_Field ("diagnostic", +Match.Diagnostic);
         end if;

         case Annot_Kind is
            when Exempt_On
               | Exempt_Region
               | Exempt_Decision_Outcome
               | Exempt_Decision_Condition
               | Exempt_Full_Decision
               | Manual_Decision_Evaluation
               | Exempt_Branch
               | Cov_Off                       =>
               if Annot_Kind = Exempt_Decision_Outcome then
                  Res.Set_Field
                    ("outcome", Match.Annotation.Get ("outcome").As_Boolean);
                  Set_Decision_Offset;
               elsif Annot_Kind = Exempt_Decision_Condition then

                  --  Condition indices are stored 0-based and reported
                  --  1-based, as in the text form.

                  Res.Set_Field
                    ("condition",
                     Natural (Match.Annotation.Get ("condition").As_Integer)
                     + 1);
                  Set_Decision_Offset;
               elsif Annot_Kind = Exempt_Full_Decision then
                  Set_Decision_Offset;
               elsif Annot_Kind = Manual_Decision_Evaluation then
                  declare
                     Values : constant TOML_Value :=
                       Match.Annotation.Get ("values");
                     Arr    : JSON_Array := Empty_Array;
                  begin
                     for I in 1 .. Values.Length loop
                        Append (Arr, Create (Values.Item (I).As_Boolean));
                     end loop;
                     Res.Set_Field ("values", Arr);
                  end;
                  Set_Decision_Offset;
               end if;
               Res.Set_Field
                 ("justification",
                  +Get_Or_Null (Match.Annotation, "justification"));

            when Dump_Buffers | Reset_Buffers  =>
               Res.Set_Field
                 ("insert_after",
                  Get_Or_Default (Match.Annotation, "insert_after", False));
               if Annot_Kind = Dump_Buffers
                 and then Match.Annotation.Has ("trace_prefix")
               then
                  Res.Set_Field
                    ("trace_prefix",
                     +Get_Or_Null (Match.Annotation, "trace_prefix"));
               end if;

            when Unknown | Exempt_Off | Cov_On =>
               null;
         end case;

         return Res;
      end To_JSON;

   begin
      --  Report the annotation files in effect, including one the project
      --  designates but that does not exist yet: a client watching them for
      --  changes needs to know about the file a first annotation will create.

      for File of Args.String_List_Args (Opt_Ext_Annotations) loop
         Append (Files, Create (+File));
      end loop;

      for Match of Results loop
         Append (Annots, To_JSON (Match));
      end loop;

      Root.Set_Field ("code", Code);
      Root.Set_Field ("message", Message);
      Root.Set_Field ("annotation_files", Files);
      Root.Set_Field ("annotations", Annots);

      Ada.Text_IO.Put_Line (Write (Root, Compact => False));
   end Print_Annotations_JSON;

   --------------------------
   -- Validate_Annotations --
   --------------------------

   procedure Validate_Annotations is
   begin
      Clear_DB (Valid_Annotation_DB);
      Iterate_Entries (Ext_Annotation_DB, Validate_Annotation'Access);
   end Validate_Annotations;

   -------------------------
   -- Validate_Annotation --
   -------------------------

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

            procedure Validate_Decision_Offset;
            --  Common helper to validate the "decision" annotation field

            function Valid_Values (Values : TOML_Value) return Boolean;
            --  Return whether Values is a valid condition evaluation array
            --  (i.e. an array of booleans with at least one element).

            ------------------------------
            -- Validate_Decision_Offset --
            ------------------------------

            procedure Validate_Decision_Offset is
            begin
               if not Annot.Has ("decision") then
                  return;
               end if;

               if Annot.Get ("decision").Kind /= TOML_Integer
                 or else
                   Annot.Get ("decision").As_Integer
                   not in 0 .. Any_Integer (Natural'Last)
               then
                  Warn
                    ("Invalid decision offset for external exemption"
                     & " annotation """
                     & (+Identifier)
                     & """, it will be ignored.");
                  All_Ok := False;
               end if;
            end Validate_Decision_Offset;

            ------------------
            -- Valid_Values --
            ------------------

            function Valid_Values (Values : TOML_Value) return Boolean is
            begin
               if Values.Kind /= TOML_Array then
                  return False;
               end if;

               if Values.Length = 0 then
                  return False;
               end if;

               for I in 1 .. Values.Length loop
                  if Values.Item (I).Kind /= TOML_Boolean then
                     return False;
                  end if;
               end loop;
               return True;
            end Valid_Values;

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

               when Exempt_On
                  | Exempt_Region
                  | Exempt_Decision_Outcome
                  | Exempt_Decision_Condition
                  | Exempt_Full_Decision
                  | Manual_Decision_Evaluation
                  | Exempt_Branch                 =>
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

                  if Annot_Kind = Exempt_Decision_Outcome then
                     if not Annot.Has ("outcome")
                       or else Annot.Get ("outcome").Kind /= TOML_Boolean
                     then
                        Warn
                          ("Missing or invalid outcome for external exemption"
                           & " annotation """
                           & (+Identifier)
                           & """, it will be ignored.");
                        All_Ok := False;
                     end if;
                     Validate_Decision_Offset;

                  elsif Annot_Kind = Exempt_Decision_Condition then
                     if not Annot.Has ("condition")
                       or else Annot.Get ("condition").Kind /= TOML_Integer
                       or else
                         Annot.Get ("condition").As_Integer
                         not in Any_Integer (Condition_Index'First)
                              .. Any_Integer (Condition_Index'Last)
                     then
                        Warn
                          ("Missing or invalid condition for external"
                           & " exemption annotation """
                           & (+Identifier)
                           & """, it will be ignored.");
                        All_Ok := False;
                     end if;
                     Validate_Decision_Offset;

                  elsif Annot_Kind = Exempt_Full_Decision then
                     Validate_Decision_Offset;

                  elsif Annot_Kind = Manual_Decision_Evaluation then
                     if not Annot.Has ("values")
                       or else not Valid_Values (Annot.Get ("values"))
                     then
                        Warn
                          ("Missing or invalid values for external"
                           & " exemption annotation """
                           & (+Identifier)
                           & """, it will be ignored.");
                        All_Ok := False;
                     end if;
                     Validate_Decision_Offset;
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
