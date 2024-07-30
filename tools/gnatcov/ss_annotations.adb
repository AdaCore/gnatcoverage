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

with GNATCOLL.VFS; use GNATCOLL.VFS;

with TOML;

with Stable_Sloc;            use Stable_Sloc;
with Stable_Sloc.TOML_Utils; use Stable_Sloc.TOML_Utils;

with Files_Table;    use Files_Table;
with Instrument;     use Instrument;
with Outputs;        use Outputs;

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
   Cov_On_Purpose : constant Ada_Qualified_Name :=
     Coverage_Namespace & To_Qualified_Name ("on");

   function Annotation_Kind
     (Match : Match_Result) return Any_Annotation_Kind;
   --  Convert the purpose string in Match.Annotation into one of the
   --  recognized annotation kinds, based on the purpose strings defined above.
   --
   --  Return Unknown if there is no "purpose" field in the annotation, or if
   --  it is empty or unknown.

   procedure Report_Failed (Match : Match_Result) with
     Pre => not Match.Success;
   --  Report the diagnostics for Match

   function "+"
     (Sloc : TOML.Source_Location) return Slocs.Local_Source_Location is
     (Line => Sloc.Line, Column => Sloc.Column) with Unreferenced;

   ---------------------
   -- Annotation_Kind --
   ---------------------

   function Annotation_Kind
     (Match : Match_Result) return Any_Annotation_Kind
   is
      Purpose : constant Ada_Qualified_Name :=
        To_Qualified_Name
          (+Get_Or_Null (Match.Annotation, "purpose"));

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

   -------------------
   -- Report_Failed --
   -------------------

   procedure Report_Failed (Match : Match_Result) is
   begin
      Warn
        (Match.File.Display_Full_Name & ": " & (+Match.Identifier) & ": "
         & (+Match.Diagnostic));
   end Report_Failed;

   --------------------------
   -- Load_Ext_Annotations --
   --------------------------

   procedure Load_Ext_Annotations (Annotation_File : Unbounded_String) is
      Load_Diags : constant Load_Diagnostic_Arr := Load_Entries
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

   procedure Import_External_Exemptions (FI : Source_File_Index)
   is
      File                 : Virtual_File;
      Matches              : Match_Result_Vec;
      Justification        : Unbounded_String;
      Kind                 : Any_Annotation_Kind;
      New_Annotations      : ALI_Annotation_Maps.Map;
   begin
      --  Exit early if there are no external annotations

      if Is_Empty (Ext_Annotation_DB) then
         return;
      end if;

      --  Match the entries on FI

      File := Create (+Get_Full_Name (FI));
      Matches := Match_Entries
        ((1 => File),
         Ext_Annotation_DB,
         Purpose_Prefix => To_Ada (Exemption_Namespace));

      --  Process each match result

      for Match of Matches loop
         Kind := Annotation_Kind (Match);

         --  Filter unknown annotations first

         if Kind not in Exempt_Region .. Exempt_Off then
            Warn
               ("Unexpected or unknown annotation kind for annotation """
               & (+Match.Identifier) & """: "
               & (+Get_Or_Null (Match.Annotation, "purpose")));

         --  Then failed matches

         elsif not Match.Success then
            Report_Failed (Match);
         else
            --  Exempt_Region will insert an Exempt_On / Exempt_Off couple of
            --  annotations.

            if Kind in Exempt_Region | Exempt_On then
               Justification := TOML_Utils.Get_Or_Null
                 (Match.Annotation, "justification");
               if Justification = Null_Unbounded_String then
                  Warn
                    (Slocs.Image (To_Sloc (Match.Location.Start_Sloc, FI))
                     & ": Missing or empty justification for external"
                     & " exemption annotation """ & (+Match.Identifier)
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
                              & (+Match.Identifier) & """");
                        end if;
                     else
                        New_Annotations.Insert (Sloc, Annot);
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
                           & (+Match.Identifier) & """");
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

   ----------------------------
   -- Get_Buffer_Annotations --
   ----------------------------

   function Get_Buffer_Annotations
     (Filename : String) return Instrument.Common.Instr_Annotation_Map
   is
      use Instrument.Common;
      VF      : constant Virtual_File := Create (+Filename);
      Matches : Match_Result_Vec;
      Kind    : Any_Annotation_Kind;
      Result  : Instr_Annotation_Map;
   begin
      --  Exit early if there are no external annotations

      if Is_Empty (Ext_Annotation_DB) then
         return Instr_Annotation_Maps.Empty_Map;
      end if;

      Matches := Match_Entries
        ((1 => VF),
         Ext_Annotation_DB,
         Purpose_Prefix => To_Ada (Buffers_Namespace));

      --  Process each annotation result

      for Match of Matches loop
         if not Match.Success then
            Report_Failed (Match);
            goto Continue;
         end if;

         Kind := Annotation_Kind (Match);
         if Kind not in Dump_Buffers | Reset_Buffers then
            Warn
               ("Unexpected or unknown annotation kind for annotation """
               & (+Match.Identifier) & """: "
               & (+Get_Or_Null (Match.Annotation, "purpose")));
            goto Continue;
         end if;
         declare
            Start_Loc : constant Slocs.Local_Source_Location :=
              +Match.Location.Start_Sloc;
            --  We only use the start location to represent buffer dump / reset
            --  annotations, as a range is ill-defined for those.

            New_Annotation : Instr_Annotation (Kind);
            Cur            : Instr_Annotation_Maps.Cursor;
            Inserted       : Boolean;
         begin
            --  Explicitly inspect each annotation kind to get a warning if a
            --  new annotation kind is added in Instrument.Common but not here.

            case New_Annotation.Kind is
               when Dump_Buffers =>
                  New_Annotation.Trace_Prefix :=
                    Get_Or_Null (Match.Annotation, "trace_prefix");

               when Reset_Buffers =>
                  null;

               when Cov_On | Cov_Off =>
                  raise Program_Error with "unreachable";
            end case;
            New_Annotation.Insert_After := False;
            declare
               use TOML;
            begin
               if Match.Annotation.Has ("insert_after") then
                  if Match.Annotation.Get ("insert_after").Kind /= TOML_Boolean
                  then
                     Warn
                       ("Invalid type for ""insert_after"" field in annotation"
                        & """" & (+Match.Identifier) & """, should be"
                        & " TOML_BOOLEAN.");
                  else
                     New_Annotation.Insert_After :=
                       Match.Annotation.Get ("insert_after").As_Boolean;
                  end if;
               end if;
            end;
            Result.Insert (Start_Loc, New_Annotation, Cur, Inserted);

            --  Tolerate duplicate annotations if they are the same

            if not Inserted and then Result.Reference (Cur) /= New_Annotation
            then
               Warn
                 (Ada.Directories.Simple_Name (Filename) & ":"
                  & Slocs.Image (Start_Loc)
                  & ": Conflicting annotations for this line, ignoring the"
                  & " external annotation """ & (+Match.Identifier) & """");
            end if;
         end;
         <<Continue>>
      end loop;

      return Result;
   end Get_Buffer_Annotations;

end SS_Annotations;
