------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;

with Coverage; use Coverage;
with Project;
with Outputs;  use Outputs;

--  This package generates one coverage statistics report per enabled coverage
--  level and one for the coverage stats of lines. If multiple report formats
--  are to be generated, these stats are created in a "stats" subdirectory.

package body Annotations.Index is

   subtype Index_Cov_States is Any_Line_State range Not_Covered .. Covered;
   subtype Percentage is Natural range 0 .. 100;

   type Stats_Per_State is array (Index_Cov_States) of Natural
   with Default_Component_Value => 0;
   --  Array of coverage percentage per coverage state

   type Project_Stats is record
      Total : Natural := 0;
      --  Total number of lines/obligations to be covered

      Stats : Stats_Per_State;
      --  Array of coverage percentage per state

      Files_Stats : Strings.Unbounded_String;
      --  String expressing the coverage stats of each of the project's units
      --  of interest.

   end record;
   --  Record storing a project's coverage information

   package Projects_Stats_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => Project_Stats);
   --  Map associating the project's name to its coverage statistics

   function "+" (P : Percentage) return String;
   --  Return a string representation of P that is always three characters long
   --  by replacing the missing digits by spaces.

   function Build_Stat_String
     (Name   : String;
      Stats  : Stats_Per_State;
      Total  : Natural;
      Lines  : Boolean;
      Is_Prj : Boolean := False) return String;
   --  Return a formated string expressing the coverage stats

   function Compute_Percentage
     (Stats : Stats_Per_State; Total : Positive) return Stats_Per_State;
   --  Return an array of coverage percentage, dividing each value of Stats by
   --  Total.

   procedure Generate_Index (L : Coverage_Level := Stmt; Lines : Boolean);
   --  Generate the coverage index file for coverage level L, or for lines if
   --  Lines is set to True.

   ----------------------
   -- Generate_Indices --
   ----------------------

   procedure Generate_Indices is
      Output_Dir    : constant String := Get_Output_Dir;
      Output_Subdir : constant String :=
        Output_Dir & "/stats" & GNAT.OS_Lib.Directory_Separator;
   begin
      --  Create a "stats" directory where the "<root-prj-name>-<metric>.index"
      --  files will be stored.

      Clean_Dir (Output_Subdir, "");
      Set_Output_Dir (Output_Subdir, Subdir => True);

      --  Generate the index for lines...
      Generate_Index (Lines => True);

      --  ... and the indices for each enabled source coverage level
      for Cov_Lvl in Source_Coverage_Level loop
         if Enabled (Cov_Lvl)
           or else (Cov_Lvl = Decision and then Decision_Coverage_Enabled)
           or else (Cov_Lvl = ATC and then Assertion_Coverage_Enabled)
         then
            Generate_Index (L => Cov_Lvl, Lines => False);
         end if;
      end loop;

      if Annotations.Multiple_Reports then
         --  Set the output dir to its original value
         Set_Output_Dir (Output_Dir);
      end if;
   end Generate_Indices;

   ---------
   -- "+" --
   ---------

   function "+" (P : Percentage) return String is
      P_Str         : Unbounded_String := +Percentage'Image (P);
      Needed_Spaces : constant Natural := 3 - (Length (P_Str) - 1);
   begin
      for I in 1 .. Needed_Spaces loop
         P_Str := " " & P_Str;
      end loop;

      return +P_Str;
   end "+";

   -----------------------
   -- Build_Stat_String --
   -----------------------

   function Build_Stat_String
     (Name   : String;
      Stats  : Stats_Per_State;
      Total  : Natural;
      Lines  : Boolean;
      Is_Prj : Boolean := False) return String
   is
      function Metric_Stats
        (Stats : Stats_Per_State; Total : Natural) return String;
      --  Express the Stats as a string

      ------------------
      -- Metric_Stats --
      ------------------

      function Metric_Stats
        (Stats : Stats_Per_State; Total : Natural) return String
      is
         Percentages : Stats_Per_State;
         Res         : Unbounded_String := +"-   -%, !   -%, +   -%";
      begin
         if Is_Prj and then Total = 0 then
            return +Res;
         end if;

         if Total /= 0 then
            Percentages := Compute_Percentage (Stats, Total);
            Res := Null_Unbounded_String;

            for State in Stats'Range loop
               Append
                 (Res,
                  State_Char (State)
                  & (+Percentages (State))
                  & "%"
                  & (if State /= Stats'Last then ", " else ""));
            end loop;
         end if;

         return +Res;
      end Metric_Stats;
   begin
      return
        +(+(if Is_Prj then "" else "    ")
          & Name
          & ":"
          & ASCII.LF
          & "        "
          & Metric_Stats (Stats, Total)
          & "     out of"
          & Natural'Image (Total)
          & (if Lines then " lines" else " obligations"));
   end Build_Stat_String;

   -------------------------
   --  Compute_Percentage --
   -------------------------

   function Compute_Percentage
     (Stats : Stats_Per_State; Total : Positive) return Stats_Per_State is
   begin
      return Res : Stats_Per_State do
         for State in Stats_Per_State'Range loop
            Res (State) := (Stats (State) * 100) / Total;
         end loop;
      end return;
   end Compute_Percentage;

   --------------------
   -- Generate_Index --
   --------------------

   procedure Generate_Index (L : Coverage_Level := Stmt; Lines : Boolean) is
      Projects_Stats      : Projects_Stats_Maps.Map :=
        Projects_Stats_Maps.Empty_Map;
      Other_Sources_Stats : Project_Stats;

      procedure Update
        (Prj_Stats  : in out Project_Stats;
         File_Total : Natural;
         File_Stats : Stats_Per_State;
         File_Name  : String;
         Lines      : Boolean;
         Is_Prj     : Boolean := False);
      --  Update a project's stats with those of a file

      procedure Process_One_File (Source_Index : Source_File_Index);
      --  Compute and store the coverage statistics for the file designated by
      --  Source_Index.

      procedure Write_Index;
      --  Create the index file and write the coverage statistics in it

      ------------
      -- Update --
      ------------

      procedure Update
        (Prj_Stats  : in out Project_Stats;
         File_Total : Natural;
         File_Stats : Stats_Per_State;
         File_Name  : String;
         Lines      : Boolean;
         Is_Prj     : Boolean := False) is
      begin
         --  If the file contained relevant obligations or lines, update its
         --  project's statistics.

         if File_Total /= 0 then

            --  If no file containing relevant coverage obligations had already
            --  been registered under this project, the project's stats are the
            --  same as that of the file for now...

            if Prj_Stats.Total = 0 then
               Prj_Stats.Total := File_Total;
               Prj_Stats.Stats := File_Stats;
            else
               --  ... otherwise update the project stats accordingly

               Prj_Stats.Total := Prj_Stats.Total + File_Total;

               --  For now the files' obligations per state are only added.
               --  Only all stats have been stored must they be divided by the
               --  total number of obligations to get the project's coverage
               --  percentages.

               for State in Stats_Per_State'Range loop
                  Prj_Stats.Stats (State) :=
                    (Prj_Stats.Stats (State) + File_Stats (State));
               end loop;
            end if;
         end if;

         --  Finally, append the string representation of the file's statistics

         Append
           (Prj_Stats.Files_Stats,
            ASCII.LF
            & (if Is_Prj then "" & ASCII.LF else "")
            & Build_Stat_String
                (File_Name,
                 File_Stats,
                 File_Total,
                 Lines  => Lines,
                 Is_Prj => Is_Prj));
      end Update;

      ----------------------
      -- Process_One_File --
      ----------------------

      procedure Process_One_File (Source_Index : Source_File_Index) is
         Info  : constant File_Info_Access := Get_File (Source_Index);
         Total : constant Natural :=
           (if Info.Kind = Source_File
            then
              (if Lines
               then Get_Total (Info.Li_Stats)
               else Info.Ob_Stats (L).Total)
            else 0);

         function Compute_File_Stats return Stats_Per_State;
         --  Compute the coverage statistics of the file designated by
         --  Source_Index

         ------------------------
         -- Compute_File_Stats --
         ------------------------

         function Compute_File_Stats return Stats_Per_State is
         begin
            return Result : Stats_Per_State do
               for State in Stats_Per_State'Range loop
                  Result (State) :=
                    (if Total = 0
                     then 0
                     else
                       (if Lines
                        then Info.Li_Stats (State)
                        else Info.Ob_Stats (L).Stats (State)));
               end loop;
            end return;
         end Compute_File_Stats;

         Stats       : constant Stats_Per_State := Compute_File_Stats;
         Source_Name : constant String := Info.Simple_Name.all;
         Prj_Name    : constant String :=
           (if Info.Kind = Source_File and then Project.Is_Project_Loaded
            then
              Project.Project_Name
                (Get_Full_Name (Source_Index, Or_Simple => True))
            else "");
         --  Note that P_Name can be "" here either because we don't have
         --  a root project at all or because we were unable to find the
         --  project to which the source pertains.

         --  Start of Process_One_File

      begin
         --  If we know to which project the file belongs to, store its stats
         --  properly. Else, store the stats in Other_Projects_Stats.

         if Prj_Name /= "" then
            declare
               procedure Update_Stats
                 (Key : String; Element : in out Project_Stats);
               --  Update the project's stats according to the file's

               ------------------
               -- Update_Stats --
               ------------------

               procedure Update_Stats
                 (Key : String; Element : in out Project_Stats)
               is
                  pragma Unreferenced (Key);
               begin
                  Update
                    (Element,
                     Total,
                     Stats,
                     Source_Name,
                     Lines  => Lines,
                     Is_Prj => False);
               end Update_Stats;

               Prj_C : constant Projects_Stats_Maps.Cursor :=
                 Projects_Stats.Find (Prj_Name);
            begin
               if not Projects_Stats_Maps.Has_Element (Prj_C) then
                  Projects_Stats.Insert
                    (Prj_Name,
                     (Total       => Total,
                      Stats       => Stats,
                      Files_Stats =>
                        ASCII.LF
                        & (+Build_Stat_String
                              (Source_Name, Stats, Total, Lines))));
               else
                  Projects_Stats.Update_Element (Prj_C, Update_Stats'Access);
               end if;
            end;
         else
            if Info.Kind = Source_File then
               Update
                 (Other_Sources_Stats,
                  Total,
                  Stats,
                  Source_Name,
                  Lines  => Lines,
                  Is_Prj => False);
            end if;
         end if;
      end Process_One_File;

      -----------------
      -- Write_Index --
      -----------------

      procedure Write_Index is
         Cov_Index_File : File_Type;

         Metric   : constant String :=
           (if Lines
            then "lines"
            else Ada.Characters.Handling.To_Lower (Coverage_Level'Image (L)));
         Filename : constant String := Metric & ".index";

         Contents : Unbounded_String := Null_Unbounded_String;
         --  Informations to be written in the index file

         Global_Stats : Project_Stats;
         --  The aggregated stats of all projects
      begin

         --  Go through all the projects' coverage statistics and append them
         --  to the Content string to be written to the index file.
         --  Additionally, compute the global coverage stats.

         for C in Projects_Stats.Iterate loop
            declare
               Prj_Name  : constant String := Projects_Stats_Maps.Key (C);
               Prj_Stats : constant Project_Stats :=
                 Projects_Stats_Maps.Element (C);
            begin
               Update
                 (Global_Stats,
                  Prj_Stats.Total,
                  Prj_Stats.Stats,
                  Prj_Name,
                  Lines  => Lines,
                  Is_Prj => True);

               Append
                 (Global_Stats.Files_Stats, ASCII.LF & Prj_Stats.Files_Stats);
            end;
         end loop;

         if Length (Other_Sources_Stats.Files_Stats) /= 0 then
            Update
              (Global_Stats,
               Other_Sources_Stats.Total,
               Other_Sources_Stats.Stats,
               "Other sources",
               Lines  => Lines,
               Is_Prj => True);

            Append
              (Global_Stats.Files_Stats,
               ASCII.LF & Other_Sources_Stats.Files_Stats);
         end if;

         Append (Contents, Global_Stats.Files_Stats);

         --  Create the index file and write its contents

         Create_Output_File (Cov_Index_File, Filename);
         Put_Line (Cov_Index_File, Metric);
         Put_Line
           (Cov_Index_File,
            ASCII.LF
            & Build_Stat_String
                ("Global",
                 Global_Stats.Stats,
                 Global_Stats.Total,
                 Is_Prj => True,
                 Lines  => Lines));
         Put_Line (Cov_Index_File, +Contents);
      end Write_Index;

   begin
      Files_Table_Iterate (Process_One_File'Access);
      Write_Index;
   end Generate_Index;

end Annotations.Index;
