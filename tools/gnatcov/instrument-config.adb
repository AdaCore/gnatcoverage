------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Command_Line;        use Command_Line;
with Files_Handling;      use Files_Handling;
with Instrument.Ada_Unit; use Instrument.Ada_Unit;
with Instrument.C;        use Instrument.C;
with Instrument.Common;   use Instrument.Common;
with Strings;             use Strings;

--  Return the instrumenter configuration generated from the command line

function Instrument.Config return Language_Instrumenter'Class is
   use Command_Line.Parser;
   Language : constant Some_Language :=
     To_Language (+Args.String_Args (Opt_Lang).Value);
   Tag      : constant Unbounded_String :=
     Value_Or_Null (Args.String_Args (Opt_Dump_Filename_Tag));

   function RTS_Source_Dirs return File_Vectors.Vector;
   --  Return the list of source directories from the Opt_RTS_Source_Dirs
   --  option.

   ---------------------
   -- RTS_Source_Dirs --
   ---------------------

   function RTS_Source_Dirs return File_Vectors.Vector is
   begin
      return Result : File_Vectors.Vector do
         for D of Args.String_List_Args (Opt_RTS_Source_Dirs) loop
            Result.Append (Create (+(+D)));
         end loop;
      end return;
   end RTS_Source_Dirs;

   --  Start of processing for Instrument.Config

begin
   case Language is
      when Ada_Language =>
         return
           Create_Ada_Instrumenter
             (Default_Charset            =>
                Args.String_Args (Opt_Ada_Default_Charset).Value,
              Tag                        => Tag,
              Config_Pragmas_Mapping     =>
                +Args.String_Args (Opt_Config_Pragmas_Mapping).Value,
              Mapping_Filename           =>
                +Args.String_Args (Opt_Gnatem).Value,
              Preprocessor_Data_Filename =>
                +Args.String_Args (Opt_Ada_Preprocessor_Data).Value);

      when C_Language   =>
         return
           Create_C_Instrumenter
             (Tag, Project_Instrumentation, RTS_Source_Dirs);

      when CPP_Language =>
         return
           Create_CPP_Instrumenter
             (Tag, Project_Instrumentation, RTS_Source_Dirs);
   end case;
end Instrument.Config;
