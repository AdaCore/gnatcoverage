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

with Command_Line;        use Command_Line;
with Instrument.Ada_Unit; use Instrument.Ada_Unit;
with Instrument.C;        use Instrument.C;
with Instrument.Common;   use Instrument.Common;
with Strings;             use Strings;

--  Return the instrumenter configuration generated from the command line

function Instrument.Config return Language_Instrumenter'Class
is
   use Command_Line.Parser;
   Language : constant Some_Language :=
     To_Language (+Args.String_Args (Opt_Lang).Value);
   Tag      : constant Unbounded_String :=
     Value_Or_Null (Args.String_Args (Opt_Dump_Filename_Tag));
begin
   case Language is
      when Ada_Language =>
         return Create_Ada_Instrumenter
           (Tag                        => Tag,
            Config_Pragmas_Mapping     =>
               +Args.String_Args (Opt_Config_Pragmas_Mapping).Value,
            Mapping_Filename           =>
               +Args.String_Args (Opt_Gnatem).Value,
            Preprocessor_Data_Filename =>
               +Args.String_Args (Opt_Ada_Preprocessor_Data).Value);
      when C_Language =>
         return Create_C_Instrumenter
           (Tag => Tag, Instr_Mode => Project_Instrumentation);
      when CPP_Language =>
         return Create_CPP_Instrumenter
           (Tag => Tag, Instr_Mode => Project_Instrumentation);
   end case;
end Instrument.Config;
