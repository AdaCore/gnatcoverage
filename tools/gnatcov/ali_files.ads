------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  ALI files reader

with Ada.Containers.Ordered_Maps;
with Ada.Streams; use Ada.Streams;

with GNAT.Regexp;
with GNAT.Strings; use GNAT.Strings;

with SC_Obligations; use SC_Obligations;
with Slocs; use Slocs;
with Types; use Types;

package ALI_Files is

   --  This unit instantiates containers and we want to avoid too much
   --  performance cost when using references to their elements, so suppress
   --  tampering checks.

   pragma Suppress (Tampering_Check);

   function Load_ALI
     (ALI_Filename         : String;
      CU                   : CU_Id;
      Ignored_Source_Files : access GNAT.Regexp.Regexp;
      Units                : out SFI_Vector;
      Deps                 : out SFI_Vector;
      With_SCOs            : Boolean) return Types.Source_File_Index;
   --  Load coverage related information (coverage exemptions and, if With_SCOs
   --  is True, source coverage obligations) from ALI_Filename. Returns the
   --  source file index for the ALI file. Subsequent calls for the same ALI
   --  file will return No_Source_File immediately, without reloading the file.
   --  Units are the units contained in this compilation.
   --
   --  Ignore all source obligations according to Ignored_Source_Files (see
   --  SC_Obligations.Load_SCOs' documentation).
   --
   --  Deps are the dependencies of the compilation.

   procedure Load_ALI (ALI_Filename : String);
   --  Load ALI information for Filename, without SCOs

   type ALI_Annotation_Kind is (Exempt_On, Exempt_Off);

   type ALI_Annotation is record
      CU : CU_Id;
      --  Compilation unit whose LI file this annotation comes from

      Kind    : ALI_Annotation_Kind;
      --  On or Off

      Message : String_Access;
      --  When Kind = Exempt_On, justification message for the exemption.
      --  This is null if no justification is given (i.e. this is never an
      --  access to an empty string).

      Count   : Natural := 0;
      --  When Kind = Exempt_On, this counts the "hits" on this exemption:
      --  exempted messages if generating a report, exempted non/partially
      --  covered lines otherwise.
   end record;

   procedure Read (S : access Root_Stream_Type'Class; V : out ALI_Annotation);
   procedure Write (S : access Root_Stream_Type'Class; V : ALI_Annotation);
   for ALI_Annotation'Read use Read;
   for ALI_Annotation'Write use Write;

   package ALI_Annotation_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Source_Location,
        Element_Type => ALI_Annotation);

   ALI_Annotations : ALI_Annotation_Maps.Map;

end ALI_Files;
