------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
with Ada.Containers.Vectors;
with Ada.Streams; use Ada.Streams;

private with GNAT.SHA1;
with GNAT.Strings; use GNAT.Strings;

with SC_Obligations; use SC_Obligations;
with Slocs; use Slocs;
with Types; use Types;

package ALI_Files is

   package SFI_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Pos,
      Element_Type => Source_File_Index);
   --  Vector of source file indices, used to map dependency indices in an
   --  ALI file to our source file indices.

   subtype SFI_Vector is SFI_Vectors.Vector;

   type SCOs_Hash is private;

   function Load_ALI
     (ALI_Filename : String;
      CU           : CU_Id;
      Units        : out SFI_Vector;
      Deps         : out SFI_Vector;
      Fingerprint  : out SCOs_Hash;
      With_SCOs    : Boolean) return Types.Source_File_Index;
   --  Load coverage related information (coverage exemptions and, if With_SCOs
   --  is True, source coverage obligations) from ALI_Filename. Returns the
   --  source file index for the ALI file. Subsequent calls for the same ALI
   --  file will return No_Source_File immediately, without reloading the file.
   --  Units are the units contained in this compilation. Deps are the
   --  dependencies of the compilation. Fingerprint is a unique hash of SCO
   --  information in the ALI file, and is undefined if With_SCOs is False.

   procedure Load_ALI (ALI_Filename : String);
   --  Load ALI information for Filename, without SCOs

   type ALI_Annotation_Kind is (Exempt_On, Exempt_Off);

   type ALI_Annotation is record
      CU : CU_Id;
      --  Compilation unit whose LI file this annotation comes from

      Kind    : ALI_Annotation_Kind;
      --  On or Off

      Message : String_Access;
      --  When Kind = Exempt_On, justification message for the exemption

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

private

   type SCOs_Hash is new GNAT.SHA1.Binary_Message_Digest;

end ALI_Files;
