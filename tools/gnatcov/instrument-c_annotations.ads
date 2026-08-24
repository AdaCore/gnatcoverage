------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2026, AdaCore                     --
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

--  Helpers to analyze annotations for C/C++ source code

with Ada.Containers.Ordered_Maps;

with Instrument.C; use Instrument.C;
with Slocs;

package Instrument.C_Annotations is

   ----------------------------------
   -- Syntax tree based processing --
   ----------------------------------

   procedure Populate_Annotations (UIC : in out C_Unit_Inst_Context);
   --  Look for exemption / disabling coverage markers in the source code and
   --  fill UIC.Annotations (and also UIC.Disable_Cov_Regions for disabling
   --  coverage markers) accordingly.

   procedure Iterate_Source_Annotations
     (Filename : String;
      Lang     : Some_Language;
      Process  :
        access procedure
          (Annot : ALI_Annotation; Span : Slocs.Local_Source_Location_Range));
   --  Call Process for each in-source GNATCOV_* annotation comment that
   --  Filename contains, in source order. Lang is the language to analyze
   --  Filename as.
   --
   --  Span designates the comment itself, i.e. the very text that materializes
   --  the annotation. Note that its Last_Sloc is exclusive: it designates the
   --  first character past the comment.
   --
   --  Unlike Populate_Annotations above, this parses Filename on its own, with
   --  clang's single-file mode: it needs none of the compiler switches of the
   --  project, it reports locations in the original file rather than in a
   --  preprocessed one, and, because tokenization re-lexes the raw source, it
   --  also sees annotations sitting in code that conditional preprocessor
   --  directives would exclude. Comments are still found by clang's own lexer,
   --  so what we extract is exactly what the instrumenter would honour.
   --
   --  Comments that are not valid Xcov annotations are skipped, with a
   --  warning.

   ----------------------------------
   -- Text buffer based processing --
   ----------------------------------

   type Annotation_Index is record
      Buffer_First : Positive;
      Priority     : Natural;
      Buffer_Next  : Natural;
   end record;
   --  Identifier for a source slice that an annotation covers.
   --
   --  The ``Buffer_First .. Buffer_Next - 1`` index range covers the source
   --  exerpt that should be replaced:
   --
   --  * For in-source annotations, this is the range that covers the comment
   --    that materializes the annotation.
   --  * For external annotations, this is an empty range (Buffer_First =
   --    Buffer_Next) and the code is inserted right before the byte at
   --    Buffer_First.
   --
   --  Priority is used to allow multiple annotations at the same location.

   function "<" (Left, Right : Annotation_Index) return Boolean
   is (Left.Buffer_First < Right.Buffer_First
       or else
         (Left.Buffer_First = Right.Buffer_First
          and then Left.Priority < Right.Priority));

   package Index_To_Annotation_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Annotation_Index,
        Element_Type => ALI_Annotation);

   procedure Populate_Annotations
     (Filename    : String;
      Buffer      : String;
      Annotations : out Index_To_Annotation_Maps.Map);
   --  Look for exemption and disabling coverage markers in the given buffer
   --  and in external annotation files, and fill Annotations accordingly.

end Instrument.C_Annotations;
