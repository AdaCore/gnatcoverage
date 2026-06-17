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

package Instrument.C_Annotations is

   ----------------------------------
   -- Syntax tree based processing --
   ----------------------------------

   procedure Populate_Annotations (UIC : in out C_Unit_Inst_Context);
   --  Look for exemption / disabling coverage markers in the source code and
   --  fill UIC.Annotations (and also UIC.Disable_Cov_Regions for disabling
   --  coverage markers) accordingly.

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

end Instrument.C_Annotations;
