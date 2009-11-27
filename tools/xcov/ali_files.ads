------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

--  ALI files reader

with Ada.Containers.Ordered_Maps;

with GNAT.Strings; use GNAT.Strings;

with Slocs; use Slocs;
with Types; use Types;

package ALI_Files is

   function Load_ALI (ALI_Filename : String) return Source_File_Index;
   --  Load coverage information (coverage exemptions and source coverage
   --  obligations) from ALI_Filename. Returns the source file index for the
   --  ALI file. Subsequent calls for the same ALI file will return
   --  No_Source_File immediately, without reloading the file.

   type ALI_Annotation_Kind is (Exempt_On, Exempt_Off);

   type ALI_Annotation is record
      Kind    : ALI_Annotation_Kind;
      Message : String_Access;
   end record;

   package ALI_Annotation_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Source_Location,
        Element_Type => ALI_Annotation);

   ALI_Annotations : ALI_Annotation_Maps.Map;

end ALI_Files;
