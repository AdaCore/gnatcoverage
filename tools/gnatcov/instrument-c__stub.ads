------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2023, AdaCore                     --
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

--  Stub of Instrument.C, to avoid pulling a dependency to libclang when
--  gnatcov is not built with C instrumentation support.

with Instrument.Common; use Instrument.Common;

package Instrument.C is

   pragma Elaborate_Body;

   type C_Family_Instrumenter_Type is
     abstract new Language_Instrumenter with null record;
   --  Common instrumentation primitives for C/C++

   type C_Instrumenter_Type is
     new C_Family_Instrumenter_Type with null record;
   --  Instrumentation primitives for C

   overriding function Language
     (Self : C_Instrumenter_Type) return Src_Supported_Language
   is (C_Language);

   type CPP_Instrumenter_Type is
     new C_Family_Instrumenter_Type with null record;
   --  Instrumentation primitives for C++

   overriding function Language
     (Self : CPP_Instrumenter_Type) return Src_Supported_Language
   is (CPP_Language);

   C_Instrumenter   : aliased C_Instrumenter_Type := (null record);
   CPP_Instrumenter : aliased CPP_Instrumenter_Type := (null record);

   procedure Postprocess_Source
     (Preprocessed_Filename  : String;
      Postprocessed_Filename : String) is null;

end Instrument.C;
