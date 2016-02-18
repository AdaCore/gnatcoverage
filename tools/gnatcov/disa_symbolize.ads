------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Highlighting;
with Traces;

package Disa_Symbolize is

   --  Call-back used to find a relocation symbol

   type Symbolizer is limited interface;
   procedure Symbolize
     (Sym      : Symbolizer;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type) is abstract;

   type Nul_Symbolizer_Type is new Symbolizer with private;

   overriding procedure Symbolize
     (Sym      : Nul_Symbolizer_Type;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type) is null;

   Nul_Symbolizer : constant Nul_Symbolizer_Type;

private
   type Nul_Symbolizer_Type is new Symbolizer with null record;
   Nul_Symbolizer : constant Nul_Symbolizer_Type := (null record);
end Disa_Symbolize;
