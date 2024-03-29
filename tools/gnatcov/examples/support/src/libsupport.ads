------------------------------------------------------------------------------
--                                                                          --
--                              GNATcoverage                                --
--                                                                          --
--                     Copyright (C) 2014-2024, AdaCore                     --
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

package Libsupport is

   --  Root package for our testsuite support library, aimed at filling RTS
   --  gaps. This particular unit is always part of the closure, so there's
   --  something to build a library from also in cases where no real "support"
   --  is needed, e.g. with native configurations.

   --  Default, noop, last_chance_handler entry hook, that individual tests
   --  tests/examples may override by interposing an object file in the link
   --  prior to the library archive.

   procedure Lch_Enter;
   pragma Export (Ada, Lch_Enter, "__lch_enter");

end Libsupport;
