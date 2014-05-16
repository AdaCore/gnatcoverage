------------------------------------------------------------------------------
--                                                                          --
--                              GNATcoverage                                --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with System; use System;
with Interfaces.C; use Interfaces.C;

package Libsupport.Memory is
  
   --  Simple services possibly invoked by the low level code generation
   --  passes, without Ada RTS violation per se at the user level (e.g. when
   --  doing array or slice assignments or comparisons).

   --  This is the really basic common ground that we want to be able to rely
   --  on for any possible kind of RTS profile, down to strict ZFP.

   function memcmp (S1 : Address; S2 : Address; N : size_t) return int;
   pragma Export (C, memcmp, "memcmp");

   procedure memcpy (Dest : Address; Src : Address; N : size_t);
   pragma Export (C, memcpy, "memcpy");

   procedure memmove (Dest : Address; Src : Address; N : size_t);
   pragma Export (C, memmove, "memmove");

end;
