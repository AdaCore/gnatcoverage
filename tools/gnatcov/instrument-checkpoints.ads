------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

with Checkpoints;           use Checkpoints;
with Instrument.Base_Types; use Instrument.Base_Types;

package Instrument.Checkpoints is

   --  Note: the following procedures must be called after the SCO units
   --  table has been saved/loaded.

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State);
   --  Save the current instrumented units map to stream

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load (CLS : access Checkpoint_Load_State);
   --  Load checkpointed instrumented unit map from stream and merge them in
   --  current state.

end Instrument.Checkpoints;
