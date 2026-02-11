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

package body Hashes is

   ----------------
   -- Start_Hash --
   ----------------

   function Start_Hash
     (Label : String; Trace : GNATCOLL_Trace) return Tracing_Hash is
   begin
      return
        (Ctx    => GNAT.SHA1.Initial_Context,
         Trace  => Trace,
         Label  => +Label,
         Buffer => <>);
   end Start_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (Self : in out Tracing_Hash; S : String) is
   begin
      GNAT.SHA1.Update (Self.Ctx, S);
      if Self.Trace.Is_Active then
         US.Append (Self.Buffer, S);
      end if;
   end Update_Hash;

   ------------
   -- Digest --
   ------------

   function Digest
     (Self : in out Tracing_Hash) return GNAT.SHA1.Binary_Message_Digest is
   begin
      if Self.Trace.Is_Active then
         Self.Trace.Trace
           ("Computing fingerprint for " & (+Self.Label) & " from:");
         Self.Trace.Trace ("BEGIN ...");
         Self.Trace.Trace (+Self.Buffer);
         Self.Trace.Trace ("... END");
      end if;
      return GNAT.SHA1.Digest (Self.Ctx);
   end Digest;

end Hashes;
