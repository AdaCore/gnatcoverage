------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This unit needs to be compilable with Ada 95 compilers

package body GNATcov_RTS.Traces.Output.Bytes_IO is

   ------------
   --  Close --
   ------------

   procedure Close (File : in out File_Type) is
   begin
      OSLIB.Close (FD => File.FD);
   end Close;

   -------------
   --  Create --
   -------------

   procedure Create (File : in out File_Type; Name : String) is
   begin
      File.FD := OSLIB.Create_File (Name => Name, Fmode => OSLIB.Binary);
      if File.FD = OSLIB.Invalid_FD then
         raise IO_Error;
      end if;
   end Create;

   ------------
   --  Write --
   ------------

   procedure Write (File : File_Type; Byte : U8) is
      N : Integer;
   begin
      N := OSLIB.Write (FD => File.FD, A => Byte'Address, N => 1);
      if N /= 1 then
         raise IO_Error;
      end if;
   end Write;

end GNATcov_RTS.Traces.Output.Bytes_IO;
