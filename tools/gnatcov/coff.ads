------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Interfaces; use Interfaces;
with System; use System;

package Coff is
   type Filehdr is record
      F_Machine : Unsigned_16;
      F_Nscns   : Unsigned_16;
      F_Timdat  : Unsigned_32;
      F_Symptr  : Unsigned_32;
      F_Nsyms   : Unsigned_32;
      F_Opthdr  : Unsigned_16;
      F_Flags   : Unsigned_16;
   end record;

   Filehdr_Size : constant Natural := Filehdr'Size / Storage_Unit;

   I386magic  : constant Unsigned_16 := 16#014c#;
   AMD64magic : constant Unsigned_16 := 16#8664#;

   F_Relflg : constant Unsigned_16 := 16#0001#;

   F_Exec : constant Unsigned_16 := 16#0002#;

   F_Lnno : constant Unsigned_16 := 16#0004#;

   F_Lsyms : constant Unsigned_16 := 16#0008#;

   type Opthdr32 is record
      Magic                          : Unsigned_16;
      Major_Linker_Version           : Unsigned_8;
      Minor_Linker_Version           : Unsigned_8;
      Size_Of_Code                   : Unsigned_32;
      Size_Of_Initialized_Data       : Unsigned_32;
      Size_Of_Uninitialized_Data     : Unsigned_32;
      Address_Of_Entry_Point         : Unsigned_32;
      Base_Of_Code                   : Unsigned_32;
      Base_Of_Data                   : Unsigned_32;
      Image_Base                     : Unsigned_32;
      Section_Alignment              : Unsigned_32;
      File_Alignment                 : Unsigned_32;
      Major_Operating_System_Version : Unsigned_16;
      Minor_Operating_System_Version : Unsigned_16;
      --  ...
   end record;

   Opt_Hdr32_Size : constant Natural := Opthdr32'Size / Storage_Unit;

   type Scnhdr is record
      S_Name    : String (1 .. 8);
      S_Paddr   : Unsigned_32;
      S_Vaddr   : Unsigned_32;
      S_Size    : Unsigned_32;
      S_Scnptr  : Unsigned_32;
      S_Relptr  : Unsigned_32;
      S_Lnnoptr : Unsigned_32;
      S_Nreloc  : Unsigned_16;
      S_Nlnno   : Unsigned_16;
      S_Flags   : Unsigned_32;
   end record;
   Scnhdr_Size : constant Natural := Scnhdr'Size / Storage_Unit;

   STYP_TEXT : constant Unsigned_32 := 16#0020#;
   STYP_DATA : constant Unsigned_32 := 16#0040#;
   STYP_BSS  : constant Unsigned_32 := 16#0080#;

   type Strent_Type is record
      E_Zeroes : Unsigned_32;
      E_Offset : Unsigned_32;
   end record;

   type Sym_Name (Inline : Boolean := True) is record
      case Inline is
         when True =>
            E_Name : String (1 .. 8);
         when False =>
            E : Strent_Type;
      end case;
   end record;
   pragma Unchecked_Union (Sym_Name);
   for Sym_Name'Size use 64;

   type Syment is record
      E        : Sym_Name;
      E_Value  : Unsigned_32;
      E_Scnum  : Unsigned_16;
      E_Type   : Unsigned_16;
      E_Sclass : Unsigned_8;
      E_Numaux : Unsigned_8;
   end record;
   for Syment'Alignment use 2;
   Symesz : constant := 18;
   for Syment'Size use Symesz * Storage_Unit;

   N_UNDEF : constant Unsigned_16 := 16#00_00#;
   N_ABS   : constant Unsigned_16 := 16#Ff_Ff#;
   N_DEBUG : constant Unsigned_16 := 16#Ff_Fe#;

   C_NULL    : constant Unsigned_8 := 0;
   C_AUTO    : constant Unsigned_8 := 1;
   C_EXT     : constant Unsigned_8 := 2;
   C_STAT    : constant Unsigned_8 := 3;
   C_REG     : constant Unsigned_8 := 4;
   C_EXTDEF  : constant Unsigned_8 := 5;
   C_LABEL   : constant Unsigned_8 := 6;
   C_ULABEL  : constant Unsigned_8 := 7;
   C_MOS     : constant Unsigned_8 := 8;
   C_ARG     : constant Unsigned_8 := 9;
   C_STRTAG  : constant Unsigned_8 := 10;
   C_MOU     : constant Unsigned_8 := 11;
   C_UNTAG   : constant Unsigned_8 := 12;
   C_TPDEF   : constant Unsigned_8 := 13;
   C_USTATIC : constant Unsigned_8 := 14;
   C_ENTAG   : constant Unsigned_8 := 15;
   C_MOE     : constant Unsigned_8 := 16;
   C_REGPARM : constant Unsigned_8 := 17;
   C_FIELD   : constant Unsigned_8 := 18;
   C_AUTOARG : constant Unsigned_8 := 19;
   C_LASTENT : constant Unsigned_8 := 20;
   C_BLOCK   : constant Unsigned_8 := 100;
   C_FCN     : constant Unsigned_8 := 101;
   C_EOS     : constant Unsigned_8 := 102;
   C_FILE    : constant Unsigned_8 := 103;
   C_LINE    : constant Unsigned_8 := 104;
   C_ALIAS   : constant Unsigned_8 := 105;
   C_HIDDEN  : constant Unsigned_8 := 106;
   C_EFCN    : constant Unsigned_8 := 255;

   type Auxent_File (Inline : Boolean := True) is record
      case Inline is
         when True =>
            X_Fname : String (1 .. 14);
         when False =>
            X_N : Strent_Type;
      end case;
   end record;
   pragma Unchecked_Union (Auxent_File);

   type Auxent_Scn is record
      X_Scnlen : Unsigned_32;
      X_Nreloc : Unsigned_16;
      X_Nlinno : Unsigned_16;
   end record;

   type Reloc is record
      R_Vaddr  : Unsigned_32;
      R_Symndx : Unsigned_32;
      R_Type   : Unsigned_16;
   end record;
   Relsz : constant Natural := Reloc'Size / Storage_Unit;

   Reloc_Rel32  : constant Unsigned_16 := 20;
   Reloc_Addr32 : constant Unsigned_16 := 6;

   type PEHdr is record
      E_MZHdr  : Unsigned_16;
      Pad1     : String (2 .. 16#3b#);
      E_Lfanew : Unsigned_32;
   end record;

   PEHdrsz : constant Natural := PEHdr'Size / Storage_Unit;

   MZhdr : constant Unsigned_16 := 16#5a_4d#;
   Pe_Magic : constant Unsigned_32 := 16#00_00_45_50#;
end Coff;
