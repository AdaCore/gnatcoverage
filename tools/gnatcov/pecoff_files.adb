------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2015-2024, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Environment_Variables;
pragma Warnings (Off, "* is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "* is an internal GNAT unit");
with Interfaces.C;              use Interfaces.C;
with System.Storage_Elements;

with GNAT.Regexp;
with GNATCOLL.Mmap; use GNATCOLL.Mmap;
with GNATCOLL.VFS;  use GNATCOLL.VFS;

with Dwarf_Handling;
with Hex_Images;      use Hex_Images;
with Inputs;
with Outputs;
with Paths;           use Paths;
with Shared_Lib_Deps; use Shared_Lib_Deps;
with Strings;         use Strings;

package body PECoff_Files is

   API_Set_Pattern : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile
       (Pattern        => "(api|ext)-[a-z0-9-]*-l[0-9]+-[0-9]+-[0-9]+\.dll",
        Case_Sensitive => False);
   --  Pattern to match the names of API sets that can appear in the import
   --  table. See
   --  <https://learn.microsoft.com/en-us/windows/win32/apiindex/
   --  windows-apisets>.

   function Read_Coff_Header_Offset (Fd : File_Descriptor) return Long_Integer;
   --  Read the offset of the PE signature (4 bytes before the COFF header).
   --  Returns 0 in case of error (bad magic number).

   function Extract_Nul_Terminated (S : String) return String;
   --  Return the first characters of S until but not including Nul, or S if
   --  there is no Nul character.

   procedure Get_Data_Directory
     (Fd : File_Descriptor; File : in out PE_File; Dir : Data_Directory);
   --  Fetch the requested data directory (RVA and size) in Fd and store it in
   --  File.

   procedure Exit_With_Error
     (File : in out PE_File; Status : Binary_File_Status; Msg : String);
   --  Assign Status to File, close the file if needed and raise Error with
   --  the filename and Msg.

   function In_Range (RVA, Size : Unsigned_32; Section : Scnhdr) return Boolean
   is (Section.S_Vaddr <= RVA
       and then Size <= Section.S_Size
       and then RVA <= Section.S_Vaddr + Section.S_Size - Size);
   --  Return whether the memory slice represented by RVA and Size fits in
   --  Section.

   function DLL_Resolution_Path
     (Program_Filename : String) return Unbounded_String;
   --  Return the path to resolve DLL dependencies.
   --
   --  Program Filename is the name of the executable that requests the DLLs to
   --  resolve (its parent directory is added to the resolution path).
   --
   --  The complete process is described here:
   --  <https://learn.microsoft.com/en-us/windows/win32/dlls/
   --  dynamic-link-library-search-order>
   --
   --  Note that our implementation is incomplete (DLL redirections, SxS
   --  manifest redirections and other are not supported), but this should be
   --  enough to reach DLLs that contain coverage buffers in practice.

   --------------------
   -- Section loader --
   --------------------

   --  The following helpers allow to efficiently load the section contents
   --  that correspond to a given RVA (Relative Virtual Address).

   package Loaded_Section_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Section_Index,
        Element_Type => Loaded_Section);

   type Scnhdr_And_Index is record
      Header : Scnhdr;
      Index  : Section_Index;
   end record;

   package Scnhdr_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unsigned_32,
        Element_Type => Scnhdr_And_Index);

   type Section_Loader is record
      Loaded : Loaded_Section_Vectors.Vector;
      --  For each section in the executable, the corresponding mapped file
      --  region, or No_Loaded_Section if it was not loaded yet.

      Headers : Scnhdr_Maps.Map;
      --  Mapping from section indexes to the corresponding section header for
      --  all sections in the executable.
   end record;

   procedure Initialize (File : PE_File; Loader : out Section_Loader);
   --  Initialize Loader from the given executable FILE

   procedure Free (Loader : out Section_Loader);
   --  Free resources allocated to Loader

   procedure Lookup_Section
     (File    : PE_File;
      Loader  : in out Section_Loader;
      RVA     : Unsigned_32;
      Size    : Unsigned_32;
      Content : out Binary_Content);
   --  Use Loader to look for the section in File that contains Size bytes that
   --  are mapped at RVA.
   --
   --  Set Content to the memory mapped buffer that corresponds to the relevant
   --  section if found, otherwise set it to Invalid_Binary_Content.
   --
   --  Note that this supports only memory slices that fit in a single section,
   --  which should be enough in practice for binaries produced by usual
   --  toolchains.

   procedure Lookup_Section_Slice
     (File    : PE_File;
      Loader  : in out Section_Loader;
      RVA     : Unsigned_32;
      Size    : Unsigned_32;
      Content : out Binary_Content);
   --  Like Lookup_Section, but set Content to the precise slice that
   --  corresponds to RVA and Size.

   procedure Read_String
     (File    : PE_File;
      Loader  : in out Section_Loader;
      RVA     : Unsigned_32;
      Content : out Binary_Content);
   --  Like Lookup_Section, but set Content to the slice that corresponds to
   --  the ASCII string that starts at RVA.

   -----------------------------
   -- Read_Coff_Header_Offset --
   -----------------------------

   function Read_Coff_Header_Offset (Fd : File_Descriptor) return Long_Integer
   is
      MS_Hdr : PEHdr;
   begin
      Lseek (Fd, 0, Seek_Set);

      if Read (Fd, MS_Hdr'Address, PEHdrsz) /= PEHdrsz then
         return 0;
      end if;

      --  Only handle little endian
      if MS_Hdr.E_MZHdr /= MZhdr then
         return 0;
      end if;

      return Long_Integer (MS_Hdr.E_Lfanew);
   end Read_Coff_Header_Offset;

   ----------------
   -- Is_PE_File --
   ----------------

   function Is_PE_File (Fd : File_Descriptor) return Boolean is
      Off      : Long_Integer;
      PE_Sig   : Unsigned_32;
      Coff_Sig : Unsigned_16;
   begin
      Lseek (Fd, 0, Seek_Set);
      Off := Read_Coff_Header_Offset (Fd);

      if Off = 0 then
         --  A pure COFF file (without the PE header).
         Lseek (Fd, 0, Seek_Set);

         if Read (Fd, Coff_Sig'Address, 2) /= 2 then
            return False;
         end if;

         return Coff_Sig in I386magic | AMD64magic;
      else
         Lseek (Fd, Off, Seek_Set);

         if Read (Fd, PE_Sig'Address, 4) /= 4 then
            return False;
         end if;

         return PE_Sig = Pe_Magic;
      end if;
   end Is_PE_File;

   ------------------------
   -- Get_Data_Directory --
   ------------------------

   procedure Get_Data_Directory
     (Fd : File_Descriptor; File : in out PE_File; Dir : Data_Directory)
   is
      Entry_Size  : constant := 8;
      Entry_Index : constant Unsigned_32 := Data_Directory'Pos (Dir);

      --  Compute the offset in Fd where we should find data directories (at
      --  the end of the optional header).

      Header_Data_Dir_Offset : constant Long_Integer :=
        File.Opt_Hdr_Off
        + Long_Integer
            (if File.Is_PE32_Plus
             then Opt_Hdr_PE32_Plus_Size
             else Opt_Hdr_PE32_Size);

      Dir_Entry : Opt_Hdr_Data_Directory;
   begin
      --  Try to read it, but only if it is present

      if Entry_Index >= File.Number_Of_RVA_And_Sizes then
         Dir_Entry := No_Data_Directory;
      else
         Lseek
           (Fd,
            Header_Data_Dir_Offset + Entry_Size * Long_Integer (Entry_Index),
            Seek_Set);
         if Read (Fd, Dir_Entry'Address, Entry_Size) /= Entry_Size then
            Exit_With_Error
              (File,
               Status_Read_Error,
               "failed to read data directory for " & Dir'Image);
         end if;
      end if;

      Trace.Trace
        ("Data directory for "
         & Dir'Image
         & " @"
         & Hex_Image (Dir_Entry.Virtual_Address)
         & " ("
         & Hex_Image (Dir_Entry.Size)
         & " bytes)");

      case Dir is
         when Import_Table =>
            File.Import_Table := Dir_Entry;

         when others       =>
            null;
      end case;
   end Get_Data_Directory;

   ---------------------
   -- Exit_With_Error --
   ---------------------

   procedure Exit_With_Error
     (File : in out PE_File; Status : Binary_File_Status; Msg : String) is
   begin
      Set_Status (File, Status);
      Close_File (File);
      raise Error with File.Filename & ": " & Msg;
   end Exit_With_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (File : PE_File; Loader : out Section_Loader) is
   begin
      Loader.Loaded.Clear;
      Loader.Headers.Clear;

      Trace.Increase_Indent ("Indexing sections...");
      for I in 0 .. File.Get_Nbr_Sections - 1 loop
         declare
            Scn : constant Scnhdr := File.Get_Scnhdr (I);
         begin
            Trace.Increase_Indent ("Found " & File.Get_Section_Name (I));
            Trace.Trace ("VAddr: " & Hex_Image (Scn.S_Vaddr));
            Trace.Trace ("Size:  " & Hex_Image (Scn.S_Size));
            Trace.Decrease_Indent;

            Loader.Loaded.Append (No_Loaded_Section);
            Loader.Headers.Include (Scn.S_Vaddr, (Scn, I));
         end;
      end loop;
      Trace.Decrease_Indent;
   end Initialize;

   ----------
   -- Free --
   ----------

   procedure Free (Loader : out Section_Loader) is
   begin
      for S of Loader.Loaded loop
         Free (S);
      end loop;
   end Free;

   --------------------
   -- Lookup_Section --
   --------------------

   procedure Lookup_Section
     (File    : PE_File;
      Loader  : in out Section_Loader;
      RVA     : Unsigned_32;
      Size    : Unsigned_32;
      Content : out Binary_Content)
   is
      use Scnhdr_Maps;

      --  Look for the section with the highest Virtual Address that appear
      --  before RVA.

      Cur     : constant Cursor := Loader.Headers.Floor (RVA);
      Item    : Scnhdr_And_Index;
      Section : Section_Index;
   begin
      Trace.Increase_Indent
        ("Looking up slice for RVA @"
         & Hex_Image (RVA)
         & " ("
         & Hex_Image (Size)
         & " bytes)");
      Content := Invalid_Binary_Content;
      if not Has_Element (Cur) then
         Trace.Decrease_Indent ("No corresponding section found");
         return;
      end if;

      --  Accept it only if its size covers RVA + Size

      Item := Element (Cur);
      if not In_Range (RVA, Size, Item.Header) then
         Trace.Decrease_Indent ("No corresponding section found");
         return;
      end if;

      Section := Item.Index;
      Trace.Trace ("Found section " & File.Get_Section_Name (Section));
      Trace.Trace ("File ptr: " & Hex_Image (Item.Header.S_Scnptr));

      --  Load the section if not already done, then fetch its contents

      if Loader.Loaded (Section) = No_Loaded_Section then
         Loader.Loaded (Section) := File.Load_Section (Section);
      end if;
      Content := Binary_Files.Content (Loader.Loaded (Section));
      Relocate (Content, Arch.Arch_Addr (Item.Header.S_Vaddr));
      Trace.Decrease_Indent;
   end Lookup_Section;

   --------------------------
   -- Lookup_Section_Slice --
   --------------------------

   procedure Lookup_Section_Slice
     (File    : PE_File;
      Loader  : in out Section_Loader;
      RVA     : Unsigned_32;
      Size    : Unsigned_32;
      Content : out Binary_Content) is
   begin
      Lookup_Section (File, Loader, RVA, Size, Content);
      if Content = Invalid_Binary_Content then
         return;
      end if;

      Content :=
        Slice (Content, Arch.Arch_Addr (RVA), Arch.Arch_Addr (RVA + Size));
   end Lookup_Section_Slice;

   -----------------
   -- Read_String --
   -----------------

   procedure Read_String
     (File    : PE_File;
      Loader  : in out Section_Loader;
      RVA     : Unsigned_32;
      Content : out Binary_Content)
   is
      RVA_Addr : constant Arch.Arch_Addr := Arch.Arch_Addr (RVA);
   begin
      Lookup_Section (File, Loader, RVA, 1, Content);
      if Content = Invalid_Binary_Content then
         return;
      end if;

      --  Look for the NUL byte in the section, starting at RVA, as it marks
      --  the end of the requested string.

      Content := Slice (Content, RVA_Addr, Content.Last);
      declare
         S : String (1 .. Natural (Length (Content)))
         with Import, Address => Address_Of (Content, RVA_Addr);
      begin
         for I in S'Range loop
            if S (I) = ASCII.NUL then

               --  I is 1-bound, so the index of the NUL byte in Content is:
               --
               --     RVA_Addr + I - 1
               --
               --  However we do not want to return the NUL byte itself, so use
               --  the following "last" bound:
               --
               --     RVA_Addr + I - 2

               Content :=
                 Slice (Content, RVA_Addr, RVA_Addr + Arch.Arch_Addr (I) - 2);
               return;
            end if;
         end loop;
      end;
   end Read_String;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Fd : File_Descriptor; Filename : String_Access) return PE_File
   is
      use System.Storage_Elements;

      function To_Address is new
        Ada.Unchecked_Conversion (Str_Access, System.Address);

      Hdr_Off   : Long_Integer;
      Opt_Hdr32 : Opt_Hdr_PE32;
      Opt_Hdr64 : Opt_Hdr_PE32_Plus;
   begin
      Trace.Trace ("Opening " & Filename.all);

      Hdr_Off := Read_Coff_Header_Offset (Fd);

      --  The PE header has three parts: the signature, the file header and the
      --  optional header. Skip the signature, which is assumed to be "PE\0\0".

      if Hdr_Off /= 0 then
         Hdr_Off := Hdr_Off + 4;
      end if;

      return
         File : PE_File :=
           (Binary_File'(Create_File (Fd, Filename)) with others => <>)
      do
         File.Is_PE32_Plus := False;

         Lseek (Fd, Hdr_Off, Seek_Set);
         if Read (Fd, File.Hdr'Address, Filehdr_Size) /= Filehdr_Size then
            Exit_With_Error
              (File, Status_Read_Error, "failed to read COFF header");
         end if;

         File.Opt_Hdr_Off := Hdr_Off + Long_Integer (Filehdr_Size);
         if File.Hdr.F_Opthdr >= Unsigned_16 (Opt_Hdr_PE32_Size) then

            --  At this point, we don't know yet if the optional header is a
            --  PE32 one or a PE32+ one. Read a PE32 one and check the magic
            --  number.

            if Read (Fd, Opt_Hdr32'Address, Opt_Hdr_PE32_Size)
              /= Opt_Hdr_PE32_Size
            then
               Exit_With_Error
                 (File,
                  Status_Read_Error,
                  "failed to read COFF optional header (PE32)");
            end if;

            case Opt_Hdr32.Magic is
               when PE32_Magic     =>
                  if File.Hdr.F_Machine /= I386magic then
                     Outputs.Fatal_Error
                       ("Unhandled CPU for PE32:" & File.Hdr.F_Machine'Img);
                  end if;

                  --  We already have the header properly decoded

                  File.Image_Base := Arch.Arch_Addr (Opt_Hdr32.Image_Base);
                  File.Number_Of_RVA_And_Sizes :=
                    Opt_Hdr32.Number_Of_RVA_And_Sizes;

               when PE32Plus_Magic =>
                  if File.Hdr.F_Machine /= AMD64magic then
                     Outputs.Fatal_Error
                       ("Unhandled CPU for PE32+:" & File.Hdr.F_Machine'Img);
                  end if;

                  --  Re-read the optional header as a PE32+ one

                  Lseek (Fd, File.Opt_Hdr_Off, Seek_Set);
                  if Read (Fd, Opt_Hdr64'Address, Opt_Hdr_PE32_Plus_Size)
                    /= Opt_Hdr_PE32_Plus_Size
                  then
                     Exit_With_Error
                       (File,
                        Status_Read_Error,
                        "failed to read COFF optional header (PE32+)");
                  end if;

                  File.Is_PE32_Plus := True;
                  File.Image_Base := Arch.Arch_Addr (Opt_Hdr64.Image_Base);
                  File.Number_Of_RVA_And_Sizes :=
                    Opt_Hdr64.Number_Of_RVA_And_Sizes;

               when others         =>
                  Outputs.Fatal_Error
                    ("Invalid optional header magic: " & Opt_Hdr32.Magic'Img);
            end case;
         else
            File.Image_Base := 0;
            File.Number_Of_RVA_And_Sizes := 0;
         end if;

         Set_Nbr_Sections (File, Section_Index (File.Hdr.F_Nscns));

         File.Set_File_Region (Read (File.File));
         File.Data := To_Address (Data (File.File_Region));

         --  Map sections.
         File.Scn :=
           To_PE_Scn_Arr_Acc
             (File.Data
              + Storage_Offset (Hdr_Off)
              + Storage_Offset (File.Hdr.F_Opthdr)
              + Storage_Offset (Filehdr_Size));

         File.Str_Off := File.Hdr.F_Symptr + File.Hdr.F_Nsyms * Symesz;

         --  Load RVA and sizes of the import table

         Get_Data_Directory (Fd, File, Import_Table);
      end return;
   end Create_File;

   -------------
   -- Get_Hdr --
   -------------

   function Get_Hdr (File : PE_File) return Filehdr is
   begin
      return File.Hdr;
   end Get_Hdr;

   ------------------------
   -- Get_Section_Length --
   ------------------------

   function Get_Section_Length
     (File : PE_File; Index : Section_Index) return Arch.Arch_Addr
   is
      pragma Assert (Index < Section_Index (File.Hdr.F_Nscns));
      Sec : Scnhdr renames File.Scn (Index);
   begin
      --  Contrary to COFF, on PE S_Paddr is the real length
      pragma Assert (Sec.S_Size >= Sec.S_Paddr);
      if Sec.S_Paddr = 0 then
         --  For object files
         return Arch.Arch_Addr (Sec.S_Size);
      else
         --  For images
         return Arch.Arch_Addr (Sec.S_Paddr);
      end if;
   end Get_Section_Length;

   ----------------------------
   -- Extract_Nul_Terminated --
   ----------------------------

   function Extract_Nul_Terminated (S : String) return String is
   begin
      for I in S'Range loop
         if S (I) = ASCII.NUL then
            return S (S'First .. I - 1);
         end if;
      end loop;
      return S;
   end Extract_Nul_Terminated;

   ----------------------
   -- Get_Section_Name --
   ----------------------

   function Get_Section_Name
     (File : PE_File; Sec : Section_Index) return String
   is
      pragma Assert (Sec < Section_Index (File.Hdr.F_Nscns));
      Name : String renames File.Scn (Sec).S_Name;
   begin
      if Name (1) = '/' then
         --  Long section name, name in string table
         declare
            Num : Unsigned_32;
         begin
            Num := 0;
            for I in 2 .. Name'Last loop
               exit when Name (I) = ASCII.NUL;
               Num :=
                 Num * 10 + (Character'Pos (Name (I)) - Character'Pos ('0'));
            end loop;
            return Get_String (File, Num);
         end;
      else
         return Extract_Nul_Terminated (Name);
      end if;
   end Get_Section_Name;

   ----------------
   -- Get_Scnhdr --
   ----------------

   function Get_Scnhdr (File : PE_File; Sec : Section_Index) return Scnhdr is
   begin
      pragma Assert (Sec < Section_Index (File.Hdr.F_Nscns));
      return File.Scn (Sec);
   end Get_Scnhdr;

   ------------------
   -- Load_Section --
   ------------------

   function Load_Section
     (File : PE_File; Index : Section_Index) return Loaded_Section
   is
      Scn    : constant Scnhdr := Get_Scnhdr (File, Index);
      Result : constant Loaded_Section :=
        +Read (File.File, File_Size (Scn.S_Scnptr), File_Size (Scn.S_Size));
   begin
      if File_Size (Size (Result)) /= File_Size (Scn.S_Size) then
         raise Error;
      end if;
      return Result;
   end Load_Section;

   --------------------
   -- Get_Image_Base --
   --------------------

   function Get_Image_Base (File : PE_File) return Arch.Arch_Addr is
   begin
      return File.Image_Base;
   end Get_Image_Base;

   -----------------
   -- Get_Symbols --
   -----------------

   function Get_Symbols (File : PE_File) return Loaded_Section is
   begin
      return
        +Read
           (File.File,
            File_Size (File.Hdr.F_Symptr),
            File_Size (File.Hdr.F_Nsyms * Symesz));
   end Get_Symbols;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (File : PE_File; Off : Unsigned_32) return String is
      use System.Storage_Elements;
   begin
      return
        Dwarf_Handling.Read_String
          (File.Data + Storage_Offset (Off + File.Str_Off));
   end Get_String;

   ---------------------
   -- Get_Symbol_Name --
   ---------------------

   function Get_Symbol_Name (File : PE_File; Sym : Syment) return String is
   begin
      if Sym.E.E.E_Zeroes = 0 then
         return Get_String (File, Sym.E.E.E_Offset);
      else
         return Extract_Nul_Terminated (Sym.E.E_Name);
      end if;
   end Get_Symbol_Name;

   -------------------------
   -- DLL_Resolution_Path --
   -------------------------

   function DLL_Resolution_Path
     (Program_Filename : String) return Unbounded_String
   is
      Result : Unbounded_String;
      Sep    : constant String := (1 => Paths.Path_Separator);

      function Get_System_Directory
        (Buffer : System.Address; Size : int) return int
      with Import, External_Name => "gnatcov_get_system_directory";

      function Get_Windows_Directory
        (Buffer : System.Address; Size : int) return int
      with Import, External_Name => "gnatcov_get_windows_directory";

      Buffer : String (1 .. 257);
      Size   : int;
   begin
      US.Append
        (Result, Ada.Directories.Containing_Directory (Program_Filename));
      US.Append (Result, Sep);

      Size := Get_System_Directory (Buffer'Address, Buffer'Length);
      if Size > 0 then
         US.Append (Result, Buffer (1 .. Natural (Size)));
         US.Append (Result, Sep);
      end if;

      Size := Get_Windows_Directory (Buffer'Address, Buffer'Length);
      if Size > 0 then
         US.Append (Result, Buffer (1 .. Natural (Size)));
         US.Append (Result, Sep);
      end if;

      US.Append (Result, ".");
      US.Append (Result, Sep);

      US.Append (Result, Ada.Environment_Variables.Value ("PATH"));
      return Result;
   end DLL_Resolution_Path;

   -------------------
   -- Imported_DLLs --
   -------------------

   function Imported_DLLs (Filename : String) return File_Sets.Set is
      Visited : File_Sets.Set;
      --  Set of already processed executables

      Result : File_Sets.Set;
      --  Set of shared libraries that were found

      Queue : String_Sets.Set;
      --  Set of shared libraries still to be processed

      DLL_Path : constant Unbounded_String := DLL_Resolution_Path (Filename);
      --  Path used to resolve DLL dependencies

      procedure Process_File
        (Filename : Unbounded_String; Is_Dependency : Boolean);
      --  Locate and analyze the Filename executable (program or shared
      --  library).
      --
      --  Add to Queue its own dependencies. Also add the processed file to
      --  Result if it is a shared library (i.e. when Is_Dependency is True).

      procedure Process_Import_Table
        (File : PE_File; Loader : in out Section_Loader);
      --  Using Loader, iterate on File's import table and add the shared
      --  libraries dependencies found there to Queue.

      function Locate_File (Filename : Unbounded_String) return Virtual_File;
      --  Return the absolute filename corresponding to the Filename executable
      --  (possibly resolved through the PATH environment variable).
      --
      --  Note that this returns a path even if the file could not be found, so
      --  that we get a filename usable for the Visited set.

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File
        (Filename : Unbounded_String; Is_Dependency : Boolean)
      is
         Filename_Acc : GNAT.OS_Lib.String_Access;
         Fd           : File_Descriptor;
         Exec         : PE_File;
         Loader       : Section_Loader;

         --  To avoid redundant work, do not process the same executable
         --  multiple times.

         Resolved : constant Virtual_File := Locate_File (Filename);
      begin
         if Visited.Contains (Resolved) then
            return;
         end if;
         Visited.Insert (Resolved);

         --  Go no further if we could not find the executable on the
         --  filesystem.

         if not Resolved.Is_Regular_File then
            Warn_Not_Found (+Filename);
            return;
         elsif Is_Dependency then
            Result.Insert (Resolved);
         end if;

         Filename_Acc := new String'(+Resolved.Full_Name);
         Trace.Increase_Indent ("Processing " & Filename_Acc.all);
         Fd := Open_Read (Filename_Acc.all, Binary);
         if Fd = Invalid_FD then
            Trace.Decrease_Indent;
            Outputs.Fatal_Error ("Could not open " & Filename_Acc.all);
         end if;
         Inputs.Log_File_Open (Filename_Acc.all);
         Exec := Create_File (Fd, Filename_Acc);

         Initialize (Exec, Loader);
         Process_Import_Table (Exec, Loader);
         Free (Loader);

         Close_File (Exec);
         Close (Fd);
         Trace.Decrease_Indent;
      end Process_File;

      --------------------------
      -- Process_Import_Table --
      --------------------------

      procedure Process_Import_Table
        (File : PE_File; Loader : in out Section_Loader)
      is
         Dir      : Opt_Hdr_Data_Directory renames File.Import_Table;
         Data_Dir : Binary_Content;
      begin
         --  There is nothing to do if the import table is missing

         if Dir = No_Data_Directory then
            return;
         end if;

         --  Fetch the import table contents

         Lookup_Section_Slice
           (File, Loader, Dir.Virtual_Address, Dir.Size, Data_Dir);
         if Data_Dir = Invalid_Binary_Content then
            return;
         end if;

         Trace.Increase_Indent
           ("Processing import table at " & Hex_Image (Data_Dir.First));

         declare
            Count   : constant Natural :=
              Natural (Length (Data_Dir)) / Idata_Directory_Entry_Size;
            Entries : constant array (1 .. Count) of Idata_Directory_Entry
            with Import, Address => Address_Of (Data_Dir, Data_Dir.First);
            Name    : Binary_Content;
         begin
            Trace.Trace ("Found" & Count'Image & " entries");
            for E of Entries loop

               --  For each import table entry (i.e. imported DLL), we are
               --  interested only in the DLL name.

               exit when E.Name_RVA = 0;
               Read_String (File, Loader, E.Name_RVA, Name);
               if Name = Invalid_Binary_Content then
                  Trace.Trace ("Invalid name RVA: " & Hex_Image (E.Name_RVA));
               else
                  declare
                     S : String (1 .. Natural (Length (Name)))
                     with Import, Address => Address_Of (Name, Name.First);

                     DLL_Filename : constant Unbounded_String := +S;
                  begin
                     Trace.Increase_Indent ("DLL found: " & S);

                     --  Ignore API sets: resolving them is non trivial, and
                     --  these are system-provided libraries anyway: it is
                     --  reasonable to consider that no coverage buffer symbols
                     --  will be fund there.

                     if GNAT.Regexp.Match (S, API_Set_Pattern) then
                        Trace.Trace ("This is an API set: ignoring it");
                     else
                        Queue.Include (DLL_Filename);
                     end if;

                     Trace.Decrease_Indent;
                  end;
               end if;
            end loop;
         end;
         Trace.Decrease_Indent;
      end Process_Import_Table;

      -----------------
      -- Locate_File --
      -----------------

      function Locate_File (Filename : Unbounded_String) return Virtual_File is
         Located : String_Access;

         --  DLL_Path can be a fairly big string, so copying it on the
         --  secondary stack is unsafe: use GNAT's internal API to deal with
         --  the underlying string access directly.

         Path_S : Big_String_Access;
         Path_L : Natural;
      begin
         Get_String (DLL_Path, Path_S, Path_L);
         Located := Locate_Regular_File (+Filename, Path_S.all (1 .. Path_L));

         return
            Result : constant Virtual_File :=
              (if Located = null
               then Create (+(+Filename), Normalize => True)
               else Create (+Located.all))
         do
            Free (Located);
         end return;
      end Locate_File;

      Is_Dependency : Boolean := False;
      --  The first processed file is not considered as a dependency

      --  Start of processing for Imported_DLLs

   begin
      Queue.Include (+Filename);
      while not Queue.Is_Empty loop
         declare
            Filename : constant Unbounded_String := Queue.First_Element;
         begin
            Queue.Delete_First;
            Process_File (Filename, Is_Dependency);
            Is_Dependency := True;
         end;
      end loop;
      return Result;
   end Imported_DLLs;

end PECoff_Files;
