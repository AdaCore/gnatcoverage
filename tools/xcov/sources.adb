------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

--  Source locations

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Sources is

   Filenames : Filename_Vectors.Vector;

   function Abridged_Image
     (Sloc : Source_Location;
      Ref  : Source_Location) return String;
   --  Return the image of Sloc, omitting elements that are common with Ref

   package Filename_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => String_Acc,
      Element_Type    => Source_File_Index,
      Hash            => Hash,
      Equivalent_Keys => Equal,
      "="             => "=");

   Filename_Map : Filename_Maps.Map;

   --  Source rebase/search types

   type Source_Search_Entry;
   type Source_Search_Entry_Acc is access Source_Search_Entry;
   type Source_Search_Entry is record
      Prefix : String_Acc;
      Next : Source_Search_Entry_Acc;
   end record;

   type Source_Rebase_Entry;
   type Source_Rebase_Entry_Acc is access Source_Rebase_Entry;
   type Source_Rebase_Entry is record
      Old_Prefix : String_Acc;
      New_Prefix : String_Acc;
      Next : Source_Rebase_Entry_Acc;
   end record;

   First_Source_Rebase_Entry : Source_Rebase_Entry_Acc := null;
   Last_Source_Rebase_Entry  : Source_Rebase_Entry_Acc := null;

   First_Source_Search_Entry : Source_Search_Entry_Acc := null;
   Last_Source_Search_Entry  : Source_Search_Entry_Acc := null;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Source_Location) return Boolean is
   begin
      if L = No_Location then
         return False;
      elsif R = No_Location then
         return True;
      end if;

      if L.Source_File < R.Source_File then
         return True;

      elsif L.Source_File > R.Source_File then
         return False;
      end if;

      if L.Line < R.Line then
         return True;

      elsif L.Line > R.Line then
         return False;
      end if;

      return L.Column < R.Column;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Source_Location) return Boolean is
   begin
      return L < R or else L = R;
   end "<=";

   --------------------
   -- Abridged_Image --
   --------------------

   function Abridged_Image
     (Sloc : Source_Location;
      Ref  : Source_Location) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      Show_Line, Show_Column : Boolean;
   begin
      if Sloc = No_Location then
         return "<no loc>";
      end if;

      if Sloc.Source_File /= Ref.Source_File then
         Result := To_Unbounded_String (Get_Name (Sloc.Source_File) & ":");
         Show_Line   := True;
         Show_Column := True;
      else
         Show_Line   := Sloc.Line /= Ref.Line;
         Show_Column := Show_Line or else Sloc.Column /= Ref.Column;
      end if;

      if Show_Line then
         Result := Result & Trim (Sloc.Line'Img, Both) & ":";
      end if;

      if Show_Column then
         Result := Result & Trim (Sloc.Column'Img, Both);
      end if;

      return To_String (Result);
   end Abridged_Image;

   -----------------------
   -- Add_Source_Search --
   -----------------------

   procedure Add_Source_Search (Prefix : String)
   is
      E : Source_Search_Entry_Acc;
   begin
      E := new Source_Search_Entry'(Prefix => new String'(Prefix),
                                    Next => null);
      if First_Source_Search_Entry = null then
         First_Source_Search_Entry := E;
      else
         Last_Source_Search_Entry.Next := E;
      end if;
      Last_Source_Search_Entry := E;
   end Add_Source_Search;

   -----------------------
   -- Add_Source_Rebase --
   -----------------------

   procedure Add_Source_Rebase (Old_Prefix : String; New_Prefix : String) is
      E : Source_Rebase_Entry_Acc;
   begin
      E := new Source_Rebase_Entry'(Old_Prefix => new String'(Old_Prefix),
                                    New_Prefix => new String'(New_Prefix),
                                    Next => null);
      if First_Source_Rebase_Entry = null then
         First_Source_Rebase_Entry := E;
      else
         Last_Source_Rebase_Entry.Next := E;
      end if;
      Last_Source_Rebase_Entry := E;
   end Add_Source_Rebase;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Name : String) return Source_File_Index is
      use Filename_Maps;
      Nam : aliased String := Name;
      Cur : constant Cursor := Filename_Map.Find (Nam'Unrestricted_Access);
   begin
      if Cur /= No_Element then
         return Element (Cur);

      else
         declare
            New_Name : constant String_Acc := new String'(Name);
         begin
            Filenames.Append (New_Name);
            Filename_Map.Insert (New_Name, Filenames.Last_Index);
            return Filenames.Last_Index;
         end;
      end if;
   end Get_Index;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Index : Source_File_Index) return String is
   begin
      return Filenames.Element (Index).all;
   end Get_Name;

   -----------
   -- Image --
   -----------

   function Image (Sloc : Source_Location) return String is
   begin
      return Abridged_Image (Sloc, Ref => No_Location);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (First_Sloc, Last_Sloc : Source_Location) return String is
   begin
      if First_Sloc = Last_Sloc then
         return Abridged_Image (First_Sloc, Ref => No_Location);
      else
         return Abridged_Image (First_Sloc, Ref => No_Location)
           & "-" & Abridged_Image (Last_Sloc, Ref => First_Sloc);
      end if;
   end Image;

   ----------
   -- Open --
   ----------

   procedure Open
     (File    : in out File_Type;
      Index   : Source_File_Index;
      Success : out Boolean)
   is

      procedure Try_Open
        (File    : in out File_Type;
         Name    : String;
         Success : out Boolean);
      --  Try to open Name, with no rebase/search information. In case of
      --  a success,

      --------------
      -- Try_Open --
      --------------

      procedure Try_Open
        (File    : in out File_Type;
         Name    : String;
         Success : out Boolean) is
      begin
         Open (File, In_File, Name);
         Success := True;
      exception
         when Name_Error =>
            Success := False;
      end Try_Open;

      Name : constant String := Get_Name (Index);
   begin
      --  Try original path

      Try_Open (File, Name, Success);

      --  Try to rebase

      if not Success then
         declare
            E : Source_Rebase_Entry_Acc := First_Source_Rebase_Entry;
            First : constant Positive := Name'First;
         begin
            while E /= null loop
               if Name'Length > E.Old_Prefix'Length
                 and then (Name (First .. First + E.Old_Prefix'Length - 1)
                           = E.Old_Prefix.all)
               then
                  Try_Open (File,
                            E.New_Prefix.all
                            & Name (First + E.Old_Prefix'Length
                                    .. Name'Last),
                            Success);
                  exit when Success;
               end if;
               E := E.Next;
            end loop;
         end;
      end if;

      --  Try source path

      if not Success then
         declare
            E : Source_Search_Entry_Acc := First_Source_Search_Entry;
         begin
            while E /= null loop
               Try_Open (File, E.Prefix.all & '/' & Name, Success);
               exit when Success;
               E := E.Next;
            end loop;
         end;
      end if;
   end Open;

end Sources;
