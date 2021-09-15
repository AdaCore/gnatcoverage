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

with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with Clang.CX_String;  use Clang.CX_String;
with Clang.Extensions; use Clang.Extensions;
with Clang.Index;      use Clang.Index;
with Clang.Rewrite;    use Clang.Rewrite;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Interfaces; use Interfaces;
with Interfaces.C;

package Instrument.C_Utils is

   package Cursor_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Cursor_T);

   -------------------------
   --  Location utilities --
   -------------------------

   function Sloc (Loc : Source_Location_T) return Source_Location;
   --  Convert a Source_Location_T to a Source_Location

   function Start_Sloc (N : Cursor_T) return Source_Location;
   --  Return the starting location of a node

   function End_Sloc (N : Cursor_T) return Source_Location;
   --  Return the end location of a node

   function Kind (N : Cursor_T) return Cursor_Kind_T;
   --  Return the kind of the node N

   function Is_Null (N : Cursor_T) return Boolean;
   --  Return True if the node N is a null cursor, false otherwise

   procedure Visit_Children (Parent : Cursor_T; Visitor : Cursor_Visitor_T);
   --  Wrapper for the Visit_Children clang procedure

   procedure Visit (Parent : Cursor_T; Visitor : Cursor_Visitor_T);
   --  Wrapper for the Visit clang procedure

   function Get_Children (N : Cursor_T) return Cursor_Vectors.Vector;
   --  Get all the children of a node. Beware, as the number of children for
   --  a given syntactic construct is not guaranteed to be the same: a while
   --  with no condition won't have the same number of children nodes as a
   --  while with all its fields filled.
   --
   --  This means that you can't rely on an index to design a specific
   --  children. You have in that case the option to extend libclang by adding
   --  functions in clang-wrapper.cc and the associated ada bindings in the
   --  package clang-extensions.

   function Is_Unit_Of_Interest
     (N        : Cursor_T;
      Filename : String) return Boolean;
   --  True if the node N corresponds to code originating from a unit of
   --  interest. This is used to avoid instrumenting included code (that comes
   --  from preprocessing).
   --
   --  TODO: For now, we make an even stronger assumption, and only return True
   --  for code that originates from the file being processed (e.g. included
   --  code won't be considered).

   function To_Vector (N : Cursor_T) return Cursor_Vectors.Vector;
   --  Turn the node N into a single element node vector

   function Get_Main (TU : Translation_Unit_T) return Cursor_T;

   --  Rewriting utilities

   procedure Add_Statement_In_Main
     (TU        : Translation_Unit_T;
      Rew       : Rewriter_T;
      Statement : String);
   --  Add a statement at the beginning of the main, and before any previously
   --  inserted string at the same location.

   procedure Add_Statement_Before_Return
     (Fun_Decl  : Cursor_T;
      Rew       : Rewriter_T;
      Statement : String);
   --  Add a statement before the return, and after any previously inserted
   --  string at the same location.

   procedure Add_Export
     (TU     : Translation_Unit_T;
      Rew    : Rewriter_T;
      Export : String);

   procedure Insert_Text_After_Start_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T);
   --  Inserts the string Text before the starting location of N.
   --
   --  If this procedure is called multiple times with the same N as parameter,
   --  the string will be inserted _after_ any previously inserted string.

   procedure Insert_Text_Before_Start_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T);
   --  Inserts the string Text before the starting location of N.
   --
   --  If this procedure is called multiple times with the same N as parameter,
   --  the string will be inserted _before_ any previously inserted string.

   procedure Insert_Text_After
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T);
   --  Same as Insert_Text_Before, but after the end location of N

   procedure Curlify
     (N   : Cursor_T;
      TU  : Translation_Unit_T;
      Rew : Rewriter_T)
   with Pre => not Is_Null (N);
   --  If the node N is not a compound statement, rewrite it by surrounding it
   --  with curly braces. Otherwise, do nothing.
   --
   --  If other text was previously inserted at the same location (N start loc,
   --  and N end loc), then the curly brace will be appended after that text,
   --  so make sure to call Curlify before further rewriting.

   --  Debugging

   procedure Print_Tokens (TU : Translation_Unit_T; N : Cursor_T);
   --  Print the tokens associated to the given node

end Instrument.C_Utils;