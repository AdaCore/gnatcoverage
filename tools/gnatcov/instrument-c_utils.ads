------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with Clang.CX_Source_Location; use Clang.CX_Source_Location;
with Clang.Index;              use Clang.Index;
with Clang.Rewrite;            use Clang.Rewrite;

with Instrument.Common; use Instrument.Common;
with Slocs;             use Slocs;

package Instrument.C_Utils is

   package Cursor_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Cursor_T);

   type Cursor_Visitor_Function is access function
     (Node : Cursor_T) return Child_Visit_Result_T with Convention => C;

   ------------------------
   -- Location utilities --
   ------------------------

   function Sloc (Loc : Source_Location_T) return Source_Location;
   --  Convert a Source_Location_T to a Source_Location

   function Presumed_Spelling_Location
     (TU             : Translation_Unit_T;
      Loc            : Source_Location_T;
      Macro_Name     : Unbounded_String;
      Builtin_Macros : Macro_Set) return Source_Location;
   --  Assuming Loc is a macro expansion location that refers to the expansion
   --  of Macro_Name, return its presumed spelling location, i.e. the actual
   --  source location of its expansion with command-line / built-in macros
   --  accounted for. If Macro_Name refers to a built-in macro (i.e. in
   --  the Builtin_Macros set), set the filename for the returned sloc
   --  to "<built-in>".

   function Start_Sloc (N : Cursor_T) return Source_Location_T;
   function Start_Sloc (N : Cursor_T) return Source_Location;
   --  Return the starting location of a node

   function End_Sloc (N : Cursor_T) return Source_Location_T;
   function End_Sloc (N : Cursor_T) return Source_Location;
   --  Return the end location of a node

   function Kind (N : Cursor_T) return Cursor_Kind_T;
   --  Return the kind of the node N

   function Is_Null (N : Cursor_T) return Boolean;
   --  Return True if the node N is a null cursor, false otherwise

   procedure Visit_Children
     (Parent  : Cursor_T;
      Visitor : not null access function
                  (Node : Cursor_T) return Child_Visit_Result_T);
   --  Wrapper for the Visit_Children clang procedure

   procedure Visit
     (Parent  : Cursor_T;
      Visitor : not null access function
                  (Node : Cursor_T) return Child_Visit_Result_T);
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

   function To_Vector (N : Cursor_T) return Cursor_Vectors.Vector;
   --  Turn the node N into a single element node vector

   function Get_Main (TU : Translation_Unit_T) return Cursor_T;
   --  Return the cursor corresponding to the definition of the "main"
   --  function, or the null cursor if there is no main function.

   function Is_Atexit_Declared (TU : Translation_Unit_T) return Boolean;
   --  Return True if there is a declaration of atexit in the given translation
   --  unit.

   -------------------------
   -- Rewriting utilities --
   -------------------------

   procedure Insert_Text_After_Start_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T);
   --  Inserts the string Text at the start location of N.
   --
   --  If this procedure is called multiple times with the same N as parameter,
   --  the string will be inserted _after_ any previously inserted string.

   procedure Insert_Text_In_Brackets
     (CmpdStmt : Cursor_T;
      Text     : String;
      Rew      : Rewriter_T);
   --  Insert the string Text just after the opening bracket of N.
   --
   --  This procedure shall be called with CompoundStmt nodes and will fail
   --  otherwise.

   procedure Insert_Text_Before_Start_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T);
   --  Inserts the string Text at the start location of N.
   --
   --  If this procedure is called multiple times with the same N as parameter,
   --  the string will be inserted _before_ any previously inserted string.

   procedure Insert_Text_After_End_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T);
   --  Inserts the string Text at the end location of N.
   --
   --  If this procedure is called multiple times with the same N as parameter,
   --  the string will be inserted _after_ any previously inserted string.

   procedure Insert_Text_Before_End_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T);
   --  Inserts the string Text at the end location of N.
   --
   --  If this procedure is called multiple times with the same N as parameter,
   --  the string will be inserted _before_ any previously inserted string.

   procedure Iterate_Tokens
     (TU      : Translation_Unit_T;
      N       : Cursor_T;
      Process : not null access procedure (Tok : Token_T));
   --  Call Process on all of the tokens within the source range of N

   ---------------
   -- Debugging --
   ---------------

   procedure Print_Tokens (TU : Translation_Unit_T; N : Cursor_T);
   --  Print the tokens associated to the given node

end Instrument.C_Utils;
