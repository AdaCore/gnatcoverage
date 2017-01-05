------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Strings;

with Strings; use Strings;

--  This generic package is meant to take care of parsing command-line
--  arguments: match a subcommand, match the options involved and extract
--  the corresponding values in a synthetic data structure. It also provides
--  automatic documentation (--help) generation.
--
--  Using this package is quite simple:
--
--    * Instanciate it with types that provide the set of supported
--      commands/options.
--
--    * Create a parser (Parser_Type) with synthetic descriptions of commands
--      (Command_Info) and options (*_Option_Info), and potentially callbacks
--      for parsing events.
--
--    * If needed, display help messages (Print_Usage).
--
--    * Parse some command-line arguments and get a result from it (Parse).
--
--    * If you have multiple command line sources (for instance real command
--      line and options from project files), you can parse both separately and
--      then merge the result (Merge).
--
--    * When you are done, destroy the parser (Destroy).
--
--  Note that by "argument", we mean a mere string from some arguments list
--  whereas "option" can designate a single argument (--verbose) or multiple
--  ones (--output report.log).
--
--  This package distinguishes three kinds of options: boolean (Bool_*), single
--  string (String_*) and multiple strings (String_List_*). Each is parsed
--  differently and produces different results:
--
--    * Boolean options do not accept values: either they are present, either
--      they are absent. The result is a boolean telling whether the option was
--      present at least once on the command-line.
--
--    * Single string options accept one value, which defines the result
--      corresponding to this option. If the option is allowed to appear
--      multiple times, only the last value provided is considered.
--
--    * Multiple strings options have two forms: greedy and non greedy. Greedy
--      ones capture all the arguments that remain on the command-line. Non
--      greedy ones accept one argument that is appened to the result, so one
--      has to introduce them multiple times on the command-line in order to
--      get multiple strings in the end.
--
--  Each kind of option can have multiple long and short names (as they will
--  appear on command-lines). Long names are of the form "--[LETTERS]" while
--  short names are of the form "-[SINGLE-LETTER]". There is an exception to
--  this: we allow long names to have a single dash prefix for oddities such as
--  "-cargs": in this case, for instance, the "-c" short option is reserved to
--  disallow ambiguity.
--
--  Each option has a set of subcommands for which it is valid. This makes it
--  possible to have multiple options that have similiar long/short names, as
--  long as they have no subcommand is common.

generic
   type Command_Type is (<>);
   --  Enumeration type for the available subcommands. Command_Type'First is
   --  used as a "no-command" in situations that allow it.

   type Bool_Options is (<>);
   --  Enumeration type providing the set of boolean options

   type String_Options is (<>);
   --  Enumeration type providing the set of single string options

   type String_List_Options is (<>);
   --  Enumeration type providing the set of multiple strings options

package Argparse is

   -----------------------
   -- Commands handling --
   -----------------------

   No_Command    : constant Command_Type := Command_Type'First;
   First_Command : constant Command_Type := Command_Type'Succ (No_Command);
   Last_Command  : constant Command_Type := Command_Type'Last;

   subtype Valid_Commands is
     Command_Type range First_Command .. Last_Command;
   --  Subset of Command_Type that excludes No_Command

   type Command_Info is private;
   --  Holder for a subcommand synthetic description

   type Command_Info_Array is
     array (Valid_Commands) of aliased Command_Info;

   type Command_Set is array (Valid_Commands) of Boolean with Pack;

   All_Commands : Command_Set := (others => True);

   function Create
     (Name        : String;
      Pattern     : String := "";
      Description : String;
      Internal    : Boolean)
      return Command_Info;
   --  Create a subcommand description.
   --
   --  Name is the name of the subcommand, as it will appear on the command
   --  line.
   --
   --  Pattern is a textual description of how the command should be used (i.e.
   --  with what options). For instance: "[OPTIONS] --foo=[BAR] [[BAZ] ...]".
   --
   --  Description is a textual description explaining what the subcommand
   --  does.
   --
   --  Internal indicates whether this command is meant to be internal or
   --  publicly documented.

   ----------------------
   -- Options handling --
   ----------------------

   type Bool_Option_Info is private;
   --  Holder for a boolean option synthetic description

   type String_Option_Info is private;
   --  Holder for a single string option synthetic description

   type String_List_Option_Info is private;
   --  Holder for a multiple strings option synthetic description

   function Create
     (Long_Name, Short_Name, Help : String := "";
      Commands                    : Command_Set := All_Commands;
      Internal                    : Boolean)
      return Bool_Option_Info
     with Pre => Long_Name'Length > 0 or else Short_Name'Length > 0;
   --  Create a boolean option description. Long_Name and Short_Name are
   --  |-separated lists of options names.

   function Create
     (Long_Name, Short_Name, Help : String := "";
      Commands                    : Command_Set := All_Commands;
      Internal, At_Most_Once      : Boolean;
      Pattern                     : String := "")
      return String_Option_Info
     with Pre => Long_Name'Length > 0 or else Short_Name'Length > 0;
   --  Create a single string option description. Long_Name and Short_Name are
   --  |-separated lists of options names. If At_Most_Once is true, the command
   --  cannot appear multiple times on the command-line.

   function Create
     (Long_Name, Short_Name, Help : String := "";
      Commands                    : Command_Set := All_Commands;
      Internal                    : Boolean;
      Greedy                      : Boolean := False;
      Pattern                     : String := "")
      return String_List_Option_Info
     with Pre => Long_Name'Length > 0 or else Short_Name'Length > 0;
   --  Create a multiple strings option description. Long_Name and Short_Name
   --  are |-separated lists of options names.

   type Bool_Option_Info_Array is
     array (Bool_Options) of aliased Bool_Option_Info;
   type String_Option_Info_Array is
     array (String_Options) of aliased String_Option_Info;
   type String_List_Option_Info_Array is
     array (String_List_Options) of aliased String_List_Option_Info;

   type Option_Kind is (Bool_Opt, String_Opt, String_List_Opt);
   type Option_Reference (Kind : Option_Kind := Bool_Opt) is record
      case Kind is
         when Bool_Opt =>
            Bool_Option        : Bool_Options;
         when String_Opt =>
            String_Option      : String_Options;
         when String_List_Opt =>
            String_List_Option : String_List_Options;
      end case;
   end record;

   ----------------------
   -- Parsers handling --
   ----------------------

   type Parser_Type is private;
   --  Hold a parser. Parsers must be created with Create and, when done with
   --  them, destroyed with Destroy.

   No_Parser : constant Parser_Type;
   --  Value for a not-yet-created parser

   type Parsed_Arguments;

   type Bool_Callback_Type is access procedure
     (Result : in out Parsed_Arguments;
      Option : Bool_Options);
   --  Callback invoked when parsing a boolean option

   type String_Callback_Type is access procedure
     (Result : in out Parsed_Arguments;
      Option : String_Options;
      Value  : String);
   --  Callback invoked when parsing a single string option

   type String_List_Callback_Type is access procedure
     (Result : in out Parsed_Arguments;
      Option : String_List_Options;
      Value  : String);
   --  Callback invoked when parsing a multiple strings option

   type Arg_Callback_Type is access procedure
     (Result : in out Parsed_Arguments;
      Value  : String);
   --  Callback invoked when parsing an argument that is not an option

   function Create
     (Command_Infos        : Command_Info_Array;
      Bool_Infos           : Bool_Option_Info_Array;
      String_Infos         : String_Option_Info_Array;
      String_List_Infos    : String_List_Option_Info_Array;

      Bool_Callback        : Bool_Callback_Type := null;
      String_Callback      : String_Callback_Type := null;
      String_List_Callback : String_List_Callback_Type := null;
      Arg_Callback         : Arg_Callback_Type := null)
      return Parser_Type;
   --  Create a new parser. When done with it, it must be destroyed with
   --  Destroy.
   --
   --  This will raise a Program_Error if the set of options is not valid
   --  (for instance, invalid or conflicting option names).

   procedure Destroy (Parser : in out Parser_Type);
   --  Destroy a parser and set it to No_Parser

   procedure Print_Usage
     (Parser        : Parser_Type;
      With_Internal : Boolean;
      Short         : Boolean;
      For_Command   : Command_Type := No_Command);
   --  Print on standard output an automatically generated help message for
   --  Parser. If With_Internal is true, descriptions for internal subcommands
   --  and options are included.
   --
   --  If Short, just print the pattern (general one if For_Command is
   --  No_Command, command-specific one otherwise) and say how to get
   --  more help.
   --
   --  If For_Command is No_Command, this displays descriptions for both
   --  subcommands and options. Otherwise, this displayes a description only
   --  for For_Command and for the options it accepts; in this case, if
   --  For_Command is internal, internal options are displayed regardless
   --  of With_Internal.

   type String_Option (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True  => Value : Unbounded_String;
      end case;
   end record;
   --  For single string options, we distinguish the case when the options did
   --  not appear on the command-line and the case when the option specified an
   --  empty value. This is specified thanks to the Present distriminant.

   type Bool_Array is array (Bool_Options) of Boolean;
   type String_Array is array (String_Options) of String_Option;
   type String_List_Array is
     array (String_List_Options) of String_Vectors.Vector;

   type Parsed_Arguments is record
      Error            : Unbounded_String;
      --  If this is set to an non-empty string (which is an error message),
      --  all the other members, except Command, must be considered as garbage
      --  (and thus not be used).

      Command          : Command_Type := No_Command;
      --  The subcommand that was matched. No_Command if no command could not
      --  be matched.

      Bool_Args        : Bool_Array := (others => False);
      --  For each boolean option, whether the option appeared on the
      --  command-line.

      String_Args      : String_Array;
      --  For each single string option, tells whether the option appeared on
      --  the command-line. If it appeared, it provides the value from the last
      --  argument.

      String_List_Args : String_List_Array;
      --  For each multiple string option, provide the list of matched values

      Remaining_Args   : String_Vectors.Vector;
      --  List of all arguments that were not options.
   end record;
   --  Data structure holding the result of command-line parsing

   function Parse
     (Parser       : Parser_Type;
      Args         : GNAT.Strings.String_List_Access;
      With_Command : Command_Type := No_Command;
      Callback     : access procedure (Result : in out Parsed_Arguments;
                                       Ref    : Option_Reference) := null)
      return Parsed_Arguments;
   --  Decode the arguments in Source according to the parsing rules in Parser
   --  and return the decoded values.
   --
   --  If With_Command is No_Command, this expects the first argument to be a
   --  command. Otherwise, it just parses options and return the input command.
   --
   --  If not null, Callback is invoked on all matched options.

   procedure Merge
     (Args       : in out Parsed_Arguments;
      Other_Args : Parsed_Arguments)
     with Pre =>
       (Ada.Strings.Unbounded.Length (Args.Error) = 0
        and then Ada.Strings.Unbounded.Length (Other_Args.Error) = 0
        and then
          (Args.Command = No_Command
           or else Args.Command = Other_Args.Command));
   --  Merge two sets of parsed arguments. Arguments from Other_Args take
   --  precedence.

   function Command_Name
     (Parser  : Parser_Type;
      Command : Valid_Commands)
      return String;
   --  Return the name for Command according to Parser. It's the name as it
   --  appears on command-lines.

   function Option_Name
     (Parser : Parser_Type;
      Ref    : Option_Reference)
      return String;
   --  Return the name for Ref according to Parser. It's a |-separated list of
   --  name as they appear on the command-lines.

   function Is_Present
     (Args : Parsed_Arguments;
      Ref  : Option_Reference) return Boolean;
   --  Return whether the Ref option is present in Args

private

   type Command_Info is record
      Name, Pattern, Description : Unbounded_String;
      Internal                   : Boolean;
   end record;

   type Option_Info is abstract tagged record
      Long_Name, Short_Name : Unbounded_String;
      Help                  : Unbounded_String;
      Commands              : Command_Set;
      Internal              : Boolean;
   end record;

   type Bool_Option_Info is new Option_Info with null record;

   type String_Option_Info is new Option_Info with record
      At_Most_Once : Boolean;
      Pattern      : Unbounded_String;
   end record;

   type String_List_Option_Info is new Option_Info with record
      Greedy  : Boolean;
      Pattern : Unbounded_String;
   end record;

   type Parser_Record;
   type Parser_Type is access Parser_Record;

   No_Parser : constant Parser_Type := null;

end Argparse;
