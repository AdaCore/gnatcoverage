------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

private package Rundrv.Expander is

   --  This unit provides the services that perform macro substitutions
   --  for references in the driver config commands or args.

   function Expand_Command
     (Command : String_Access) return String_Access;
   --  Expand macro arguments in COMMAND. Return null if COMMAND is null or
   --  if the expanded value is empty. Return an access to the non-empty value
   --  with the macro reference expanded otherwise.

   function Expand_Arguments
     (Args, Eargs : String_List_Access) return String_List;
   --  Expand a possible macro reference in each argument of the provided ARGS
   --  list and return the new list of arguments with references expanded. An
   --  argument entry that ends up empty once expanded is simply skipped from
   --  the final list.
   --
   --  Use the provided EARGS list for %eargs. If a non-empty EARGS list is
   --  provided but no %eargs reference is found, append EARGS at the end of
   --  the list we return.

   --  The macro references are substituted with values returned by internal
   --  functions. Allowing expansion to yield an empty result is useful to let
   --  references operate by mere side effects of these function calls, e.g.
   --  to set environment variables, without actually inserting anything in
   --  the final command line.

   --  Macro references are recognized as %<macro-name>. For a given argument,
   --  text is allowed before and after the macro reference. This allows specs
   --  like "--trace=%trace,extra-arg-as-needed".  The set of recognized macro
   --  names is described below:

   ----------------------
   -- Available macros --
   ----------------------

   --  %exe:
   --  -----
   --  The executable filename provided on the command line, as-is,
   --  without fullpath translation.

   --  %exe_dir:
   --  ---------
   --  The directory part of %exe, possibly empty.

   --  %trace:
   --  -------
   --  Arguments to pass to the execution engine to request the production of
   --  a trace, for engines that support parameters controlling history.  This
   --  our output trace filename prefixed by a history control argument if we
   --  are targeting mcdc analysis. The prefix is "histmap=<decision-map>," if
   --  we have SCOs at hand, just "history," otherwise.

   --  %tracefile:
   --  -----------
   --  Our output trace filename.

   --  %eargs:
   --  -------
   --  The -eargs list passed on the command line.

   --  %valgrind:
   --  ----------
   --  Valgrind command name to use.
   --  "<prefix>/libexec/gnatcoverage/bin/valgrind" if this exists, meaning we
   --  have a valgrind install tree bundled-in. "valgrind" otherwise, meaning
   --  we'll rely on PATH to provide a suitable valgrind with the coverage tool
   --  installed.

   --  %set_valgrind_env:
   --  ------------------
   --  When we have a valgrind install bundled-in, set the VALGRIND_LIB
   --  environment variable to designate the corresponding lib dir, where
   --  our coverage tool should have been installed together with the core
   --  valgrind libs.

   ---------------------------------
   -- Valgrind selection strategy --
   ---------------------------------

   --  The logic here is as follows:

   --  * When it is there, resort to a bundled-in verion of ours that we
   --    know works in most regular cases and that we could improve over
   --    time for corner situations if real need be.
   --
   --  * Arrange for things to just work by default in environments where
   --    there is no bundled-in version but possibly a proper one on PATH,
   --    e.g. when building manually from source. The coverage plugin must
   --    have been installed in the corresponding lib dir in this case.

   --  * If there's no bundled-in version and there is no valgrind on PATH
   --    or the one there doesn't work (e.g. < 3.7.0), users can get things
   --    to work by just installing a recent one (>= 3.7.0) on PATH, plus
   --    the coverage plugin.
   --
   --  * If there is a bundled-in version and it doesn't work for a given
   --    app (e.g. fork/exec failure because valgrind is located not at its
   --    configured prefix), move it away and fallback on a PATH oriented
   --    strategy.

private

   --  For performance reasons, we might need to segregate sets for commands
   --  or arguments based on actual needs expressed in our internal config
   --  database, allowing to minimize useless matching attempts. While not yet
   --  at this point, we have setup provisions for this possibility.

   function Exe return String;
   function Exe_Dir return String;
   function Trace return String;
   function Tracefile return String;

   function Set_Valgrind_Env return String;
   function Valgrind return String;

   --  A table saying which value function to call for each macro. Better
   --  extracted out to be computed once only. This is scanned in order during
   --  expansion and entries with a common macro-name prefix should be ordered
   --  such that the longest name comes first.

   type Smacro_Entry is record
      Key  : String_Access;
      Eval : access function return String;
   end record;

   type Smacro_Table is array (Integer range <>) of Smacro_Entry;

   Common_Smtable : aliased constant Smacro_Table :=
     ((Key => new String'("%exe"),
       Eval => Exe'Access),

      (Key => new String'("%exe_dir"),
       Eval => Exe_Dir'Access),

      (Key => new String'("%tracefile"),
       Eval => Tracefile'Access),

      (Key => new String'("%trace"),
       Eval => Trace'Access),

      (Key => new String'("%valgrind"),
       Eval => Valgrind'Access),

      (Key => new String'("%set_valgrind_env"),
       Eval => Set_Valgrind_Env'Access)
     );

   Smtable_For_Args : Smacro_Table renames Common_Smtable;
   Smtable_For_Commands : Smacro_Table renames Common_Smtable;

end Rundrv.Expander;
