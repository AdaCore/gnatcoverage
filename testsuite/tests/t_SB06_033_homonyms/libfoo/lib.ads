pragma Ada_2012;

package Lib is
   function First return Integer;
   function Next (I : Integer) return Integer
   with Inline_Always;
   --  Always inlining this function is necessary to introduce code that
   --  references installed source files (i.e. stub files from gnatcov's point
   --  of view) for lib.adb into the _ada_main symbol (i.e. a routine analyzed
   --  for source coverage, from main.adb).
   --
   --  This makes "gnatcov coverage" try to get line info for this inlined
   --  code, i.e. get line info on a stub file, which used to make gnatcov fail
   --  on a precondition.
end;
