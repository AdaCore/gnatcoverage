with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

package Doc_Generator.Target_Tests is

   procedure Parse_File (Path : String);

   procedure Print;

private

   Target_Tag : constant String := "--  @target";
   Id_Tag : constant String := "--  @id{";

   type Target is
      record
         ID : Ada.Strings.Unbounded.Unbounded_String;
         Subprogram : Ada.Strings.Unbounded.Unbounded_String;
         Description : Ada.Strings.Unbounded.Unbounded_String;
         In_File : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Target_Ref is access all Target;

   package Target_List is new Ada.Containers.Doubly_Linked_Lists
     (Target_Ref);

   All_Targets : Target_List.List := Target_List.Empty_List;

end Doc_Generator.Target_Tests;

