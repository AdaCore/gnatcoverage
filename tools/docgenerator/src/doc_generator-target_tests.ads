with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

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
         Code : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Target_Ref is access all Target;

   package Target_List is new Ada.Containers.Doubly_Linked_Lists
     (Target_Ref);

   All_Targets : Target_List.List := Target_List.Empty_List;

   --  a map to get easy access to a target from its function name

   package Target_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (String,
      Target_Ref,
      Ada.Strings.Hash,
      "=");

   T_Map : Target_Map.Map := Target_Map.Empty_Map;

end Doc_Generator.Target_Tests;

