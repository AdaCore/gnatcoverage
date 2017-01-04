pragma Restrictions (No_Elaboration_Code);

package Foo is
   type Myint is new Integer;   --# decl
   X : Myint := 12;             --# decl
   procedure Set_X (V : Myint); --# decl
end;

