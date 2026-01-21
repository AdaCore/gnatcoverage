pragma Ada_2012;

package Action is

   type T_Action is abstract tagged private;

   type T_NA_Action_Class is not null access all T_Action'Class;

   procedure Init (Obj : in out T_Action) is abstract;

   procedure Run (Obj : in out T_Action) is abstract;

private
   type T_Action is abstract tagged null record;

end Action;
