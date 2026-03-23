pragma Ada_2012;

package Bar is

   type My_Boolean is new Boolean with Convention => C, Object_Size => 8;

   type Rec is record
      Val : My_Boolean;
   end record;

   procedure Msg (M : String);
   procedure Proc (V : Rec; B : My_Boolean);

end Bar;
