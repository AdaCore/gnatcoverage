separate(Pkg)
package body Sep is

   Baz : Boolean := False; -- # decl
   function Bar return Boolean is
   begin
      return Foo or else Baz; -- # eval
   end Bar;

end Sep;
