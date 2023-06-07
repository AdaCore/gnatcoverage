
package body Hello is

   function Get_Hello (Formal : Boolean) return String;
   --  Returns Hello or Hi depending on the Formal parameter
   
   ---------------
   -- Get_Hello --
   ---------------
   
   function Get_Hello (Formal : Boolean) return String is
   begin
      if Formal then      -- # test_formal
         return "Hello";  -- # formal
      else
         return "Hi";     -- # casual
      end if;
   end Get_Hello;
   
   procedure Process (S : String) is
      Len : Integer;
      pragma Volatile (Len);
   begin
      Len := S'Length;
   end;
   
   ---------------
   -- Say_Hello --
   ---------------

   procedure Say_Hello (Who : String;  Formal : Boolean := True) is
      Hello : constant String := Get_Hello (Formal);
   begin
      if Who = "" then                          -- # test_nobody
         Process (Hello & "!");    -- # nobody
      else
         Process (Hello & " " & Who & "!"); -- # someone
      end if;
   end Say_Hello;

end Hello;

