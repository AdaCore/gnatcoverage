-- Common functional code that will get to be called from various contexts

package Colors is

   type Color is (Red, Green, Blue); -- color values
   type Code  is (R, G, B); -- color codes

   function Code_For (Colv : Color) return Code;

end;
