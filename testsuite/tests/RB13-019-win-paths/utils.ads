with Interfaces.C.Strings; use Interfaces.C.Strings;

package Utils is
   procedure Print (Msg : chars_ptr);
   pragma Export (C, Print, "utils_print");
end Utils;
