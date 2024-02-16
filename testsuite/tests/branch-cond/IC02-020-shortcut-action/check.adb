package body Check is

   K_Nul : constant Character := '0';

   function Validate (This : T_Str; Str : String) return Boolean is
   begin
      pragma Warnings (Off, "index for * may assume lower bound of 1");
      return
        (This.Len <= This.Size
           and then ((This.Len = 0 and then Str (1) = K_Nul)
                     or else (Str (1) = This.Tab (1)
                                and then Str (This.Len) = This.Tab (This.Len)
                                and then Str (This.Len + 1) = K_Nul)));
      pragma Warnings (On, "index for * may assume lower bound of 1");
   end;
end;
