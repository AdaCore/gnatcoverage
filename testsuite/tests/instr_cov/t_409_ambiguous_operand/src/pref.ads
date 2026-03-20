pragma Ada_2012;

package Pref is

   type Preference is new Boolean;

   function Get_Pref (P : Preference) return Preference is ((case P is
            when True => True,
            when False => False));

   function Get_Pref (P : Preference) return String
      is ((case P is
            when True => "TRUE",
            when False => "FALSE"));
end Pref;
