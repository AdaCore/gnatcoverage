--  This package contains declarations with discrete subtype definitions at
--  the library level and as local declarations.

with Support; use Support;
package Discrete_Subtype_Definitions is

   type Global_Arr is array (Integer range Identity (1) .. 10) of Float; -- # dcl

   generic
      Par : Integer;
   package P1_G is
      Arr : array (Integer range 1 .. Identity (Par)) of Integer;  -- # g1_dcl
   end P1_G;

   generic
      Par : Integer;
   package P2_G is
      type Arr is array (Integer range 1 .. Identity (Par)) of Integer; -- # g2_dcl
   end P2_G;

   function Some_Fun_1
     (I, J, K, L : Integer)
      return       Boolean;

   function Some_Fun_2
     (I, J, K, L : Integer)
      return       Boolean;
   --  Two completely meaningless functions, their purpose is to create the
   --  real-life-looking contest for a discrete subtype definition in the
   --  body.

end Discrete_Subtype_Definitions;
