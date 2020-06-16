pragma Ada_2012;

with Ada.Tags; use Ada.Tags;
package body Values is

   function R_Locate (X : Integer; R : Range_T) return Position_T is
      Pos : Position_T; -- # range_decl
   begin
      Pos := (if X >= R.LB and then X <= R.UB -- # range_assign
	      then In_Range else Out_Range);  -- # range_expr
      return Pos; -- # range_return
   end;

  function P_Locate (X : Integer; P : Point_T) return Position_T is
      Pos : Position_T;	 -- # point_decl
   begin
      Pos := (if X = P.V then In_Range else Out_Range); -- # point_assign
      return Pos; -- # point_return
   end;

   type Ikind_T is (IK_Range, IK_Point, IK_Unknown);

   function Ikind (I : Interval_T'Class) return Ikind_T is
   begin
      return -- # ikind_return
	(if I'Tag = Range_T'Tag then IK_Range -- # ikind_expr
	else (if  I'Tag = Point_T'Tag then IK_Point -- # ikind_expr
	      else IK_Unknown)); -- # ikind_expr
   end;

   function Locate (X : Integer; I : Interval_T'Class) return Position_T is
   begin
      return (case Ikind(I) is -- # loc_return
	 when IK_Range => R_Locate (X, Range_T(I)),  -- # loc_expr
	 when IK_Point => P_Locate (X, Point_T(I)),  -- # loc_expr
	 when IK_Unknown => Out_Range);  -- # loc_expr
   end;
end;
