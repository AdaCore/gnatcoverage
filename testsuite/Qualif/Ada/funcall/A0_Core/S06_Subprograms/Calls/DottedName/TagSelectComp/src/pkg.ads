pragma Ada_2012;

package Pkg is

    type Point is tagged record                                 -- # decl
        X, Y : Float;                                           -- # dstmt
    end record;                                                 -- # dstmt

    function Create (X, Y : Float) return Point is (X, Y);      -- # expr_pri

    type Pt_Acc is access function (X, Y : Float) return Point; -- # decl

    function Run_Callback (Callback : Pt_Acc) return Point;

    function Id (P : Point) return Point'Class is (P);          -- # expr_fun

    function Id (F : Float) return Float is (F);                -- # expr_fun

    function Get_X (P : Point) return Float is (P.X);           -- # expr_fun
end Pkg;
