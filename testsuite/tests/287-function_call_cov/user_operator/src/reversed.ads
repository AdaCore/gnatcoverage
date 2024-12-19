pragma Ada_2012;

package Reversed is
    type T is new Integer;                              -- # ok

    One : T := 1;                                       -- # ok
    Two : T := 2;                                       -- # ok

    --  Let's make it go the other way to keep things interesting
    function "<" (L, R : T) return Boolean is (L > R);  -- # ok

    package Inner is
        function "+" (Arg : T) return T is (0 - Arg);   -- # ok
    end Inner;
end Reversed;
