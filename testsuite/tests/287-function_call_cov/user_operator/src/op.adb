pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

procedure Op
is
    function "+" (L, R : Boolean) return String is ("Executed"); -- # fun

    T :          Boolean := True;                                -- # t_decl
    S : constant String  := T + T;                               -- # op_call
begin
    Put_Line (S);                                                -- # put_line
end Op;
