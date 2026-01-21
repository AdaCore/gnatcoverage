pragma Ada_2012;

with Action;
with Interface_A;

package Package_A is

   type T_A is new Action.T_Action
     and Interface_A.T_Interface_A
   with private; -- Statement on the line was not executed

   type T_NA_A is not null access all T_A; -- Statement on the line was not executed

   function NA_A return T_NA_A;

   overriding procedure Init (Obj : in out T_A);

   overriding procedure Run (Obj : in out T_A);

   overriding function Get_Variable (Obj : in T_A) return Integer;

   type Enum is (ABC, DEF); -- Statement on the line was not executed

private

   type T_A is new Action.T_Action -- Statement on the line was not executed
     and Interface_A.T_Interface_A
   with record
      Variable : Integer;
      Enumeration : Enum;
   end record;

end Package_A;
