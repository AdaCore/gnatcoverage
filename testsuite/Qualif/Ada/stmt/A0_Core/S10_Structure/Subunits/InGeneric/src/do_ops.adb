with Ops; use Ops;
with Pack;

procedure Do_Ops (Do_Touch : Boolean; Opd : in out Opdata) is
   package Pack_Instance is new Pack (T => Opdata);
begin
   if Do_Touch then                       -- # doops
      Pack_Instance.Separate_Subp (Opd);  -- # gsub
   end if;
end Do_Ops;
