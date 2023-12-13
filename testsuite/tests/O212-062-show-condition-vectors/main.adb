with Pkg; use Pkg;
procedure Main is
begin
   Mystery (False, True, True);
   Mystery (False, False, False);
   Mystery (True, True, True);
   Mystery (True, True, False);
   Other_Proc (True, True);
   Other_Proc (False, True);
end Main;

