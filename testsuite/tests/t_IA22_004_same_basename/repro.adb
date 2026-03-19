with put;

procedure repro is
  procedure c_main;
  pragma import (c, c_main);
begin
  c_main;
end repro;
