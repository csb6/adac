procedure Main is
  type Boolean is (True, False);
  procedure foo(a: Boolean; b: Boolean) is
  begin
    null;
  end;
begin
  foo(True, False);
  foo(True, True);
end Main;
