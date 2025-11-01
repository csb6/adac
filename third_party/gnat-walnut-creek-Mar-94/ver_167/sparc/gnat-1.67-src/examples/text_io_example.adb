with Text_Io; use Text_Io;
procedure Text_IO_Example is
   package Int_Io is new Integer_IO (Integer); use Int_Io;
begin
   Put ("The following should be a five in base two:"); 
   Put (5, Width => 7, Base => 2);
   New_Line;
end Text_IO_Example;
