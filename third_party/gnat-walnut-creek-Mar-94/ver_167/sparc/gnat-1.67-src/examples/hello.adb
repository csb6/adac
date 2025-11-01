With IO; use IO;
procedure Hello is
   C : Character;
   I : Integer;
   S : String (1 .. 100);
begin
   Put_Line ("Hello. Welcome to the GNAT IO example" & "!");
   Put ("Please enter a single character now followed by <CR> ");
   Get (C);
   Put ("Your character is: ");
   Put (C);
   Get (C);  --  read the <CR>
   New_Line (2);

   Put ("Please enter a String now followed by <CR> :");
   Get_Line (S, I);
   Put ("Your String is : ");
   Put_Line (S (1 .. I));
   Put ("Its length is : ");
   Put (I);
   New_Line (2);

   Put ("Please enter an integer now followed by <CR> ");
   Get (I);
   Put ("Your number is: ");
   Put (I);
   New_Line (2);
end; 
