package Io is

   procedure Get (X : out Integer);
   procedure Put (X : Integer);

   procedure Get (C : out Character);
   procedure Put (C : Character);

   procedure Put (S : String);
   procedure Put_Line (S : String);

   procedure New_Line (Spacing : Positive := 1);
   procedure Get_Line (Item : in out String; Last : out Natural);
   
end Io;
