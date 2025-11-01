package body IO is

   procedure Get (X : out Integer) is
      function Getint return Integer; 
      pragma Interface (C, Getint);
      pragma Interface_Name (Getint, "get_int");
   begin
      X := Getint;
   end Get;

   procedure Put (X : Integer) is
      procedure Putint (X : Integer);
      pragma Interface (C, Putint);
      pragma Interface_Name (Putint, "put_int");
   begin
      Putint (X); 
   end Put;

   procedure Get (C : out Character) is
      function Getchar return Character;
      pragma Interface (C, Getchar);
   begin
      C := Getchar;
   end Get;

   procedure Put (C : Character) is
      procedure Putchar (C : Character);
      pragma Interface (C, Putchar);
   begin
      Putchar (C);
   end Put;

   procedure Put (S : String) is
   begin
      for I in S'range loop
         Put (S (I));
      end loop;
   end Put;
   
   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;
   
   procedure New_Line (Spacing : Positive := 1) is
   begin
      for I in 1 .. Spacing loop
         Put (Ascii.LF);
      end loop;
   end New_Line;

   procedure Get_Line (Item : in out String; Last : out Natural) is 
      I_Length : Integer := Item'Length;
      Nstore : Integer := 0;
      C : Character;
   begin 
      loop
         Get (C);
         exit when Nstore = I_Length;
         if C = Ascii.Lf then
            exit;
         end if;
         Item (Item'First + Nstore) := C;
         Nstore := Nstore + 1;
      end loop;
      Last := Item'First + Nstore - 1;
   end Get_Line;
      

end IO;
