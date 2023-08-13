with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Spark with
   SPARK_Mode => On,
   Global     => (In_Out => Ada.Text_IO.File_System)
is
   type Node_Id is mod 2**10;
   subtype Node_Index is Node_Id range 1 .. Node_Id'Last;

   type AANode is record
      Left, Right : Node_Id;
      Key         : Integer;
      Level       : Natural;
   end record;

   Nil          : constant Node_Id := 0;
   Next_Node_Id : Node_Index       := 1;

   --  Allocate 1024 nodes up front.
   Node_Store : array (Node_Index) of aliased AANode :=
     (others => AANode'(Nil, Nil, -1, 0));

   procedure Print (Tree : Node_Id; Space : Unbounded_String) with
      Pre => Tree /= Nil
   is
      T : constant AANode := Node_Store (Tree);
   begin
      if Natural'Last - Length (Space) - 2 - T.Key'Image'Length < 0 then
         Put_Line ("Tree overflow!");
         return;
      end if;

      if T.Left /= Nil then
         Print (T.Left, Space & "  ");
      end if;

      Put_Line (To_String (Space & T.Key'Image));

      if T.Right /= Nil then
         Print (T.Right, Space & "  ");
      end if;
   end Print;

   procedure Insert (Tree : in out Node_Id; Key : Integer) with
      Pre => Next_Node_Id < Node_Id'Last
   is
   begin
      if Tree = Nil then
         Node_Store (Next_Node_Id) :=
           AANode'(Key => Key, Left => Nil, Right => Nil, Level => 1);

         Tree         := Next_Node_Id;
         Next_Node_Id := Next_Node_Id + 1;

         return;
      end if;

      declare
         N : AANode := Node_Store (Tree);
      begin
         if Key < N.Key then
            Insert (N.Left, Key);
         else
            Insert (N.Right, Key);
         end if;

         Node_Store (Tree) := N;
      end;

      --Skew (Tree);
      --Split (Tree);
   end Insert;
begin
   if Argument_Count /= 1 then
      Put_Line ("usage: ./spark <file>");
      return;
   end if;

   declare
      File_Name : constant String := Argument (1);
      File      : Ada.Text_IO.File_Type;
      Input     : Integer;

      Root : Node_Id := Nil;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);
      pragma Assert (Ada.Text_IO.Is_Open (File));

      while not End_Of_File (File) loop
         Ada.Integer_Text_IO.Get (File, Input);

         if Next_Node_Id = Node_Id'Last then
            Put_Line ("Tree overflow!");
            return;
         end if;

         Insert (Root, Input);
      end loop;

      if Root /= Nil then
         Print (Root, To_Unbounded_String (""));
      end if;

      Ada.Text_IO.Close (File);
      pragma Assert (not Ada.Text_IO.Is_Open (File));
   end;
end Spark;
