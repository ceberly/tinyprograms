with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Spark with
   SPARK_Mode => On
is
   --  Allocate 1024 nodes up front.
   type Node_Id is mod 2**10;

   type AANode is record
      Left, Right : Node_Id;
      Key         : Integer;
      Level       : Natural;
   end record;

   Nil : constant Node_Id := 0;

--  Add the Nil to the store at location 0 so we don't have to do
--  as many Nil checks.
--  See https://www.cs.umd.edu/class/fall2019/cmsc420-0201/Lects/lect06-aa.pdf
   Node_Store : array (Node_Id) of aliased AANode :=
     (0 => AANode'(Nil, Nil, -1, 0), others => <>);

   Next_Node_Id : Node_Id := 1;

   procedure Skew (Tree : in out Node_Id) is
      T : constant access AANode := Node_Store (Tree)'Access;
   begin
      if T.Left = Nil then
         return;
      end if;

      declare
         L           : constant access AANode := Node_Store (T.Left)'Access;
         Prev_T_Left : constant Node_Id       := T.Left;
      begin
         if L.Level = T.Level then
            T.Left  := L.Right;
            L.Right := Tree;

            --   NOTE: this is really just the Node_Id of 'L'
            Tree := Prev_T_Left;
         end if;
      end;
   end Skew;

   procedure Split (Tree : in out Node_Id) is
      T : constant access AANode := Node_Store (Tree)'Access;
   begin
      if T.Right = Nil then
         return;
      end if;

      declare
         R            : constant access AANode := Node_Store (T.Right)'Access;
         RR           : constant access AANode := Node_Store (R.Right)'Access;
         Prev_T_Right : constant Node_Id       := T.Right;
      begin
         if R.Right = Nil or else RR.Level /= T.Level then
            return;
         end if;

         T.Right := R.Left;
         R.Left  := Tree;

         --  Assume small enough trees for this example...
         pragma Assume (R.Level < Positive'Last);
         R.Level := R.Level + 1;

         Tree := Prev_T_Right;
      end;
   end Split;

   procedure Insert (Tree : in out Node_Id; Key : Integer) is
   begin
      if Tree = Nil then
         Node_Store (Next_Node_Id) :=
           AANode'(Key => Key, Left => Nil, Right => Nil, Level => 1);

         Tree         := Next_Node_Id;
         Next_Node_Id := Next_Node_Id + 1;

         return;
      end if;

      declare
         T : constant access AANode := Node_Store (Tree)'Access;
      begin
         if Key < T.Key then
            Insert (T.Left, Key);
         else
            Insert (T.Right, Key);
         end if;
      end;

      Skew (Tree);
      Split (Tree);
   end Insert;

   procedure Print (Tree : Node_Id; Space : Unbounded_String) is
      T : constant access constant AANode := Node_Store (Tree)'Access;
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

         Insert (Root, Input);

         --  if Positive'Last - Root.Level = 0 then
         --     Put_Line ("Tree overflow!");
         --     return;
         --  end if;
      end loop;

      --  'warning: "Root" is set by "Print" but not used after the call'
      --  why?
      if Root /= Nil then
         Print (Root, To_Unbounded_String (""));
      end if;

      Ada.Text_IO.Close (File);
      pragma Assert (not Ada.Text_IO.Is_Open (File));
   end;
end Spark;
