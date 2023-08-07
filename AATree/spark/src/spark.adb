with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Spark with
   SPARK_Mode => On
is
   type AATree;
   type Tree_Ptr is access AATree;
   type AATree is record
      Key   : Integer;
      Left  : Tree_Ptr;
      Right : Tree_Ptr;
      Level : Positive;
   end record;

   procedure Skew (Tree : in out Tree_Ptr) with
      Pre  => Tree /= null,
      Post => Tree /= null
   is
      L : Tree_Ptr;
   begin
      if Tree.Left = null or else Tree.Left.Level /= Tree.Level then
         return;
      end if;

      L         := Tree.Left;
      Tree.Left := L.Right;
      L.Right   := Tree;

      Tree := L;
   end Skew;

   procedure Split (Tree : in out Tree_Ptr) with
      Pre  => Tree /= null,
      Post => Tree /= null
   is
      L : Tree_Ptr;
   begin
      if
        (Tree.Right = null or else Tree.Right.Right = null
         or else Tree.Right.Right.Level /= Tree.Level)
      then
         return;
      end if;

      L          := Tree.Right;
      Tree.Right := L.Left;
      L.Left     := Tree;

      --  Assume small enough trees for this example...
      pragma Assume (L.Level < Positive'Last);
      L.Level := L.Level + 1;

      Tree := L;
   end Split;

   procedure Insert (Tree : in out Tree_Ptr; Key : Integer) with
      Post => Tree /= null
   is
   begin
      if Tree = null then
         Tree :=
           new AATree'(Key => Key, Left => null, Right => null, Level => 1);
         return;
      end if;

      if Key < Tree.Key then
         Insert (Tree.Left, Key);
      else
         Insert (Tree.Right, Key);
      end if;

      Skew (Tree);
      Split (Tree);
   end Insert;

   procedure Print (Tree : Tree_Ptr; Space : Unbounded_String) with
      Pre => Tree /= null
   is
   begin
      if Natural'Last - Length (Space) - 2 - Tree.Key'Image'Length < 0 then
         Put_Line ("Tree overflow!");
         return;
      end if;

      if Tree.Left /= null then
         Print (Tree.Left, Space & "  ");
      end if;

      Put_Line (To_String (Space & Tree.Key'Image));

      if Tree.Right /= null then
         Print (Tree.Right, Space & "  ");
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

      Root : Tree_Ptr := null;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);
      pragma Assert (Ada.Text_IO.Is_Open (File));

      while not End_Of_File (File) loop
         Ada.Integer_Text_IO.Get (File, Input);

         Insert (Root, Input);

         if Positive'Last - Root.Level = 0 then
            Put_Line ("Tree overflow!");
            return;
         end if;
      end loop;

      --  'warning: "Root" is set by "Print" but not used after the call'
      --  why?
      if Root /= null then
         Print (Root, To_Unbounded_String (""));
      end if;

      Ada.Text_IO.Close (File);
      pragma Assert (not Ada.Text_IO.Is_Open (File));
   end;
end Spark;
