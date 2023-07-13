with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

procedure Spark with
   SPARK_Mode => On
is
   type AATree;
   type Tree_Ptr is access AATree;

   type AATree is limited record
      Key   : Positive;
      Left  : Tree_Ptr;
      Right : Tree_Ptr;
      Level : Positive;
   end record;

   procedure Skew (Tree : in out Tree_Ptr) with
      Global => null,
      Pre    => Tree /= null,
      Post   => Tree /= null
   is
      L : Tree_Ptr;
   begin
      if Tree.Left = null then
         return;
      end if;

      if Tree.Left.Level = Tree.Level then
         L         := Tree.Left;
         Tree.Left := L.Right;
         L.Right   := Tree;
         Tree      := L;
      end if;
   end Skew;

   procedure Split (Tree : in out Tree_Ptr) with
      Global => null,
      Pre    => Tree /= null,
      Post   => Tree /= null
   is
      L : Tree_Ptr;
   begin
      if Tree.Right = null or else Tree.Right.Right = null then
         return;
      end if;

      if Tree.Right.Right.Level = Tree.Level then
         L          := Tree.Right;
         Tree.Right := L.Left;
         L.Left     := Tree;
         --   Assume we have a sane sized tree for now to prevent overflow ...
         pragma Assume (L.Level < Natural'Last / 2);
         L.Level := L.Level + 1;

         Tree := L;
      end if;
   end Split;

   procedure Insert (Tree : in out Tree_Ptr; K : Positive) with
      Global => null,
      Post   => Tree /= null
   is
   begin
      if Tree = null then
         Tree :=
           new AATree'(Key => K, Left => null, Right => null, Level => 1);
         return;
      end if;

      if K < Tree.Key then
         Insert (Tree.Left, K);
      else
         Insert (Tree.Right, K);
      end if;

      Skew (Tree);
      Split (Tree);
   end Insert;

   --  Print from a dereferenced AATree to avoid confusing SPARK
   --  TODO: fix this ("[Print modifies Root but the value is unused after]")
   procedure Print (Tree : AATree; Space : Unbounded_String) is
   begin
      if Natural'Last - Length (Space) - 4 - Tree.Key'Image'Length < 0 then
         Put_Line ("Tree Overflow!");
         return;
      end if;

      if Tree.Left /= null then
         Print (Tree.Left.all, Space & "  ");
      end if;

      Put_Line (To_String (Space & Tree.Key'Image));

      if Tree.Right /= null then
         Print (Tree.Right.all, Space & "  ");
      end if;
   end Print;

   Root : Tree_Ptr := null;

   Input : constant array (Positive range <>) of Integer :=
     (81, 99, 10, 32, 8, 19, 3, 78);
begin
   for Number in Input'Range loop
      Insert (Root, Input (Number));
   end loop;

   Print (Root.all, To_Unbounded_String (""));
end Spark;
