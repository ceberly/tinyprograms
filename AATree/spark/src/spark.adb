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
      Pre  => Tree /= null,
      Post => Tree /= null
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
      Pre  => Tree /= null,
      Post => Tree /= null
   is
      L : Tree_Ptr;
   begin
      if Tree.Right = null or else Tree.Right.Right = null then
         return;
      end if;

      if Tree.Right.Right.Level = Tree.Level then
         -- Prevent level from overflowing.
         -- not very good error handling :)
         if Tree.Right.Level = Positive'Last then
            return;
         end if;

         L          := Tree.Right;
         Tree.Right := L.Left;
         L.Left     := Tree;
         L.Level    := L.Level + 1;
         Tree       := L;
      end if;
   end Split;

   procedure Insert (Tree : in out Tree_Ptr; K : Positive) is
   begin
      if Tree = null then
         Tree :=
           new AATree'(Key => K, Left => null, Right => null, Level => 1);
         return;
      end if;

      --  duplicate keys are ignored in every example
      --  I can find. -Chris
      if K <= Tree.Key then
         Insert (Tree.Left, K);
      elsif K > Tree.Key then
         Insert (Tree.Right, K);
      else
         return;
      end if;

      Skew (Tree);
      Split (Tree);
   end Insert;

   procedure Print (Tree : Tree_Ptr; Space : Unbounded_String) is
   begin
      if Tree = null then
         return;
      end if;

      Print (Tree.Left, Space & "  ");

      Put_Line (To_String (Space) & Tree.Key'Image);

      Print (Tree.Right, Space & "  ");
   end Print;

   Root : Tree_Ptr := null;

   Input : constant array (Positive range <>) of Integer :=
     (123, 9_879_871, 8_187_123, 1_278_123, 187, 1_238_761, 2_847);
begin
   for Number in Input'Range loop
      Insert (Root, Input (Number));
   end loop;

   Print (Root, To_Unbounded_String (""));
end Spark;
