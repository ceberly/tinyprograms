with System.Storage_Elements; use System.Storage_Elements;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

-- with Ada.Numerics.Discrete_Random;

procedure Spark with
   SPARK_Mode => On
is
   -- subtype Random_Range is Integer range 1 .. 4_096;
   -- package Random_Int is new Ada.Numerics.Discrete_Random (Random_Range);
   -- use Random_Int;

   -- G : Generator;

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

   function Insert (Tree : Tree_Ptr; K : Positive) return Tree_Ptr with
      Post => Tree /= null
   is
   begin
      if Tree = null then
           return new AATree'(Key => K, Left => null, Right => null, Level => 1);
      end if;

      if K < Tree.Key then
         Insert (Tree.Left, K);
      else
         Insert (Tree.Right, K);
      end if;

      -- Skew (Tree);
      -- Split (Tree);
   end Insert;

   -- Print from a dereferenced AATree to avoid confusing SPARK
   -- TODO: fix this ("[Print modifies Root but the value is unused after]")
   procedure Print (Tree : AATree; Space : Unbounded_String) is
   begin
      if Natural'Last - Length (Space) - 4 - Tree.Key'Image'Length < 0 then
         -- tree overflow
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
     (123, 9_879_871, 8_187_123, 1_278_123, 187, 1_238_761, 2_847);
begin
--   Reset (G);
--
--   for Number in 0 .. 1_0000 loop
--      Insert (Root, Random (G));
--   end loop;
--

   for Number in Input'Range loop
      Root = Insert (Root, Input (Number));
      -- Put_Line (To_Integer (Root'Address)'Image);
   end loop;

   -- Print (Root.all, To_Unbounded_String (""));
end Spark;
