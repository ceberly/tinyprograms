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

   type Tree_Container is limited record
      Root : Tree_Ptr;
   end record;

   function Has_No_Left_Red_Child (Tree : access AATree) return Boolean is
     (Tree.Left = null or else Tree.Left.Level < Tree.Level) with
      Ghost,
      Pre => Tree /= null;

--   function Skew (Tree : Tree_Ptr) return Tree_Ptr with
--      Global => null,
--      Pre    => Tree /= null
--   is
--      L : Tree_Ptr;
--   begin
--      if Tree.Left /= null and then Tree.Left.Level = Tree.Level then
--         L         := Tree.Left;
--         Tree.Left := L.Right;
--         L.Right   := Tree;
--
--         return L;
--      end if;
--
--      return Tree;
--   end Skew;
--
--   function Split (Tree : Tree_Ptr) return Tree_Ptr with
--      Global => null,
--      Pre    => Tree /= null
--   is
--      L : Tree_Ptr;
--   begin
--      if Tree.Right /= null and then Tree.Right.Right /= null
--        and then Tree.Right.Right.Level = Tree.Level
--      then
--         L          := Tree.Right;
--         Tree.Right := L.Left;
--         L.Left     := Tree;
--         --   Assume we have a sane sized tree for now to prevent overflow ...
--         pragma Assume (L.Level < Natural'Last / 2);
--         L.Level := L.Level + 1;
--
--         return L;
--      end if;
--
--      return Tree;
--   end Split;
--
   --   https://docs.adacore.com/spark2014-docs/html/ug/en/source/access.html#borrowing
   procedure Insert_Existing (Tree : access AATree; K : Positive) with
      Pre => Tree /= null
   is
      X : access AATree := Tree;
   begin
      while X /= null loop
         if K < X.Key then
            if X.Left = null then
               X.Left := new AATree'(K, null, null, 1);
               -- X now refers to the parent of the new node
               -- which is where we want to start Split (Skew (X))...
               exit;
            end if;

            X := X.Left;
         else
            if X.Right = null then
               X.Right := new AATree'(K, null, null, 1);
               -- " "
               exit;
            end if;

            X := X.Right;
         end if;
      end loop;

--      while X /= Original_Root loop
--         X := Split (Skew (X));
--      end loop;
--
--      X := Split (Skew (X));
   end Insert_Existing;

   procedure Insert (Tree : in out Tree_Container; K : Positive) with
      Post => Tree.Root /= null
   is
   begin
      if Tree.Root = null then
         Tree.Root := new AATree'(K, null, null, 1);
      else
         Insert_Existing (Tree.Root, K);
      end if;
   end Insert;

   --  Print from a dereferenced AATree to avoid confusing SPARK
   --  TODO: fix this ("[Print modifies Root but the value is unused after]")
   procedure Print (Tree : AATree; Space : Unbounded_String) is
   begin
      if Natural'Last - Length (Space) - 4 - Tree.Key'Image'Length < 0 then
         Put_Line ("Tree Overflow!");
         return;
      end if;

      if Tree.Right /= null then
         Print (Tree.Right.all, Space & "  ");
      end if;

      Put_Line (To_String (Space & Tree.Key'Image));

      if Tree.Left /= null then
         Print (Tree.Left.all, Space & "  ");
      end if;
   end Print;

   Tree : Tree_Container;

   Input : constant array (Positive range <>) of Integer :=
     (81, 99, 10, 32, 8, 19, 3, 78);
begin
   for Number in Input'Range loop
      Insert (Tree, Input (Number));
   end loop;

   Print (Tree.Root.all, To_Unbounded_String (""));
end Spark;
