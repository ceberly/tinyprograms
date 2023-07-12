with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Aatree with
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

   procedure Insert (Tree : in out Tree_Ptr; K : Positive) with
      Post => Tree /= null;

   procedure Print (Tree : Tree_Ptr; Space : Unbounded_String);
end Aatree;
