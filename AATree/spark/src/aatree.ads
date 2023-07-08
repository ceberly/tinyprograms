package AATree with
   SPARK_Mode => On
is
   type Tree;
   type Tree_Ptr is access Tree;

   type Tree is limited record
      Key   : Positive;
      Left  : Tree_Ptr;
      Right : Tree_Ptr;
      Level : Positive;
   end record;

   procedure Insert (Sub_Tree : in out Tree_Ptr; V : Positive);
   procedure Print (Root : Tree_Ptr) with
      SPARK_Mode => Off;
end AATree;
