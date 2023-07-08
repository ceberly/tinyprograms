with AATree;

procedure Spark is
   Numbers : constant array (Positive range <>) of Natural := (1, 2, 3, 4, 5);
   Root    : AATree.Tree_Ptr                               := null;
begin
   for Number in Numbers'Range loop
      AATree.Insert (Root, Number);
   end loop;

   AATree.Print (Root);
end Spark;
