with Ada.Numerics.Discrete_Random;

with AATree;

procedure Main is
   subtype Random_Range is Integer range 1 .. 4096;
   package Random_Int is new Ada.Numerics.Discrete_Random (Random_Range);
   use Random_Int;

   Numbers : constant array (Positive range <>) of Natural := (1, 2, 3, 4, 5);
   Root    : AATree.Tree_Ptr                               := null;

   G : Generator;
begin
   Reset (G);

   for Number in Numbers'Range loop
      AATree.Insert (Root, Random (G));
   end loop;

   AATree.Print (Root);
end Main;
