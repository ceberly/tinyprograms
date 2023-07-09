with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AATree;

procedure Main is
   subtype Random_Range is Integer range 1 .. 4_096;
   package Random_Int is new Ada.Numerics.Discrete_Random (Random_Range);
   use Random_Int;

   Root : AATree.Tree_Ptr := null;

   G : Generator;
begin
   Reset (G);

   for Number in 0 .. 10 loop
      AATree.Insert (Root, Random (G));
   end loop;

   AATree.Print (Root, To_Unbounded_String (""));
end Main;
