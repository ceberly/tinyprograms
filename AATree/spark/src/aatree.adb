with Ada.Text_IO; use Ada.Text_IO;

package body AATree with
   SPARK_Mode => On
is
   procedure Insert (Root : in out Tree_Ptr; V : Positive) is
   begin
      if Root = null then
         Root := new Tree'(Key => V, Left => null, Right => null, Level => 1);
         return;
      end if;

      null;
   end Insert;

   procedure Print (Root : Tree_Ptr) with
      SPARK_Mode => Off
   is
   begin
      if Root = null then
         return;
      end if;

      Print (Root.Left);

      Put_Line (Root.Key'Image);

      Print (Root.Right);
   end Print;
end AATree;
