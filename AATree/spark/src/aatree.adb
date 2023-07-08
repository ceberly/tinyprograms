with Ada.Text_IO; use Ada.Text_IO;

package body AATree with
   SPARK_Mode => On
is
   procedure Insert (Sub_Tree : in out Tree_Ptr; V : Positive) is
   begin
      null;
   end Insert;

   procedure Print (Root : Tree_Ptr) with
      SPARK_Mode => Off
   is
   begin
      Put_Line ("OK");
   end Print;
end AATree;
