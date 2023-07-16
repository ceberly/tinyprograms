with Ada.Text_IO; use Ada.Text_IO;

procedure Spark with
   SPARK_Mode => On
is
   type Byte is mod 256;

   File      : Ada.Text_IO.File_Type;
   File_Name : constant String := "../_tests/helloworld.bf";

   --   Impose some reasonable bounds for this example.
   subtype Data_Pointer_Type is Integer range 0 .. 10_000;
   Data_Stack : array (Data_Pointer_Type'Range) of Byte := (others => 0);

   --  10k loop depth (really the number of nested '[' characters).
   Instruction_Stack : array (0 .. 10_000) of Integer := (others => 0);

   Instruction_Pointer : Integer           := 0;
   Data_Pointer        : Data_Pointer_Type := 0;

   --   ~1MB file max
   Program : array (0 .. 10**6) of Character := (others => ASCII.NUL);

   Input     : Character;
   Had_Error : Boolean;

   procedure Step with
      Global =>
      (In_Out =>
         (Data_Pointer, Data_Stack, Had_Error, Ada.Text_IO.File_System))
   is
      Loop_Stack_Depth : Integer;
      Loop_End         : Integer;
   begin
      case Program (Instruction_Pointer) is
         when '>' =>
            if Data_Pointer = Data_Pointer_Type'Last then
               Had_Error := True;
               return;
            end if;

            Data_Pointer := Data_Pointer + 1;
         when '<' =>
            if Data_Pointer = Data_Pointer_Type'First then
               Had_Error := True;
               return;
            end if;

            Data_Pointer := Data_Pointer - 1;
         when '+' =>
            Data_Stack (Data_Pointer) := Data_Stack (Data_Pointer) + 1;
         when '-' =>
            Data_Stack (Data_Pointer) := Data_Stack (Data_Pointer) - 1;
         when '.' =>
            Put_Line (Character'Val (Data_Stack (Data_Pointer))'Image);
         when '[' =>
            Loop_Stack_Depth := 1;
            Loop_End         := Instruction_Pointer + 1;
         when ']' =>
            Had_Error := True;
         when ',' =>
            --   TODO: support input.
            Had_Error := True;
         when others =>
            Had_Error := True;
      end case;
   end Step;

   I : Integer := 0;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);
   pragma Assert (Ada.Text_IO.Is_Open (File));

   loop
      Ada.Text_IO.Get (File, Input);
      Program (I) := Input;
      I           := I + 1;

      exit when Ada.Text_IO.End_Of_File (File);
   end loop;

   Ada.Text_IO.Close (File);
   pragma Assert (Ada.Text_IO.Is_Open (File) = False);

   Had_Error := False;

   while Instruction_Pointer < I loop
      Step;

      if Had_Error then
         Put_Line ("Something spooky happened...");
         exit;
      end if;

      Instruction_Pointer := Instruction_Pointer + 1;
   end loop;
end Spark;
