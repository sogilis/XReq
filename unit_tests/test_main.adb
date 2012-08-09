with Ada.Text_Io;
with GNAT.Traceback.Symbolic;
with Ada.Exceptions;

with xreqlib.string_tables.test;

procedure test_main is

   type test is access procedure;
   procedure run_test (proc : test) is
      use ada.text_io;
      use Ada.Exceptions;
      use GNAT.Traceback.Symbolic;
   begin
      proc.all;
   exception
      when E : others =>
         Put_Line (Current_Error, Exception_Name (E) & ":");
         Put_Line (Current_Error, Exception_Message (E));
         Put_Line (Current_Error, "Stack Trace:");
         Put_Line (Current_Error, Symbolic_Traceback (E));
   end run_test;

begin
   run_test (xreqlib.string_tables.test.run'access);
end test_main;

