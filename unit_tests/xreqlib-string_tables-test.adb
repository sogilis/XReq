with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;    use Ada.Text_IO;

package body xreqlib.string_tables.test is

   procedure run is
      T1, T2, T3 : Table;
   begin
      T1.Add_Data_Set ("DS");
      T1.Add_Data ("DS", "A");
      T1.Add_Data ("DS", "B");
      T1.Add_Data ("DS", "C");
      Put_Line (T1.To_String);

      T2.Add_Data_Set ("DS");
      T2.Add_Data ("DS", "C");
      T2.Add_Data ("DS", "B");
      T2.Add_Data ("DS", "A");
      Put_Line (T2.To_String);

      Assert (T1 /= T2);
      T2.Sort_Data_Sets ("DS");
      T3.Set_Header_Kind (First_Row);
      T3.Import_Data_Set (T2, "DS", "DS");
      Put_Line (T3.To_String);

      T3.Compare_With (T1, Ignore_Missing_Headers => True);
   end run;

end xreqlib.string_tables.test;
