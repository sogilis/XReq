--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Result is

   ----------------------------------
   --  Result_Step_Type  --  Make  --
   ----------------------------------

   procedure Make (S              : out Result_Step_Type;
                   Procedure_Name : in  String)
   is
   begin
      S := (
         Procedure_Name => To_Unbounded_String (Procedure_Name));
   end Make;

   ----------------------------------
   --  Result_Step_Type  --  Name  --
   ----------------------------------

   function Procedure_Name (S : in Result_Step_Type) return String is
   begin
      return To_String (S.Procedure_Name);
   end Procedure_Name;

end AdaSpec.Result;
