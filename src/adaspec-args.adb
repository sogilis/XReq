--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Args is

   function Text (A : in Argument_Type) return String is
   begin
      return To_String (A.Text);
   end Text;

   procedure Set_Text (A : in out Argument_Type;
                       T : in     String)
   is
   begin
      A.Text := To_Unbounded_String (T);
   end Set_Text;

end AdaSpec.Args;
