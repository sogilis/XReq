--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Args is

   function Text (A : Argument_Type) return String is
   begin
      return To_String (A.Text);
   end Text;

end AdaSpec.Args;
