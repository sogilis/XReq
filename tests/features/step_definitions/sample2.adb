--                         Copyright (C) 2010, Sogilis                       --

with Text_IO;

use Text_IO;

package body Sample2 is

   procedure I_am_in_front_of_a_cake_machine (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I am in front of a cake machine");
   end I_am_in_front_of_a_cake_machine;

   procedure I_insert_money (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I insert money");
   end I_insert_money;

   procedure I_push_the_button (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I push the button");
   end I_push_the_button;

   procedure I_get_a_cake (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I get a cake");
   end I_get_a_cake;

end Sample2;
