with Ada.Strings.Unbounded;
with XReqLib.Asserts;
use  Ada.Strings.Unbounded;
use  XReqLib.Asserts;

package body Strings is

   First_Long_String, Second_String, Result : Unbounded_String;

   function Decode (Str : in String) return Unbounded_String is
      Buffer : Unbounded_String;
      I      : Natural := Str'First;
   begin
      while I <= Str'Last loop
         if Str (I) = '\' then
            I := I + 1;
            case Str (I) is
               when 'n' =>
                  Append (Buffer, ASCII.LF);
               when '\' =>
                  Append (Buffer, Str (I));
               when others =>
                  Append (Buffer, "\" & Str (I));
            end case;
         else
            Append (Buffer, Str (I));
         end if;
         I := I + 1;
      end loop;
      return Buffer;
   end Decode;


   procedure When_I_concatenate_and (Args : in out Arg_Type) is
   begin
      Result := To_Unbounded_String (Args.Match (1) & Args.Match (2));
   end When_I_concatenate_and;

   procedure Then_I_Get (Args : in out Arg_Type) is
   begin
      Assert (Args.Match (1) = To_string (Result));
   end Then_I_Get;

   procedure Given_the_long_string (Args : in out Arg_Type) is
   begin
      First_Long_String := To_Unbounded_String (Args.Text);
   end Given_the_long_string;

   procedure When_I_compare_it_with (Args : in out Arg_Type) is
   begin
      Second_String := Decode (Args.Match (1));
   end When_I_compare_it_with;

   procedure Then_they_are_equal (Args : in out Arg_Type) is
   begin
      Assert (First_Long_String = Second_String,
              "The strings are not equal: """ & To_String (First_Long_String) &
              """ /= """ & To_String (Second_String) & """");
   end Then_they_are_equal;

end Strings;
