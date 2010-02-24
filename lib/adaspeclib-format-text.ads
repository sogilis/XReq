--                         Copyright (C) 2010, Sogilis                       --

package AdaSpecLib.Format.Text is

   procedure Put_Feature    (Feature    : in String);
   procedure Put_Background (Background : in String);
   procedure Put_Scenario   (Scenario   : in String);
   procedure Put_Step       (Step       : in Step_Type;
                             Name       : in String);

end AdaSpecLib.Format.Text;
