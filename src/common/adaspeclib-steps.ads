--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.String_Tables;

package AdaSpecLib.Steps is

   type Step_I    is interface;
   type Step_IPtr is access all Step_I'Class;

   ---------------
   --  Getters  --
   ---------------

   function To_String (S : in Step_I) return String             is abstract;
   function To_Regexp (S : in Step_I) return String             is abstract;
   function Position  (S : in Step_I) return Position_Type      is abstract;
   function Prefix    (S : in Step_I) return Step_Kind          is abstract;
   function Stanza    (S : in Step_I) return String             is abstract;

   ---------------
   --  Setters  --
   ---------------

   procedure Set_Prefix   (S        : in out Step_I;
                           Prefix   : in     Step_Kind)         is abstract;
   procedure Set_Position (S        : in out Step_I;
                           Position : in     Position_Type)     is abstract;
   procedure Set_Stanza   (S        : in out Step_I;
                           Stanza   : in     String)            is abstract;

   ------------------------------
   --  Arguments Manipulation  --
   ------------------------------

   type Step_Argument_Kind is (Step_Argument_Text,
                               Step_Argument_Table);

   function  Arg_First   (S   : in     Step_I)  return Natural  is abstract;
   function  Arg_Last    (S   : in     Step_I)  return Natural  is abstract;
   function  Arg_Kind    (S   : in     Step_I;
                          Idx : in     Natural)
                                return Step_Argument_Kind       is abstract;
   function  Arg_Text    (S   : in     Step_I;
                          Idx : in     Natural) return String   is abstract;
   function  Arg_Table   (S   : in     Step_I;
                          Idx : in     Natural)
                                return String_Tables.Table      is abstract;
   procedure Arg_Append  (S   : in out Step_I;
                          Arg : in     Step_Argument_Type)      is abstract;
   procedure Arg_Replace (S   : in out Step_I;
                          Idx : in     Natural;
                          Arg : in     Step_Argument_Type)      is abstract;

end AdaSpecLib.Steps;
