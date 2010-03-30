--                         Copyright (C) 2010, Sogilis                       --

package AdaSpecLib.Interface_Steps is

   type Step_Interface is interface;

   function  To_String    (S : in Step_Interface;
                           K : in Step_All_Kind := Step_Null)
                                                  return String    is abstract;

   function  Arg_First    (S : in Step_Interface) return Natural   is abstract;
   function  Arg_Last     (S : in Step_Interface) return Integer   is abstract;
--  function  Arg_Element  (S : in Step_Interface;
--                         I : in Natural)        return Argument_Type
--                                                                 is abstract;

   function  Position     (S : in Step_Interface) return Position_Type
                                                                   is abstract;
   function  Stanza       (S : in Step_Interface) return String    is abstract;
   function  Kind         (S : in Step_Interface) return Step_Kind is abstract;

   procedure Set_Position (S      : in out Step_Interface;
                           Pos    : in     Position_Type)          is abstract;
   procedure Set_Stanza   (S      : in out Step_Interface;
                           Stanza : in     String)                 is abstract;
   procedure Set_Kind     (S      : in out Step_Interface;
                           Kind   : in     Step_Kind)              is abstract;
--  procedure Arg_Append   (S      : in out Step_Interface;
--                         E      : in     Argument_Type)          is abstract;

end AdaSpecLib.Interface_Steps;
