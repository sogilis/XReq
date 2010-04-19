--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with XReqLib.String_Tables;

use Ada.Strings.Unbounded;

package XReqLib.Interface_Scenarios is

   type Scenario_Interface is interface;

--  procedure Step_Append  (Scenario : in out Scenario_Interface;
--                         Stanza   : in     Step_Type)            is abstract;
   function  Step_First   (Scenario : in     Scenario_Interface)
                                      return Natural               is abstract;
   function  Step_Last    (Scenario : in     Scenario_Interface)
                                      return Integer               is abstract;
   function  Step_Count   (Scenario : in     Scenario_Interface)
                                      return Natural               is abstract;
--  function  Step_Element (Scenario : in     Scenario_Interface;
--                         Index    : in     Natural)
--                                    return Step_Type             is abstract;

   function  Outline      (S : in Scenario_Interface) return Boolean
                                                                   is abstract;
   function  Name         (S : in Scenario_Interface) return String
                                                                   is abstract;
   function  Position     (S : in Scenario_Interface) return Position_Type
                                                                   is abstract;
   function  Tag_Vector   (S : in Scenario_Interface) return String_Vector
                                                                   is abstract;
   function  Table        (S : in Scenario_Interface)
                           return XReqLib.String_Tables.Table   is abstract;

   procedure Set_Table    (S     : in out Scenario_Interface;
                           Table : in     XReqLib.String_Tables.Table)
                                                                   is abstract;

   procedure Output_Steps (S     : in     Scenario_Interface;
                           Buf   : in out Unbounded_String)        is abstract;

end XReqLib.Interface_Scenarios;
