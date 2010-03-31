--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Util.Strings;
with AdaSpec.Steps;

use Ada.Strings.Unbounded;
use Util.Strings;
use AdaSpec.Steps;

package body AdaSpec.Result_Scenarios is

   ----------------------------------------
   --  Result_Scenario_Type  --  Append  --
   ----------------------------------------

   procedure Append           (Res      : in out Result_Scenario_Type;
                               Step     : in     Result_Step_Type)
   is
   begin
      Res.Step_Append (Step);
   end Append;

   --------------------------------------------------
   --  Result_Scenario_Type  --  Process_Scenario  --
   --------------------------------------------------

   procedure Process_Scenario (Res      : out Result_Scenario_Type;
                               Scenario : in  Scenario_Type;
                               Steps    : in  Step_Definitions_Type;
                               Log      : in  Logger_Ptr;
                               Errors   : out Boolean;
                               Step_Matching : in Boolean := False)
   is
      use Result_Steps;
      use Result_Steps_Vectors2;
      J         : Result_Steps.Cursor;
      Stanza    : Step_Type;
      Res_St    : Result_Step_Type;
      StepsV    : Result_Steps.Vector;
      Steps_tmp : Result_Steps.Vector;
      Err       : Boolean;
      Scenarios : Result_Steps_Vectors2.Vector;
   begin
      Errors := False;
      Res := New_Scenario (Scenario.Name,
                           Scenario.Position,
                           Scenario.Outline,
                           Scenario.Tag_Vector);
      for I in Scenario.Step_First .. Scenario.Step_Last loop
         Stanza := Scenario.Step_Element (I);
         if Scenario.Outline then
            Err := False;
            Res_St := New_Result_Step (Stanza);
         else
            Res_St.Process_Step (Stanza, Steps, Log, Err, Step_Matching);
         end if;
         if Err then
            Errors := True;
         else
            Append (StepsV, Res_St);
            Res.Step_Append (Res_St);
         end if;
      end loop;
      if Scenario.Outline then
         for Y in Scenario.Table.First_Y + 1 .. Scenario.Table.Last_Y loop
            Steps_tmp := StepsV;
            for X in Scenario.Table.First_X .. Scenario.Table.Last_X loop
               declare
                  Item  : constant String := Scenario.Table.Item (X, Y, "");
                  Label : constant String := "<" & Scenario.Table.Item
                                   (X, Scenario.Table.First_Y, "") & ">";
               begin
                  J := First (Steps_tmp);
                  while Has_Element (J) loop
                     Res_St := Element (J);
                     Res_St.Set_Stanza (Replace (Res_St.Stanza, Label, Item));
                     Replace_Element (Steps_tmp, J, Res_St);
                     Next (J);
                  end loop;
               end;
            end loop;
            J := First (Steps_tmp);
            while Has_Element (J) loop
               Res_St := Element (J);
               Log.Put_Line (Step_Type (Res_St).To_String);
               Process_Step (Res_St, Step_Type (Res_St),
                             Steps, Log, Err, Step_Matching);
               if Err then
                  Errors := True;
               end if;
               Replace_Element (Steps_tmp, J, Res_St);
               Next (J);
            end loop;
            Append (Scenarios, Steps_tmp);
         end loop;
         Res.Scenarios := Scenarios;
         Res.Set_Table (Scenario.Table);
      end if;
   end Process_Scenario;

   -------------------------------------------
   --  Result_Scenario_Type  --  To_String  --
   -------------------------------------------

   function  To_String        (Res      : in     Result_Scenario_Type;
                               Indent   : in     String := "")
                                          return String
   is
      use Result_Steps;
      CRLF   : constant String := "" & ASCII.LF;
      Buffer : Unbounded_String;
   begin
      for I in Res.Step_First .. Res.Step_Last loop
         Append (Buffer,
                 Indent & Step_Type (Res.Step_Element (I)).To_String & CRLF);
      end loop;
      return To_String (Buffer);
   end To_String;

   --------------------
   --  New_Scenario  --
   --------------------

   function  New_Scenario (Name     : in     String;
                           Position : in     Position_Type := Null_Position;
                           Outline  : in     Boolean := False;
                           Tags     : in     AdaSpecLib.String_Vector :=
                                      AdaSpecLib.String_Vectors.Empty_Vector)
                                      return Result_Scenario_Type
   is
   begin
      return Result_Scenario_Type'
        (Scenarios_Package.New_Scenario (Name, Position, Outline, Tags)
         with others => <>);
   end New_Scenario;

   ---------------------
   --  Outline_First  --
   ---------------------

   function  Outline_First (S : in Result_Scenario_Type) return Natural is
      pragma Unreferenced (S);
   begin
      return 0;
   end Outline_First;

   --------------------
   --  Outline_Last  --
   --------------------

   function  Outline_Last  (S : in Result_Scenario_Type) return Integer is
   begin
      return Outline_Count (S) - 1;
   end Outline_Last;

   ---------------------
   --  Outline_Count  --
   ---------------------

   function  Outline_Count (S : in Result_Scenario_Type) return Natural is
      use Result_Steps_Vectors2;
      use Ada.Containers;
   begin
      return Integer (Length (S.Scenarios));
   end Outline_Count;

   --------------------------
   --  Outline_Step_First  --
   --------------------------

   function  Outline_Step_First   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural
   is
      pragma Unreferenced (S, Outline);
   begin
      return 0;
   end Outline_Step_First;

   -------------------------
   --  Outline_Step_Last  --
   -------------------------

   function  Outline_Step_Last    (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Integer
   is
   begin
      return Outline_Step_Count (S, Outline) - 1;
   end Outline_Step_Last;

   --------------------------
   --  Outline_Step_Count  --
   --------------------------

   function  Outline_Step_Count   (S       : in Result_Scenario_Type;
                                   Outline : in Natural)  return Natural
   is
      use Ada.Containers;
      use Result_Steps;
      use Result_Steps_Vectors2;
   begin
      return Integer (Length (Element (S.Scenarios, Outline)));
   end Outline_Step_Count;

   ----------------------------
   --  Outline_Step_Element  --
   ----------------------------

   function  Outline_Step_Element (S       : in Result_Scenario_Type;
                                   Outline : in Natural;
                                   Step    : in Natural)
                                   return Result_Step_Type
   is
      use Result_Steps;
      use Result_Steps_Vectors2;
      V : Result_Steps.Vector;
   begin
      V := Element (S.Scenarios, Outline);
      return Element (V, Step);
   end Outline_Step_Element;

end AdaSpec.Result_Scenarios;
