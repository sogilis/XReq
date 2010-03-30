--                         Copyright (C) 2010, Sogilis                       --


package body AdaSpec.Result_Scenarios is

   ----------------------------------------
   --  Result_Scenario_Type  --  Append  --
   ----------------------------------------

   procedure Append           (Res      : in out Result_Scenario_Type;
                               Step     : in     Result_Step_Type)
   is
      use Result_Steps;
   begin
      Append (Res.Steps, Step);
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
      for I in Scenario.Step_First .. Scenario.Step_Last loop
         Stanza := Scenario.Step_Element (I);
         if Scenario.Outline then
            Err := False;
            Make (Res_St, "", Stanza, Match_Vectors.Empty_Vector);
         else
            Process_Step (Res_St, Stanza, Steps, Log, Err, Step_Matching);
         end if;
         if Err then
            Errors := True;
         else
            Append (StepsV, Res_St);
         end if;
      end loop;
      if not Scenario.Outline then
         Res := (Outline => False,
                 Name    => To_Unbounded_String (Scenario.Name),
                 Pos     => Scenario.Position,
                 Tags    => Scenario.Tag_Vector,
                 Steps   => StepsV);
      else
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
                     Res_St.Step.Set_Stanza
                       (Replace (Res_St.Step.Stanza, Label, Item));
                     Replace_Element (Steps_tmp, J, Res_St);
                     Next (J);
                  end loop;
               end;
            end loop;
            J := First (Steps_tmp);
            while Has_Element (J) loop
               Res_St := Element (J);
               Log.Put_Line (To_String (Res_St.Step));
               Process_Step (Res_St, Res_St.Step, Steps, Log, Err,
                             Step_Matching);
               if Err then
                  Errors := True;
               end if;
               Replace_Element (Steps_tmp, J, Res_St);
               Next (J);
            end loop;
            Append (Scenarios, Steps_tmp);
         end loop;
         Res := (Outline   => True,
                 Name      => To_Unbounded_String (Scenario.Name),
                 Pos       => Scenario.Position,
                 Tags      => Scenario.Tag_Vector,
                 Steps     => StepsV,
                 Table     => Scenario.Table,
                 Scenarios => Scenarios);
      end if;
   end Process_Scenario;

end AdaSpec.Result_Scenarios;
