--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Reporter.Text;
with AUnit.Run;
with Suite;

procedure Test_Main is
   procedure Runner is new AUnit.Run.Test_Runner (Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_Main;
