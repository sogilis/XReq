-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

package body XReqLib.Register is

   type Hook_List_Type;
   type Hook_List_Access is access all Hook_List_Type;

   type Hook_List_Type is
      record
         Timing   : Hook_Timing;
         Position : Hook_Position;
         Proc     : Hook_Procedure := null;
         Next     : Hook_List_Access := null;
      end record;

   All_Hooks : Hook_List_Access := null;

   procedure Register_Hook (Timing   : Hook_Timing;
                            Position : Hook_Position;
                            Callback : Hook_Procedure) is
   begin
      All_Hooks := new Hook_List_Type'(Timing, Position, Callback, All_Hooks);
   end Register_Hook;

   procedure Call_Hook (Timing   : Hook_Timing;
                        Position : Hook_Position)
   is
      P : Hook_List_Access := All_Hooks;
   begin
      while P /= null loop
         if P.Timing = Timing and P.Position = Position then
            P.Proc (Timing, Position);
         end if;
         P := P.Next;
      end loop;
   end Call_Hook;

   type Feature_List_Type;
   type Feature_List_Access is access all Feature_List_Type;

   type Feature_List_Type is
      record
         Proc : Feature_Procedure := null;
         Next : Feature_List_Access := null;
      end record;

   All_Features : Feature_List_Access := null;

   procedure Register_Feature (Proc : Feature_Procedure) is
   begin
      All_Features := new Feature_List_Type'(Proc, All_Features);
   end Register_Feature;

   procedure Call_Features
     (Format     : in out XReqLib.Format.Format_Ptr;
      Cond       : in     XReqLib.Format.Conditional_Type;
      Report     : in out XReqLib.Report.Report_Type;
      List_Mode  : in Boolean := False;
      Count_Mode : in Boolean := False)
   is
      P : Feature_List_Access := All_Features;
   begin
      Call_Hook (Hook_Begin, Hook_Test_Suite);
      while P /= null loop
         P.Proc (Format, Cond, Report, List_Mode, Count_Mode);
         P := P.Next;
      end loop;
      Call_Hook (Hook_End, Hook_Test_Suite);
   end Call_Features;

end XReqLib.Register;
