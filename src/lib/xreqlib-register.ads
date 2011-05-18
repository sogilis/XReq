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

with XReqLib.Format;
with XReqLib.Report;

package XReqLib.Register is

   type Feature_Procedure is access procedure
     (Format     : in out XReqLib.Format.Format_Ptr;
      Cond       : in     XReqLib.Format.Conditional_Type;
      Report     : in out XReqLib.Report.Report_Type;
      List_Mode  : in Boolean := False;
      Count_Mode : in Boolean := False);

   procedure Register_Feature (Proc : Feature_Procedure);

   procedure Call_Features
     (Format     : in out XReqLib.Format.Format_Ptr;
      Cond       : in     XReqLib.Format.Conditional_Type;
      Report     : in out XReqLib.Report.Report_Type;
      List_Mode  : in Boolean := False;
      Count_Mode : in Boolean := False);

end XReqLib.Register;
