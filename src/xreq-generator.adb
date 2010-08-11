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

with XReq.Generator.Ada05;
with XReq.Generator.C;
with XReq.Lang;

use XReq.Lang;

package body XReq.Generator is

   procedure Generate (Job : in  Job_Type;
                       Env : in  Environment_Handle;
                       Log : in  Logger_Ptr;
                       Gen : out Generator_Ptr)
   is
      G : Generator_Ptr;
   begin
      case Env.Ref.Language is
         when Lang_Ada =>
            G := new Ada05.Ada_Generator_Type;
         when Lang_C =>
            G := new C.C_Generator_Type;
      end case;
      G.Make (Job, Env);
      G.Generate (Log);
      Gen := G;
   end Generate;

   procedure Generate  (Job : in  Job_Type;
                        Env : in  Environment_Handle;
                        Log : in  Logger_Ptr)
   is
      Gen : Generator_Ptr;
   begin
      Generate (Job, Env, Log, Gen);
      Free (Gen);
   end Generate;

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Environment_Handle;
                             Log  : in Logger_Ptr;
                             Make : in Boolean := False)
   is
   begin
      case Env.Ref.Language is
         when Lang_Ada =>
            Ada05.Generate_Suite (Gens, Name, Env, Log, Make);
         when Lang_C =>
            C.Generate_Suite (Gens, Name, Env, Log, Make);
      end case;
   end Generate_Suite;

end XReq.Generator;
