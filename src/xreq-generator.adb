--                         Copyright (C) 2010, Sogilis                       --

with XReq.Generator.Ada05;
with XReq.Lang;
with XReqLib;

use XReq.Lang;

package body XReq.Generator is

   procedure Generate (Job : in  Job_Type;
                       Env : in  Job_Environment;
                       Log : in  Logger_Ptr;
                       Gen : out Generator_Ptr)
   is
      G : Generator_Ptr;
   begin
      case Env.Language is
         when Lang_Ada =>
            G := new Ada05.Ada_Generator_Type;
         when Lang_C =>
            raise XReqLib.Not_Yet_Implemented;
      end case;
      G.Make (Job, Env);
      G.Generate (Log);
      Gen := G;
   end Generate;

   procedure Generate  (Job : in  Job_Type;
                        Env : in  Job_Environment;
                        Log : in  Logger_Ptr)
   is
      Gen : Generator_Ptr;
   begin
      Generate (Job, Env, Log, Gen);
      Free (Gen);
   end Generate;

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment;
                             Log  : in  Logger_Ptr;
                             Make : in Boolean := False)
   is
   begin
      case Env.Language is
         when Lang_Ada =>
            Ada05.Generate_Suite (Gens, Name, Env, Log, Make);
         when Lang_C =>
            raise XReqLib.Not_Yet_Implemented;
      end case;
   end Generate_Suite;

end XReq.Generator;
