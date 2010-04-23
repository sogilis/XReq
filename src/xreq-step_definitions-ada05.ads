--                         Copyright (C) 2010, Sogilis                       --

with Util.Strings.Pool;

use Util.Strings.Pool;

package XReq.Step_Definitions.Ada05 is


   --  Called in XReq.Steps.Load
   procedure Parse_Directory (Steps      : in out Step_Definitions_Type;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False);
   --  IMPORTANT: deallocate Steps_Type

   --  Called in XReq.Steps.Add_Steps
   procedure Add_Steps       (Steps      : in out Step_Definitions_Type;
                              New_Steps  : in     String_Set;
                              Step_Pkg   : in     String;
                              Directory  : in     String;
                              Logger     : in     Logger_Ptr);
   --  IMPORTANT: deallocate Steps_Type


   type Ada_Step_File_Type is new Step_File_Type with private;
   type Ada_Step_File_Ptr  is access all Ada_Step_File_Type'Class;

   procedure Make (S          : out Ada_Step_File_Type;
                   File_Name  : in  String;
                   Fill_Steps : in  Boolean := False);

   overriding procedure Parse     (S          : in out Ada_Step_File_Type;
                                   Logger     : in     Logger_Ptr);

private

   type Ada_Step_File_Type is new Step_File_Type with
      record
         Fill_Steps : Boolean := False;
         Procedures : String_Pool;
      end record;

end XReq.Step_Definitions.Ada05;
