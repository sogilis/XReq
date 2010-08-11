
package XReq.Step_Definitions.C is

   --  Called in XReq.Steps.Load
   procedure Parse_Directory (Steps      : in out Step_File_List_Type;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False);
   --  IMPORTANT: deallocate Steps_Type


   type C_Step_File_Type is new Step_File_Type with private;
   type C_Step_File_Ptr  is access all C_Step_File_Type'Class;

   procedure Make (S          : out C_Step_File_Type;
                   File_Name  : in  String;
                   Fill_Steps : in  Boolean := False);

   overriding procedure Parse     (S          : in out C_Step_File_Type;
                                   Logger     : in     Logger_Ptr);

private

   type C_Step_File_Type is new Step_File_Type with
      record
         Fill_Steps : Boolean := False;
      end record;

end XReq.Step_Definitions.C;
