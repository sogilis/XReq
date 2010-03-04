generic
   type File_Type (<>) is limited private;
   with procedure Put (File : in out File_Type; Item : in String);
package AdaSpecLib.Format_HTML_Template is

   pragma Style_Checks (Off);

   procedure feature_begin
        (File : in out File_Type;
         Param_id : in String;
         Param_name : in String;
         Param_description : in String);

   procedure step_end
        (File : in out File_Type);

   procedure page_begin
        (File : in out File_Type);

   procedure background_begin
        (File : in out File_Type;
         Param_feature_id : in String);

   procedure scenario_begin
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String);

   procedure step_begin
        (File : in out File_Type;
         Param_status : in String;
         Param_stanza : in String);

   procedure scenario_end
        (File : in out File_Type);

   procedure report
        (File : in out File_Type);

   procedure background_end
        (File : in out File_Type);

   procedure page_end
        (File : in out File_Type);

   procedure feature_end
        (File : in out File_Type);

   procedure step_string
        (File : in out File_Type;
         Param_string : in String);

   pragma Style_Checks (On);

end AdaSpecLib.Format_HTML_Template;
