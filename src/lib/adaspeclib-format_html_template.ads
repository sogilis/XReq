generic
   type File_Type (<>) is limited private;
   with procedure Put (File : in out File_Type; Item : in String);
package AdaSpecLib.Format_HTML_Template is

   pragma Style_Checks (Off);

   procedure page_begin
        (File : in out File_Type;
         Param_title : in String);

   procedure feature_begin
        (File : in out File_Type;
         Param_id : in String;
         Param_name : in String;
         Param_description : in String);

   procedure background_begin
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String;
         Param_title : in String);

   procedure background_end
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String);

   procedure scenario_begin
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String;
         Param_title : in String);

   procedure step_begin
        (File : in out File_Type;
         Param_status : in String;
         Param_stanza : in String);

   procedure step_string
        (File : in out File_Type;
         Param_string : in String);

   procedure step_error_background
        (File : in out File_Type;
         Param_error : in String;
         Param_feature_id : in String;
         Param_num : in String);

   procedure step_error_scenario
        (File : in out File_Type;
         Param_error : in String;
         Param_feature_id : in String;
         Param_num : in String);

   procedure step_end
        (File : in out File_Type);

   procedure scenario_end
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String);

   procedure feature_end
        (File : in out File_Type;
         Param_feature_id : in String);

   procedure report_begin
        (File : in out File_Type;
         Param_status : in String;
         Param_num_scenarios : in String;
         Param_num_scenarios_fail : in String;
         Param_num_scenarios_pass : in String;
         Param_num_steps : in String;
         Param_num_steps_fail : in String;
         Param_num_steps_skip : in String;
         Param_num_steps_pass : in String);

   procedure report_menu_begin
        (File : in out File_Type);

   procedure report_menu_feature_begin
        (File : in out File_Type;
         Param_status : in String;
         Param_feature_id : in String;
         Param_name : in String);

   procedure report_menu_scenarios_begin
        (File : in out File_Type);

   procedure report_menu_background
        (File : in out File_Type;
         Param_status : in String;
         Param_feature_id : in String;
         Param_num : in String;
         Param_name : in String);

   procedure report_menu_scenario
        (File : in out File_Type;
         Param_status : in String;
         Param_feature_id : in String;
         Param_num : in String;
         Param_name : in String);

   procedure report_menu_scenarios_end
        (File : in out File_Type);

   procedure report_menu_feature_end
        (File : in out File_Type);

   procedure report_menu_end
        (File : in out File_Type);

   procedure report_end
        (File : in out File_Type);

   procedure page_end
        (File : in out File_Type);

   pragma Style_Checks (On);

end AdaSpecLib.Format_HTML_Template;
