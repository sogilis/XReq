package body AdaSpecLib.Format_HTML_Template is

   pragma Style_Checks (Off);

   procedure page_begin
        (File : in out File_Type;
         Param_title : in String) is
   begin
      Put (File, "<?xml version=""1.0"" encoding=""UTF-8""?>" & ASCII.LF);
      Put (File, "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Strict//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"">" & ASCII.LF);
      Put (File, "<html xmlns=""http://www.w3.org/1999/xhtml"" xml:lang=""en"" lang=""en"">" & ASCII.LF);
      Put (File, "  <head>" & ASCII.LF);
      Put (File, "    <title>");
      Put (File, Param_title);
      Put (File, "</title>" & ASCII.LF);
      Put (File, "    <style type=""text/css"">" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "a {" & ASCII.LF);
      Put (File, "  color: inherit;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".clear {" & ASCII.LF);
      Put (File, "  clear: both;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".capture {" & ASCII.LF);
      Put (File, "  font-weight: bold;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "table {" & ASCII.LF);
      Put (File, "  border-collapse: collapse;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "td {" & ASCII.LF);
      Put (File, "  border-collapse: collapse;" & ASCII.LF);
      Put (File, "  border: solid thin black;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".fail {" & ASCII.LF);
      Put (File, "  color: #c20000;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".pass {" & ASCII.LF);
      Put (File, "  color: #3d7700;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".skip {" & ASCII.LF);
      Put (File, "  color: #004444;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".feature.fail .feature-header {" & ASCII.LF);
      Put (File, "  background-color: #fffbd3;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".feature.pass .feature-header {" & ASCII.LF);
      Put (File, "  background-color: #dbffb4;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".feature.skip .feature-header {" & ASCII.LF);
      Put (File, "  background-color: #e0ffff;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario {" & ASCII.LF);
      Put (File, "  margin-left: 3em;" & ASCII.LF);
      Put (File, "  border-left-width: thin;" & ASCII.LF);
      Put (File, "  border-left-style: solid;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.fail {" & ASCII.LF);
      Put (File, "  background-color: #fffbd3;" & ASCII.LF);
      Put (File, "  border-color: #c20000;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.pass {" & ASCII.LF);
      Put (File, "  background-color: #dbffb4;" & ASCII.LF);
      Put (File, "  border-color: #65c400;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.skip {" & ASCII.LF);
      Put (File, "  background-color: #e0ffff;" & ASCII.LF);
      Put (File, "  border-color: aqua;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.fail td {" & ASCII.LF);
      Put (File, "  border-color: #c20000;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.pass td {" & ASCII.LF);
      Put (File, "  border-color: #65c400;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.skip td {" & ASCII.LF);
      Put (File, "  border-color: aqua;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.fail h3 {" & ASCII.LF);
      Put (File, "  color: #fffbd3;" & ASCII.LF);
      Put (File, "  background-color: #c20000;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.pass h3 {" & ASCII.LF);
      Put (File, "  color: #dbffb4;" & ASCII.LF);
      Put (File, "  background-color: #65c400;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".scenario.skip h3 {" & ASCII.LF);
      Put (File, "  color: #004444;" & ASCII.LF);
      Put (File, "  background-color: aqua;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".step {" & ASCII.LF);
      Put (File, "  margin-left: 3em;" & ASCII.LF);
      Put (File, "  border-left: 1em solid transparent;" & ASCII.LF);
      Put (File, "  margin-top: 0.25em;" & ASCII.LF);
      Put (File, "  padding-top: 0.25em;" & ASCII.LF);
      Put (File, "  padding-bottom: 0.25em;" & ASCII.LF);
      Put (File, "  padding-left: 0.5em;" & ASCII.LF);
      Put (File, "  font-size: 0.8em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".step.fail {" & ASCII.LF);
      Put (File, "  border-left-color: #c20000;" & ASCII.LF);
      Put (File, "  background-color: #fffbd3;" & ASCII.LF);
      Put (File, "  border-top-width: thin;" & ASCII.LF);
      Put (File, "  border-top-style: solid;" & ASCII.LF);
      Put (File, "  border-bottom-width: thin;" & ASCII.LF);
      Put (File, "  border-bottom-style: solid;" & ASCII.LF);
      Put (File, "  border-right-width: thin;" & ASCII.LF);
      Put (File, "  border-right-style: solid;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, ".step.pass {" & ASCII.LF);
      Put (File, "  border-left-color: #65c400;" & ASCII.LF);
      Put (File, "  background-color: #dbffb4;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, ".step.skip {" & ASCII.LF);
      Put (File, "  border-left-color: aqua;" & ASCII.LF);
      Put (File, "  background-color: #e0ffff;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".step.fail pre.string {" & ASCII.LF);
      Put (File, "  background-color: white;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".step.pass pre.string {" & ASCII.LF);
      Put (File, "  background-color: white;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".step.skip pre.string {" & ASCII.LF);
      Put (File, "  background-color: white;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "pre.string {" & ASCII.LF);
      Put (File, "  margin: 0.5em;" & ASCII.LF);
      Put (File, "  margin-left: 2em;" & ASCII.LF);
      Put (File, "  margin-right: 7em;" & ASCII.LF);
      Put (File, "  padding: 0.5em 0 0.5em 0.5em;" & ASCII.LF);
      Put (File, "  -moz-border-radius-topleft: 1em;" & ASCII.LF);
      Put (File, "  -webkit-border-radius-topleft: 1em;" & ASCII.LF);
      Put (File, "  border-radius-topleft: 1em;" & ASCII.LF);
      Put (File, "  -moz-border-radius-bottomright: 1em;" & ASCII.LF);
      Put (File, "  -webkit-border-radius-bottomright: 1em;" & ASCII.LF);
      Put (File, "  border-radius-bottomright: 1em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".trace {" & ASCII.LF);
      Put (File, "  margin: 0.5em;" & ASCII.LF);
      Put (File, "  margin-left: 2em;" & ASCII.LF);
      Put (File, "  margin-right: 7em;" & ASCII.LF);
      Put (File, "  padding: 0.5em 0 0.5em 0.5em;" & ASCII.LF);
      Put (File, "  background-color: white;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "body {" & ASCII.LF);
      Put (File, "  padding-top: 5em;" & ASCII.LF);
      Put (File, "  padding-left: 15em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#title {" & ASCII.LF);
      Put (File, "  position: absolute;" & ASCII.LF);
      Put (File, "  padding-left: 2em;" & ASCII.LF);
      Put (File, "  left: 15em;" & ASCII.LF);
      Put (File, "  top: 0;" & ASCII.LF);
      Put (File, "  right: 0;" & ASCII.LF);
      Put (File, "  overflow: hidden;" & ASCII.LF);
      Put (File, "  height: 5em;" & ASCII.LF);
      Put (File, "  color: white;" & ASCII.LF);
      Put (File, "  z-index: 1;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#title-background {" & ASCII.LF);
      Put (File, "  position: absolute;" & ASCII.LF);
      Put (File, "  left: 0;" & ASCII.LF);
      Put (File, "  right: 0;" & ASCII.LF);
      Put (File, "  top: 0;" & ASCII.LF);
      Put (File, "  height: 5em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#summary {" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#summary {" & ASCII.LF);
      Put (File, "  position: fixed;" & ASCII.LF);
      Put (File, "  bottom: 0;" & ASCII.LF);
      Put (File, "  left: 0;" & ASCII.LF);
      Put (File, "  width: 15em;" & ASCII.LF);
      Put (File, "  overflow: auto;" & ASCII.LF);
      Put (File, "  height: 100%;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#summary .report {" & ASCII.LF);
      Put (File, "  /*" & ASCII.LF);
      Put (File, "  top: 0;" & ASCII.LF);
      Put (File, "  left: 0;" & ASCII.LF);
      Put (File, "  width: 15em;" & ASCII.LF);
      Put (File, "  height: 8em;" & ASCII.LF);
      Put (File, "  overflow: visible;" & ASCII.LF);
      Put (File, "  z-index: 1;" & ASCII.LF);
      Put (File, "  */" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#summary .menu {" & ASCII.LF);
      Put (File, "  padding: 0.25em;" & ASCII.LF);
      Put (File, "  margin-right: 1em;" & ASCII.LF);
      Put (File, "  border-right: thin solid #EEEEEE;" & ASCII.LF);
      Put (File, "  background-color: white;" & ASCII.LF);
      Put (File, "  /*padding-top: 8em;*/" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#summary .report > *, #summary .menu > * {" & ASCII.LF);
      Put (File, "  font-size: 0.7em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".menu a {" & ASCII.LF);
      Put (File, "  text-decoration: none;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".menu *.fail > a {" & ASCII.LF);
      Put (File, "  font-weight: bold;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".menu > ul ul, .menu > ul ol {" & ASCII.LF);
      Put (File, "  padding: 0;" & ASCII.LF);
      Put (File, "  padding-left: 1em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".menu li {" & ASCII.LF);
      Put (File, "  margin: 0;" & ASCII.LF);
      Put (File, "  padding: 0;" & ASCII.LF);
      Put (File, "  margin-left: 1em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".menu > ul {" & ASCII.LF);
      Put (File, "  margin: 0;" & ASCII.LF);
      Put (File, "  padding: 0;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, ".menu > ul > li {" & ASCII.LF);
      Put (File, "  margin-top: 1em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, ".menu > ul > li > ul {" & ASCII.LF);
      Put (File, "  margin-top: 0.5em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#summary .report {" & ASCII.LF);
      Put (File, "  border-top: thin solid transparent;" & ASCII.LF);
      Put (File, "  /* BUG: without this, there is a spall space at the top */" & ASCII.LF);
      Put (File, "  -moz-border-radius-bottomright: 1em;" & ASCII.LF);
      Put (File, "  -webkit-border-radius-bottomright: 1em;" & ASCII.LF);
      Put (File, "  border-radius-bottomright: 1em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".report > * {" & ASCII.LF);
      Put (File, "  font-size: 0.75em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".report ul {" & ASCII.LF);
      Put (File, "  margin: .25em;" & ASCII.LF);
      Put (File, "  padding: .25em;" & ASCII.LF);
      Put (File, "  list-style-type: none;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".report.fail, #title-background.fail {" & ASCII.LF);
      Put (File, "  color: #fffbd3;" & ASCII.LF);
      Put (File, "  background-color: #c20000;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".report.pass, #title-background.pass {" & ASCII.LF);
      Put (File, "  color: #dbffb4;" & ASCII.LF);
      Put (File, "  background-color: #65c400;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, ".report.skip, #title-background.skip {" & ASCII.LF);
      Put (File, "  color: #004444;" & ASCII.LF);
      Put (File, "  background-color: aqua;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "li.report-steps, li.report-scenarios {" & ASCII.LF);
      Put (File, "  display: block;" & ASCII.LF);
      Put (File, "  float: left;" & ASCII.LF);
      Put (File, "  margin-left: 0.5em;" & ASCII.LF);
      Put (File, "  margin-right: 0.5em;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "    </style>" & ASCII.LF);
      Put (File, "  </head>" & ASCII.LF);
      Put (File, "  <body>" & ASCII.LF);
      Put (File, "    <div id=""title"">" & ASCII.LF);
      Put (File, "      <h1>Test suite results</h1>" & ASCII.LF);
      Put (File, "    </div>" & ASCII.LF);
   end page_begin;

   procedure feature_begin
        (File : in out File_Type;
         Param_id : in String;
         Param_name : in String;
         Param_description : in String) is
   begin
      Put (File, "    <div id=""feature-");
      Put (File, Param_id);
      Put (File, """ class=""feature skip"">" & ASCII.LF);
      Put (File, "      <div class=""feature-header"">" & ASCII.LF);
      Put (File, "        <h2 class=""title-feature"">Feature: ");
      Put (File, Param_name);
      Put (File, "</h2>" & ASCII.LF);
      Put (File, "        <pre>");
      Put (File, Param_description);
      Put (File, "</pre>" & ASCII.LF);
      Put (File, "      </div>" & ASCII.LF);
   end feature_begin;

   procedure background_begin
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String;
         Param_title : in String) is
   begin
      Put (File, "      <div id=""feature-");
      Put (File, Param_feature_id);
      Put (File, "-background-");
      Put (File, Param_num);
      Put (File, """ class=""scenario skip"">" & ASCII.LF);
      Put (File, "        <h3 class=""title-background"">Background: ");
      Put (File, Param_title);
      Put (File, "</h3>" & ASCII.LF);
   end background_begin;

   procedure background_end
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String) is
   begin
      Put (File, "        <script type=""text/javascript"">/*<![CDATA[*/" & ASCII.LF);
      Put (File, "          var scenario = document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, "-background-");
      Put (File, Param_num);
      Put (File, """);" & ASCII.LF);
      Put (File, "          if (scenario.className != ""scenario fail"") {" & ASCII.LF);
      Put (File, "            scenario.className = ""scenario pass""" & ASCII.LF);
      Put (File, "          };" & ASCII.LF);
      Put (File, "        /*]]>*/</script>" & ASCII.LF);
      Put (File, "      </div> <!-- background -->" & ASCII.LF);
   end background_end;

   procedure scenario_begin
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String;
         Param_title : in String) is
   begin
      Put (File, "      <div id=""feature-");
      Put (File, Param_feature_id);
      Put (File, "-scenario-");
      Put (File, Param_num);
      Put (File, """ class=""scenario skip"">" & ASCII.LF);
      Put (File, "        <h3 class=""title-scenario"">Scenario: ");
      Put (File, Param_title);
      Put (File, "</h3>" & ASCII.LF);
   end scenario_begin;

   procedure scenario_label
        (File : in out File_Type;
         Param_label : in String) is
   begin
      Put (File, "        <p>");
      Put (File, Param_label);
      Put (File, "</p>" & ASCII.LF);
   end scenario_label;

   procedure step_begin
        (File : in out File_Type;
         Param_status : in String;
         Param_stanza : in String) is
   begin
      Put (File, "        <div class=""step ");
      Put (File, Param_status);
      Put (File, """>" & ASCII.LF);
      Put (File, "          <p>");
      Put (File, Param_stanza);
      Put (File, "</p>" & ASCII.LF);
   end step_begin;

   procedure step_string
        (File : in out File_Type;
         Param_string : in String) is
   begin
      Put (File, "          <pre class=""string"">");
      Put (File, Param_string);
      Put (File, "</pre>" & ASCII.LF);
   end step_string;

   procedure step_table_begin
        (File : in out File_Type) is
   begin
      Put (File, "          <table>" & ASCII.LF);
   end step_table_begin;

   procedure step_table_row_begin
        (File : in out File_Type) is
   begin
      Put (File, "            <tr>" & ASCII.LF);
   end step_table_row_begin;

   procedure step_table_cell
        (File : in out File_Type;
         Param_string : in String) is
   begin
      Put (File, "              <td>");
      Put (File, Param_string);
      Put (File, "</td>" & ASCII.LF);
   end step_table_cell;

   procedure step_table_row_end
        (File : in out File_Type) is
   begin
      Put (File, "            </tr>" & ASCII.LF);
   end step_table_row_end;

   procedure step_table_end
        (File : in out File_Type) is
   begin
      Put (File, "          </table>" & ASCII.LF);
   end step_table_end;

   procedure step_separator
        (File : in out File_Type) is
   begin
      Put (File, "          <hr />" & ASCII.LF);
   end step_separator;

   procedure step_paragraph
        (File : in out File_Type;
         Param_string : in String) is
   begin
      Put (File, "          <p>");
      Put (File, Param_string);
      Put (File, "</p>" & ASCII.LF);
   end step_paragraph;

   procedure step_error_background
        (File : in out File_Type;
         Param_error : in String;
         Param_trace : in String;
         Param_feature_id : in String;
         Param_num : in String) is
   begin
      Put (File, "          <hr />" & ASCII.LF);
      Put (File, "          <pre class=""error"">");
      Put (File, Param_error);
      Put (File, "</pre>" & ASCII.LF);
      Put (File, "          <p>Stack trace:</p>" & ASCII.LF);
      Put (File, "          <pre class=""error trace"">");
      Put (File, Param_trace);
      Put (File, "</pre>" & ASCII.LF);
      Put (File, "          <script type=""text/javascript"">/*<![CDATA[*/" & ASCII.LF);
      Put (File, "            document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, "-background-");
      Put (File, Param_num);
      Put (File, """).className = ""scenario fail"";" & ASCII.LF);
      Put (File, "            document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, """).className = ""feature fail"";" & ASCII.LF);
      Put (File, "          /*]]>*/</script>" & ASCII.LF);
   end step_error_background;

   procedure step_error_scenario
        (File : in out File_Type;
         Param_error : in String;
         Param_trace : in String;
         Param_feature_id : in String;
         Param_num : in String) is
   begin
      Put (File, "          <hr />" & ASCII.LF);
      Put (File, "          <pre class=""error"">");
      Put (File, Param_error);
      Put (File, "</pre>" & ASCII.LF);
      Put (File, "          <p>Stack trace:</p>" & ASCII.LF);
      Put (File, "          <pre class=""error trace"">");
      Put (File, Param_trace);
      Put (File, "</pre>" & ASCII.LF);
      Put (File, "          <script type=""text/javascript"">/*<![CDATA[*/" & ASCII.LF);
      Put (File, "            document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, "-scenario-");
      Put (File, Param_num);
      Put (File, """).className = ""scenario fail"";" & ASCII.LF);
      Put (File, "            document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, """).className = ""feature fail"";" & ASCII.LF);
      Put (File, "          /*]]>*/</script>" & ASCII.LF);
   end step_error_scenario;

   procedure step_end
        (File : in out File_Type) is
   begin
      Put (File, "        </div> <!-- step -->" & ASCII.LF);
   end step_end;

   procedure scenario_end
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String) is
   begin
      Put (File, "        <script type=""text/javascript"">/*<![CDATA[*/" & ASCII.LF);
      Put (File, "          var backgrnd = document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, "-background-1"");" & ASCII.LF);
      Put (File, "          var scenario = document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, "-scenario-");
      Put (File, Param_num);
      Put (File, """);" & ASCII.LF);
      Put (File, "          if (backgrnd.className != ""scenario fail"" && scenario.className != ""scenario fail"") {" & ASCII.LF);
      Put (File, "            scenario.className = ""scenario pass""" & ASCII.LF);
      Put (File, "          };" & ASCII.LF);
      Put (File, "        /*]]>*/</script>" & ASCII.LF);
      Put (File, "      </div> <!-- scenario -->" & ASCII.LF);
   end scenario_end;

   procedure feature_end
        (File : in out File_Type;
         Param_feature_id : in String) is
   begin
      Put (File, "      <script type=""text/javascript"">/*<![CDATA[*/" & ASCII.LF);
      Put (File, "        var feature = document.getElementById(""feature-");
      Put (File, Param_feature_id);
      Put (File, """);" & ASCII.LF);
      Put (File, "        if (feature.className != ""feature fail"") {" & ASCII.LF);
      Put (File, "          feature.className = ""feature pass""" & ASCII.LF);
      Put (File, "        };" & ASCII.LF);
      Put (File, "      /*]]>*/</script>" & ASCII.LF);
      Put (File, "    </div> <!-- feature -->" & ASCII.LF);
   end feature_end;

   procedure report_begin
        (File : in out File_Type;
         Param_status : in String;
         Param_num_scenarios : in String;
         Param_num_scenarios_fail : in String;
         Param_num_scenarios_pass : in String;
         Param_num_steps : in String;
         Param_num_steps_fail : in String;
         Param_num_steps_skip : in String;
         Param_num_steps_pass : in String) is
   begin
      Put (File, "    <div id=""title-background"" class=""");
      Put (File, Param_status);
      Put (File, """>&nbsp;</div>" & ASCII.LF);
      Put (File, "    <div id=""summary"" class=""");
      Put (File, Param_status);
      Put (File, """>" & ASCII.LF);
      Put (File, "      <div class=""report ");
      Put (File, Param_status);
      Put (File, """>" & ASCII.LF);
      Put (File, "        <ul>" & ASCII.LF);
      Put (File, "          <li class=""report-scenarios"">");
      Put (File, Param_num_scenarios);
      Put (File, " scenarios" & ASCII.LF);
      Put (File, "            <ul>" & ASCII.LF);
      Put (File, "              <li>");
      Put (File, Param_num_scenarios_fail);
      Put (File, " failed</li>" & ASCII.LF);
      Put (File, "              <li>");
      Put (File, Param_num_scenarios_pass);
      Put (File, " passed</li>" & ASCII.LF);
      Put (File, "            </ul>" & ASCII.LF);
      Put (File, "          </li>" & ASCII.LF);
      Put (File, "          <li class=""report-steps"">");
      Put (File, Param_num_steps);
      Put (File, " steps" & ASCII.LF);
      Put (File, "            <ul>" & ASCII.LF);
      Put (File, "              <li>");
      Put (File, Param_num_steps_fail);
      Put (File, " failed</li>" & ASCII.LF);
      Put (File, "              <li>");
      Put (File, Param_num_steps_skip);
      Put (File, " skipped</li>" & ASCII.LF);
      Put (File, "              <li>");
      Put (File, Param_num_steps_pass);
      Put (File, " passed</li>" & ASCII.LF);
      Put (File, "            </ul>" & ASCII.LF);
      Put (File, "          </li>" & ASCII.LF);
      Put (File, "          <li class=""clear"">Finished in 0 seconds</li>" & ASCII.LF);
      Put (File, "        </ul>" & ASCII.LF);
      Put (File, "      </div>" & ASCII.LF);
   end report_begin;

   procedure report_menu_begin
        (File : in out File_Type) is
   begin
      Put (File, "      <div class=""menu"">" & ASCII.LF);
      Put (File, "        <ul>" & ASCII.LF);
   end report_menu_begin;

   procedure report_menu_feature_begin
        (File : in out File_Type;
         Param_status : in String;
         Param_feature_id : in String;
         Param_name : in String) is
   begin
      Put (File, "          <li class=""");
      Put (File, Param_status);
      Put (File, """>" & ASCII.LF);
      Put (File, "            <a href=""#feature-");
      Put (File, Param_feature_id);
      Put (File, """>");
      Put (File, Param_name);
      Put (File, "</a>" & ASCII.LF);
   end report_menu_feature_begin;

   procedure report_menu_scenarios_begin
        (File : in out File_Type) is
   begin
      Put (File, "            <ul>" & ASCII.LF);
   end report_menu_scenarios_begin;

   procedure report_menu_background
        (File : in out File_Type;
         Param_status : in String;
         Param_feature_id : in String;
         Param_num : in String;
         Param_name : in String) is
   begin
      Put (File, "              <li class=""title-background ");
      Put (File, Param_status);
      Put (File, """><a href=""#feature-");
      Put (File, Param_feature_id);
      Put (File, "-background-");
      Put (File, Param_num);
      Put (File, """>");
      Put (File, Param_name);
      Put (File, "</a></li>" & ASCII.LF);
   end report_menu_background;

   procedure report_menu_scenario
        (File : in out File_Type;
         Param_status : in String;
         Param_feature_id : in String;
         Param_num : in String;
         Param_name : in String) is
   begin
      Put (File, "              <li class=""title-scenario ");
      Put (File, Param_status);
      Put (File, """><a href=""#feature-");
      Put (File, Param_feature_id);
      Put (File, "-scenario-");
      Put (File, Param_num);
      Put (File, """>");
      Put (File, Param_name);
      Put (File, "</a></li>" & ASCII.LF);
   end report_menu_scenario;

   procedure report_menu_scenarios_end
        (File : in out File_Type) is
   begin
      Put (File, "            </ul>" & ASCII.LF);
   end report_menu_scenarios_end;

   procedure report_menu_feature_end
        (File : in out File_Type) is
   begin
      Put (File, "          </li>" & ASCII.LF);
   end report_menu_feature_end;

   procedure report_menu_end
        (File : in out File_Type) is
   begin
      Put (File, "        </ul>" & ASCII.LF);
      Put (File, "      </div>" & ASCII.LF);
   end report_menu_end;

   procedure report_end
        (File : in out File_Type) is
   begin
      Put (File, "    </div>" & ASCII.LF);
   end report_end;

   procedure page_end
        (File : in out File_Type) is
   begin
      Put (File, "  </body>" & ASCII.LF);
      Put (File, "</html>" & ASCII.LF);
   end page_end;

   pragma Style_Checks (On);

end AdaSpecLib.Format_HTML_Template;
