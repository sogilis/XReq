package body AdaSpecLib.Format_HTML_Template is

   pragma Style_Checks (Off);

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
      Put (File, "        <p>");
      Put (File, Param_description);
      Put (File, "</p>" & ASCII.LF);
      Put (File, "      </div>" & ASCII.LF);
   end feature_begin;

   procedure step_end
        (File : in out File_Type) is
   begin
      Put (File, "        </div> <!-- step -->" & ASCII.LF);
   end step_end;

   procedure page_begin
        (File : in out File_Type) is
   begin
      Put (File, "<?xml version=""1.0"" encoding=""UTF-8""?>" & ASCII.LF);
      Put (File, "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Strict//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"">" & ASCII.LF);
      Put (File, "<html xmlns=""http://www.w3.org/1999/xhtml"" xml:lang=""en"" lang=""en"">" & ASCII.LF);
      Put (File, "  <head>" & ASCII.LF);
      Put (File, "    <title>Test suite</title>" & ASCII.LF);
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
      Put (File, "  position: fixed;" & ASCII.LF);
      Put (File, "  left: 0;" & ASCII.LF);
      Put (File, "  top: 0;" & ASCII.LF);
      Put (File, "  bottom: 0;" & ASCII.LF);
      Put (File, "  width: 15em;" & ASCII.LF);
      Put (File, "  overflow: auto;" & ASCII.LF);
      Put (File, "}" & ASCII.LF);
      Put (File, "" & ASCII.LF);
      Put (File, "#summary .menu {" & ASCII.LF);
      Put (File, "  font-size: 0.8em;" & ASCII.LF);
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
      Put (File, "  <body class=""pass"">" & ASCII.LF);
      Put (File, "    <div id=""title"">" & ASCII.LF);
      Put (File, "      <h1>Test suite results</h1>" & ASCII.LF);
      Put (File, "    </div>" & ASCII.LF);
   end page_begin;

   procedure background_begin
        (File : in out File_Type;
         Param_feature_id : in String) is
   begin
      Put (File, "      <div id=""feature-");
      Put (File, Param_feature_id);
      Put (File, "-background"" class=""scenario skip"">" & ASCII.LF);
   end background_begin;

   procedure scenario_begin
        (File : in out File_Type;
         Param_feature_id : in String;
         Param_num : in String) is
   begin
      Put (File, "      <div id=""feature-");
      Put (File, Param_feature_id);
      Put (File, "-scenario-");
      Put (File, Param_num);
      Put (File, """ class=""scenario skip"">" & ASCII.LF);
   end scenario_begin;

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

   procedure scenario_end
        (File : in out File_Type) is
   begin
      Put (File, "      </div> <!-- scenario -->" & ASCII.LF);
   end scenario_end;

   procedure report
        (File : in out File_Type) is
   begin
      Put (File, "    <div id=""title-background"" class=""fail"">&nbsp;</div>" & ASCII.LF);
      Put (File, "    <div id=""summary"" class=""fail"">" & ASCII.LF);
      Put (File, "      <div class=""report fail"">" & ASCII.LF);
      Put (File, "        <ul>" & ASCII.LF);
      Put (File, "          <li class=""report-scenarios"">10 scenarios" & ASCII.LF);
      Put (File, "            <ul>" & ASCII.LF);
      Put (File, "              <li>3 failed</li>" & ASCII.LF);
      Put (File, "              <li>7 passed</li>" & ASCII.LF);
      Put (File, "            </ul>" & ASCII.LF);
      Put (File, "          </li>" & ASCII.LF);
      Put (File, "          <li class=""report-steps"">100 steps" & ASCII.LF);
      Put (File, "            <ul>" & ASCII.LF);
      Put (File, "              <li>20 failed</li>" & ASCII.LF);
      Put (File, "              <li>20 skipped</li>" & ASCII.LF);
      Put (File, "              <li>60 passed</li>" & ASCII.LF);
      Put (File, "            </ul>" & ASCII.LF);
      Put (File, "          </li>" & ASCII.LF);
      Put (File, "          <li class=""clear"">Finished in 0 seconds</li>" & ASCII.LF);
      Put (File, "        </ul>" & ASCII.LF);
      Put (File, "      </div>" & ASCII.LF);
      Put (File, "      <ul class=""menu"">" & ASCII.LF);
      Put (File, "        <li class=""fail"">" & ASCII.LF);
      Put (File, "          <a href=""#feature-1"">Feature: Ambiguous step definition error reporting</a>" & ASCII.LF);
      Put (File, "          <ul>" & ASCII.LF);
      Put (File, "            <li class=""title-background fail""><a href=""#feature-1-background"">Background</a></li>" & ASCII.LF);
      Put (File, "            <li class=""title-scenario skip""><a href=""#feature-1-scenario-1"">Scenario</a></li>" & ASCII.LF);
      Put (File, "            <li class=""title-scenario skip""><a href=""#feature-1-scenario-2"">Scenario</a></li>" & ASCII.LF);
      Put (File, "          </ul>" & ASCII.LF);
      Put (File, "        </li>" & ASCII.LF);
      Put (File, "      </ul>" & ASCII.LF);
      Put (File, "    </div>" & ASCII.LF);
   end report;

   procedure background_end
        (File : in out File_Type) is
   begin
      Put (File, "      </div> <!-- background -->" & ASCII.LF);
   end background_end;

   procedure page_end
        (File : in out File_Type) is
   begin
      Put (File, "  </body>" & ASCII.LF);
      Put (File, "</html>" & ASCII.LF);
   end page_end;

   procedure feature_end
        (File : in out File_Type) is
   begin
      Put (File, "    </div> <!-- feature -->" & ASCII.LF);
   end feature_end;

   procedure step_string
        (File : in out File_Type;
         Param_string : in String) is
   begin
      Put (File, "          <pre class=""string"">");
      Put (File, Param_string);
      Put (File, "</pre>" & ASCII.LF);
   end step_string;

   pragma Style_Checks (On);

end AdaSpecLib.Format_HTML_Template;
