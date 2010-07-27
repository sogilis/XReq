#! python
# -*- coding: utf-8 -*-

"""
XReq  -  Plugin for XReq

This plugin use a special project file to configure XReq: Makefile.xreq
This is also a standard Makefile and its variables are used to configure the
XReq project. When reading this file, no substitutions are made in variable
values.

Project variables:

  SUBDIRS:              Subdirectories to look for subprojects
  SUBMAKEFILE:          Makefile name: Makefire.xreq
  FEATURE_DIR:          directory where the .feature files are
  STEP_DEFINITIONS:     directory where the step definitions are
  RESULT_DIR:           where to generate the test suite
  TEST_SUITE:           name of the test suite
  GENERATED_STEPS:      package name where to put generated steps

Available commands for external use:

  def compile(filename = None):
    Compile the filename `filename' or the current project if not given

  def generate_steps(filename = None):
    Generate the step definitions

  def run_tests():
    Run the test suite, and show it once completed

  def browse_test_report():
    Browse the test report

  def go_to_spec():
    Parse the current buffer with xreq --partial --step-matching
    The result is used to jump to the step definition file

  def edit_makefile():
    Open in an editor the Makefile.xreq project file.

  def show_feature_browser():
    Show the feature browser and refresh its content.
    
TODO:

 * the go to spec feature only search on the parent XReq project and not
   child projects.
"""

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

import GPS
import gtk, gobject
import os
import re
import subprocess
import shlex

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                                XReq Project                                ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class XReqProject:

  def __init__(self, makefile = None):
    if makefile == None:
      makefile = makefile_path();
    GPS.Console().write ("Open project %s\n" % makefile)
    self._dirname  = os.path.dirname(makefile)
    self._basename = os.path.basename(makefile)
    self._name     = os.path.basename(self._dirname)
    self._makefile = makefile;
    self._vars     = parse_makefile(makefile)
    self._subdirs  = shlex.split(self._vars['SUBDIRS'])
    self._subprojects = {}
    for d in self._subdirs:
      dir  = os.path.join(self._dirname, d)
      if os.path.isdir(dir):
        path = os.path.join(dir, self._vars['SUBMAKEFILE'])
        proj = XReqProject(path)
        proj._name = d
        self._subprojects[d] = proj
  
  def _makereletive(self, active, path):
    if active:
      return path;
    else:
      return os.path.join(self._dirname, path);
  
  def name(self):
    return self._name;
  
  def dirname(self):
    return self._dirname
  
  def basename(self):
    return self._basename
  
  def makefile_path(self, relative=False):
    return self._makefile
   
  def subprojects(self):
    return self._subprojects
  
  def subdirs(self, relative=False):
    if relative:
      res = []
      for d in self._subdirs:
        res.append(os.path.join(self._dirname, d))
      return res
    else:
      return self._subdirs
  
  def feature_dir(self, relative=False):
    return self._makereletive(relative, self._vars['FEATURE_DIR'])
  
  def steps_dir(self, relative=False):
    return self._makereletive(relative, self._vars['STEP_DEFINITIONS_DIR'])
  
  def result_dir(self, relative=False):
    return self._makereletive(relative, self._vars['RESULT_DIR'])
  
  def test_suite_basename(self):
    return self._vars['TEST_SUITE']
  
  def test_suite_path(self, relative=False):
    return os.path.join(self.result_dir(relative), self.test_suite_basename());
  
  def generated_steps_package(self):
    return self._vars['GENERATED_STEPS']
  
  def html_report_path(self, relative=False):
    return self._makereletive(relative, self._vars['REPORT_FILE'])
    
  def xreq_flags(self):
    return shlex.split(self._vars['XREQFLAGS'])

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                         Makefile related functions                         ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def makefile_path():
  return os.path.join(os.path.dirname(GPS.Project.root().file().name()), "Makefile.xreq")


def create_makefile(file = makefile_path()):
  if not os.path.isfile(file):
    f = open(file, "w")
    f.write ("""
#########################
##  XReq Project File  ##
#########################

# SUBDIRS:              Subdirectories to look for subprojects
# SUBMAKEFILE:          Makefile name: Makefire.xreq
# FEATURE_DIR:          directory where the .feature files are
# STEP_DEFINITIONS_DIR: directory where the step definitions are
# RESULT_DIR:           where to generate the test suite
# TEST_SUITE:           name of the test suite
# GENERATED_STEPS:      package name where to put generated steps
# REPORT_FILE:          where the HTML reports should go

SUBDIRS=
SUBMAKEFILE=Makefile.xreq
FEATURE_DIR=features
STEP_DEFINITIONS_DIR=features/step_definitions
RESULT_DIR=features/tests
TEST_SUITE=test_suite
GENERATED_STEPS=Generated_Steps
REPORT_FILE=features/tests/xreq-report.html

XREQFLAGS=
GPRFLAGS=-m

XREQ=xreq
GPRBUILD=gprbuild

FEATURE_FILES=$(shell find '$(FEATURE_DIR)' -name '*.feature')
ifeq ($(FEATURE_FILES),)
MAIN_TARGET=
else
MAIN_TARGET=$(RESULT_DIR)/$(TEST_SUITE)
endif

all: $(MAIN_TARGET)
	for d in $(SUBDIRS); do $(MAKE) -C "$$d" -f "$(SUBMAKEFILE)" all || exit $$?; done
.PHONY: all

$(RESULT_DIR)/$(TEST_SUITE): $(RESULT_DIR)/$(TEST_SUITE).gpr
	$(GPRBUILD) -P$< $(GPRFLAGS)
.PHONY: $(RESULT_DIR)/$(TEST_SUITE)

$(RESULT_DIR)/$(TEST_SUITE).gpr: $(FEATURE_FILES)
	$(XREQ) -x '$(TEST_SUITE)' -s '$(STEP_DEFINITIONS_DIR)' $(XREQFLAGS) $+

clean:
	-$(RM) $(RESULT_DIR)/$(TEST_SUITE).{gpr,adb}
	-$(RM) $(RESULT_DIR)/feature_*.{ads,adb}
	-@for d in $(SUBDIRS); do $(MAKE) -C "$$d" -f "$(SUBMAKEFILE)" clean; done
.PHONY: clean

    """)
    f.close()
  return file

def parse_makefile(filename = makefile_path()):
  file = create_makefile(filename)
  #GPS.Console().write("Parse %s\n" % file)
  res = {
    'SUBDIRS'             : "",
    'SUBMAKEFILE'         : "Makefile.xreq",
    'FEATURE_DIR'         : "features",
    'STEP_DEFINITIONS_DIR': "features/steps",
    'RESULT_DIR'          : "features/tests",
    'TEST_SUITE'          : "",
    'GENERATED_STEPS'     : "Generated_Steps",
    'REPORT_FILE'         : "features/tests/xreq-report.html",
    'XREQFLAGS'           : "",}
  f = open (file, "r")
  for line in f.readlines():
    var, eq, val = line.partition("=")
    var = var.strip()
    val = val.strip()
    if eq == "=" and re.match("[A-Z][A-Z0-9_]*", var):
      res[var] = val
      #GPS.Console().write("%s=%s\n" % (var, val))
  f.close()
  return res

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                                Main actions                                ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def compile(filename = None, prj = XReqProject()):
  Command_XReq(filename, prj = prj)

def generate_steps(filename = None, prj = XReqProject()):
  Command_XReq(filename, run_gprbuild = False, generate_steps = True, prj = prj)

def run_tests(prj = XReqProject()):
  compile(prj = prj)
  Command_TestSuite(prj = prj)
  browse_test_report(prj = prj)

def browse_test_report(prj = XReqProject()):
  GPS.HTML.browse(prj.html_report_path())

def go_to_spec(prj = XReqProject()):
  context = GPS.current_context()
  file    = context.file()
  line_no = context.location().line()
  buffer  = GPS.EditorBuffer.get(file)
  #GPS.Console().write ("%s line %d\n" % (file.name(), line_no))
  if not buffer.is_modified():
    filename = file.name()
    delete   = False
  else:
    filename = GPS.dump(buffer.get_chars())
    delete   = True
  args = ["xreq", "--partial", "--step-matching", "--step", prj.steps_dir(), filename]
  p = subprocess.Popen(args, stdout=subprocess.PIPE)
  step_file = None
  step_line = None
  for line in p.stdout.readlines():
    m = re.match('^Step Matching: "(.*):([0-9]+)" matches "(.*):([0-9]+)" procedure (.*)$', line)
    if m:
      l = int(m.group(2))
      if l > line_no:
        break
      else:
        #GPS.Console().write ("line %d match %s (%s:%d)\n" % (
        #  int(m.group(2)), m.group(5), m.group(3), int(m.group(4))));
        step_file = m.group(3)
        step_line = int(m.group(4))
  p.stdout.close()
  if delete:
    os.remove(filename)
  if step_file:
    open_file(step_file, step_line)

def edit_makefile():
  open_file(create_makefile())

def show_feature_browser():
  win = GPS.MDI.get ('Features')
  if win:
    win.raise_window();
  else:
    #GPS.MDI.get ("Project").raise_window()
    view = FeatureBrowser ()
    GPS.MDI.add (view, "Features", "Features")
    win = GPS.MDI.get ('Features')
    #win.float(True)
    #GPS.MDI.get ("Project").raise_window()
    #win.float(False)
    win.raise_window()
    win.split (False)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                          Editor related functions                          ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def open_file(file, line=None, always_open=False):
  if line:
    GPS.Console().write ("Open %s line %d\n" % (file, line))
  else:
    GPS.Console().write ("Open %s\n" % file)
  if not always_open and not line and file.endswith(".gpr"):
    GPS.Project.load(file);
  else:
    ed = GPS.EditorBuffer.get (GPS.File (file))
    GPS.MDI.get_by_child (ed.current_view()).raise_window()
    if line:
      ed.current_view().goto(GPS.EditorLocation (ed, line, 0) )

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                              Launch  gprbuild                              ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class Command_GprBuild (GPS.Process):
  location_category = "Build test suite"
  def on_match (self, matched, unmatched):
    matched = re.sub("^\n*", "", matched)
    matched = re.sub("\n+", "\n", matched)
    for line in matched.split("\n"):
      line = "%s/%s" % (self.prj.result_dir(), line)
      GPS.Locations.parse(line,
                          self.location_category,
                          regexp = "^(.*)\:([0-9]+)\:([0-9]+)\: +(.*)$",
                          file_index = 1,
                          line_index = 2,
                          column_index = 3,
                          msg_index = 4)
    GPS.Console().write(matched)

  def __init__ (self, projectfile, prj = XReqProject()):
    self.prj = prj
    GPS.Console().write("\n\n")
    GPS.Locations.remove_category(self.location_category);
    GPS.Process.__init__ (self, 'gprbuild -m -vP0 -d """-P%s"""' % projectfile,
      show_command = True,
      regexp = "^.+$",
      single_line_regexp = True,
      on_match = self.on_match,
      progress_regexp = "^completed ([0-9]+) out of ([0-9]+).*$",
      progress_current = 1,
      progress_total   = 2)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                                Launch  xreq                                ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class Command_XReq(GPS.Process):
  location_category = "Build features"
  def on_match (self, matched, unmatched):
    matched = re.sub("^\n*", "", matched)
    matched = re.sub("\n+", "\n", matched)
    for line in matched.split("\n"):
      GPS.Locations.parse(line,
                          self.location_category,
                          regexp = "^(.*)\:([0-9]+)\: ERROR: +(.*)$",
                          file_index = 1,
                          line_index = 2,
                          msg_index = 3)
    GPS.Console().write(matched)

  def on_exit (self, status, output):
    GPS.Console().write(output)
    if self.run_gprbuild and self.prj.test_suite_basename():
      Command_GprBuild(self.prj.test_suite_path(), self.prj)

  def __init__ (self, filename=None, run_gprbuild=True, prj = XReqProject(), generate_steps=False):
    self.prj = prj
    self.run_gprbuild = run_gprbuild

    GPS.Console().write("\n\n")
    GPS.Locations.remove_category(self.location_category);

    command="xreq --progress --keep-going --quiet";
    if prj.test_suite_basename() and not filename:
      command = command + ' --executable """%s"""' % prj.test_suite_basename()
    command = command + ' --step """%s""" ' % prj.steps_dir()
    command = command + ' --output """%s"""' % prj.result_dir()
    if generate_steps:
      command = command + ' --fill-steps-in """%s"""' % prj.generated_steps_package()
    flags = prj.xreq_flags()
    if len(flags):
      command = command + ' """%s"""' % ('""" """'.join(flags))
    if filename:
      command = command + ' """%s"""' % filename
    else:
      dirs = [prj.feature_dir()]
      for d in dirs:
        for f in os.listdir(d):
          fullpath = os.path.join(prj.feature_dir(), f)
          if f.endswith(".feature") and os.path.isfile(fullpath):
            command = command + ' """%s"""' % fullpath
          elif os.path.isdir(fullpath):
            dirs.append(fullpath)

    GPS.Process.__init__ (self, command,
      show_command = True,
      regexp = "^.+$",
      single_line_regexp = True,
      on_match = self.on_match,
      on_exit  = self.on_exit,
      progress_regexp = "^completed ([0-9]+) out of ([0-9]+)$",
      progress_current = 1,
      progress_total   = 2)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                             Launch  test suite                             ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class Command_TestSuite(GPS.Process):
  def on_match (self, matched, unmatched):
    matched = re.sub("^\n*", "", matched)
    matched = re.sub("\n+", "\n", matched)
    GPS.Console().write(matched)

  def on_exit (self, status, output):
    GPS.Console().write(output)

  def __init__ (self, prj=XReqProject()):
    self.prj = prj
    self.test_suite = prj.test_suite_path()

    GPS.Console().write("\n\n")

    command = self.test_suite
    command = command + ' -d -f html'
    command = command + ' -o """%s"""' % prj.html_report_path()

    GPS.Process.__init__ (self, command,
      show_command = True,
      regexp = "^.+$",
      single_line_regexp = True,
      on_match = self.on_match,
      on_exit  = self.on_exit)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                         Feature Browser  side view                         ##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

class FeatureBrowser(gtk.Table):
  def __init__(self):
    self.img_x = gtk.icon_theme_get_default().load_icon(gtk.STOCK_EXECUTE, gtk.ICON_SIZE_MENU, 0)
    self.img_d = gtk.icon_theme_get_default().load_icon(gtk.STOCK_DIRECTORY, gtk.ICON_SIZE_MENU, 0)
    self.img_f = gtk.icon_theme_get_default().load_icon(gtk.STOCK_FILE, gtk.ICON_SIZE_MENU, 0)

    gtk.Table.__init__(self, rows=2, columns=1);
    self.toolBar = gtk.Toolbar()

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_REFRESH)
    btn.set_label("Refresh")
    btn.set_tooltip_text("Refresh the feature view")
    btn.connect ('clicked', lambda x: self.refresh())
    self.toolBar.insert(btn, 1)

    #btn = gtk.ToolButton()
    #btn.set_icon_name(gtk.STOCK_EDIT)
    #btn.set_label("Edit project file")
    #btn.set_tooltip_text("Edit the XReq Makefile")
    #btn.connect ('clicked', lambda x: edit_makefile())
    #self.toolBar.insert(btn, 2)

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_EXECUTE)
    btn.set_label("Compile")
    btn.set_tooltip_text("Compile features")
    btn.connect ('clicked', lambda x: compile(prj = self.activated_project))
    self.toolBar.insert(btn, 3)

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_CONVERT)
    btn.set_label("Generate Steps")
    btn.set_tooltip_text("Generate missing step definitions")
    btn.connect ('clicked', lambda x: (generate_steps(prj = self.activated_project), compile(prj = self.activated_project)))
    self.toolBar.insert(btn, 4)

    btn = gtk.ToolButton()
    btn.set_icon_name(gtk.STOCK_MEDIA_PLAY)
    btn.set_label("Test")
    btn.set_tooltip_text("Start test suite")
    btn.connect ('clicked', lambda x: run_tests(prj = self.activated_project))
    self.toolBar.insert(btn, 5)

    self.attach(self.toolBar, 0, 1, 0, 1, yoptions=0)
    self.treeModel = gtk.TreeStore(gtk.gdk.Pixbuf, gobject.TYPE_STRING, gobject.TYPE_PYOBJECT)
    self.treeView = gtk.TreeView(self.treeModel);
    self.populateModel();
    col = gtk.TreeViewColumn()
    self.treeView.append_column(col);
    cell = gtk.CellRendererPixbuf()
    col.pack_start(cell, False)
    col.add_attribute(cell, 'pixbuf', 0)
    cell = gtk.CellRendererText()
    col.pack_start(cell, True)
    col.add_attribute(cell, 'text', 1)
    col.set_cell_data_func(cell, self.filter_prj_column)
    self.treeView.set_search_column(1)
    self.treeView.set_headers_visible(False)
    self.treeView.connect("row-activated", self.feature_activated)
    scroll = gtk.ScrolledWindow()
    scroll.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
    scroll.add(self.treeView)
    self.attach(scroll, 0, 1, 1, 2)

  def filter_prj_column(self, column, cell, model, it):
    if model.get_value(it, 2) == self.activated_project:
      cell.set_property("weight", 700);
    else:
      cell.set_property("weight", 400);

  def populateModel(self):
    prj = XReqProject();
    self.root_project = prj
    self.activated_project = prj
    self.treeModel.clear()
    it = self.populateModelFromProject(XReqProject())
    path = self.treeModel.get_path(it)
    self.activated_row = gtk.TreeRowReference(self.treeModel, path)
    self.treeView.expand_all()
  
  def populateModelFromProject(self, prj, main_it = None):
    img_x = self.img_x
    img_d = self.img_d
    img_f = self.img_f
    
    # [PRJ]  Project
    main_it = self.treeModel.append(main_it, [img_x, prj.name(), prj])
    # [FILE] Makefile.xreq
    if os.path.isfile(prj.makefile_path()):
      self.treeModel.append(main_it, [img_f, prj.basename(), prj.makefile_path()])
    # [FILE] results.html
    if os.path.isfile(prj.html_report_path()):
      self.treeModel.append(main_it, [img_f, os.path.basename(prj.html_report_path()), prj.html_report_path()])
    # [DIR]  Features
    if os.path.isdir(prj.feature_dir()):
      it = self.treeModel.append(main_it, [img_d, "Features", prj.feature_dir()]);
      if 0 == self.populateFeatureFileRecursive(it, prj.feature_dir(), img_d, img_f):
        self.treeModel.remove(it)
    # [DIR]  Step Definitions
    if os.path.isdir(prj.steps_dir()):
      it = self.treeModel.append(main_it, [img_d, "Step Definitions", prj.steps_dir()]);
      for f in os.listdir(prj.steps_dir()):
        fullname = os.path.join(prj.steps_dir(), f)
        if (f.endswith (".adb") or f.endswith (".ads")) and os.path.isfile(fullname):
          self.treeModel.append(it, [img_f, f, fullname])
    # [DIR]  Generated Files
    if os.path.isdir(prj.result_dir()):
      it = self.treeModel.append(main_it, [img_d, "Generated Files", prj.result_dir()]);
      for f in os.listdir(prj.result_dir()):
        fullname = os.path.join(prj.result_dir(), f)
        if (f.endswith (".adb") or f.endswith (".ads") or f.endswith (".gpr")) and os.path.isfile(fullname):
          self.treeModel.append(it, [img_f, f, fullname])
    # [PRJ]  Subprojects
    subprojects = prj.subprojects()
    for p in subprojects:
      self.populateModelFromProject(subprojects[p], main_it)

    return main_it

  def populateFeatureFileRecursive(self, it, dir, img_d, img_f):
    nfile = 0
    for f in os.listdir(dir):
      fullname = os.path.join(dir, f)
      if f.endswith (".feature") and os.path.isfile(fullname):
        self.treeModel.append(it, [img_f, f, fullname])
        nfile = nfile + 1
      elif os.path.isdir(fullname):
        i = self.treeModel.append(it, [img_d, f, fullname]);
        n = self.populateFeatureFileRecursive(i, fullname, img_d, img_f)
        if n == 0:
          self.treeModel.remove(i)
        else:
          nfile = nfile + n
    return nfile

  def refresh(self):
    self.populateModel()

  def feature_activated(self, tree, path, view_col, *user):
    model = tree.get_model()
    it = model.get_iter(path)
    col2 = model.get_value(it, 2)
    if isinstance(col2, XReqProject):
      # get data
      model2 = self.activated_row.get_model()
      path2 = self.activated_row.get_path()
      it2 = model2.get_iter(path)
      # update activated project
      self.activated_row = gtk.TreeRowReference(model, path)
      self.activated_project = col2
      # Notify row changed
      model.row_changed(path, it)
      model2.row_changed(path2, it2)
    elif col2.endswith(".html"):
      GPS.HTML.browse(col2)
    else:
      open_file(col2)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
##                                   Hooks                                   .##
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

def on_project_changed(hook):
  if os.path.isfile(makefile_path()):
    show_feature_browser()

GPS.Hook ("project_changed").add (on_project_changed)
