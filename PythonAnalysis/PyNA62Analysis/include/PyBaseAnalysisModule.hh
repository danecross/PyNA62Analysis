/*
 *
 * header file for PyBaseAnalysis
 *
 * contains definiton of the PyBaseAnalysis type so it can be used in PyAnalyzer
 *
 */


#ifndef __PYBASEANALYSIS_HH__
#define __PYBASEANALYSIS_HH__

#define VALUE_ERR_AN_VERB -2
#define VALUE_ERR_CORE_VERB -3
#define NULL_VERBOSITY -4
#define PYOBJECT_NOT_STRING -5

using namespace std;

typedef struct{

        PyObject_HEAD
	PyObject *currentPath;
        PyObject *inputFiles;

        PyObject *extraLibs;
        PyObject *extraLibsDirs;
        PyObject *extraIncludedDirs;
        PyObject *parameters;

        PyObject *coreVerbosity;
        PyObject *anVerbosity;

        PyObject *logFile;
        PyObject *outputFile;
        PyObject *primitiveFile;

        PyObject *graphicMode;
        PyObject *useDownscaling;
        PyObject *histoMode;
        PyObject *fastStart;
        PyObject *skipIsFatal;
        PyObject *continuousReading;
        PyObject *filter;
        PyObject *specialOnly;

        PyObject *startEvent;
        PyObject *NEvents;
        PyObject *NBursts;
	PyObject *maxBurst;
	
        PyObject *analyzers;

        PyObject *noCheckDetectors;
	PyObject *noCheckBadBurst;

        NA62Analysis::Core::BaseAnalysis *ban = 0;      // maintain reference to C++ base analysis object after configuration
//      TApplication *theApp = 0;                       // maintain reference to App for visual implementations

        bool noSkipBadBurst;
        bool noCheckEvents;

	vector<string> *burstsToIgnore;
	vector<string> *eventsToIgnore;

	int numAnalyzers;

} PyBaseAnalysis;

// PYTHON METHOD USABLE BY PYTHON USER
static PyObject * PBAN_printInfo(PyBaseAnalysis *, PyObject *);
static PyObject * PBAN_loadEvent(PyBaseAnalysis *, PyObject *);
static PyObject * PBAN_addAnalyzer(PyBaseAnalysis *, PyObject *);
static PyObject * PBAN_configure(PyBaseAnalysis *, PyObject *);

//ALLOCATION AND DEALLOCATION METHODS
static void PyBaseAnalysis_dealloc(PyBaseAnalysis *);
static PyObject * PyBaseAnalysis_new(PyTypeObject *, PyObject *, PyObject *);

//METHODS USED BY ABOVE METHODS
static PyObject * generateParameters(PyBaseAnalysis *);
static string getFileString(PyObject *);
static int addPyItemsToVector(PyObject *, vector<string> *);
static int setGlobalVerbosity(NA62Analysis::Core::BaseAnalysis *, PyObject *, PyObject *);
static string generateConfigFile(PyBaseAnalysis *);
static NA62Analysis::Verbosity::CoreVerbosityLevel getGlobalVerb(PyObject *);
static NA62Analysis::Verbosity::AnalyzerVerbosityLevel getAnalyzerVerb(PyObject *);

static PyMemberDef PyBanMembers[] = {

        {(char *)"input_files", T_OBJECT_EX, offsetof(PyBaseAnalysis, inputFiles), 0},
	{(char *)"currentPath", T_OBJECT_EX, offsetof(PyBaseAnalysis, currentPath), 0},
        {(char *)"extra_libs", T_OBJECT_EX, offsetof(PyBaseAnalysis,extraLibs), 0 },
        {(char *)"extra_libs_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraLibsDirs), 0},
        {(char *)"extra_include_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraIncludedDirs), 0} ,
        {(char *)"parameters", T_OBJECT_EX, offsetof(PyBaseAnalysis, parameters), 0} ,
        {(char *)"coreVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, coreVerbosity), 0},
        {(char *)"anVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, anVerbosity), 0},
        {(char *)"logFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, logFile), 0},
        {(char *)"graphicMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, graphicMode), 0},
        {(char *)"useDownscaling", T_OBJECT_EX, offsetof(PyBaseAnalysis, useDownscaling), 0},
        {(char *)"histoMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, histoMode), 0},
        {(char *)"primitiveFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, primitiveFile), 0},
        {(char *)"fastStart", T_OBJECT_EX, offsetof(PyBaseAnalysis, fastStart), 0},
        {(char *)"skipIsFatal", T_OBJECT_EX, offsetof(PyBaseAnalysis, skipIsFatal), 0},
        {(char *)"continuousReading", T_OBJECT_EX, offsetof(PyBaseAnalysis, continuousReading), 0},
        {(char *)"filter", T_OBJECT_EX, offsetof(PyBaseAnalysis, filter), 0},
        {(char *)"specialOnly", T_OBJECT_EX, offsetof(PyBaseAnalysis, specialOnly), 0},
        {(char *)"bursts_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, burstsToIgnore), 0},
        {(char *)"events_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, eventsToIgnore), 0},
        {(char *)"startEvent", T_OBJECT_EX, offsetof(PyBaseAnalysis, startEvent), 0 },
        {(char *)"NEvents", T_OBJECT_EX, offsetof(PyBaseAnalysis, NEvents), 0},
        {(char *)"NBursts", T_OBJECT_EX, offsetof(PyBaseAnalysis, NBursts), 0},
        {(char *)"analyzers", T_OBJECT_EX, offsetof(PyBaseAnalysis, analyzers), 0},
        {(char *)"noCheckDetectors", T_OBJECT_EX, offsetof(PyBaseAnalysis, noCheckDetectors), 0},
	{(char *)"noCheckBadBurst", T_OBJECT_EX, offsetof(PyBaseAnalysis, noCheckBadBurst), 0},
	{(char *)"maxBurst", T_OBJECT_EX, offsetof(PyBaseAnalysis, maxBurst), 0}, 
        {NULL}

};



static PyMethodDef PBAN_methods[] = {
        {"configure", (PyCFunction) PBAN_configure, METH_NOARGS,
                "Configure the Base Analysis type based on the inputs from the user"},
	{"addAnalyzer", (PyCFunction)PBAN_addAnalyzer, METH_VARARGS, 
		"Add an analyzer intance to the analyzers list"}, 
	{"loadEvent", (PyCFunction)PBAN_loadEvent, METH_VARARGS,
		"load the ith event for analysis"}, 
	{"printInfo", (PyCFunction)PBAN_printInfo, METH_NOARGS, 
		"print information about what has been instantiated in BaseAnalysis instance"}, 
        {NULL}
};

static PyTypeObject PyBaseAnalysisS= {
        PyVarObject_HEAD_INIT(0, 0)
        .tp_name = "BaseAnalysis",
        .tp_basicsize = sizeof(PyBaseAnalysis),
        .tp_itemsize = 0,
        .tp_dealloc = (destructor) PyBaseAnalysis_dealloc,
        .tp_print = 0,
        .tp_getattr = 0,
        .tp_setattr = 0,
        .tp_as_async = 0,
        .tp_repr = 0,
        .tp_as_number = 0,
        .tp_as_sequence = 0,
        .tp_as_mapping = 0,
        .tp_hash = 0,
        .tp_call = 0,
        .tp_str = 0,
        .tp_getattro = 0,
        .tp_setattro = 0,
        .tp_as_buffer = 0,
        .tp_flags = Py_TPFLAGS_DEFAULT,
        .tp_doc = "BaseAnalysis object to keep track of configuration information",
        .tp_traverse = 0,
        .tp_clear = 0,
        .tp_richcompare = 0,
        .tp_weaklistoffset = 0,
        .tp_iter = 0,
        .tp_iternext = 0,
        .tp_methods = PBAN_methods,
        .tp_members = PyBanMembers,
        .tp_getset = 0,
        .tp_base = 0,
        .tp_dict = 0,
        .tp_descr_get = 0,
        .tp_descr_set = 0,
        .tp_dictoffset = 0,
        .tp_init = 0,
        .tp_alloc = 0,
        .tp_new = PyBaseAnalysis_new,
        .tp_free = 0,
        .tp_is_gc = 0,
        .tp_bases = 0,
        .tp_mro = 0,
        .tp_cache = 0,
        .tp_subclasses = 0,
        .tp_weaklist = 0,
        .tp_del = 0,
        .tp_version_tag = 0,
        .tp_finalize = 0
};





#endif














