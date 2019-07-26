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

using namespace std;



typedef struct{

        PyObject_HEAD
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

        PyObject *burstsToIgnore;
        PyObject *eventsToIgnore;

        PyObject *startEvent;
        PyObject *NEvents;
        PyObject *NBursts;

        PyObject *analyzers;

        PyObject *noCheckDetectors;

        NA62Analysis::Core::BaseAnalysis *ban = 0;      // maintain reference to C++ base analysis object after configuration
//      TApplication *theApp = 0;                       // maintain reference to App for visual implementations

        bool noSkipBadBurst;
        bool noCheckEvents;

} PyBaseAnalysis;

// PYTHON METHOD USABLE BY PYTHON USER
static PyObject * PBAN_addAnalyzer(PyBaseAnalysis *, PyObject *);
static PyObject * PBAN_configure(PyBaseAnalysis *, PyObject *);

//ALLOCATION AND DEALLOCATION METHODS
static void PyBaseAnalysis_dealloc(PyBaseAnalysis *);
static PyObject * PyBaseAnalysis_new(PyTypeObject *, PyObject *, PyObject *);

//METHODS USED BY ABOVE METHODS
static string getFileString(PyObject *);
static int setGlobalVerbosity(NA62Analysis::Core::BaseAnalysis *, PyObject *, PyObject *);
static string * generateConfigFile(NA62Analysis::Core::BaseAnalysis *);
static NA62Analysis::Verbosity::CoreVerbosityLevel getGlobalVerb(PyObject *);
static NA62Analysis::Verbosity::AnalyzerVerbosityLevel getAnalyzerVerb(PyObject *);


static PyMemberDef PyBanMembers[] = {

        {"input_files", T_OBJECT_EX, offsetof(PyBaseAnalysis, inputFiles), 0},
        {"extra_libs", T_OBJECT_EX, offsetof(PyBaseAnalysis,extraLibs), 0 },
        {"extra_libs_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraLibsDirs), 0},
        {"extra_include_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraIncludedDirs), 0} ,
        {"parameters", T_OBJECT_EX, offsetof(PyBaseAnalysis, parameters), 0} ,
        {"coreVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, coreVerbosity), 0},
        {"anVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, anVerbosity), 0},
        {"logFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, logFile), 0},
        {"graphicMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, graphicMode), 0},
        {"useDownscaling", T_OBJECT_EX, offsetof(PyBaseAnalysis, useDownscaling), 0},
        {"histoMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, histoMode), 0},
        {"primitiveFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, primitiveFile), 0},
        {"fastStart", T_OBJECT_EX, offsetof(PyBaseAnalysis, fastStart), 0},
        {"skipIsFatal", T_OBJECT_EX, offsetof(PyBaseAnalysis, skipIsFatal), 0},
        {"continuousReading", T_OBJECT_EX, offsetof(PyBaseAnalysis, continuousReading), 0},
        {"filter", T_OBJECT_EX, offsetof(PyBaseAnalysis, filter), 0},
        {"specialOnly", T_OBJECT_EX, offsetof(PyBaseAnalysis, specialOnly), 0},
        {"bursts_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, burstsToIgnore), 0},
        {"events_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, eventsToIgnore), 0},
        {"startEvent", T_OBJECT_EX, offsetof(PyBaseAnalysis, startEvent), 0 },
        {"NEvents", T_OBJECT_EX, offsetof(PyBaseAnalysis, NEvents), 0},
        {"NBursts", T_OBJECT_EX, offsetof(PyBaseAnalysis, NBursts), 0},
        {"analyzers", T_OBJECT_EX, offsetof(PyBaseAnalysis, analyzers), 0},
        {"noCheckDetectors", T_OBJECT_EX, offsetof(PyBaseAnalysis, noCheckDetectors), 0},
        {NULL}

};



static PyMethodDef PBAN_methods[] = {
        {"configure", (PyCFunction) PBAN_configure, METH_NOARGS,
                "Configure the Base Analysis type based on the inputs from the user"},
	{"addAnalyzer", (PyCFunction)PBAN_addAnalyzer, METH_VARARGS, 
		"Add an analyzer intance to the analyzers list"}, 
        {NULL}
};

static PyTypeObject PyBaseAnalysisS= {
        PyVarObject_HEAD_INIT(NULL, 0)
        .tp_name = "BaseAnalysis",
        .tp_basicsize = sizeof(PyBaseAnalysis),
        .tp_itemsize = 0,
        .tp_dealloc = (destructor) PyBaseAnalysis_dealloc,
        .tp_print = NULL,
        .tp_getattr = NULL,
        .tp_setattr = NULL,
        .tp_as_async = NULL,
        .tp_repr = 0,
        .tp_as_number = 0,
        .tp_as_sequence = 0,
        .tp_as_mapping = 0,
        .tp_hash = 0,
        .tp_call = NULL,
        .tp_str = 0,
        .tp_getattro = NULL,
        .tp_setattro = NULL,
        .tp_as_buffer = 0,
        .tp_flags = Py_TPFLAGS_DEFAULT,
        .tp_doc = "BaseAnalysis object to keep track of configuration information",
        .tp_traverse = NULL,
        .tp_clear = NULL,
        .tp_richcompare = NULL,
        .tp_weaklistoffset = NULL,
        .tp_iter = NULL,
        .tp_iternext = NULL,
        .tp_methods = PBAN_methods,
        .tp_members = PyBanMembers,
        .tp_getset = NULL,
        .tp_base = NULL,
        .tp_dict = NULL,
        .tp_descr_get = NULL,
        .tp_descr_set = NULL,
        .tp_dictoffset = NULL,
        .tp_init = NULL,
        .tp_alloc = NULL,
        .tp_new = PyBaseAnalysis_new,
        .tp_free = NULL,
        .tp_is_gc = NULL,
        .tp_bases = NULL,
        .tp_mro = NULL,
        .tp_cache = NULL,
        .tp_subclasses = NULL,
        .tp_weaklist = NULL,
        .tp_del = NULL,
        .tp_version_tag = 0,
        .tp_finalize = 0
};





#endif














