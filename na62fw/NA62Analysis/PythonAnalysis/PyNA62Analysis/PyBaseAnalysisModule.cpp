/*
 *
 *  Written by: Dane Cross
 *
 *  Python BaseAnalysis struct
 *  Keeps track of information to be put into the BaseAnalysis 
 *
 *
 *
 * */



#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include "UserMethods.hh"
#include<iostream>
#include <structmember.h>
#include <TApplication.h>
#include "BaseAnalysis.hh"
#include "include/PyAnalysis.hh"

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
	
	NA62Analysis::Core::BaseAnalysis *ban = 0; 	// maintain reference to C++ base analysis object after configuration
//	TApplication *theApp = 0;		   	// maintain reference to App for visual implementations

	bool noSkipBadBurst;
	bool noCheckEvents;

} PyBaseAnalysis;

// PYTHON DESTRUCTOR
static void PyBaseAnalysis_dealloc(PyBaseAnalysis *self){

	Py_XDECREF(self->inputFiles);

	Py_XDECREF(self->extraLibs);
        Py_XDECREF(self->extraLibsDirs);
        Py_XDECREF(self->extraIncludedDirs);
        Py_XDECREF(self->parameters);

        Py_XDECREF(self->coreVerbosity);
        Py_XDECREF(self->anVerbosity);

        Py_XDECREF(self->logFile);
	Py_XDECREF(self->outputFile);
	Py_XDECREF(self->primitiveFile);

        Py_XDECREF(self->graphicMode);
        Py_XDECREF(self->useDownscaling);
        Py_XDECREF(self->histoMode);
        Py_XDECREF(self->fastStart);
        Py_XDECREF(self->skipIsFatal);
        Py_XDECREF(self->continuousReading);
        Py_XDECREF(self->filter);
        Py_XDECREF(self->specialOnly);
        
	Py_XDECREF(self->burstsToIgnore);
	Py_XDECREF(self->eventsToIgnore);

	Py_XDECREF(self->startEvent);
	Py_XDECREF(self->NEvents);
	Py_XDECREF(self->NBursts);

	Py_XDECREF(self->analyzers);

	Py_XDECREF(self->noCheckDetectors);

//	if (self->ban != NULL){delete self->ban;}
//	if (self->theApp != NULL){delete self->theApp;}
	
	Py_TYPE(self)->tp_free((PyObject *) self);
}

// PYTHON CONSTRUCTOR
static PyObject * PyBaseAnalysis_new(PyTypeObject *type, PyObject *args, PyObject *kwds){

	PyBaseAnalysis *self;
	self = (PyBaseAnalysis *) type->tp_alloc(type, 0);
	if (self != NULL){
		self->inputFiles = PyList_New(0);
		if (self->inputFiles == NULL){
			Py_DECREF(self);
			return NULL;
		}
		self->extraLibs = PyList_New(0);
		if(self->extraLibs == NULL){
			Py_DECREF(self);
                        return NULL;
		}
		self->extraLibsDirs = PyList_New(0);
                if(self->extraLibsDirs == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->extraIncludedDirs = PyList_New(0);
                if(self->extraIncludedDirs == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->parameters = PyList_New(0);
                if(self->parameters == NULL){
                        Py_DECREF(self);
                        return NULL;
                }


		self->coreVerbosity = PyUnicode_FromString("normal");
                if (self->coreVerbosity == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->anVerbosity = PyUnicode_FromString("normal");
                if (self->anVerbosity == NULL){
                        Py_DECREF(self);
                        return NULL;
                }


		self->logFile = PyUnicode_FromString("");
                if (self->logFile == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->outputFile = PyUnicode_FromString("");
		if (self->outputFile == NULL){
			Py_DECREF(self);
			return NULL;
		}

	
		self->graphicMode = PyBool_FromLong(0);
                if (self->graphicMode == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->useDownscaling = PyBool_FromLong(0);
                if (self->useDownscaling == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->histoMode = PyBool_FromLong(0);
                if (self->histoMode == NULL){
                        Py_DECREF(self);
                        return NULL;
                }	
		self->primitiveFile = PyBool_FromLong(0);
                if (self->primitiveFile == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->fastStart = PyBool_FromLong(0);
                if (self->fastStart == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->skipIsFatal = PyBool_FromLong(0);
                if (self->skipIsFatal == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->continuousReading = PyBool_FromLong(0);
                if (self->continuousReading == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->filter = PyBool_FromLong(0);
                if (self->filter == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->specialOnly = PyBool_FromLong(0);
                if (self->specialOnly == NULL){
                        Py_DECREF(self);
                        return NULL;
                }

		self->burstsToIgnore = PyList_New(0);
		if (self->burstsToIgnore == NULL){
			Py_DECREF(self);
                        return NULL;
		}
		self->eventsToIgnore = PyList_New(0);
		if (self->eventsToIgnore == NULL){
			Py_DECREF(self);
                        return NULL;
		}
              	

		self->startEvent = PyLong_FromLong(0);
		if (self->startEvent == NULL){
			Py_DECREF(self);
			return NULL;
		}		
		self->NEvents = PyLong_FromLong(0);
		if (self->NEvents == NULL){
			Py_DECREF(self);
			return NULL;
		}
		self->NBursts = PyLong_FromLong(0);
                if (self->NBursts == NULL){
                        Py_DECREF(self);
                        return NULL;
                }

		self->analyzers = PyList_New(0);
		if (self->analyzers == NULL){
			Py_DECREF(self);
			return NULL;
		}

		self->noCheckDetectors = PyBool_FromLong(0);
		if (self->noCheckDetectors == NULL){
			Py_DECREF(self);
			return NULL;
		}

		self->noSkipBadBurst = false;
		self->noCheckEvents = false;
	}

	self->ban = new NA62Analysis::Core::BaseAnalysis();

	return (PyObject *) self;

}


// REGISTER PYTHON MEMBERS
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

static string getFileString(PyObject *);
static void Init(NA62Analysis::Core::BaseAnalysis *, string, PyObject *);//, PyObject *, PyObject *, PyObject *);
static int Process(PyObject *, int, int);
static int setGlobalVerbosity(NA62Analysis::Core::BaseAnalysis *, PyObject *, PyObject *);
static string * generateConfigFile(NA62Analysis::Core::BaseAnalysis *);


// METHOD DEFINITIONS
static PyObject * PBAN_configure(PyBaseAnalysis *self, PyObject *Py_UNUSED){

	cout << "\nPBAN_configure called...reading in configuration information." << endl;

	int verbSet = setGlobalVerbosity(self->ban, self->coreVerbosity, self->anVerbosity);
	if (verbSet == VALUE_ERR_AN_VERB){
		PyErr_SetString(PyExc_ValueError, "invalid core verbosity"); 
		return NULL;
	}
	else if (verbSet == VALUE_ERR_CORE_VERB){
		PyErr_SetString(PyExc_ValueError, "invalid analyzer verbosity") ; 
		return NULL;
	}
	else if (verbSet == NULL_VERBOSITY){
		PyErr_SetString(PyExc_ValueError, "error setting verbosities") ; 
		return NULL;
	}

	if (PyUnicode_Check(self->logFile) && PyUnicode_GET_LENGTH(self->logFile) > 0){
		self->ban->SetLogToFile(PyUnicode_AsUTF8(self->logFile));
	}

	self->ban->SetGraphicMode(PyObject_IsTrue(self->graphicMode));	

	self->ban->SetDownscaling(PyObject_IsTrue(self->useDownscaling));

	if(PyObject_IsTrue(self->histoMode)) 
		self->ban->SetReadType(NA62Analysis::Core::IOHandlerType::kHISTO);
        else self->ban->SetReadType(NA62Analysis::Core::IOHandlerType::kTREE);

	if(PyUnicode_Check(self->primitiveFile) && PyUnicode_GET_SIZE(self->primitiveFile) > 0)
		self->ban->InitPrimitives();

	if(PyObject_IsTrue(self->fastStart)){self->ban->SetFastStart(true);}

	if(PyObject_IsTrue(self->skipIsFatal)){self->ban->SetSkipIsFatal(true);}

	if(PyObject_IsTrue(self->continuousReading)){self->ban->SetContinuousReading(true);}

	if(PyObject_IsTrue(self->filter)){
		self->ban->SetFiltering(true);
		self->noSkipBadBurst = true; 
		self->noCheckEvents = true;  
	}

	if(PyObject_IsTrue(self->specialOnly)){self->ban->SetSpecialOnly(true);}

	//self->ban->SetIsPython(true); //<-- TODO: alter baseanalysis class

	if (PyBool_Check(self->noCheckDetectors) && PyObject_IsTrue(self->noCheckDetectors)){
		self->noCheckEvents = true;
	}
	else if (PyList_Check(self->noCheckDetectors) && PyList_Size(self->noCheckDetectors) > 0){
		//noCheckSystems.insert(nameOfSystem); //TODO
	}
	
	/*$$FORCENOBADBURSTDETECTORS$$*/ //<-- TODO
	/*$$FORCEPARAMETERS$$*/ //<-- TODO

	if (PyList_Size(self->inputFiles) > (Py_ssize_t)(0)){
		cout << "Number of input files: " << PyList_Size(self->inputFiles) << endl << endl;
		cout << "calling getFileString..." << endl;
		string inputFileString = getFileString(self->inputFiles);
		cout << "calling ban->AddInputFiles ..." << endl;
		self->ban->AddInputFiles(inputFileString, (int)PyList_Size(self->inputFiles));
	}

	cout << "variable setting done" << endl;
	
	//DEF_ANALYZER is the ClassName of the analyzer. Defined by Makefile target
	/*$$ANALYZERSNEW$$*/ //<-- TODO

	if(self->primitiveFile != NULL && PyUnicode_Check(self->primitiveFile)){
		PyErr_SetString(PyExc_ValueError, "primitive file must be a string path to the file.");
		return NULL;
	}
	else if (self->primitiveFile !=NULL && 
			PyUnicode_Check(self->primitiveFile) &&
			PyUnicode_GET_SIZE(self->primitiveFile) > 0)
		self->ban->SetPrimitiveFile(PyUnicode_AsUTF8(self->primitiveFile));

	TString *nullstr;
	string *configFile = generateConfigFile(self->ban);

	cout << "\ncalling Init()" << endl << endl;

//	self->ban->Init(*PyUnicode_AsUTF8(self->outputFile),
//                  	*PyUnicode_AsUTF8(self->parameters),
//                  	*configFile,
//                  	*nullstr, //TODO: figure out reference file
//                  	true); //TODO: figure out ignoreNonExistingTrees
	int retCode = EXIT_SUCCESS;
//	if(self->continuousReading && PyList_Size(self->inputFiles) == (Py_ssize_t)(0)){
//		cout << "WARNING: there are no input files to read continuously. " << endl;
//	}
//	else if (self->continuousReading){self->ban->StartContinuous(inputFileString);}
//	else{
//		retCode = Process(self->startEvent, (int)(PyLong_AsLong(self->NEvents)), (int)(PyLong_AsLong(self->NBursts))); 
//	}

//	if(self->graphicMode){self->theApp->Run();}

	/*$$ANALYZERSDELETE$$*/ //<-- TODO

	cout << "BaseAnalysis is configured. " << endl;

	return PyBool_FromLong(retCode);

}

static NA62Analysis::Verbosity::CoreVerbosityLevel getGlobalVerb(PyObject *);
static NA62Analysis::Verbosity::AnalyzerVerbosityLevel getAnalyzerVerb(PyObject *);

static int setGlobalVerbosity(NA62Analysis::Core::BaseAnalysis *ban, PyObject *global, PyObject *analyzer){
	
	NA62Analysis::Verbosity::CoreVerbosityLevel core;
	NA62Analysis::Verbosity::AnalyzerVerbosityLevel an;

	if ( !PyUnicode_Check(global) ){
		PyErr_SetString(PyExc_ValueError, "global verbosity must be enetered in string format");
		return VALUE_ERR_CORE_VERB;
	}
	else if ( !PyUnicode_Check(analyzer) ){
		PyErr_SetString(PyExc_ValueError, "analyzer verbosity must be entered in string format");
		return VALUE_ERR_AN_VERB;
	}

	core = getGlobalVerb(global);
	an = getAnalyzerVerb(analyzer);
	if ( core > -1 && an > -1){
		ban->SetGlobalVerbosity(core, an);
		return EXIT_SUCCESS;
	}
	else{
		return NULL_VERBOSITY;
	}
}

static NA62Analysis::Verbosity::CoreVerbosityLevel getGlobalVerb(PyObject *coreStr){
	string input = PyUnicode_AsUTF8(coreStr);

        cout << "Core verbosity output: " << input << endl;

	if ( input == "always"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kAlways;}
	else if ( input == "normal" ) {return NA62Analysis::Verbosity::CoreVerbosityLevel::kNormal;}
	else if (input == "extended"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kExtended ;}
        else if (input == "debug"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kDebug ;}
        else if (input == "trace"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kTrace ;}
        else if (input == "cDisable"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kCDisable ;}
	else{
		cout << "WARNING: your coreVerbosity level is not a valid choice.\n	options are: always, normal, extended, debug, trace, cDisable."<< endl;
		return NA62Analysis::Verbosity::CoreVerbosityLevel::kNormal;
	}
}

static NA62Analysis::Verbosity::AnalyzerVerbosityLevel getAnalyzerVerb(PyObject *anStr){

	string input = PyUnicode_AsUTF8(anStr);
	
	cout << "Analyzer verbosity output: " << input << endl;;

	if (input == "always"){return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserAlways;}
	else if (input == "normal") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserNormal ;}
        else if (input == "user") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUser ;}
        else if (input == "uDisable") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUDisable ;}
	else{
		cout << "WARNING: your anVerbosity level is not a valid choice.\n   	options are: always, normal, user, uDisable." << endl;
                return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserNormal;
	}
	
}

static string getFileString(PyObject *files){

	string inp_file = "";
        for (Py_ssize_t i = 0 ; i < PyList_Size(files) ; ++i){
                PyObject *file = PyList_GetItem(files, i);
                if(!PyUnicode_Check(file)){
                        PyErr_SetString(PyExc_ValueError, "input files must be string paths.");
                        return NULL;
                }
                inp_file += PyUnicode_AsUTF8(file);
	}

	return inp_file;

}

//TODO: write this
static string * generateConfigFile(NA62Analysis::Core::BaseAnalysis *ban){
	string fileName = "hello world";

	return &fileName;

}

//TODO: write these
static void Init(NA62Analysis::Core::BaseAnalysis *ban, string outputFile, PyObject *parameters){//, 
	//		PyObject *configFile, PyObject *referenceFile, PyObject *ignoreNonExistingTrees){
	
	

}
static int Process(PyObject *startEvent, int NEvents, int NBursts){
	//empty ....
	return 0;
}

static PyMethodDef PBAN_methods[] = {
    	{"configure", (PyCFunction) PBAN_configure, METH_NOARGS,
     		"Configure the Base Analysis type based on the inputs from the user"},
    	{NULL}  
};

static PyTypeObject PyBaseAnalysisS = {
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



static PyModuleDef PyBanModule = {

	PyModuleDef_HEAD_INIT,
	.m_name = "PyBaseAnalysis",
	.m_doc = "Python Base Analysis data struct",
	.m_size = -1,

};

PyMODINIT_FUNC PyInit_PyBaseAnalysis(void){

	PyObject *m;
    	if (PyType_Ready(&PyBaseAnalysisS) < 0 ){
		return NULL;
	}

	m = PyModule_Create(&PyBanModule);
	if (m == NULL){
		return NULL;
	}

	Py_INCREF(&PyBaseAnalysisS);
	PyModule_AddObject(m, "PyBaseAnalysis", (PyObject *) &PyBaseAnalysisS);

	return m;
}














