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

using namespace std;

typedef struct{

        PyObject_HEAD
        PyObject *inputFiles;
        PyObject *preAnalyzers;
	PyObject *analyzers;

	PyObject *extraLibs;
	PyObject *extraLibsDirs;
	PyObject *extraIncludedDirs;
	PyObject *parameters;

        PyObject *coreVerbosity;
        PyObject *anVerbosity;

        PyObject *useLogFile;
        PyObject *graphicMode;
        PyObject *useDownscaling;
        PyObject *histoMode;
        PyObject *usePrimitiveFile;
        PyObject *fastStart;
        PyObject *skipIsFatal;
        PyObject *continuousReading;
        PyObject *filter;
        PyObject *specialOnly;

	PyObject *burstsToIgnore;
	PyObject *eventsToIgnore;

	NA62Analysis::Core::BaseAnalysis *ban = 0; 	// maintain reference to C++ base analysis object after configuration
	TApplication *theApp = 0;		   	// maintain reference to App for visual implementations

} PyBaseAnalysis;

// PYTHON DESTRUCTOR
static void PyBaseAnalysis_dealloc(PyBaseAnalysis *self){

	Py_XDECREF(self->inputFiles);
	Py_XDECREF(self->preAnalyzers);
	Py_XDECREF(self->analyzers);

	Py_XDECREF(self->extraLibs);
        Py_XDECREF(self->extraLibsDirs);
        Py_XDECREF(self->extraIncludedDirs);
        Py_XDECREF(self->parameters);

        Py_XDECREF(self->coreVerbosity);
        Py_XDECREF(self->anVerbosity);

        Py_XDECREF(self->useLogFile);
        Py_XDECREF(self->graphicMode);
        Py_XDECREF(self->useDownscaling);
        Py_XDECREF(self->histoMode);
        Py_XDECREF(self->usePrimitiveFile);
        Py_XDECREF(self->fastStart);
        Py_XDECREF(self->skipIsFatal);
        Py_XDECREF(self->continuousReading);
        Py_XDECREF(self->filter);
        Py_XDECREF(self->specialOnly);
        
	Py_XDECREF(self->burstsToIgnore);
	Py_XDECREF(self->eventsToIgnore);

//	delete ban;
//	delete theApp;

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
		self->preAnalyzers = PyList_New(0);
		if (self->preAnalyzers == NULL){
			Py_DECREF(self);
			return NULL;
		}
		self->analyzers = PyList_New(0);
		if (self->analyzers == NULL){
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


		self->coreVerbosity = PyUnicode_FromString("");
                if (self->coreVerbosity == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->anVerbosity = PyUnicode_FromString("");
                if (self->anVerbosity == NULL){
                        Py_DECREF(self);
                        return NULL;
                }


		self->useLogFile = PyBool_FromLong(0);
                if (self->useLogFile == NULL){
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
		self->usePrimitiveFile = PyBool_FromLong(0);
                if (self->usePrimitiveFile == NULL){
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
              
	}
	return (PyObject *) self;

}


// REGISTER PYTHON MEMBERS
static PyMemberDef PyBanMembers[] = {

	{"input_files", T_OBJECT_EX, offsetof(PyBaseAnalysis, inputFiles), 0}, 
	{"pre_analyzers", T_OBJECT_EX, offsetof(PyBaseAnalysis, preAnalyzers), 0}, 
	{"analyzers", T_OBJECT_EX, offsetof(PyBaseAnalysis, analyzers), 0} ,
	{"extra_libs", T_OBJECT_EX, offsetof(PyBaseAnalysis,extraLibs), 0 }, 
	{"extra_libs_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraLibsDirs), 0}, 
        {"extra_include_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraIncludedDirs), 0} , 
	{"parameters", T_OBJECT_EX, offsetof(PyBaseAnalysis, parameters), 0} , 
	{"coreVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, coreVerbosity), 0}, 
	{"anVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, anVerbosity), 0}, 
	{"useLogFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, useLogFile), 0}, 
        {"graphicMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, graphicMode), 0}, 
        {"useDownscaling", T_OBJECT_EX, offsetof(PyBaseAnalysis, useDownscaling), 0}, 
        {"histoMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, histoMode), 0}, 
        {"usePrimitiveFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, usePrimitiveFile), 0}, 
        {"fastStart", T_OBJECT_EX, offsetof(PyBaseAnalysis, fastStart), 0}, 
        {"skipIsFatal", T_OBJECT_EX, offsetof(PyBaseAnalysis, skipIsFatal), 0}, 
        {"continuousReading", T_OBJECT_EX, offsetof(PyBaseAnalysis, continuousReading), 0}, 
        {"filter", T_OBJECT_EX, offsetof(PyBaseAnalysis, filter), 0}, 
        {"specialOnly", T_OBJECT_EX, offsetof(PyBaseAnalysis, specialOnly), 0}, 
	{"bursts_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, burstsToIgnore), 0}, 
	{"events_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, eventsToIgnore), 0}, 
        {NULL}

};

static string getFileString(PyObject *);
static void Init(NA62Analysis::Core::BaseAnalysis *, PyObject *, PyObject *, PyObject *, PyObject *, PyObject *);
static int Process(PyObject *, int, int);
static int setGlobalVerbosity(NA62Analysis::Core::BaseAnalysis *, PyObject *, PyObject *);

// METHOD DEFINITIONS
static PyObject * PBAN_configure(PyBaseAnalysis *self, PyObject *Py_UNUSED){

	self->ban = new NA62Analysis::Core::BaseAnalysis();
	if (!setGlobalVerbosity(self->ban, self->coreVerbosity, self->anVerbosity)){return NULL;}
//	fLogFile <== TODO
	self->ban->SetGraphicMode(PyObject_IsTrue(self->graphicMode));	
	self->ban->SetDownscaling(PyObject_IsTrue(self->useDownscaling));
	if(PyObject_IsTrue(self->histoMode)) 
		self->ban->SetReadType(NA62Analysis::Core::IOHandlerType::kHISTO);
        else self->ban->SetReadType(NA62Analysis::Core::IOHandlerType::kTREE);
	if(PyObject_IsTrue(self->usePrimitiveFile)){self->ban->InitPrimitives();}
	if(PyObject_IsTrue(self->fastStart)){self->ban->SetFastStart(true);}
	if(PyObject_IsTrue(self->skipIsFatal)){self->ban->SetSkipIsFatal(true);}
	if(PyObject_IsTrue(self->continuousReading)){self->ban->SetContinuousReading(true);}
	if(PyObject_IsTrue(self->filter)){
		self->ban->SetFiltering(true);
		//fNoSkipBadBurst = true; <-- TODO
		//fNoCheckEvents = true;  <-- TODO
	}
	if(PyObject_IsTrue(self->specialOnly)){self->ban->SetSpecialOnly(true);}

	/*$$FORCENOCHECKDETECTORS$$*/ //<-- TODO
	/*$$FORCENOBADBURSTDETECTORS$$*/ //<-- TODO
	/*$$FORCEPARAMETERS$$*/ //<-- TODO

	string inputFileString = getFileString(self->inputFiles);
	self->ban->AddInputFiles(inputFileString, (int)PyList_Size(self->inputFiles));
	
	//DEF_ANALYZER is the ClassName of the analyzer. Defined by Makefile target
	/*$$ANALYZERSNEW$$*/ //<-- TODO

	if(PyObject_IsTrue(self->usePrimitiveFile)){/*//primitiveFile ----> new entry in ban*/}
/*	Init(self->ban, getFileString(self->outputFile), 	//TODO: add outputFile to the parameters
			self->parameters, 		//TODO: parameters: what should it be?
			self->configFile, 		//TODO: add config file
			self->referenceFile, 		//TODO: add reference file
			self->ignoreNonExistingTrees);	//TODO: add ignoreNonExistingTrees
*/	int retCode = 0;
//	if(self->continuousReading){self->ban->startContinuous(inputFileString);}
//	else{
//		retCode = Process(startEvent, NEvents, NBursts); // TODO: what is NEvents and NBursts
//	}

	if(self->graphicMode){self->theApp->Run();}

	/*$$ANALYZERSDELETE$$*/ //<-- TODO

	return PyBool_FromLong(retCode);

}

static NA62Analysis::Verbosity::CoreVerbosityLevel getGlobalVerb(PyObject *);
static NA62Analysis::Verbosity::AnalyzerVerbosityLevel getAnalyzerVerb(PyObject *);

static int setGlobalVerbosity(NA62Analysis::Core::BaseAnalysis *ban, PyObject *global, PyObject *analyzer){
	
	NA62Analysis::Verbosity::CoreVerbosityLevel core;
	NA62Analysis::Verbosity::AnalyzerVerbosityLevel an;

	if ( !PyUnicode_Check(global) ){
		PyErr_SetString(PyExc_ValueError, "global verbosity must be enetered in string format");
		return 0;
	}
	else if ( !PyUnicode_Check(analyzer) ){
		PyErr_SetString(PyExc_ValueError, "analyzer verbosity must be entered in string format");
		return 0;
	}

	core = getGlobalVerb(global);
	an = getAnalyzerVerb(analyzer);
	if ( core != NULL && an != NULL){
		ban->SetGlobalVerbosity(core, an);
		return 1;
	}
	else{
		return 0;
	}
}

static NA62Analysis::Verbosity::CoreVerbosityLevel getGlobalVerb(PyObject *coreStr){
	string input = PyUnicode_AsUTF8(coreStr);

	if ( input == "always"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kAlways;}
	else if ( input == "normal" ) {return NA62Analysis::Verbosity::CoreVerbosityLevel::kNormal;}
	else if (input == "extended"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kExtended ;}
        else if (input == "debug"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kDebug ;}
        else if (input == "trace"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kTrace ;}
        else if (input == "cDisable"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kCDisable ;}
	else{
		PyErr_SetString(PyExc_ValueError, "your coreVerbosity is not a valid input. choices are: always, normal, extended, debug, trace, cDisable.");
		return NA62Analysis::Verbosity::CoreVerbosityLevel::kNormal; //TODO: figure out how to throw an error value here
	}
}

static NA62Analysis::Verbosity::AnalyzerVerbosityLevel getAnalyzerVerb(PyObject *anStr){

	string input = PyUnicode_AsUTF8(anStr);
	
	if (input == "always"){return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserAlways;}
	else if (input == "normal") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserNormal ;}
        else if (input == "user") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUser ;}
        else if (input == "uDisable") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUDisable ;}
	else{
		PyErr_SetString(PyExc_ValueError, "your anVerbosity is not a valid input. choices are: always, normal, user, uDisable.");
                return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserNormal; //TODO: figure out how to throw an error value here
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

//TODO: write these
static void Init(NA62Analysis::Core::BaseAnalysis *ban, PyObject *outputFile, PyObject *parameters, 
			PyObject *configFile, PyObject *referenceFile, PyObject *ignoreNonExistingTrees){
	//empty ....
}

static int Process(PyObject startEvent, int NEvents, int NBursts){
	//empty ....
	return 0;
}

static PyMethodDef PBAN_methods[] = {
    	{"configure", (PyCFunction) PBAN_configure, METH_NOARGS,
     		"Configure the Base Analysis type based on the inputs from the user"
    	},
    	{NULL}  
};

static PyTypeObject PyBaseAnalysisS = {
	PyVarObject_HEAD_INIT(NULL, 0)
    	.tp_name = "Configure.BaseAnalysis",
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














