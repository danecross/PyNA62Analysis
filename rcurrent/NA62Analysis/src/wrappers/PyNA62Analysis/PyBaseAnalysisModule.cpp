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


using namespace std;

NA62Analysis::Core::BaseAnalysis *ban = 0;
TApplication *theApp = 0;

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
		if(self->extraLibs = NULL){
			Py_DECREF(self);
                        return NULL;
		}
		self->extraLibsDirs = PyList_New(0);
                if(self->extraLibsDirs = NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->extraIncludedDirs = PyList_New(0);
                if(self->extraIncludedDirs = NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->parameters = PyList_New(0);
                if(self->parameters = NULL){
                        Py_DECREF(self);
                        return NULL;
                }


		self->coreVerbosity = PyBool_FromLong(0);
                if (self->coreVerbosity == NULL){
                        Py_DECREF(self);
                        return NULL;
                }
		self->anVerbosity = PyBool_FromLong(0);
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

	{"input_files", T_OBJECT_EX, offsetof(PyBaseAnalysis, inputFiles), 0,
		"list of input files to use" }, 
	{"pre_analyzers", T_OBJECT_EX, offsetof(PyBaseAnalysis, preAnalyzers), 0,
		"list of pre analyzers to use"}, 
	{"analyzers", T_OBJECT_EX, offsetof(PyBaseAnalysis, analyzers), 0, 
		"list of analyzers to make"}, 
	{"extra_libs", T_OBJECT_EX, offsetof(PyBaseAnalysis,extraLibs), 0, 
		"list of extra libraries to include"}, 
	{"extra_libs_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraLibsDirs), 0, 
                "list of extra library directories"},
        {"extra_include_dirs", T_OBJECT_EX, offsetof(PyBaseAnalysis, extraIncludedDirs), 0, 
                "list of extra include directories"},
	{"parameters", T_OBJECT_EX, offsetof(PyBaseAnalysis, parameters), 0, 
		"list of extra compiling parameters"},
	{"coreVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, coreVerbosity), 0, 
		"coreVerbosity flag"}, 
	{"anVerbosity", T_OBJECT_EX, offsetof(PyBaseAnalysis, anVerbosity), 0,
                "anVerbosity flag"},
	{"useLogFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, useLogFile), 0,
                "useLogFile flag"},
        {"graphicMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, graphicMode), 0,
                "graphicMode flag"},
        {"useDownscaling", T_OBJECT_EX, offsetof(PyBaseAnalysis, useDownscaling), 0,
                "useDownscaling flag"},
        {"histoMode", T_OBJECT_EX, offsetof(PyBaseAnalysis, histoMode), 0,
                "histoMode flag"},
        {"usePrimitiveFile", T_OBJECT_EX, offsetof(PyBaseAnalysis, usePrimitiveFile), 0,
                "usePrimitiveFile flag"},
        {"fastStart", T_OBJECT_EX, offsetof(PyBaseAnalysis, fastStart), 0,
                "fastStart flag"},
        {"skipIsFatal", T_OBJECT_EX, offsetof(PyBaseAnalysis, skipIsFatal), 0,
                "skipIsFatal flag"},
        {"continuousReading", T_OBJECT_EX, offsetof(PyBaseAnalysis, continuousReading), 0,
                "continuousReading flag"},
        {"filter", T_OBJECT_EX, offsetof(PyBaseAnalysis, filter), 0,
                "filter flag"},
        {"specialOnly", T_OBJECT_EX, offsetof(PyBaseAnalysis, specialOnly), 0,
                "specialOnly flag"},
	{"bursts_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, burstsToIgnore), 0, 
		"list of bursts to ignore"}, 
	{"events_to_ignore", T_OBJECT_EX, offsetof(PyBaseAnalysis, eventsToIgnore), 0, 
		"list of events to ignore"}, 
        {NULL}

};

// METHOD DEFINITIONS
static PyObject * PBAN_configure(PyBaseAnalysis *self, PyObject *Py_UNUSED){

	return PyBool_FromLong(0);

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














