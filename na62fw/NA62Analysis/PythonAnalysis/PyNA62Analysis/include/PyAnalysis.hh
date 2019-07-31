/*
 *
 *  Header file for PyAnalyzer.cpp
 *
 * */



#ifndef PY_NA62ANALYSIS
#define PY_NA62ANALYSIS

//#include <structmember.h>
#include "UserMethods.hh"

using namespace std;
using namespace NA62Analysis;
using namespace Core;

typedef struct {

        PyObject_HEAD

        PyObject *name;

        UserMethods *um;

	MCSimple fMCSimple;
	HistoHandler fHisto;
	
} PyAnalyzer ;


static void PyAnalyzer_dealloc(PyAnalyzer *);
static PyObject * PyAnalyzer_new(PyTypeObject *, PyObject *, PyObject *);
static int PyAnalyzer_init(PyAnalyzer *, PyObject *, PyObject *);

// METHOD DEFINITONS FOR PYTHON USER
static PyObject * PAN_MC_addParticle(PyAnalyzer *, PyObject *);
static PyObject * PAN_MC_status(PyAnalyzer *, PyObject *);
static PyObject * PAN_MC_getEvent(PyAnalyzer *, PyObject *);
static PyObject * PAN_getOutput(PyAnalyzer *, PyObject *);
static PyObject * PAN_BookHisto(PyAnalyzer *, PyObject *);
static PyObject * PAN_requestTree(PyAnalyzer *, PyObject *);

static string extended(){return "[PyAnalysis     ] ";}

static PyMemberDef PyAnalysisMembers[] = {

        {"name", T_OBJECT_EX, offsetof(PyAnalyzer, name), 0},
        {NULL}

};

static PyMethodDef PyAnalysis_methods[] = {

        {"MC_addParticle", (PyCFunction) PAN_MC_addParticle, METH_VARARGS,
		""}, 
	{"MCstatus", (PyCFunction) PAN_MC_status, METH_NOARGS, 
		"retreives the status of the fMCSimple instance in string format"}, 
	{"MC_getEvent", (PyCFunction) PAN_MC_getEvent, METH_NOARGS, 
		"retreives the nearest event"}, 
	{"getOutput", (PyCFunction) PAN_getOutput, METH_VARARGS, 
		""}, 
	{"bookHisto", (PyCFunction) PAN_BookHisto, METH_VARARGS, 
		"books a histogram for output"}, 
	{"requestTree", (PyCFunction) PAN_requestTree, METH_VARARGS, 
		"requests an event tree"}, 
	{NULL}

};

static PyTypeObject PyAnalysisS = {
        PyVarObject_HEAD_INIT(NULL, 0)
        .tp_name = "PyAnalyzer",
        .tp_basicsize = sizeof(PyAnalyzer),
        .tp_itemsize = 0,
        .tp_dealloc = (destructor) PyAnalyzer_dealloc,
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
        .tp_doc = "Analysis object to keep track of analyzer information",
        .tp_traverse = NULL,
        .tp_clear = NULL,
        .tp_richcompare = NULL,
        .tp_weaklistoffset = NULL,
        .tp_iter = NULL,
        .tp_iternext = NULL,
        .tp_methods = PyAnalysis_methods,
        .tp_members = PyAnalysisMembers,
        .tp_getset = NULL,
        .tp_base = NULL,
        .tp_dict = NULL,
        .tp_descr_get = NULL,
        .tp_descr_set = NULL,
        .tp_dictoffset = NULL,
        .tp_init = (initproc) PyAnalyzer_init,
        .tp_alloc = NULL,
        .tp_new = PyAnalyzer_new,
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



