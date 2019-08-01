/*
 *
 *  Header file for PyAnalyzer.cpp
 *
 * */



#ifndef PY_NA62ANALYSIS
#define PY_NA62ANALYSIS

//#include <structmember.h>
#include "UserMethods.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include <TFile.h>
#include "Event.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Persistency.hh"

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
static PyObject * PAN_registerOutput(PyAnalyzer *, PyObject *);
static PyObject * PAN_getEvent(PyAnalyzer *, PyObject *);
static PyObject * PAN_MC_getNParticles(PyAnalyzer *, PyObject *);
static PyObject * PAN_BookHistoArray(PyAnalyzer *, PyObject *);

static string extended(){return "[PyAnalysis     ] ";}

static PyMemberDef PyAnalysisMembers[] = {

        {(char *)"name", T_OBJECT_EX, offsetof(PyAnalyzer, name), 0},
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
	{"registerOutput", (PyCFunction) PAN_registerOutput, METH_VARARGS, 
		""},
	{"getEvent", (PyCFunction) PAN_getEvent, METH_VARARGS, 
		"gets previously requested event from tree"},  
	{"MC_numParticles", (PyCFunction) PAN_MC_getNParticles, METH_NOARGS, 
		"returns number of MC particles"},
	{"bookHistoArray", (PyCFunction) PAN_BookHistoArray, METH_VARARGS, 
		"books histo array"},
	{NULL}

};

static PyTypeObject PyAnalysisS = {
        PyVarObject_HEAD_INIT(0, 0)
        .tp_name = "PyAnalyzer",
        .tp_basicsize = sizeof(PyAnalyzer),
        .tp_itemsize = 0,
        .tp_dealloc = (destructor) PyAnalyzer_dealloc,
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
        .tp_doc = "Analysis object to keep track of analyzer information",
        .tp_traverse = 0,
        .tp_clear = 0,
        .tp_richcompare = 0,
        .tp_weaklistoffset = 0,
        .tp_iter = 0,
        .tp_iternext = 0,
        .tp_methods = PyAnalysis_methods,
        .tp_members = PyAnalysisMembers,
        .tp_getset = 0,
        .tp_base = 0,
        .tp_dict = 0,
        .tp_descr_get = 0,
        .tp_descr_set = 0,
        .tp_dictoffset = 0,
        .tp_init = (initproc) PyAnalyzer_init,
        .tp_alloc = 0,
        .tp_new = PyAnalyzer_new,
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



