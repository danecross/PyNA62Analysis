/*
 *
 *  Header file for PyAnalyzer.cpp
 *
 * */



#ifndef PY_NA62ANALYSIS
#define PY_NA62ANALYSIS


#include <Python.h>
#include <structmember.h>
#include "UserMethods.hh"

using namespace NA62Analysis;
using namespace Core;

typedef struct {

        PyObject_HEAD

        PyObject *name;

        UserMethods *um;

} PyAnalyzer ;


static void PyAnalyzer_dealloc(PyAnalyzer *);
static PyObject * PyAnalyzer_new(PyTypeObject *, PyObject *, PyObject *);
static int PyAnalyzer_init(PyAnalyzer *, PyObject *, PyObject *);


static PyMemberDef PyAnalysisMembers[] = {

        {"name", T_OBJECT_EX, offsetof(PyAnalyzer, name), 0},
        {NULL}

};

static PyMethodDef PyAnalysis_methods[] = {

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



