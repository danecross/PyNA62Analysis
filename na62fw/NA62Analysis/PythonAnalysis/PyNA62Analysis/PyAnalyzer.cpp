/* 
 * PyAnalyzer Module
 *
 * 
 * this module is a wrapper for the analyzer class. 
 *
 * creates a basic UserMethods instance and calls those methods for
 * each analyzer.  
 *
 */
 
#ifndef __PY_ANALYZER__
#define __PY_ANALYZER__

#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include "UserMethods.hh"
#include<iostream>
#include <structmember.h>
#include "include/PyAnalysis.hh"

using namespace std;

static void PyAnalyzer_dealloc(PyAnalyzer *self){

	Py_XDECREF(self->name);

	if(self->um)
		delete self->um;

	Py_TYPE(self)->tp_free((PyObject *) self);

}

static PyObject * PyAnalyzer_new(PyTypeObject *type, PyObject *args, PyObject *kwds){

	PyAnalyzer *self;
	self = (PyAnalyzer *) type->tp_alloc(type, 0);

	if (self != NULL){
		self->name = PyUnicode_FromString("");
		if(self->name == NULL){
			Py_DECREF(self);
			return NULL;
		}
	
	
	}

}

static int PyAnalyzer_init(PyAnalyzer *self, PyObject *args, PyObject *kwds){

	static char *kwlist[] = {"name", NULL};
	PyObject *name; PyObject *tmp;

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "s", kwlist, &name)){
                return -1;
        }

	if (name){
		tmp = self->name;
		Py_INCREF(name);
		self->name = name;
		Py_XDECREF(tmp);
	}

	return 0;
}


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


static PyModuleDef PyAnalysisModule = {

        PyModuleDef_HEAD_INIT,
        .m_name = "PyAnalysis",
        .m_doc = "Python Analysis data struct",
        .m_size = -1,

};


PyMODINIT_FUNC PyInit_PyAnalyzer(void){

        PyObject *m;
        if (PyType_Ready(&PyAnalysisS) < 0 ){
                return NULL;
        }

        m = PyModule_Create(&PyAnalysisModule);
        if (m == NULL){
                return NULL;
        }

        Py_INCREF(&PyAnalysisS);
        PyModule_AddObject(m, "PyAnalysis", (PyObject *) &PyAnalysisS);

        return m;
}


#endif



