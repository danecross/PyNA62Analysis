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

// METHODS USABLE FROM PYTHON MODULE


//DEALLOC
static void PyAnalyzer_dealloc(PyAnalyzer *self){

	Py_XDECREF(self->name);

	if(self->um){delete self->um;}

	Py_TYPE(self)->tp_free((PyObject *) self);

}

// INITIALIZER
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

	return (PyObject *)self;

}

// CUSTOM ANALYZER
static int PyAnalyzer_init(PyAnalyzer *self, PyObject *args, PyObject *kwds){

	static char *kwlist[] = {"name", NULL};
	PyObject *name; PyObject *tmp; 

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "|s", kwlist, &name)){
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



