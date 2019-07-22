/*
 *
 * Written by: Dane Cross and Amanda Hoebel
 * This code defines Python wrapper functions for the UserMethods class
 *
 *
 * */

#include<math.h>
#include <TApplication.h>
#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include "UserMethods.hh"
#include<iostream>
#include"include/PyBaseAnalysisModule.hh"
using namespace std;
 

NA62Analysis::Core::BaseAnalysis *ban = 0;
TApplication *theApp = 0;

static int initBan(PyObject *);

static PyObject* UM_configure(PyObject *self, PyObject *args){
        
	PyBaseAnalysis *PyBan;
	int status;
	if (!PyArg_ParseTuple(args, "O!", &PyBaseAnalysis, &PyBan)){
		return NULL;
	}
	else if (!PyBan->inputFiles){
		PyErr_SetString(PyExc_AttributeError, "Please set input files.");
		return NULL;
	}

	ban = new NA62Analysis::Core::BaseAnalysis();
	status = initBan(PyBan);

	return PyLong_FromLong(0);
}

static int initBan(PyObject *PyBan){

	if (!PyObject_IsInstance(PyBan, &PyBaseAnalysis)){
		PyErr_SetString(PyExc_AttributeError, "argument must be of type \"PyBaseAnalysis\".");
		return NULL;
	}

	

	return EXIT_SUCCESS;

}


static PyMethodDef UserMethods_configure[] = {

        {"configure", UM_configure, METH_VARARGS, "configures our base analysis object"},
        {NULL, NULL, 0, NULL}

};

static struct PyModuleDef UserMethods_ConfigureModule = {

        PyModuleDef_HEAD_INIT,
        "Configure",
        NULL,
        -1,
        UserMethods_configure

};



PyMODINIT_FUNC PyInit_Configure(void){

        return PyModule_Create(&UserMethods_ConfigureModule);

}










