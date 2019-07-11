/*
 * Written by: Dane Cross and Amanda Hoebel
 * 
 *
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
//#include"UserMethodsModule.hh"
using namespace std;

/*   OUR FUNCTIONS GO HERE   */

static PyObject* UM_test(PyObject *self, PyObject *args){
        const char *command;
        int sts;
        if ( !PyArg_ParseTuple(args, "s", &command) ){
                return NULL; 
        }
        sts = system(command);
        return PyLong_FromLong(sts);
}

//static NA62Analysis::Core::BaseAnalysis *ban = 0;
//static TApplication *theApp = 0;

static PyObject* UM_setInputFiles(PyObject *self, PyObject *args){
	
	PyListObject *inputFiles;

	if ( !PyArg_ParseTuple(args, "O!", &PyList_Type, &inputFiles)){
		
		PyErr_SetString(PyExc_TypeError, "input files must be in list format.");
    		return NULL;

	}

	Py_ssize_t nInputFiles = PyList_Size((PyObject*)(inputFiles));

	if (nInputFiles <= 0){
		PySys_WriteStdout("\n\n");
		PySys_WriteStdout((char *)nInputFiles);
		PySys_WriteStdout("\n\n");
		PyErr_Format(PyExc_ValueError, "Length of inputFiles must be greater than 0.");
               	return NULL;
       	}
	//TODO: set the input files
	return PyLong_FromLong(0);	
}

static PyObject* UM_setPreAnalyzers(PyObject *self, PyObject *args){

	PyListObject *preAnalyzers;

	if ( !PyArg_ParseTuple(args, "O!", &PyList_Type, &preAnalyzers)){

                PyErr_SetString(PyExc_TypeError, "pre-analyzers must be in list format.");
                return NULL;

        }

	//TODO: set preanalyzers 

	return PyLong_FromLong(0);
}

//TODO: figure this out. 
static PyObject* UM_configure(PyObject *self, PyObject *args){
	return PyLong_FromLong(0);	
}

static PyObject* RequestL0Data(PyObject* self, PyObject* args){
	return RequestL0Data();
}

static PyMethodDef UserMethods[] = {

	{"system", UM_test, METH_VARARGS, "execute shell command."},
	{"configure", UM_configure, METH_NOARGS, "configures our base analysis object"}, 
	{"set_input_files", UM_setInputFiles, METH_VARARGS, "add input files to base analysis object"},
	{"set_preAnalyzers", UM_setPreAnalyzers, METH_VARARGS, "add pre analyzers to the base analysis object"},
	{"RequestL0Data", RequestL0Data, METH_NOARGS, "Calls function 'RequestL0Data' "},
        {NULL, NULL, 0, NULL}

};

static struct PyModuleDef UserMethodsModule = {

        PyModuleDef_HEAD_INIT,
        "UserMethods",
        NULL,
        -1,
        UserMethods

};



PyMODINIT_FUNC PyInit_UserMethods(void){

	return PyModule_Create(&UserMethodsModule);

}


