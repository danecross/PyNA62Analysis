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


static PyObject* RequestL0Data(PyObject* self, PyObject* args){
	return UserMethods::RequestL0Data();
}

static PyMethodDef UserMethods[] = {

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


