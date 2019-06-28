/*
 * Written by: Dane Cross and Amanda Hoebel
 * 
 *
 * This code defines Python wrapper functions for the UserMethods class
 *
 *
 * */


#define PY_SIZE_T_CLEAN
#include <stdio.h>
#include <Python.h>
#include "UserMethods.hh"

/*   OUR FUNCTIONS GO HERE   */





static PyMethodDef UserMethods[] = {

        {NULL, NULL, 0, NULL}

}

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


