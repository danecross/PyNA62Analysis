/*
 * Written by: Dane Cross and Amanda Hoebel
 * 
 *
 * This code defines Python wrapper functions for the UserMethods class
 *
 *
 * */


#include <Python.h>
//#include "../SummerProject/rcurrent/NA62Analysis/include/UserMethods.hh"
#include "../include/UserMethods.hh"

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




static PyMethodDef UserMethods[] = {

	{"system", UM_test, METH_VARARGS, "execute shell command."},
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


