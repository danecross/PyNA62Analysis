

/* This module converts the C++ function system(String) into a Python-callable function. 
 *
 * This example was basically ripped off of Python docs: https://docs.python.org/3/extending/extending.html
 *
 * */

#define PY_SIZE_T_CLEAN
#include <stdio.h>
#include <Python.h>

/*  METHOD WRAPPER  */
static PyObject*
spam_system(PyObject *self, PyObject *args){
	const char *command;
	int sts;


	// error handling
	if ( !PyArg_ParseTuple(args, "s", &command) ){
		return NULL; //this is the error code for this function
	}

	// function calling
	sts = system(command);
	return PyLong_FromLong(sts);
}


/*  METHOD TABLE  */
static PyMethodDef SpamMethods[] = {

      //{"python method name", wrapper name defined above, METH_VARARGS (see notes), "description"}

	{"system", spam_system, METH_VARARGS, "execute shell command."}, 
	{NULL, NULL, 0, NULL}

};


/*  MODULE DEFINITION STRUCTURE  */
static struct PyModuleDef spammodule = {

	PyModuleDef_HEAD_INIT, 
	"spam", 
	NULL, 
	-1, 
	SpamMethods

};

PyMODINIT_FUNC
PyInit_spam(void){

	return PyModule_Create(&spammodule);

}





