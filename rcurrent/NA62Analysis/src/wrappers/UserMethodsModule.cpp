/*
 * Written by: Dane Cross and Amanda Hoebel
 * 
 *
 * This code defines Python wrapper functions for the UserMethods class
 *
 *
 * */

#include<math.h>
#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include "UserMethods.hh"



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

static PyObject* UserMethods::ExportAllPlot(PyObject* self, PyObject* args)
{
	std::map<TString, TTree*> &trees;
	std::map<TString, void*> &branches)
	fHisto.ExportAllPlot(trees, branches);
	return Py_None;
}

static PyObject* UserMethods::UpdateCanvas(PyObject* self, PyObject* args)
{
	TString canvasName;
	if (!PyArg_ParseTuple(args, "i", &n))
		return NULL;
	return Py_BuildValue("i", fHisto.UpdateCanvas(canvasName));
}


static PyMethodDef UserMethods[] = {

	{"system", UM_test, METH_VARARGS, "execute shell command."},
	{ "ExportAllPlot", ExportAllPlot, METH_NOARGS, "Exports all plots" },
	{ "UpdateCanvas", UpdateCanvas, METH_VARARGS, "Updates canvas" },
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


