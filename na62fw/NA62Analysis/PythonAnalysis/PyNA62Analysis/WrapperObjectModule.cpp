
#ifndef __WRAPPEROBJ_CC__
#define __WRAPPEROBJ_CC__

#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include <iostream>
#include <fstream>
#include <structmember.h>


#include "include/WrapperObjectModule.hh"

static void WO_dealloc(WrapperObj *self){

	if (self->event){delete self->event;}
	if (self->detectorEvent) {delete self->detectorEvent;}

	Py_TYPE(self)->tp_free((PyObject *) self);
}

static PyObject * WO_new(PyTypeObject *type, PyObject *args, PyObject *kwds){

	cout << "new is called" << endl;

	WrapperObj *self;
        self = (WrapperObj *) type->tp_alloc(type, 0);

	if (self != NULL){
		self->name = PyUnicode_FromString("");
		if (self->name == NULL){
			return NULL;
		}
	}

        return (PyObject *)self;

}

static PyModuleDef WrapperObjectModule = {

	PyModuleDef_HEAD_INIT,
        .m_name = "WrapperObject",
        .m_doc = "Wrapper for NA62 objects",
        .m_size = -1,

};



PyMODINIT_FUNC PyInit_WrapperObject(void){

        PyObject *m;
        if (PyType_Ready(&WrapperObject) < 0 ){
                return NULL;
        }

        m = PyModule_Create(&WrapperObjectModule);
        if (m == NULL){
                return NULL;
        }

        Py_INCREF(&WrapperObject);
        PyModule_AddObject(m, "WrapperObject", (PyObject *) &WrapperObject);

        return m;
}




#endif 



