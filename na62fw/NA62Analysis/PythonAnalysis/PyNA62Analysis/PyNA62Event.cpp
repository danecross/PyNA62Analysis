
#ifndef __PY_NA62EVENT__
#define __PY_NA62EVENT__

#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include <iostream>
#include <fstream>
#include <structmember.h>
#include <TApplication.h>

#include "include/PyNA62Event.hh"


static void PyNA62Event_dealloc(PyNA62Event *self){
	if (self->event){delete self->event;}
	Py_TYPE(self)->tp_free((PyObject *) self);
}


static PyObject * PyNA62Event_init(PyTypeObject *type, PyObject *args, PyObject *kwds){
	
	PyNA62Event *self;

	self = (PyNA62Event *) type->tp_alloc(type, 0);

	if (self != NULL){
		self->event = new Event();
	}
	
	return (PyObject *)self;

}

static PyObject * newPyNA62Event(PyTypeObject *type, Event *event){

	PyNA62Event *self;

        self = (PyNA62Event *) type->tp_alloc(type, 0);
	Py_XINCREF(self);

        if (self != NULL){
		self->event = event;
	}

	return (PyObject *)self;
}

static PyModuleDef PyNA62EventModule = {

        PyModuleDef_HEAD_INIT,
        .m_name = "PyNA62Event",
        .m_doc = "Python Event wrapper",
        .m_size = -1,

};


PyMODINIT_FUNC PyInit_PyNA62Event(void){

	PyObject *m;
	if (PyType_Ready(&PyEvent) < 0){
		return NULL;
	}

	m = PyModule_Create(&PyNA62EventModule);
	if(m == NULL){
		return NULL;
	}

	Py_INCREF(&PyEvent);
	PyModule_AddObject(m, "PyNA62Event", (PyObject *) &PyEvent);

	return m;

}

#endif

