
#ifndef __WRAPPEROBJ_CC__
#define __WRAPPEROBJ_CC__

#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include <iostream>
#include <fstream>
#include <structmember.h>


#include "include/WrapperObjectModule.hh"



static PyObject * PAN_getNCandidates(WrapperObj *self, PyObject *args){
	
	int nCand;	

	if (self->detectorEvent == nullptr){
		stringstream n;
		n << "\'" << (string)PyUnicode_AsUTF8(self->name) << "\' has no attribute getNCandidates";
		PyErr_SetString(PyExc_AttributeError, n.str().c_str());
		return NULL;
	}

	if ((string)PyUnicode_AsUTF8(self->name) == "TRecoGigaTrackerEvent")
		nCand = ((TRecoGigaTrackerEvent *)(self->detectorEvent))->GetNCandidates();
	else if ((string)PyUnicode_AsUTF8(self->name) == "TRecoSpectrometerEvent")
		nCand = ((TRecoSpectrometerEvent *)(self->detectorEvent))->GetNCandidates();
	else if ((string)PyUnicode_AsUTF8(self->name) == "TRecoLKrEvent")
		nCand = ((TRecoLKrEvent *)(self->detectorEvent))->GetNCandidates();
	else{
		nCand = 0;
	}

        return PyLong_FromLong(nCand);
}






static void WO_dealloc(WrapperObj *self){

	if (self->event){delete self->event;}
	if (self->detectorEvent) {delete self->detectorEvent;}

	Py_TYPE(self)->tp_free((PyObject *) self);
}

static int WO_init(WrapperObj *self, PyObject *unused, PyObject *notused){
	return EXIT_SUCCESS;
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



