/* 
 * PyAnalyzer Module
 *
 * 
 * this module is a wrapper for the analyzer class. 
 *
 * creates a basic UserMethods instance and calls those methods for
 * each analyzer.  
 *
 */
 
#ifndef __PY_ANALYZER__
#define __PY_ANALYZER__

#include <Python.h>
#define PY_SSIZE_T_CLEAN
#include <structmember.h>

#include "UserMethods.hh"
#include "include/PyAnalysis.hh"
#include "PyNA62Event.cpp"

#include "Event.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Persistency.hh"

#include <iostream>
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

// METHODS USABLE FROM PYTHON MODULE
static PyObject * PAN_MC_addParticle(PyAnalyzer *self, PyObject *args){
	int source, type;
	
	if (!PyArg_ParseTuple(args, "ii", &source, &type)){
		PyErr_SetString(PyExc_ValueError, "MC_addParticle has two integer arguments.");
		return NULL;
	}

	int ID = self->fMCSimple.AddParticle(source, type);
	
	return PyLong_FromLong(ID);

}

PyObject * PAN_MC_getEvent(PyAnalyzer *self, PyObject *args){

	PyObject *newEvent = newPyNA62Event(&PyEvent, self->um->GetMCEvent());

	return newEvent;

}

static PyObject * PAN_MC_status(PyAnalyzer *self, PyObject *args){


	if ( self->fMCSimple.fStatus == MCSimple::kEmpty){
		return PyUnicode_FromString("empty");
	} else if (self->fMCSimple.fStatus == MCSimple::kComplete){
		return PyUnicode_FromString("complete");
	} else if (self->fMCSimple.fStatus == MCSimple::kMissing){
		return PyUnicode_FromString("missing");
	}

	return PyUnicode_FromString("");

}

static PyObject * PAN_getOutput(PyAnalyzer *self, PyObject *args){

	UserMethods::OutputState state; const char *name; std::stringstream n;
	PyObject *valueTuple = PyTuple_New(2); string request ;
	
	if (!PyArg_ParseTuple(args, "s", &name)){
                PyErr_SetString(PyExc_ValueError, "getOutput has one string argument.");
                return NULL;
        }

	n << (string)(PyUnicode_AsUTF8(self->name)) << "." << (string)(name);
	request = n.str();

	auto vertex = (self->um->GetOutput(request, state));
	
	if (!state){PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("uninit"));}
	else if (state == UserMethods::OutputState::kOUninit){PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("uninit"));}
	else if (state == UserMethods::OutputState::kOValid) {PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("valid"));}
	else if (state == UserMethods::OutputState::kOInvalid) {PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("invalid"));}
	else {PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("no state type"));}

	//TODO: TVector3 ? should we make a wrapper or is there a way to use PyROOT?
	PyTuple_SetItem(valueTuple, 0, PyLong_FromLong(0));
	return valueTuple;

}

static PyObject * PAN_BookHisto(PyAnalyzer *self, PyObject *args){
	
	const char *type, *yaxis, *title ; float one, two, three, four, five, six;
	if (!PyArg_ParseTuple(args, "s|ssffffff", &type, &yaxis, &title, &one, &two, &three, &four, &five, &six)){
                PyErr_SetString(PyExc_ValueError, "bookHisto requires at least 1 string argument.");
                return NULL;
        }

	if ((string)type == ("TH1I")){self->um->BookHisto(new TH1I(yaxis, title, one, two, three));}
	else if ((string)type == ("TH2I")){self->um->BookHisto(new TH2I(yaxis, title, one, two, three, four, five, six));}
	else if ((string)type == ("TGraph")){self->um->BookHisto(new TGraph()); title = "TGraph";}
	else{
		PyErr_SetString(PyExc_ValueError, "Not a valid Histogram type. Supported types: TH1I, TH2I, TGraph");
		return NULL;
	}

	cout << extended() << "Booked Histogram: " << (string)(title) << endl;

	return PyBool_FromLong(EXIT_SUCCESS);	

}

static PyObject * PAN_requestTree(PyAnalyzer *self, PyObject *args){

	cout << extended() << "requesting tree " ;
	
	const char *name, *type;
	if (!PyArg_ParseTuple(args, "ss", &name, &type)){
                PyErr_SetString(PyExc_ValueError, "requestTree requires 2 string arguments: name and type.");
                return NULL;
        }
	if ((string)type == ("TRecoLKrEvent")){cout << (string)name << "..."; self->um->RequestTree((string)name, new TRecoLKrEvent);}
	else if ((string)type == ("TRecoGigaTrackerEvent")){cout << (string)name << "..."; self->um->RequestTree((string)name, new TRecoGigaTrackerEvent);}
	else if ((string)type == ("TRecoSpectrometerEvent")){cout << (string)name << "..."; self->um->RequestTree((string)name, new TRecoSpectrometerEvent);}
	else {
		PyErr_SetString(PyExc_ValueError, "invalid type request. supported types are: TRecoLKrEvent, TRecoGigaTrackerEvent, TRecoSpectrometerEvent");
		return NULL;
	}

	cout << "success" << endl;
	
	return PyBool_FromLong(EXIT_SUCCESS);

}

static PyObject * PAN_registerOutput(PyAnalyzer *self, PyObject *args){
	
	const char *name, *type;
	if (!PyArg_ParseTuple(args, "ss", &name, &type)){
                PyErr_SetString(PyExc_ValueError, "registerOutput requires 2 string arguments: name and type.");
                return NULL;
        }
//	cout << extended() << "Registering " << (string)name << " of type " << (string)type << endl;
	if ((string)type == ("KinePart")){KinePart obj;	self->um->RegisterOutput((string)name, &obj);}
	else if ((string)type == ("TVector3")){TVector3 obj; self->um->RegisterOutput((string)name, &obj);}
	else{
		PyErr_SetString(PyExc_ValueError, "invalid type request. supported types are: KinePart, TVector");
		return NULL;
	}

	return PyLong_FromLong(EXIT_SUCCESS);
}


//DEALLOC
static void PyAnalyzer_dealloc(PyAnalyzer *self){

	Py_XDECREF(self->name);

	if(self->um != NULL){delete self->um;}
//	if(self->fHisto != NULL){delete self->fHisto;}
//	if(self->fMCSimple != NULL){delete self->fMCSimple;}

	Py_TYPE(self)->tp_free((PyObject *) self);

}

// INITIALIZER
static PyObject * PyAnalyzer_new(PyTypeObject *type, PyObject *args, PyObject *kwds){

	PyAnalyzer *self;
	self = (PyAnalyzer *) type->tp_alloc(type, 0);

	if (self != NULL){
		self->name = PyUnicode_FromString("");
		if(self->name == NULL){
			Py_DECREF(self);
			return NULL;
		}
		
	}

	return (PyObject *)self;

}

// CUSTOM ANALYZER
static int PyAnalyzer_init(PyAnalyzer *self, PyObject *args, PyObject *kwds){

	const char *name; PyObject *tmp; PyObject *pyName;

	if (!PyArg_ParseTuple(args, "s", &name)){
		PyErr_SetString(PyExc_ValueError, "name must be a string");
                return -1;
        }

	pyName = PyUnicode_FromString(name);

	if (name){
		tmp = self->name;
		Py_INCREF(pyName);
		self->name = pyName;
		Py_XDECREF(tmp);
	}

	return 0;
}


static PyModuleDef PyAnalysisModule = {

        PyModuleDef_HEAD_INIT,
        .m_name = "PyAnalysis",
        .m_doc = "Python Analysis data struct",
        .m_size = -1,

};


PyMODINIT_FUNC PyInit_PyAnalyzer(void){

        PyObject *m;
        if (PyType_Ready(&PyAnalysisS) < 0 ){
                return NULL;
        }

        m = PyModule_Create(&PyAnalysisModule);
        if (m == NULL){
                return NULL;
        }

        Py_INCREF(&PyAnalysisS);
        PyModule_AddObject(m, "PyAnalysis", (PyObject *) &PyAnalysisS);

        return m;
}


#endif



