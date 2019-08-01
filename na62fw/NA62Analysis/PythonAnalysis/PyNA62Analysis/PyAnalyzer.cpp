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
#include "WrapperObjectModule.cpp"

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

	WrapperObj *newEvent = PyObject_New(WrapperObj, &WrapperObject);
	Py_INCREF(newEvent);
	newEvent->event = (Event *)self->um->GetMCEvent();
	return (PyObject *)newEvent;

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

	UserMethods::OutputState state; const char *name; WrapperObj *v;
	PyObject *valueTuple = PyTuple_New(2); string request ;
	
	if (!PyArg_ParseTuple(args, "s", &name)){
                PyErr_SetString(PyExc_ValueError, "getOutput has one string argument.");
                return NULL;
        }

	stringstream n;
	n << (string)PyUnicode_AsUTF8(self->name) << "." << (string)name;

	cout << extended() << "getting output: " << n.str() << "...";
	
	TVector3 *vertex = (TVector3*)(self->um->GetOutput(n.str().c_str(), state));
	
	if (!state){PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("uninit"));}
	else if (state == UserMethods::OutputState::kOUninit){PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("uninit"));}
	else if (state == UserMethods::OutputState::kOValid) {PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("valid"));}
	else if (state == UserMethods::OutputState::kOInvalid) {PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("invalid"));}
	else {PyTuple_SetItem(valueTuple, 1, PyUnicode_FromString("no state type"));}

	if (!vertex){
		cout << "not found." << endl;
		PyTuple_SetItem(valueTuple, 0, PyBool_FromLong(0));
	}
	else {
		v = PyObject_New(WrapperObj, &WrapperObject);
		Py_INCREF(v);
		v->vector3 = vertex;
		PyTuple_SetItem(valueTuple, 0, (PyObject *)v);
		cout << "success" << endl;
	}

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

static PyObject * PAN_BookHistoArray(PyAnalyzer *self, PyObject *args){

	const char *type, *xaxis, *yaxis ; float one, two, three, four, five, six, seven;
	if (!PyArg_ParseTuple(args, "sss|fffffff", &type, &xaxis, &yaxis, &one, &two, &three, &four, &five, &six, &seven)){
                PyErr_SetString(PyExc_ValueError, "bookHistoArray requires at 3 string arguments");
                return NULL;
        }

	if((string)type == ("TH2I")){self->um->BookHistoArray(new TH2I(xaxis, yaxis, one, two, three, four, five, six), seven);}
	else{
		PyErr_SetString(PyExc_ValueError, "invlaid type. Supported types: TH2I");
		return NULL;
	}

	cout << extended() << "Booked Histogram Array: " << (string)(xaxis) << endl;

	return PyBool_FromLong(EXIT_SUCCESS);
}

static PyObject * PAN_requestTree(PyAnalyzer *self, PyObject *args){

	const char *name, *type;
	if (!PyArg_ParseTuple(args, "ss", &name, &type)){
                PyErr_SetString(PyExc_ValueError, "requestTree requires 2 string arguments: name and type.");
                return NULL;
        }

	if ((string)type == ("TRecoLKrEvent")){self->um->RequestTree((string)name, new TRecoLKrEvent);}
	else if ((string)type == ("TRecoGigaTrackerEvent")){self->um->RequestTree((string)name, new TRecoGigaTrackerEvent);}
	else if ((string)type == ("TRecoSpectrometerEvent")){self->um->RequestTree((string)name, new TRecoSpectrometerEvent);}
	else if ((string)type == ("TRecoCedarEvent")){self->um->RequestTree((string)name, new TRecoCedarEvent);}
	else if ((string)type == ("TRecoCHANTIEvent")){self->um->RequestTree((string)name, new TRecoCHANTIEvent);}
        else if ((string)type == ("TLAVEvent")){self->um->RequestTree((string)name, new TLAVEvent);}
        else if ((string)type == ("TMUV0Event")){self->um->RequestTree((string)name, new TMUV0Event);}
        else if ((string)type == ("TMUV1Event")){self->um->RequestTree((string)name, new TMUV1Event);}
        else if ((string)type == ("TMUV2Event")){self->um->RequestTree((string)name, new TMUV2Event);}
        else if ((string)type == ("TMUV3Event")){self->um->RequestTree((string)name, new TMUV3Event);}
        else if ((string)type == ("TRICHEvent")){self->um->RequestTree((string)name, new TRICHEvent);}
        else if ((string)type == ("TSpectrometerEvent")){self->um->RequestTree((string)name, new TSpectrometerEvent);}
        else if ((string)type == ("TSACEvent")){self->um->RequestTree((string)name, new TSACEvent);}
        else if ((string)type == ("THACEvent")){self->um->RequestTree((string)name, new THACEvent);}
        else if ((string)type == ("TCHODEvent")){self->um->RequestTree((string)name, new TCHODEvent);}
        else if ((string)type == ("TRecoIRCEvent")){self->um->RequestTree((string)name, new TRecoIRCEvent);}
        else if ((string)type == ("TRecoSAVEvent")){self->um->RequestTree((string)name, new TRecoSAVEvent);}
	else {
		PyErr_SetString(PyExc_ValueError, "invalid type request. supported types are:\n\tTRecoLKrEvent, TRecoGigaTrackerEvent, TRecoSpectrometerEvent, \n\tTRecoCedarEvent, TRecoCHANTIEvent, TLAVEvent, TMUV0Event, \n\tTMUV1Event, TMUV2Event, TMUV3Event, TRICHEvent, \n\tTSpectrometerEvent, TSACEvent, THACEvent, TCHODEvent, \n\tTRecoIRCEvent, TRecoSAVEvent");
		return NULL;
	}

	
	
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

static PyObject * PAN_getEvent(PyAnalyzer *self, PyObject *args){

	const char *type; TDetectorVEvent *event; WrapperObj *theObj;
	if (!PyArg_ParseTuple(args, "s", &type)){
                PyErr_SetString(PyExc_ValueError, "getEvent requires 1 string argument: type.");
                return NULL;
        }
	cout << extended() << "Getting event " << ((string)type) << endl;


	if ((string)type == ("TRecoGigaTrackerEvent")){event = (TRecoGigaTrackerEvent *)self->um->GetEvent<TRecoGigaTrackerEvent>();}
	else if ((string)type == ("TRecoSpectrometerEvent")){event = (TRecoSpectrometerEvent *)self->um->GetEvent<TRecoSpectrometerEvent>();}
	else if ((string)type == ("TRecoLKrEvent")){event = (TRecoLKrEvent *)self->um->GetEvent<TRecoLKrEvent>();}
	else{
		event = nullptr;
		cout << "not in events list..." << endl;
	}

	if (event != nullptr){
                theObj = PyObject_New(WrapperObj, &WrapperObject);
		Py_INCREF(theObj);
		((WrapperObj *)theObj)->detectorEvent = event;
		((WrapperObj *)theObj)->name = PyUnicode_FromString((char *)type);
		return (PyObject *)theObj;
        }
	else {
		cout << "no such event found" << endl;
	}

	return PyBool_FromLong(0);

}

static PyObject * PAN_bookCounter(PyAnalyzer *self, PyObject *args){
	
	const char *name;
	if (!PyArg_ParseTuple(args, "s", &name)){
                PyErr_SetString(PyExc_ValueError, "bookCounter requires 1 string argument: name.");
                return NULL;
        }
	
	cout << extended() << "Booking counter: " << (string)name << endl;

	self->um->BookCounter((string)name);

	return PyLong_FromLong(0);
}

static PyObject * PAN_MC_getNParticles(PyAnalyzer *self, PyObject *args){	
	
	return PyLong_FromLong(0);
}

//DEALLOC
static void PyAnalyzer_dealloc(PyAnalyzer *self){

	Py_XDECREF(self->name);

	if(self->um != NULL){delete self->um;}

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



