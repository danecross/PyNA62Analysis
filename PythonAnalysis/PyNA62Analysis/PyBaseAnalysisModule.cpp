/*
 *
 *  Written by: Dane Cross
 *
 *  Python BaseAnalysis struct
 *  Keeps track of information to be put into the BaseAnalysis 
 *
 *
 *
 * */



#include <python3.6m/Python.h>
#define PY_SSIZE_T_CLEAN

#include "UserMethods.hh"

#include <iostream>
#include <fstream>
#include <structmember.h>
#include <TApplication.h>
#include "BaseAnalysis.hh"
#include "include/PyBaseAnalysisModule.hh"
#include "PyAnalyzer.cpp"

#include "Verbose.hh"

using namespace std;
using namespace NA62Analysis;


// METHOD DEFINITIONS

static PyObject * PBAN_printInfo(PyBaseAnalysis *self, PyObject *args){
	
	self->ban->PrintInitSummary();

	return Py_None;
}

static PyObject * PBAN_loadEvent(PyBaseAnalysis *self, PyObject *args){

	Long64_t i; bool status;
	if (!PyArg_ParseTuple(args, "L", &i)){
		PyErr_SetString(PyExc_ValueError, "loadEvent takes 1 integer argument.");
                return NULL;
        }

	status = self->ban->GetIOHandler()->LoadEvent(i);

        EventHeader* rawHeader = ((InputTree *)self->ban->GetIOHandler())->GetEventHeaderEvent("Reco");
        UInt_t currentBurstID = rawHeader->GetBurstID();
	cout << "burstID: " << currentBurstID << endl;

	return PyBool_FromLong(status);
}

static int addAnalyzer(NA62Analysis::Core::BaseAnalysis *self, PyObject *newAnalyzer){

	PyAnalyzer *newAn; //const std::string &name; 

	PyTypeObject* type = newAnalyzer->ob_type;
	string typeName = type->tp_name;
	if (typeName != "PyAnalyzer"){
		PyErr_SetString(PyExc_ValueError, "argument to addAnalyzer must be an analyzer.");
		return EXIT_FAILURE;
	}

	newAn = ((PyAnalyzer *)newAnalyzer);
	newAn->um = new UserMethods(self);
	self->AddAnalyzer((NA62Analysis::Analyzer*)(newAn->um));

	return EXIT_SUCCESS;

}

/*
 * from main.cc file that still needs to be done: 
 * 	- fExternalCDBDirectoryPath in main() 
 *	- fConditionsServiceExitLevel in main()
 *	- continuous reading
 *	- graphic mode
 *
 * */
static PyObject * PBAN_configure(PyBaseAnalysis *self, PyObject *args){

	self->ban = new NA62Analysis::Core::BaseAnalysis();
/*
	PyObject *analyzers; PyObject *an;
	if (!PyArg_ParseTuple(args, "O", &analyzers)){
                PyErr_SetString(PyExc_ValueError, "argument to configure must be a list of analyzers.");
                return NULL;
        }

	self->analyzers = PyList_New(0);
	for (Py_ssize_t i = 0 ; i < PyList_Size(analyzers) ; ++i){
		an = PyList_GetItem(analyzers, i);
		if (!addAnalyzer(self->ban, an)){PyErr_Print(); return NULL;}
		self->numAnalyzers += 1;
		PyList_Append(self->analyzers, an);
	}
*/

	self->ban->SetIsPython(true); 

	int verbSet = setGlobalVerbosity(self->ban, self->coreVerbosity, self->anVerbosity);
	if (verbSet == VALUE_ERR_AN_VERB){
		PyErr_SetString(PyExc_ValueError, "invalid core verbosity"); 
		return NULL;
	}
	else if (verbSet == VALUE_ERR_CORE_VERB){
		PyErr_SetString(PyExc_ValueError, "invalid analyzer verbosity") ; 
		return NULL;
	}
	else if (verbSet == NULL_VERBOSITY){
		PyErr_SetString(PyExc_ValueError, "error setting verbosities") ; 
		return NULL;
	}

	if (PyUnicode_Check(self->logFile) && PyUnicode_GET_LENGTH(self->logFile) > 0){
		self->ban->SetLogToFile(PyUnicode_AsUTF8(self->logFile));
	}

	self->ban->SetGraphicMode(PyObject_IsTrue(self->graphicMode));	

	self->ban->SetDownscaling(PyObject_IsTrue(self->useDownscaling));

	if(PyObject_IsTrue(self->histoMode)) 
		self->ban->SetReadType(NA62Analysis::Core::IOHandlerType::kHISTO);
        else{self->ban->SetReadType(NA62Analysis::Core::IOHandlerType::kTREE);}

	if(PyUnicode_Check(self->primitiveFile) && PyUnicode_GET_SIZE(self->primitiveFile) > 0)
		self->ban->InitPrimitives();

	if(PyObject_IsTrue(self->fastStart)){self->ban->SetFastStart(true);}

	if(PyObject_IsTrue(self->skipIsFatal)){self->ban->SetSkipIsFatal(true);}

	if(PyObject_IsTrue(self->continuousReading)){self->ban->SetContinuousReading(true);}

	if(PyObject_IsTrue(self->filter)){
		self->ban->SetFiltering(true);
		self->noSkipBadBurst = true; 
		self->noCheckEvents = true;  
	}

	if(PyObject_IsTrue(self->specialOnly)){self->ban->SetSpecialOnly(true);}


	if (PyBool_Check(self->noCheckDetectors) && PyObject_IsTrue(self->noCheckDetectors)){
		self->noCheckEvents = true;
	}
	else if (PyList_Check(self->noCheckDetectors) && PyList_Size(self->noCheckDetectors) > 0){
		if (addPyItemsToVector(self->noCheckDetectors, self->eventsToIgnore) == PYOBJECT_NOT_STRING){
			PyErr_SetString(PyExc_ValueError , "noCheckDetectors values must be strings");
			return NULL;
		}
	}
	if (PyBool_Check(self->noCheckBadBurst) && PyObject_IsTrue(self->noCheckBadBurst)){
		self->noSkipBadBurst = true;
	}
	else if (PyList_Check(self->noCheckBadBurst) && PyList_Size(self->noCheckBadBurst) > 0){
		if (addPyItemsToVector(self->noCheckBadBurst, self->burstsToIgnore) == PYOBJECT_NOT_STRING){
                        PyErr_SetString(PyExc_ValueError , "noCheckBadBurst values must be strings");
                        return NULL;
                }
	}

	self->parameters = generateParameters(self);

	cout << "here-- adding input files" << endl;
	if (PyList_Size(self->inputFiles) > (Py_ssize_t)(0)){
		string inputFileString = getFileString(self->inputFiles);
		self->ban->AddInputFiles((TString)(inputFileString.c_str()), (int)PyList_Size(self->inputFiles));
	} 

	cout << "INPUT FILES READY " << endl;
	
	if(self->primitiveFile != NULL && PyUnicode_Check(self->primitiveFile)){
		PyErr_SetString(PyExc_ValueError, "primitive file must be a string path to the file.");
		return NULL;
	} else if (self->primitiveFile !=NULL && 
			PyUnicode_Check(self->primitiveFile) &&
			PyUnicode_GET_SIZE(self->primitiveFile) > 0)
		self->ban->SetPrimitiveFile(PyUnicode_AsUTF8(self->primitiveFile));

	string configFile = generateConfigFile(self);

	if (!PyUnicode_Check(self->outputFile) || PyUnicode_GET_LENGTH(self->outputFile) != 0){
		PyErr_SetString(PyExc_ValueError, "must set an output file.");
		return NULL;
	} else if (!PyUnicode_Check(self->parameters)){
		self->parameters = PyUnicode_FromString("");
	}

	cout << "calling init...." << endl;
	self->ban->Init((TString)PyUnicode_AsUTF8(self->outputFile),
                  	(TString)PyUnicode_AsUTF8(self->parameters),
                  	(TString)(configFile.c_str()),
                  	(TString)"dummyRefName", //TODO: figure out reference file
                  	false); //TODO: figure out ignoreNonExistingTrees
	int retCode = EXIT_SUCCESS;

	cout << "init called" << endl;

//	self->startEvent = PyLong_FromLong(ban->GetFirstGoodEvent());
	self->NEvents = PyLong_FromLong(self->ban->GetNEvents());

	return PyBool_FromLong(0);

}

//TODO: make this work for parameters in general
static PyObject * generateParameters(PyBaseAnalysis *self){
		
	return PyUnicode_FromString("dummy=123");
	
}

static int setGlobalVerbosity(NA62Analysis::Core::BaseAnalysis *ban, PyObject *global, PyObject *analyzer){
	
	NA62Analysis::Verbosity::CoreVerbosityLevel core;
	NA62Analysis::Verbosity::AnalyzerVerbosityLevel an;

	if ( !PyUnicode_Check(global) ){
		PyErr_SetString(PyExc_ValueError, "global verbosity must be enetered in string format");
		return VALUE_ERR_CORE_VERB;
	}
	else if ( !PyUnicode_Check(analyzer) ){
		PyErr_SetString(PyExc_ValueError, "analyzer verbosity must be entered in string format");
		return VALUE_ERR_AN_VERB;
	}

	core = getGlobalVerb(global);
	an = getAnalyzerVerb(analyzer);

	if ( core > -1 && an > -1){
		ban->SetGlobalVerbosity(core, an);
		return EXIT_SUCCESS;
	}
	else{
		return NULL_VERBOSITY;
	}
}

static NA62Analysis::Verbosity::CoreVerbosityLevel getGlobalVerb(PyObject *coreStr){
	string input = PyUnicode_AsUTF8(coreStr);

	if ( input == "always"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kAlways;}
	else if ( input == "normal" ) {return NA62Analysis::Verbosity::CoreVerbosityLevel::kNormal;}
	else if (input == "extended"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kExtended ;}
        else if (input == "debug"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kDebug ;}
        else if (input == "trace"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kTrace ;}
        else if (input == "cDisable"){return NA62Analysis::Verbosity::CoreVerbosityLevel::kCDisable ;}
	else{return NA62Analysis::Verbosity::CoreVerbosityLevel::kNormal;}
}

static NA62Analysis::Verbosity::AnalyzerVerbosityLevel getAnalyzerVerb(PyObject *anStr){

	string input = PyUnicode_AsUTF8(anStr);
	
//	cout << BANextended() << "Analyzer verbosity output: " << input << endl;;

	if (input == "always"){return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserAlways;}
	else if (input == "normal") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserNormal ;}
        else if (input == "user") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUser ;}
        else if (input == "uDisable") {return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUDisable ;}
	else{
//		cout << BANextended() << "WARNING: your anVerbosity level is not a valid choice.\n   	options are: always, normal, user, uDisable." << endl;
                return NA62Analysis::Verbosity::AnalyzerVerbosityLevel::kUserNormal;
	}
	
}

static string getFileString(PyObject *files){

	string inp_file = "";
        for (Py_ssize_t i = 0 ; i < PyList_Size(files) ; ++i){
                PyObject *file = PyList_GetItem(files, i);
                if(!PyUnicode_Check(file)){
                        PyErr_SetString(PyExc_ValueError, "input files must be string paths.");
                        return NULL;
                }
                inp_file += PyUnicode_AsUTF8(file);
	}

	return inp_file;

}

static int addPyItemsToVector(PyObject *list, vector<string> *vec){

	PyObject *item;

	for ( Py_ssize_t i = 0 ; i < PyList_Size(list) ; ++i){
		item = PyList_GetItem(list, i);
		if (!PyUnicode_Check(item)){
			return PYOBJECT_NOT_STRING;
		}
		vec->push_back(PyUnicode_AsUTF8(item));
	}

	return EXIT_SUCCESS;
	
}

//TODO: write this
static string  generateConfigFile(PyBaseAnalysis *self){

	ofstream config ("config");

	config << "analyzers =";
	if (!PyList_Check(self->analyzers) || PyList_Size(self->analyzers) == 0){
		PyErr_SetString(PyExc_ValueError, "analyzers must be a non-empty list of analyzer names");
		return NULL;
	} 
	for ( Py_ssize_t i = 0 ; i < PyList_Size(self->analyzers) ; ++i){
		PyObject *item = PyList_GetItem(self->analyzers, i);
		PyObject *name = ((PyAnalyzer *)item)->name;
		
		if (!PyUnicode_Check(name) || name == NULL){
			PyErr_SetString(PyExc_ValueError, "analyzer name must be a string");
			return NULL;
		}
		
		config << " " << (string)(PyUnicode_AsUTF8(name)); 
	}
	config << endl;

	config << "exec = dummyExec" << endl;
	
	config.close();
	
	string configFile = (string)(PyUnicode_AsUTF8(self->currentPath)) + "/config";

	return configFile;

}

static void PyBaseAnalysis_dealloc(PyBaseAnalysis *self){

        Py_XDECREF(self->inputFiles);
	Py_XDECREF(self->currentPath);

        Py_XDECREF(self->extraLibs);
        Py_XDECREF(self->extraLibsDirs);
        Py_XDECREF(self->extraIncludedDirs);
        Py_XDECREF(self->parameters);

        Py_XDECREF(self->coreVerbosity);
        Py_XDECREF(self->anVerbosity);

        Py_XDECREF(self->logFile);
        Py_XDECREF(self->outputFile);
        Py_XDECREF(self->primitiveFile);

        Py_XDECREF(self->graphicMode);
        Py_XDECREF(self->useDownscaling);
        Py_XDECREF(self->histoMode);
        Py_XDECREF(self->fastStart);
        Py_XDECREF(self->skipIsFatal);
        Py_XDECREF(self->continuousReading);
        Py_XDECREF(self->filter);
        Py_XDECREF(self->specialOnly);

        Py_XDECREF(self->startEvent);
        Py_XDECREF(self->NEvents);
        Py_XDECREF(self->NBursts);
	Py_XDECREF(self->maxBurst);

        Py_XDECREF(self->analyzers);

        Py_XDECREF(self->noCheckDetectors);
	Py_XDECREF(self->noCheckBadBurst);

        Py_TYPE(self)->tp_free((PyObject *) self);

}


static int PyBaseAnalysis_init(PyBaseAnalysis *self, PyObject *args, PyObject *kwds){

	if (self != nullptr){	
                self->noSkipBadBurst = false;
                self->noCheckEvents = false;

		self->eventsToIgnore = new vector<string>();
		self->burstsToIgnore = new vector<string>();

		self->numAnalyzers = 0;

		self->ban = (NA62Analysis::Core::BaseAnalysis*)PyMem_Malloc(sizeof((self->ban)));
        }

        return EXIT_SUCCESS;

}

static PyModuleDef PyBanModule = {

	PyModuleDef_HEAD_INIT,
	.m_name = "PyBaseAnalysis",
	.m_doc = "Python Base Analysis data struct",
	.m_size = -1,

};

extern "C" {
PyMODINIT_FUNC PyInit_PyBaseAnalysis(void){

	PyObject *m;
    	if (PyType_Ready(&PyBaseAnalysisS) < 0 ){
		return NULL;
	}

	m = PyModule_Create(&PyBanModule);
	if (m == NULL){
		return NULL;
	}

	Py_INCREF(&PyBaseAnalysisS);
	PyModule_AddObject(m, "PyBaseAnalysis", (PyObject *) &PyBaseAnalysisS);

	return m;
}

}












