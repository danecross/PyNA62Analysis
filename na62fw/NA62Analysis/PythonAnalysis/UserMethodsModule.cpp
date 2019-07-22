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

static PyObject* UserMethods::DrawAllPlot(PyObject* self, PyObject* args)
{
	fHisto.DrawAllPlot(fAnalyzerName);
	return Py_None;
}

static PyObject* UserMethods::UpdatePlots(PyObject* self, PyObject* args)
{
	Long64_t evtNbr;
	fHisto.UpdatePlots(evtNbr);
	return Py_None;
}

static PyObject* UserMethods::SaveAllPlots(PyObject* self, PyObject* args)
{
	If (!fDisableSave) {
		fHisto.SaveAllPlots(fAnalyzerName);
	}
	return Py_None;
}

static PyObject* UserMethods::SaveNonEmptyPlots(PyObject* self, PyObject* args)
{
	If (!fDisableSave) {
		fHisto.SaveAllPlots(fAnalyzerName, false);
	}
	return Py_None;
}

static PyObject* UserMethods::SetUpdateInterval(PyObject* self, PyObject* args)
{
	int interval;
	std::cout << extended() << "Setting plot update interval to " << interval << std::endl;
	fHisto.SetUpdateInterval(interval);
	return Py_None;
}

static PyObject* UserMethods::BookCounter(PyObject* self, PyObject* args)
{
	TString cName;
	if (!fParent) return;
	fParent->GetCounterHandler()->BookCounter(fAnalyzerName + TString(".") + cName);
	return Py_None;
}

static PyObject* UserMethods::AddCounterToEventFraction(PyObject* self, PyObject* args)
{
	TString efName, cName;
	if (!fParent) return;
	fParent->GetCounterHandler()->AddCounterToEventFraction(efName, fAnalyzerName + TString(".") + cName);
	return Py_None;
}

static PyObject* UserMethods::NewEventFraction(PyObject* self, PyObject* args)
{
	TString name;
	if (!fParent) return;
	fParent->GetCounterHandler()->NewEventFraction(name);
	return Py_None;
}

static PyObject* UserMethods::DefineSampleSizeCounter(PyObject* self, PyObject* args)
{
	TString efName, cName;
	if (!fParent) return;
	fParent->GetCounterHandler()->DefineSampleSizeCounter(efName, fAnalyzerName + TString(".") + cName);
	return Py_None;
}

static PyObject* UserMethods::SetSignificantDigits(PyObject* self, PyObject* args)
{
	TString efName;
	int v;
	if (!fParent) return;
	fParent->GetCounterHandler()->SetSignificantDigits(efName, v);
	return Py_None;
}

static PyObject* UserMethods::SetCounterValue(PyObject* self, PyObject* args)
{
	TString cName;
	int v;
	if (!fParent) return;
	fParent->GetCounterHandler()->SetCounterValue(fAnalyzerName + TString(".") + cName, v);
	return Py_None;
}

static PyObject* UserMethods::IncrementCounter(PyObject* self, PyObject* args)
{
	TString cName;
	int delta;
	if (!fParent) return;
	fParent->GetCounterHandler()->IncrementCounter(fAnalyzerName + TString(".") + cName, delta);
	return Py_None;
}

static PyObject* UserMethods::DecrementCounter(PyObject* self, PyObject* args)
{
	TString cName;
	int delta;
	if (!fParent) return;
	fParent->GetCounterHandler()->DecrementCounter(fAnalyzerName + TString(".") + cName, delta);
	return Py_None;
}

static PyObject* UserMethods::IncrementCounter(PyObject* self, PyObject* args)
{
	TString cName;
	if (!fParent) return;
	fParent->GetCounterHandler()->IncrementCounter(fAnalyzerName + TString(".") + cName);
	return Py_None;
}

static PyObject* UserMethods::DecrementCounter(PyObject* self, PyObject* args)
{
	TString cName;
	if (!fParent) return;
	fParent->GetCounterHandler()->DecrementCounter(fAnalyzerName + TString(".") + cName);
	return Py_None;
}

static PyObject* UserMethods::GetCounterValue(PyObject* self, PyObject* args)
{
	TString cName;
	if (!fParent) return -1;
	return Py_BuildValue("i", fParent->GetCounterHandler()->DecrementCounter(fAnalyzerName + TString(".") + cName));
}

static PyObject* UserMethods::RegisterOutput(PyObject* self, PyObject* args)
{
	TString name;
	const void* const address;
	if (!fParent) return;
	fParent->RegisterOutput(fAnalyzerName + TString(".") + name, address);
	return Py_None;
}

static PyObject* UserMethods::SetOutputState(PyObject* self, PyObject* args)
{
	TString name;
	OutputState state;
	if (!fParent) return;
	fParent->SetOutputState(fAnalyzerName + TString(".") + name, state);
	return Py_None;
}

static PyObject* UserMethods::GetOutput(PyObject* self, PyObject* args)
{
	TString name;
	OutputState &state;
	if (!fParent) return nullptr;
	return Py_BuildValue("s", fParent->GetOutput(name, state);)
}

static PyObject* UserMethods::GetOutput(PyObject* self, PyObject* args)
{
	TString name;
	if (!fParent) return nullptr;
	OutputState state;
	return Py_BuildValue("s", fParent->GetOutput(name, state);)
}

static PyObject* UserMethods::GetOutputVoid(PyObject* self, PyObject* args)
{
	TString name;
	OutputState &state;
	if (!fParent) return nullptr;
	return Py_BuildValue("s", fParent->GetOutput(name, state);)
}

static PyObject* UserMethods::RequestBeamData(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("Reco", "Beam", "BeamData", new BeamData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestL0Data(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("Reco", "L0TP", "L0TPData", new L0TPData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestHLTData(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree("Reco", "HLT", "HLTEvent", new HLTEvent);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestL1Data(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree("Reco", "L1TP", "L1TPData", new L1TPData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestL2Data(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree("Reco", "L2EB", "L2EBData", new L2EBData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestL0SpecialTrigger(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "L0TP", "L0TPSpecialTrigger", new L0TPSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestL1SpecialTrigger(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "L1TP", "L1TPSpecialTrigger", new L1TPSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestL2SpecialTrigger(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "L2EB", "L2EBSpecialTrigger", new L2EBSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestBeamSpecialTrigger(PyObject* self, PyObject* args)
{
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "Beam", "BeamSpecialTrigger", new BeamSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
	return Py_None;
}

static PyObject* UserMethods::RequestTree(PyObject* self, PyObject* args)
{
	TDetectorVEvent *evt;
	TString outputStage;
	if (!fParent) return;
	if (fParent->IsTreeType()) 
		fParent->GetIOTree()->RequestTree(detectorName, evt, outputstage);
	else {
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
		delete evt;
	}
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
	{ "DrawAllPlot", DrawAllPlot, METH_NOARGS, "Draws all plots" },
	{ "UpdatePlots", UpdatePlots, METH_NOARGS, "Updates plots" },
	{ "SaveAllPlots", SaveAllPlots, METH_NOARGS, "Saves all plots" },
	{ "SaveNonEmptyPlots", SaveNonEmptyPlots, METH_NOARGS, "Saves non-empty plots" },
	{ "SetUpdateInterval", SetUpdateInterval, METH_NOARGS, "Sets update interval" },
	{ "BookCounter", BookCounter, METH_NOARGS, "Book counter" },
	{ "AddCounterToEventFraction", AddCounterToEventFraction, METH_NOARGS, "Adds counter to event fraction" },
	{ "NewEventFraction", NewEventFraction, METH_NOARGS, "Makes new event fraction" },
	{ "DefineSampleSizeCounter", DefineSampleSizeCounter, METH_NOARGS, "Defines sample size counter" },
	{ "SetSignificantDigits", SetSignificantDigits, METH_NOARGS, "Sets significant digits" },
	{ "SetCounterValue", SetCounterValue, METH_NOARGS, "Sets counter value" },
	{ "IncrementCounter", IncrementCounter, METH_NOARGS, "Sets increment counter" },
	{ "DecrementCounter", DecrementCounter, METH_NOARGS, "Sets decrement counter" },
	{ "SetSignificantDigits", SetSignificantDigits, METH_NOARGS, "Sets significant digits" },
	{ "IncrementCounter", IncrementCounter, METH_NOARGS, "Sets increment counter" },
	{ "DecrementCounter", DecrementCounter, METH_NOARGS, "Sets decrement counter" },
	{ "GetCounterValue", GetCounterValue, METH_VARARGS, "Gets counter value" },
	{ "RegisterOutput", RegisterOutput, METH_NOARGS, "Registers output" },
	{ "SetOutputState", SetOutputState, METH_NOARGS, "Sets output state" },
	{ "GetOutput", GetOutput, METH_VARARGS, "Gets output" },
	{ "GetOutput", GetOutput, METH_VARARGS, "Gets output" },
	{ "GetOutputVoid", GetOutputVoid, METH_VARARGS, "Gets output" },
	{ "RequestBeamData", RequestBeamData, METH_NOARGS, "Requests beam data" },
	{ "RequestL0Data", RequestL0Data, METH_NOARGS, "Requests L0 Data" },
	{ "RequestHLTData", RequestHLTData, METH_NOARGS, "Requests HLT Data" },
	{ "RequestL1Data", RequestL1Data, METH_NOARGS, "Requests L1 Data" },
	{ "RequestL2Data", RequestL2Data, METH_NOARGS, "Requests L2 Data" },
	{ "RequestL0SpecialTrigger", RequestL0SpecialTrigger, METH_NOARGS, "Requests L0 special trigger" },
	{ "RequestL1SpecialTrigger", RequestL1SpecialTrigger, METH_NOARGS, "Requests L1 special trigger" },
	{ "RequestL2SpecialTrigger", RequestL2SpecialTrigger, METH_NOARGS, "Requests L2 special trigger" },
	{ "RequestBeamSpecialTrigger", RequestBeamSpecialTrigger, METH_NOARGS, "Requests beam special trigger" },
	{ "RequestTree", RequestTree, METH_NOARGS, "Requests tree" },
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


