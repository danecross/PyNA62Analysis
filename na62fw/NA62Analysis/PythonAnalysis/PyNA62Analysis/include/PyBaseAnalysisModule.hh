/*
 *
 *  Header file for PyBaseAnalysisModule.cpp
 *
 * */



#ifndef PYBASEANALYSIS
#define PYBASEANALYSIS


#include <Python.h>
#include <structmember.h>

typedef struct{

        PyObject_HEAD
        PyObject *inputFiles;
        PyObject *preAnalyzers;

	PyObject *coreVerbosity;
	PyObject *anVerbosity;
	PyObject *useLogFile;
	PyObject *graphicMode;
	PyObject *useDownscaling;
	PyObject *histoMode;
	PyObject *usePrimitiveFile;
	PyObject *fastStart;
	PyObject *skipIsFatal;
	PyObject *continuousReading;
	PyObject *filter;
	PyObject *specialOnly;

} PyBaseAnalysis;


#endif



