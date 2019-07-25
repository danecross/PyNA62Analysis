/*
 *
 *  Header file for PyAnalyzer.cpp
 *
 * */



#ifndef PY_NA62ANALYSIS
#define PY_NA62ANALYSIS


#include <Python.h>
#include <structmember.h>
#include "UserMethods.hh"

using namespace NA62Analysis;
using namespace Core;

typedef struct {

        PyObject_HEAD

        PyObject *name;

        UserMethods *um;

} PyAnalyzer ;


#endif



