

#ifndef __WRAPPEROBJ_HH__
#define __WRAPPEROBJ_HH__

using namespace std;

#include "Event.hh"

#include <TChain.h>
#include <TFile.h>
#include "Event.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Persistency.hh"

typedef struct{

        PyObject_HEAD
	
	PyObject *name;

        Event *event;
	TDetectorVEvent *detectorEvent;
	TVector3 *vector3;
	
} WrapperObj ;


static void WO_dealloc(WrapperObj *);
//static PyObject * WO_init(PyTypeObject *, PyObject *, PyObject *);
static PyObject * WO_new(PyTypeObject *, PyObject *, PyObject *);
//static PyObject * newWrapperObject(string *);


static PyMemberDef WOMembers[] = {
        {NULL}
};

static PyMethodDef WOMethods[] = {
        {NULL}
};

static PyTypeObject WrapperObject= {
        PyVarObject_HEAD_INIT(NULL, 0)
        .tp_name = "PyNA62Analysis.WrapperObject",
        .tp_basicsize = sizeof(WrapperObj),
        .tp_itemsize = 0,
        .tp_dealloc = (destructor) WO_dealloc,
        .tp_print = 0,
        .tp_getattr = 0,
        .tp_setattr = 0,
        .tp_as_async = 0,
        .tp_repr = 0,
        .tp_as_number = 0,
        .tp_as_sequence = 0,
        .tp_as_mapping = 0,
        .tp_hash = 0,
        .tp_call = 0,
        .tp_str = 0,
        .tp_getattro = 0,
        .tp_setattro = 0,
        .tp_as_buffer = 0,
        .tp_flags = Py_TPFLAGS_DEFAULT,
        .tp_doc = "General Wrapper for NA62 (or ROOT) object",
        .tp_traverse = 0,
        .tp_clear = 0,
        .tp_richcompare = 0,
        .tp_weaklistoffset = 0,
        .tp_iter = 0, 
        .tp_iternext = 0,
        .tp_methods = WOMethods,
        .tp_members = WOMembers,
        .tp_getset = NULL,
        .tp_base = NULL,
        .tp_dict = NULL,
        .tp_descr_get = NULL,
        .tp_descr_set = NULL,
        .tp_dictoffset = NULL,
        .tp_init = NULL,
        .tp_alloc = NULL,
        .tp_new = WO_new,
        .tp_free = NULL,
        .tp_is_gc = NULL,
        .tp_bases = NULL,
        .tp_mro = NULL,
        .tp_cache = NULL,
        .tp_subclasses = NULL,
        .tp_weaklist = NULL,
        .tp_del = NULL,
        .tp_version_tag = 0,
        .tp_finalize = 0
};



#endif


