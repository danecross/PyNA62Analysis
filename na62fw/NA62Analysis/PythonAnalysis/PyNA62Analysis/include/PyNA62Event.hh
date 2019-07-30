

#ifndef __PYNA62EVENT_HH__
#define __PYNA62EVENT_HH__

#include "Event.hh"

using namespace std;

typedef struct{

	PyObject_HEAD
	Event *event;

} PyNA62Event ;

static void PyNA62Event_dealloc(PyNA62Event *);

static PyObject * PyNA62Event_init(PyTypeObject *, PyObject *, PyObject *);

static PyObject * newPyNA62Event(PyTypeObject *, Event *);



static PyMemberDef PyEventMembers[] = {
	{NULL}
};

static PyMethodDef PyEventMethods[] = {
	{NULL}
};


static PyTypeObject PyEvent= {
        PyVarObject_HEAD_INIT(NULL, 0)
        .tp_name = "NA62Event",
        .tp_basicsize = sizeof(PyNA62Event),
        .tp_itemsize = 0,
        .tp_dealloc = (destructor) PyNA62Event_dealloc,
        .tp_print = NULL,
        .tp_getattr = NULL,
        .tp_setattr = NULL,
        .tp_as_async = NULL,
        .tp_repr = 0,
        .tp_as_number = 0,
        .tp_as_sequence = 0,
        .tp_as_mapping = 0,
        .tp_hash = 0,
        .tp_call = NULL,
        .tp_str = 0,
        .tp_getattro = NULL,
        .tp_setattro = NULL,
        .tp_as_buffer = 0,
        .tp_flags = Py_TPFLAGS_DEFAULT,
        .tp_doc = "BaseAnalysis object to keep track of configuration information",
        .tp_traverse = NULL,
        .tp_clear = NULL,
        .tp_richcompare = NULL,
        .tp_weaklistoffset = NULL,
        .tp_iter = NULL,
        .tp_iternext = NULL,
        .tp_methods = PyEventMethods,
        .tp_members = PyEventMembers,
        .tp_getset = NULL,
        .tp_base = NULL,
        .tp_dict = NULL,
        .tp_descr_get = NULL,
        .tp_descr_set = NULL,
        .tp_dictoffset = NULL,
        .tp_init = (initproc) PyNA62Event_init,
        .tp_alloc = NULL,
        .tp_new = NULL,
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


