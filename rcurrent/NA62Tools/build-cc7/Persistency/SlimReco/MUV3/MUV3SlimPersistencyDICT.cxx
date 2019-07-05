// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV3SlimPersistencyDICT

/*******************************************************************/
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#define G__DICTIONARY
#include "RConfig.h"
#include "TClass.h"
#include "TDictAttributeMap.h"
#include "TInterpreter.h"
#include "TROOT.h"
#include "TBuffer.h"
#include "TMemberInspector.h"
#include "TInterpreter.h"
#include "TVirtualMutex.h"
#include "TError.h"

#ifndef G__ROOT
#define G__ROOT
#endif

#include "RtypesImp.h"
#include "TIsAProxy.h"
#include "TFileMergeInfo.h"
#include <algorithm>
#include "TCollectionProxyInfo.h"
/*******************************************************************/

#include "TDataMember.h"

// Since CINT ignores the std namespace, we need to do so in this file.
namespace std {} using namespace std;

// Header files passed as explicit arguments
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3/include/TSlimRecoMUV3Candidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3/include/TSlimRecoMUV3Event.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3/include/TSlimRecoMUV3Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoMUV3Candidate(void *p = 0);
   static void *newArray_TSlimRecoMUV3Candidate(Long_t size, void *p);
   static void delete_TSlimRecoMUV3Candidate(void *p);
   static void deleteArray_TSlimRecoMUV3Candidate(void *p);
   static void destruct_TSlimRecoMUV3Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV3Candidate*)
   {
      ::TSlimRecoMUV3Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV3Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV3Candidate", ::TSlimRecoMUV3Candidate::Class_Version(), "", 24,
                  typeid(::TSlimRecoMUV3Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV3Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV3Candidate) );
      instance.SetNew(&new_TSlimRecoMUV3Candidate);
      instance.SetNewArray(&newArray_TSlimRecoMUV3Candidate);
      instance.SetDelete(&delete_TSlimRecoMUV3Candidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV3Candidate);
      instance.SetDestructor(&destruct_TSlimRecoMUV3Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV3Candidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV3Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV3Candidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV3Event(void *p = 0);
   static void *newArray_TSlimRecoMUV3Event(Long_t size, void *p);
   static void delete_TSlimRecoMUV3Event(void *p);
   static void deleteArray_TSlimRecoMUV3Event(void *p);
   static void destruct_TSlimRecoMUV3Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV3Event*)
   {
      ::TSlimRecoMUV3Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV3Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV3Event", ::TSlimRecoMUV3Event::Class_Version(), "", 95,
                  typeid(::TSlimRecoMUV3Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV3Event::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV3Event) );
      instance.SetNew(&new_TSlimRecoMUV3Event);
      instance.SetNewArray(&newArray_TSlimRecoMUV3Event);
      instance.SetDelete(&delete_TSlimRecoMUV3Event);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV3Event);
      instance.SetDestructor(&destruct_TSlimRecoMUV3Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV3Event*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV3Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV3Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV3Hit(void *p = 0);
   static void *newArray_TSlimRecoMUV3Hit(Long_t size, void *p);
   static void delete_TSlimRecoMUV3Hit(void *p);
   static void deleteArray_TSlimRecoMUV3Hit(void *p);
   static void destruct_TSlimRecoMUV3Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV3Hit*)
   {
      ::TSlimRecoMUV3Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV3Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV3Hit", ::TSlimRecoMUV3Hit::Class_Version(), "", 136,
                  typeid(::TSlimRecoMUV3Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV3Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV3Hit) );
      instance.SetNew(&new_TSlimRecoMUV3Hit);
      instance.SetNewArray(&newArray_TSlimRecoMUV3Hit);
      instance.SetDelete(&delete_TSlimRecoMUV3Hit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV3Hit);
      instance.SetDestructor(&destruct_TSlimRecoMUV3Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV3Hit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV3Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV3Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV3Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV3Candidate::Class_Name()
{
   return "TSlimRecoMUV3Candidate";
}

//______________________________________________________________________________
const char *TSlimRecoMUV3Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV3Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV3Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV3Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV3Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV3Event::Class_Name()
{
   return "TSlimRecoMUV3Event";
}

//______________________________________________________________________________
const char *TSlimRecoMUV3Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV3Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV3Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV3Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV3Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV3Hit::Class_Name()
{
   return "TSlimRecoMUV3Hit";
}

//______________________________________________________________________________
const char *TSlimRecoMUV3Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV3Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV3Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV3Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV3Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoMUV3Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV3Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV3Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV3Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV3Candidate(void *p) {
      return  p ? new(p) ::TSlimRecoMUV3Candidate : new ::TSlimRecoMUV3Candidate;
   }
   static void *newArray_TSlimRecoMUV3Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV3Candidate[nElements] : new ::TSlimRecoMUV3Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV3Candidate(void *p) {
      delete ((::TSlimRecoMUV3Candidate*)p);
   }
   static void deleteArray_TSlimRecoMUV3Candidate(void *p) {
      delete [] ((::TSlimRecoMUV3Candidate*)p);
   }
   static void destruct_TSlimRecoMUV3Candidate(void *p) {
      typedef ::TSlimRecoMUV3Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV3Candidate

//______________________________________________________________________________
void TSlimRecoMUV3Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV3Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV3Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV3Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV3Event(void *p) {
      return  p ? new(p) ::TSlimRecoMUV3Event : new ::TSlimRecoMUV3Event;
   }
   static void *newArray_TSlimRecoMUV3Event(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV3Event[nElements] : new ::TSlimRecoMUV3Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV3Event(void *p) {
      delete ((::TSlimRecoMUV3Event*)p);
   }
   static void deleteArray_TSlimRecoMUV3Event(void *p) {
      delete [] ((::TSlimRecoMUV3Event*)p);
   }
   static void destruct_TSlimRecoMUV3Event(void *p) {
      typedef ::TSlimRecoMUV3Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV3Event

//______________________________________________________________________________
void TSlimRecoMUV3Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV3Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV3Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV3Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV3Hit(void *p) {
      return  p ? new(p) ::TSlimRecoMUV3Hit : new ::TSlimRecoMUV3Hit;
   }
   static void *newArray_TSlimRecoMUV3Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV3Hit[nElements] : new ::TSlimRecoMUV3Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV3Hit(void *p) {
      delete ((::TSlimRecoMUV3Hit*)p);
   }
   static void deleteArray_TSlimRecoMUV3Hit(void *p) {
      delete [] ((::TSlimRecoMUV3Hit*)p);
   }
   static void destruct_TSlimRecoMUV3Hit(void *p) {
      typedef ::TSlimRecoMUV3Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV3Hit

namespace ROOT {
   static TClass *vectorlETSlimRecoMUV3HitgR_Dictionary();
   static void vectorlETSlimRecoMUV3HitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoMUV3HitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoMUV3HitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoMUV3HitgR(void *p);
   static void deleteArray_vectorlETSlimRecoMUV3HitgR(void *p);
   static void destruct_vectorlETSlimRecoMUV3HitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoMUV3Hit>*)
   {
      vector<TSlimRecoMUV3Hit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoMUV3Hit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoMUV3Hit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoMUV3Hit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoMUV3HitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoMUV3Hit>) );
      instance.SetNew(&new_vectorlETSlimRecoMUV3HitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoMUV3HitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoMUV3HitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoMUV3HitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoMUV3HitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoMUV3Hit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoMUV3Hit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoMUV3HitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoMUV3Hit>*)0x0)->GetClass();
      vectorlETSlimRecoMUV3HitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoMUV3HitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoMUV3HitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV3Hit> : new vector<TSlimRecoMUV3Hit>;
   }
   static void *newArray_vectorlETSlimRecoMUV3HitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV3Hit>[nElements] : new vector<TSlimRecoMUV3Hit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoMUV3HitgR(void *p) {
      delete ((vector<TSlimRecoMUV3Hit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoMUV3HitgR(void *p) {
      delete [] ((vector<TSlimRecoMUV3Hit>*)p);
   }
   static void destruct_vectorlETSlimRecoMUV3HitgR(void *p) {
      typedef vector<TSlimRecoMUV3Hit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoMUV3Hit>

namespace ROOT {
   static TClass *vectorlETSlimRecoMUV3CandidategR_Dictionary();
   static void vectorlETSlimRecoMUV3CandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoMUV3CandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoMUV3CandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoMUV3CandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoMUV3CandidategR(void *p);
   static void destruct_vectorlETSlimRecoMUV3CandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoMUV3Candidate>*)
   {
      vector<TSlimRecoMUV3Candidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoMUV3Candidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoMUV3Candidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoMUV3Candidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoMUV3CandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoMUV3Candidate>) );
      instance.SetNew(&new_vectorlETSlimRecoMUV3CandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoMUV3CandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoMUV3CandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoMUV3CandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoMUV3CandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoMUV3Candidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoMUV3Candidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoMUV3CandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoMUV3Candidate>*)0x0)->GetClass();
      vectorlETSlimRecoMUV3CandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoMUV3CandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoMUV3CandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV3Candidate> : new vector<TSlimRecoMUV3Candidate>;
   }
   static void *newArray_vectorlETSlimRecoMUV3CandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV3Candidate>[nElements] : new vector<TSlimRecoMUV3Candidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoMUV3CandidategR(void *p) {
      delete ((vector<TSlimRecoMUV3Candidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoMUV3CandidategR(void *p) {
      delete [] ((vector<TSlimRecoMUV3Candidate>*)p);
   }
   static void destruct_vectorlETSlimRecoMUV3CandidategR(void *p) {
      typedef vector<TSlimRecoMUV3Candidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoMUV3Candidate>

namespace {
  void TriggerDictionaryInitialization_libMUV3SlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3/../../FullReco/MUV3/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV3SlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class TSlimRecoMUV3Candidate;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoMUV3Hit;
class TSlimRecoMUV3Event;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV3SlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TSLIMMUV3CANDIDATE_H
#define TSLIMMUV3CANDIDATE_H

#include <RtypesCore.h>
#include <vector>
#include "TVector3.h"
#include "NA62Global.hh"
#include "TSlimRecoVCandidate.hh"

class TRecoMUV3Candidate;

class TSlimRecoMUV3Candidate : public TSlimRecoVCandidate {

public:
  TSlimRecoMUV3Candidate();
  explicit TSlimRecoMUV3Candidate(TRecoMUV3Candidate*);
  virtual ~TSlimRecoMUV3Candidate() {}

  // Setters for members
  void SetTime(Float_t val)     { fTime  = val;       }
  void SetTime1(Float_t val)    { fTime1 = val;       }
  void SetTime2(Float_t val)    { fTime2 = val;       }
  void SetChannel1(Short_t val) { fChannel1 = val;    }
  void SetChannel2(Short_t val) { fChannel2 = val;    }
  void SetType(Char_t val)      { fType = val;        }

  // Getters for members
  Float_t GetTime()  const      { return fTime;       }
  Float_t GetTime1() const      { return fTime1;      }
  Float_t GetTime2() const      { return fTime2;      }
  Short_t GetChannel1() const   { return fChannel1;   }
  Short_t GetChannel2() const   { return fChannel2;   }
  Char_t  GetType() const       { return fType;       }

  // Derived methods
  Short_t  GetTileID()          { return fChannel1%200;                  }
  Bool_t   IsTight()            { return (fType==kTightCandidate);       }
  Bool_t   IsLoose()            { return (fType==kLooseCandidate);       }
  Bool_t   IsLooseMasked()      { return (fType==kLooseMaskedCandidate); }
  Bool_t   IsInner()            { return ((fChannel1%200)>=144);         }
  Bool_t   IsOuter()            { return ((fChannel1%200)< 144);         }
  Double_t GetAverageTime(); ///< Average time of the two hits (for tight candidates only) [ns]
  Double_t GetDeltaTime();   ///< Difference of time between two hits (for tight candidates only) [ns]
  Double_t GetX();
  Double_t GetY();
  Double_t GetZ()               { return 246800.0;                       }
  TVector3 GetPosition();

  // Conversion functions
  virtual void FromReco(TRecoVCandidate*);
  virtual void ToReco  (TRecoVCandidate*);

private:
  Char_t  fType;       ///< kTightCandidate, kLooseCandidate, kLooseMaskedCandidate, kUndefinedCandidate
  Short_t fChannel1;   ///< Valid IDs: 0-151 except 65,66,77,78; 200-351 except 265,266,277,278
  Short_t fChannel2;   ///< Valid IDs: -1 (for loose candidates); 0-151 except 65,66,77,78; 200-351 except 265,266,277,278
  Float_t fTime;       ///< Candidate time [ns]
  Float_t fTime1;      ///< Time of the 1st hit [ns]
  Float_t fTime2;      ///< Time of the 2nd hit [ns]; -999 for loose candidates

  ClassDef(TSlimRecoMUV3Candidate, 1)
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TRECOMUV3EVENTSLIM_H
#define TRECOMUV3EVENTSLIM_H

#include <RtypesCore.h>
#include <vector>
#include "TSlimRecoVEvent.hh"
#include "TSlimRecoMUV3Candidate.hh"

class TRecoMUV3Event;

class TSlimRecoMUV3Event : public TSlimRecoVEvent {

public:
  TSlimRecoMUV3Event();
  explicit TSlimRecoMUV3Event(TRecoMUV3Event *evReco);
  virtual ~TSlimRecoMUV3Event() {}

  void Reset(); // clears the candidate vector
  void ClearCandidates();

  void AddCandidate(TSlimRecoMUV3Candidate c) { fCandidates.emplace_back(std::move(c)); }

  Int_t GetNCandidates()                               const { return fCandidates.size(); }
  std::vector<TSlimRecoMUV3Candidate>& GetCandidates()       { return fCandidates;        }
  TSlimRecoVCandidate* GetCandidate(UInt_t iCand)            { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

  // Conversion functions
  virtual void FromReco(TRecoVEvent*);
  virtual void ToReco  (TRecoVEvent*);
private:
  std::vector<TSlimRecoMUV3Candidate> fCandidates;

  ClassDef(TSlimRecoMUV3Event, 1)
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TSLIMMUV3HIT_H
#define TSLIMMUV3HIT_H

#include "TSlimRecoVHit.hh"

class TRecoMUV3Hit;

class TSlimRecoMUV3Hit : public TSlimRecoVHit {

public:
  TSlimRecoMUV3Hit()              {}
  explicit TSlimRecoMUV3Hit(TRecoMUV3Hit*) {}
  virtual ~TSlimRecoMUV3Hit()     {}

  // Conversion functions
  virtual void FromReco(TRecoVHit*) {}
  virtual void ToReco  (TRecoVHit*) {}

private:

  ClassDef(TSlimRecoMUV3Hit, 1)
};

#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoMUV3Candidate", payloadCode, "@",
"TSlimRecoMUV3Event", payloadCode, "@",
"TSlimRecoMUV3Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV3SlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV3SlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV3SlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV3SlimPersistency() {
  TriggerDictionaryInitialization_libMUV3SlimPersistency_Impl();
}
