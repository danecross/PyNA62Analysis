// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV1SlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV1/include/TSlimRecoMUV1Candidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV1/include/TSlimRecoMUV1Event.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV1/include/TSlimRecoMUV1Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoMUV1Candidate(void *p = 0);
   static void *newArray_TSlimRecoMUV1Candidate(Long_t size, void *p);
   static void delete_TSlimRecoMUV1Candidate(void *p);
   static void deleteArray_TSlimRecoMUV1Candidate(void *p);
   static void destruct_TSlimRecoMUV1Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV1Candidate*)
   {
      ::TSlimRecoMUV1Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV1Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV1Candidate", ::TSlimRecoMUV1Candidate::Class_Version(), "", 12,
                  typeid(::TSlimRecoMUV1Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV1Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV1Candidate) );
      instance.SetNew(&new_TSlimRecoMUV1Candidate);
      instance.SetNewArray(&newArray_TSlimRecoMUV1Candidate);
      instance.SetDelete(&delete_TSlimRecoMUV1Candidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV1Candidate);
      instance.SetDestructor(&destruct_TSlimRecoMUV1Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV1Candidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV1Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV1Candidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV1Hit(void *p = 0);
   static void *newArray_TSlimRecoMUV1Hit(Long_t size, void *p);
   static void delete_TSlimRecoMUV1Hit(void *p);
   static void deleteArray_TSlimRecoMUV1Hit(void *p);
   static void destruct_TSlimRecoMUV1Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV1Hit*)
   {
      ::TSlimRecoMUV1Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV1Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV1Hit", ::TSlimRecoMUV1Hit::Class_Version(), "TSlimRecoMUV1Hit.hh", 6,
                  typeid(::TSlimRecoMUV1Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV1Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV1Hit) );
      instance.SetNew(&new_TSlimRecoMUV1Hit);
      instance.SetNewArray(&newArray_TSlimRecoMUV1Hit);
      instance.SetDelete(&delete_TSlimRecoMUV1Hit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV1Hit);
      instance.SetDestructor(&destruct_TSlimRecoMUV1Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV1Hit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV1Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV1Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV1Event(void *p = 0);
   static void *newArray_TSlimRecoMUV1Event(Long_t size, void *p);
   static void delete_TSlimRecoMUV1Event(void *p);
   static void deleteArray_TSlimRecoMUV1Event(void *p);
   static void destruct_TSlimRecoMUV1Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV1Event*)
   {
      ::TSlimRecoMUV1Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV1Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV1Event", ::TSlimRecoMUV1Event::Class_Version(), "", 36,
                  typeid(::TSlimRecoMUV1Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV1Event::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV1Event) );
      instance.SetNew(&new_TSlimRecoMUV1Event);
      instance.SetNewArray(&newArray_TSlimRecoMUV1Event);
      instance.SetDelete(&delete_TSlimRecoMUV1Event);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV1Event);
      instance.SetDestructor(&destruct_TSlimRecoMUV1Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV1Event*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV1Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV1Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV1Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV1Candidate::Class_Name()
{
   return "TSlimRecoMUV1Candidate";
}

//______________________________________________________________________________
const char *TSlimRecoMUV1Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV1Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV1Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV1Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV1Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV1Hit::Class_Name()
{
   return "TSlimRecoMUV1Hit";
}

//______________________________________________________________________________
const char *TSlimRecoMUV1Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV1Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV1Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV1Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV1Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV1Event::Class_Name()
{
   return "TSlimRecoMUV1Event";
}

//______________________________________________________________________________
const char *TSlimRecoMUV1Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV1Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV1Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV1Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV1Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoMUV1Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV1Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV1Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV1Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV1Candidate(void *p) {
      return  p ? new(p) ::TSlimRecoMUV1Candidate : new ::TSlimRecoMUV1Candidate;
   }
   static void *newArray_TSlimRecoMUV1Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV1Candidate[nElements] : new ::TSlimRecoMUV1Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV1Candidate(void *p) {
      delete ((::TSlimRecoMUV1Candidate*)p);
   }
   static void deleteArray_TSlimRecoMUV1Candidate(void *p) {
      delete [] ((::TSlimRecoMUV1Candidate*)p);
   }
   static void destruct_TSlimRecoMUV1Candidate(void *p) {
      typedef ::TSlimRecoMUV1Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV1Candidate

//______________________________________________________________________________
void TSlimRecoMUV1Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV1Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV1Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV1Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV1Hit(void *p) {
      return  p ? new(p) ::TSlimRecoMUV1Hit : new ::TSlimRecoMUV1Hit;
   }
   static void *newArray_TSlimRecoMUV1Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV1Hit[nElements] : new ::TSlimRecoMUV1Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV1Hit(void *p) {
      delete ((::TSlimRecoMUV1Hit*)p);
   }
   static void deleteArray_TSlimRecoMUV1Hit(void *p) {
      delete [] ((::TSlimRecoMUV1Hit*)p);
   }
   static void destruct_TSlimRecoMUV1Hit(void *p) {
      typedef ::TSlimRecoMUV1Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV1Hit

//______________________________________________________________________________
void TSlimRecoMUV1Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV1Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV1Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV1Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV1Event(void *p) {
      return  p ? new(p) ::TSlimRecoMUV1Event : new ::TSlimRecoMUV1Event;
   }
   static void *newArray_TSlimRecoMUV1Event(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV1Event[nElements] : new ::TSlimRecoMUV1Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV1Event(void *p) {
      delete ((::TSlimRecoMUV1Event*)p);
   }
   static void deleteArray_TSlimRecoMUV1Event(void *p) {
      delete [] ((::TSlimRecoMUV1Event*)p);
   }
   static void destruct_TSlimRecoMUV1Event(void *p) {
      typedef ::TSlimRecoMUV1Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV1Event

namespace ROOT {
   static TClass *vectorlETSlimRecoMUV1HitgR_Dictionary();
   static void vectorlETSlimRecoMUV1HitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoMUV1HitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoMUV1HitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoMUV1HitgR(void *p);
   static void deleteArray_vectorlETSlimRecoMUV1HitgR(void *p);
   static void destruct_vectorlETSlimRecoMUV1HitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoMUV1Hit>*)
   {
      vector<TSlimRecoMUV1Hit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoMUV1Hit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoMUV1Hit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoMUV1Hit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoMUV1HitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoMUV1Hit>) );
      instance.SetNew(&new_vectorlETSlimRecoMUV1HitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoMUV1HitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoMUV1HitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoMUV1HitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoMUV1HitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoMUV1Hit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoMUV1Hit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoMUV1HitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoMUV1Hit>*)0x0)->GetClass();
      vectorlETSlimRecoMUV1HitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoMUV1HitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoMUV1HitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV1Hit> : new vector<TSlimRecoMUV1Hit>;
   }
   static void *newArray_vectorlETSlimRecoMUV1HitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV1Hit>[nElements] : new vector<TSlimRecoMUV1Hit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoMUV1HitgR(void *p) {
      delete ((vector<TSlimRecoMUV1Hit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoMUV1HitgR(void *p) {
      delete [] ((vector<TSlimRecoMUV1Hit>*)p);
   }
   static void destruct_vectorlETSlimRecoMUV1HitgR(void *p) {
      typedef vector<TSlimRecoMUV1Hit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoMUV1Hit>

namespace ROOT {
   static TClass *vectorlETSlimRecoMUV1CandidategR_Dictionary();
   static void vectorlETSlimRecoMUV1CandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoMUV1CandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoMUV1CandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoMUV1CandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoMUV1CandidategR(void *p);
   static void destruct_vectorlETSlimRecoMUV1CandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoMUV1Candidate>*)
   {
      vector<TSlimRecoMUV1Candidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoMUV1Candidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoMUV1Candidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoMUV1Candidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoMUV1CandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoMUV1Candidate>) );
      instance.SetNew(&new_vectorlETSlimRecoMUV1CandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoMUV1CandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoMUV1CandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoMUV1CandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoMUV1CandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoMUV1Candidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoMUV1Candidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoMUV1CandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoMUV1Candidate>*)0x0)->GetClass();
      vectorlETSlimRecoMUV1CandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoMUV1CandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoMUV1CandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV1Candidate> : new vector<TSlimRecoMUV1Candidate>;
   }
   static void *newArray_vectorlETSlimRecoMUV1CandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV1Candidate>[nElements] : new vector<TSlimRecoMUV1Candidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoMUV1CandidategR(void *p) {
      delete ((vector<TSlimRecoMUV1Candidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoMUV1CandidategR(void *p) {
      delete [] ((vector<TSlimRecoMUV1Candidate>*)p);
   }
   static void destruct_vectorlETSlimRecoMUV1CandidategR(void *p) {
      typedef vector<TSlimRecoMUV1Candidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoMUV1Candidate>

namespace {
  void TriggerDictionaryInitialization_libMUV1SlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV1/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV1/../../FullReco/MUV1/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV1/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV1SlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoMUV1Hit.hh")))  TSlimRecoMUV1Hit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoMUV1Candidate;
class TSlimRecoMUV1Event;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV1SlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOMUV1CANDIDATE_HH
#define TSLIMRECOMUV1CANDIDATE_HH

#include "TSlimRecoMUVCandidate.hh"


class TSlimRecoMUV1Candidate : public TSlimRecoMUVCandidate {

public:
	TSlimRecoMUV1Candidate (){}
	explicit TSlimRecoMUV1Candidate (TRecoVCandidate *cand);
	virtual ~TSlimRecoMUV1Candidate (){}

	void FromReco(TRecoVCandidate *cand);
	void ToReco(TRecoVCandidate *cand);

    ClassDef(TSlimRecoMUV1Candidate, 1)
};


#endif
#ifndef TSLIMRECOMUV1EVENT_HH
#define TSLIMRECOMUV1EVENT_HH

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoMUV1Hit.hh"
#include "TSlimRecoMUV1Candidate.hh"


class TSlimRecoMUV1Event : public TSlimRecoVEvent {

public:
	TSlimRecoMUV1Event ()          { Reset(); }
	virtual ~TSlimRecoMUV1Event () {}

	void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

	void AddCandidate(TSlimRecoMUV1Candidate c) { fCandidates.emplace_back(std::move(c)); }
	void AddHit(TSlimRecoMUV1Hit h)             { fHits.emplace_back(std::move(h));       }

	Int_t GetNHits()                                    const { return fHits.size();       }
	std::vector<TSlimRecoMUV1Hit>& GetHits()                  { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                        { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
	Int_t GetNCandidates()                              const { return fCandidates.size(); }
	std::vector<TSlimRecoMUV1Candidate>& GetCandidates()      { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

	void FromReco(TRecoVEvent *evReco);
	void ToReco(TRecoVEvent *evReco);
private:
	std::vector<TSlimRecoMUV1Hit> fHits;
	std::vector<TSlimRecoMUV1Candidate> fCandidates;

    ClassDef(TSlimRecoMUV1Event, 1)
};

#endif
#ifndef TSLIMRECOMUV1HIT_HH
#define TSLIMRECOMUV1HIT_HH
#include "TSlimRecoMUVHit.hh"
#include <TVector3.h>

class TSlimRecoMUV1Hit : public TSlimRecoMUVHit {

public:
	TSlimRecoMUV1Hit ()               {}
	explicit TSlimRecoMUV1Hit (TRecoVHit *hit);
	virtual ~TSlimRecoMUV1Hit ()      {}

	Int_t GetQuadrant ()              const;
	Float_t GetScintillatorPosition() const;
	Float_t GetEnergy ()              const { return GetCharge()/1.575; }
	Bool_t IsLongScintillator ()      const;
    Int_t GetScintillatorNumber ()    const;
    Int_t GetSide ()                  const;
    TVector3 GetPosition()            const;

	void FromReco(TRecoVHit *hit);
	void ToReco(TRecoVHit *hit);

	ClassDef(TSlimRecoMUV1Hit, 1)
};


#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoMUV1Candidate", payloadCode, "@",
"TSlimRecoMUV1Event", payloadCode, "@",
"TSlimRecoMUV1Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV1SlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV1SlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV1SlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV1SlimPersistency() {
  TriggerDictionaryInitialization_libMUV1SlimPersistency_Impl();
}
