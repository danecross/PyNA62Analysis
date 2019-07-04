// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV2SlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV2/include/TSlimRecoMUV2Candidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV2/include/TSlimRecoMUV2Event.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV2/include/TSlimRecoMUV2Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoMUV2Candidate(void *p = 0);
   static void *newArray_TSlimRecoMUV2Candidate(Long_t size, void *p);
   static void delete_TSlimRecoMUV2Candidate(void *p);
   static void deleteArray_TSlimRecoMUV2Candidate(void *p);
   static void destruct_TSlimRecoMUV2Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV2Candidate*)
   {
      ::TSlimRecoMUV2Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV2Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV2Candidate", ::TSlimRecoMUV2Candidate::Class_Version(), "", 12,
                  typeid(::TSlimRecoMUV2Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV2Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV2Candidate) );
      instance.SetNew(&new_TSlimRecoMUV2Candidate);
      instance.SetNewArray(&newArray_TSlimRecoMUV2Candidate);
      instance.SetDelete(&delete_TSlimRecoMUV2Candidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV2Candidate);
      instance.SetDestructor(&destruct_TSlimRecoMUV2Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV2Candidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV2Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV2Candidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV2Hit(void *p = 0);
   static void *newArray_TSlimRecoMUV2Hit(Long_t size, void *p);
   static void delete_TSlimRecoMUV2Hit(void *p);
   static void deleteArray_TSlimRecoMUV2Hit(void *p);
   static void destruct_TSlimRecoMUV2Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV2Hit*)
   {
      ::TSlimRecoMUV2Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV2Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV2Hit", ::TSlimRecoMUV2Hit::Class_Version(), "TSlimRecoMUV2Hit.hh", 6,
                  typeid(::TSlimRecoMUV2Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV2Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV2Hit) );
      instance.SetNew(&new_TSlimRecoMUV2Hit);
      instance.SetNewArray(&newArray_TSlimRecoMUV2Hit);
      instance.SetDelete(&delete_TSlimRecoMUV2Hit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV2Hit);
      instance.SetDestructor(&destruct_TSlimRecoMUV2Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV2Hit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV2Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV2Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV2Event(void *p = 0);
   static void *newArray_TSlimRecoMUV2Event(Long_t size, void *p);
   static void delete_TSlimRecoMUV2Event(void *p);
   static void deleteArray_TSlimRecoMUV2Event(void *p);
   static void destruct_TSlimRecoMUV2Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV2Event*)
   {
      ::TSlimRecoMUV2Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV2Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV2Event", ::TSlimRecoMUV2Event::Class_Version(), "", 36,
                  typeid(::TSlimRecoMUV2Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV2Event::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV2Event) );
      instance.SetNew(&new_TSlimRecoMUV2Event);
      instance.SetNewArray(&newArray_TSlimRecoMUV2Event);
      instance.SetDelete(&delete_TSlimRecoMUV2Event);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV2Event);
      instance.SetDestructor(&destruct_TSlimRecoMUV2Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV2Event*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV2Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV2Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV2Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV2Candidate::Class_Name()
{
   return "TSlimRecoMUV2Candidate";
}

//______________________________________________________________________________
const char *TSlimRecoMUV2Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV2Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV2Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV2Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV2Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV2Hit::Class_Name()
{
   return "TSlimRecoMUV2Hit";
}

//______________________________________________________________________________
const char *TSlimRecoMUV2Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV2Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV2Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV2Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV2Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV2Event::Class_Name()
{
   return "TSlimRecoMUV2Event";
}

//______________________________________________________________________________
const char *TSlimRecoMUV2Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV2Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV2Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV2Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV2Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoMUV2Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV2Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV2Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV2Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV2Candidate(void *p) {
      return  p ? new(p) ::TSlimRecoMUV2Candidate : new ::TSlimRecoMUV2Candidate;
   }
   static void *newArray_TSlimRecoMUV2Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV2Candidate[nElements] : new ::TSlimRecoMUV2Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV2Candidate(void *p) {
      delete ((::TSlimRecoMUV2Candidate*)p);
   }
   static void deleteArray_TSlimRecoMUV2Candidate(void *p) {
      delete [] ((::TSlimRecoMUV2Candidate*)p);
   }
   static void destruct_TSlimRecoMUV2Candidate(void *p) {
      typedef ::TSlimRecoMUV2Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV2Candidate

//______________________________________________________________________________
void TSlimRecoMUV2Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV2Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV2Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV2Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV2Hit(void *p) {
      return  p ? new(p) ::TSlimRecoMUV2Hit : new ::TSlimRecoMUV2Hit;
   }
   static void *newArray_TSlimRecoMUV2Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV2Hit[nElements] : new ::TSlimRecoMUV2Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV2Hit(void *p) {
      delete ((::TSlimRecoMUV2Hit*)p);
   }
   static void deleteArray_TSlimRecoMUV2Hit(void *p) {
      delete [] ((::TSlimRecoMUV2Hit*)p);
   }
   static void destruct_TSlimRecoMUV2Hit(void *p) {
      typedef ::TSlimRecoMUV2Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV2Hit

//______________________________________________________________________________
void TSlimRecoMUV2Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV2Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV2Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV2Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV2Event(void *p) {
      return  p ? new(p) ::TSlimRecoMUV2Event : new ::TSlimRecoMUV2Event;
   }
   static void *newArray_TSlimRecoMUV2Event(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV2Event[nElements] : new ::TSlimRecoMUV2Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV2Event(void *p) {
      delete ((::TSlimRecoMUV2Event*)p);
   }
   static void deleteArray_TSlimRecoMUV2Event(void *p) {
      delete [] ((::TSlimRecoMUV2Event*)p);
   }
   static void destruct_TSlimRecoMUV2Event(void *p) {
      typedef ::TSlimRecoMUV2Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV2Event

namespace ROOT {
   static TClass *vectorlETSlimRecoMUV2HitgR_Dictionary();
   static void vectorlETSlimRecoMUV2HitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoMUV2HitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoMUV2HitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoMUV2HitgR(void *p);
   static void deleteArray_vectorlETSlimRecoMUV2HitgR(void *p);
   static void destruct_vectorlETSlimRecoMUV2HitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoMUV2Hit>*)
   {
      vector<TSlimRecoMUV2Hit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoMUV2Hit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoMUV2Hit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoMUV2Hit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoMUV2HitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoMUV2Hit>) );
      instance.SetNew(&new_vectorlETSlimRecoMUV2HitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoMUV2HitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoMUV2HitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoMUV2HitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoMUV2HitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoMUV2Hit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoMUV2Hit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoMUV2HitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoMUV2Hit>*)0x0)->GetClass();
      vectorlETSlimRecoMUV2HitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoMUV2HitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoMUV2HitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV2Hit> : new vector<TSlimRecoMUV2Hit>;
   }
   static void *newArray_vectorlETSlimRecoMUV2HitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV2Hit>[nElements] : new vector<TSlimRecoMUV2Hit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoMUV2HitgR(void *p) {
      delete ((vector<TSlimRecoMUV2Hit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoMUV2HitgR(void *p) {
      delete [] ((vector<TSlimRecoMUV2Hit>*)p);
   }
   static void destruct_vectorlETSlimRecoMUV2HitgR(void *p) {
      typedef vector<TSlimRecoMUV2Hit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoMUV2Hit>

namespace ROOT {
   static TClass *vectorlETSlimRecoMUV2CandidategR_Dictionary();
   static void vectorlETSlimRecoMUV2CandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoMUV2CandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoMUV2CandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoMUV2CandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoMUV2CandidategR(void *p);
   static void destruct_vectorlETSlimRecoMUV2CandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoMUV2Candidate>*)
   {
      vector<TSlimRecoMUV2Candidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoMUV2Candidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoMUV2Candidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoMUV2Candidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoMUV2CandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoMUV2Candidate>) );
      instance.SetNew(&new_vectorlETSlimRecoMUV2CandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoMUV2CandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoMUV2CandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoMUV2CandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoMUV2CandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoMUV2Candidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoMUV2Candidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoMUV2CandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoMUV2Candidate>*)0x0)->GetClass();
      vectorlETSlimRecoMUV2CandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoMUV2CandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoMUV2CandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV2Candidate> : new vector<TSlimRecoMUV2Candidate>;
   }
   static void *newArray_vectorlETSlimRecoMUV2CandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV2Candidate>[nElements] : new vector<TSlimRecoMUV2Candidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoMUV2CandidategR(void *p) {
      delete ((vector<TSlimRecoMUV2Candidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoMUV2CandidategR(void *p) {
      delete [] ((vector<TSlimRecoMUV2Candidate>*)p);
   }
   static void destruct_vectorlETSlimRecoMUV2CandidategR(void *p) {
      typedef vector<TSlimRecoMUV2Candidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoMUV2Candidate>

namespace {
  void TriggerDictionaryInitialization_libMUV2SlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV2/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV2/../../FullReco/MUV2/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/MUV2/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV2SlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoMUV2Hit.hh")))  TSlimRecoMUV2Hit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoMUV2Candidate;
class TSlimRecoMUV2Event;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV2SlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOMUV2CANDIDATE_HH
#define TSLIMRECOMUV2CANDIDATE_HH

#include "TSlimRecoMUVCandidate.hh"


class TSlimRecoMUV2Candidate : public TSlimRecoMUVCandidate {

public:
	TSlimRecoMUV2Candidate (){}
	explicit TSlimRecoMUV2Candidate (TRecoVCandidate *cand);
	virtual ~TSlimRecoMUV2Candidate (){}

	void FromReco(TRecoVCandidate *cand);
	void ToReco(TRecoVCandidate *cand);

    ClassDef(TSlimRecoMUV2Candidate, 1)
};


#endif
#ifndef TSLIMRECOMUV2EVENT_HH
#define TSLIMRECOMUV2EVENT_HH

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoMUV2Hit.hh"
#include "TSlimRecoMUV2Candidate.hh"


class TSlimRecoMUV2Event : public TSlimRecoVEvent {

public:
	TSlimRecoMUV2Event ()          { Reset(); }
	virtual ~TSlimRecoMUV2Event () {}

	void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

	void AddCandidate(TSlimRecoMUV2Candidate c) { fCandidates.emplace_back(std::move(c)); }
	void AddHit(TSlimRecoMUV2Hit h)             { fHits.emplace_back(std::move(h));       }

	Int_t GetNHits()                                     const { return fHits.size();       }
	std::vector<TSlimRecoMUV2Hit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                         { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
	Int_t GetNCandidates()                               const { return fCandidates.size(); }
	std::vector<TSlimRecoMUV2Candidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)            { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

	void FromReco(TRecoVEvent *evReco);
	void ToReco(TRecoVEvent *evReco);
private:
	std::vector<TSlimRecoMUV2Hit> fHits;
	std::vector<TSlimRecoMUV2Candidate> fCandidates;

    ClassDef(TSlimRecoMUV2Event, 1)
};

#endif
#ifndef TSLIMRECOMUV2HIT_HH
#define TSLIMRECOMUV2HIT_HH
#include "TSlimRecoMUVHit.hh"
#include <TVector3.h>

class TSlimRecoMUV2Hit : public TSlimRecoMUVHit {

public:
	TSlimRecoMUV2Hit ()               {}
	explicit TSlimRecoMUV2Hit (TRecoVHit *hit);
	virtual ~TSlimRecoMUV2Hit ()      {}

	Int_t GetQuadrant ()              const;
	Float_t GetScintillatorPosition() const;
	Float_t GetEnergy ()              const { return GetCharge()/1.12; }
    Int_t GetScintillatorNumber ()    const;
    Int_t GetSide ()                  const;
    TVector3 GetPosition ()           const;

	void FromReco(TRecoVHit *hit);
	void ToReco(TRecoVHit *hit);

    ClassDef(TSlimRecoMUV2Hit, 1)
};


#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoMUV2Candidate", payloadCode, "@",
"TSlimRecoMUV2Event", payloadCode, "@",
"TSlimRecoMUV2Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV2SlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV2SlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV2SlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV2SlimPersistency() {
  TriggerDictionaryInitialization_libMUV2SlimPersistency_Impl();
}
