// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME NewCHODSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NewCHOD/include/TSlimRecoNewCHODCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NewCHOD/include/TSlimRecoNewCHODEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NewCHOD/include/TSlimRecoNewCHODHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoNewCHODCandidate(void *p = 0);
   static void *newArray_TSlimRecoNewCHODCandidate(Long_t size, void *p);
   static void delete_TSlimRecoNewCHODCandidate(void *p);
   static void deleteArray_TSlimRecoNewCHODCandidate(void *p);
   static void destruct_TSlimRecoNewCHODCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoNewCHODCandidate*)
   {
      ::TSlimRecoNewCHODCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoNewCHODCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoNewCHODCandidate", ::TSlimRecoNewCHODCandidate::Class_Version(), "", 24,
                  typeid(::TSlimRecoNewCHODCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoNewCHODCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoNewCHODCandidate) );
      instance.SetNew(&new_TSlimRecoNewCHODCandidate);
      instance.SetNewArray(&newArray_TSlimRecoNewCHODCandidate);
      instance.SetDelete(&delete_TSlimRecoNewCHODCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoNewCHODCandidate);
      instance.SetDestructor(&destruct_TSlimRecoNewCHODCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoNewCHODCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoNewCHODCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoNewCHODCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoNewCHODEvent(void *p = 0);
   static void *newArray_TSlimRecoNewCHODEvent(Long_t size, void *p);
   static void delete_TSlimRecoNewCHODEvent(void *p);
   static void deleteArray_TSlimRecoNewCHODEvent(void *p);
   static void destruct_TSlimRecoNewCHODEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoNewCHODEvent*)
   {
      ::TSlimRecoNewCHODEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoNewCHODEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoNewCHODEvent", ::TSlimRecoNewCHODEvent::Class_Version(), "", 92,
                  typeid(::TSlimRecoNewCHODEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoNewCHODEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoNewCHODEvent) );
      instance.SetNew(&new_TSlimRecoNewCHODEvent);
      instance.SetNewArray(&newArray_TSlimRecoNewCHODEvent);
      instance.SetDelete(&delete_TSlimRecoNewCHODEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoNewCHODEvent);
      instance.SetDestructor(&destruct_TSlimRecoNewCHODEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoNewCHODEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoNewCHODEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoNewCHODEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoNewCHODHit(void *p = 0);
   static void *newArray_TSlimRecoNewCHODHit(Long_t size, void *p);
   static void delete_TSlimRecoNewCHODHit(void *p);
   static void deleteArray_TSlimRecoNewCHODHit(void *p);
   static void destruct_TSlimRecoNewCHODHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoNewCHODHit*)
   {
      ::TSlimRecoNewCHODHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoNewCHODHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoNewCHODHit", ::TSlimRecoNewCHODHit::Class_Version(), "", 134,
                  typeid(::TSlimRecoNewCHODHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoNewCHODHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoNewCHODHit) );
      instance.SetNew(&new_TSlimRecoNewCHODHit);
      instance.SetNewArray(&newArray_TSlimRecoNewCHODHit);
      instance.SetDelete(&delete_TSlimRecoNewCHODHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoNewCHODHit);
      instance.SetDestructor(&destruct_TSlimRecoNewCHODHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoNewCHODHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoNewCHODHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoNewCHODHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoNewCHODCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoNewCHODCandidate::Class_Name()
{
   return "TSlimRecoNewCHODCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoNewCHODCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoNewCHODCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoNewCHODCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoNewCHODCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoNewCHODEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoNewCHODEvent::Class_Name()
{
   return "TSlimRecoNewCHODEvent";
}

//______________________________________________________________________________
const char *TSlimRecoNewCHODEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoNewCHODEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoNewCHODEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoNewCHODEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoNewCHODHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoNewCHODHit::Class_Name()
{
   return "TSlimRecoNewCHODHit";
}

//______________________________________________________________________________
const char *TSlimRecoNewCHODHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoNewCHODHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoNewCHODHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoNewCHODHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoNewCHODHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoNewCHODCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoNewCHODCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoNewCHODCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoNewCHODCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoNewCHODCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoNewCHODCandidate : new ::TSlimRecoNewCHODCandidate;
   }
   static void *newArray_TSlimRecoNewCHODCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoNewCHODCandidate[nElements] : new ::TSlimRecoNewCHODCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoNewCHODCandidate(void *p) {
      delete ((::TSlimRecoNewCHODCandidate*)p);
   }
   static void deleteArray_TSlimRecoNewCHODCandidate(void *p) {
      delete [] ((::TSlimRecoNewCHODCandidate*)p);
   }
   static void destruct_TSlimRecoNewCHODCandidate(void *p) {
      typedef ::TSlimRecoNewCHODCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoNewCHODCandidate

//______________________________________________________________________________
void TSlimRecoNewCHODEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoNewCHODEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoNewCHODEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoNewCHODEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoNewCHODEvent(void *p) {
      return  p ? new(p) ::TSlimRecoNewCHODEvent : new ::TSlimRecoNewCHODEvent;
   }
   static void *newArray_TSlimRecoNewCHODEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoNewCHODEvent[nElements] : new ::TSlimRecoNewCHODEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoNewCHODEvent(void *p) {
      delete ((::TSlimRecoNewCHODEvent*)p);
   }
   static void deleteArray_TSlimRecoNewCHODEvent(void *p) {
      delete [] ((::TSlimRecoNewCHODEvent*)p);
   }
   static void destruct_TSlimRecoNewCHODEvent(void *p) {
      typedef ::TSlimRecoNewCHODEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoNewCHODEvent

//______________________________________________________________________________
void TSlimRecoNewCHODHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoNewCHODHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoNewCHODHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoNewCHODHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoNewCHODHit(void *p) {
      return  p ? new(p) ::TSlimRecoNewCHODHit : new ::TSlimRecoNewCHODHit;
   }
   static void *newArray_TSlimRecoNewCHODHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoNewCHODHit[nElements] : new ::TSlimRecoNewCHODHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoNewCHODHit(void *p) {
      delete ((::TSlimRecoNewCHODHit*)p);
   }
   static void deleteArray_TSlimRecoNewCHODHit(void *p) {
      delete [] ((::TSlimRecoNewCHODHit*)p);
   }
   static void destruct_TSlimRecoNewCHODHit(void *p) {
      typedef ::TSlimRecoNewCHODHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoNewCHODHit

namespace ROOT {
   static TClass *vectorlETSlimRecoNewCHODHitgR_Dictionary();
   static void vectorlETSlimRecoNewCHODHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoNewCHODHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoNewCHODHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoNewCHODHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoNewCHODHitgR(void *p);
   static void destruct_vectorlETSlimRecoNewCHODHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoNewCHODHit>*)
   {
      vector<TSlimRecoNewCHODHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoNewCHODHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoNewCHODHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoNewCHODHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoNewCHODHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoNewCHODHit>) );
      instance.SetNew(&new_vectorlETSlimRecoNewCHODHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoNewCHODHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoNewCHODHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoNewCHODHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoNewCHODHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoNewCHODHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoNewCHODHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoNewCHODHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoNewCHODHit>*)0x0)->GetClass();
      vectorlETSlimRecoNewCHODHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoNewCHODHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoNewCHODHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoNewCHODHit> : new vector<TSlimRecoNewCHODHit>;
   }
   static void *newArray_vectorlETSlimRecoNewCHODHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoNewCHODHit>[nElements] : new vector<TSlimRecoNewCHODHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoNewCHODHitgR(void *p) {
      delete ((vector<TSlimRecoNewCHODHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoNewCHODHitgR(void *p) {
      delete [] ((vector<TSlimRecoNewCHODHit>*)p);
   }
   static void destruct_vectorlETSlimRecoNewCHODHitgR(void *p) {
      typedef vector<TSlimRecoNewCHODHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoNewCHODHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoNewCHODCandidategR_Dictionary();
   static void vectorlETSlimRecoNewCHODCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoNewCHODCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoNewCHODCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoNewCHODCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoNewCHODCandidategR(void *p);
   static void destruct_vectorlETSlimRecoNewCHODCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoNewCHODCandidate>*)
   {
      vector<TSlimRecoNewCHODCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoNewCHODCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoNewCHODCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoNewCHODCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoNewCHODCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoNewCHODCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoNewCHODCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoNewCHODCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoNewCHODCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoNewCHODCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoNewCHODCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoNewCHODCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoNewCHODCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoNewCHODCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoNewCHODCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoNewCHODCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoNewCHODCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoNewCHODCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoNewCHODCandidate> : new vector<TSlimRecoNewCHODCandidate>;
   }
   static void *newArray_vectorlETSlimRecoNewCHODCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoNewCHODCandidate>[nElements] : new vector<TSlimRecoNewCHODCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoNewCHODCandidategR(void *p) {
      delete ((vector<TSlimRecoNewCHODCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoNewCHODCandidategR(void *p) {
      delete [] ((vector<TSlimRecoNewCHODCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoNewCHODCandidategR(void *p) {
      typedef vector<TSlimRecoNewCHODCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoNewCHODCandidate>

namespace {
  void TriggerDictionaryInitialization_libNewCHODSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NewCHOD/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NewCHOD/../../FullReco/NewCHOD/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/NewCHOD/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libNewCHODSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class TSlimRecoNewCHODCandidate;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoNewCHODHit;
class TSlimRecoNewCHODEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libNewCHODSlimPersistency dictionary payload"

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

#ifndef TSLIMRECONEWCHODCANDIDATE_H
#define TSLIMRECONEWCHODCANDIDATE_H

#include <RtypesCore.h>
#include "NA62Global.hh"
#include "TSlimRecoVCandidate.hh"
#include "TVector3.h"

class TRecoNewCHODHit;
class TRecoVHit;

class TSlimRecoNewCHODCandidate : public TSlimRecoVCandidate {

public:
  TSlimRecoNewCHODCandidate();
  explicit TSlimRecoNewCHODCandidate(TRecoNewCHODHit*);
  virtual ~TSlimRecoNewCHODCandidate() {}

  // Setters for members
  void SetTime1(Float_t val)    { fTime1 = val;       }
  void SetTime2(Float_t val)    { fTime2 = val;       }
  void SetChannel1(Short_t val) { fChannel1 = val;    }
  void SetChannel2(Short_t val) { fChannel2 = val;    }
  void SetType(Char_t val)      { fType = val;        }

  // Getters for members
  Float_t GetTime1() const      { return fTime1;      }
  Float_t GetTime2() const      { return fTime2;      }
  Short_t GetChannel1() const   { return fChannel1;   }
  Short_t GetChannel2() const   { return fChannel2;   }
  Char_t  GetType() const       { return fType;       }

  // Derived methods
  Float_t  GetTime() { return (fType==kTightCandidate) ? 0.5*(fTime1+fTime2) : fTime1; }
  Int_t    GetTileID();
  Int_t    GetSeqTileID();
  Int_t    GetQuadrantID();
  Double_t GetX();
  Double_t GetY();
  Double_t GetZ()        { return 238131.5;                         }
  TVector3 GetPosition() { return TVector3(GetX(), GetY(), GetZ()); }

  // Conversion functions
  virtual void FromReco(TRecoVCandidate*) {}
  virtual void ToReco  (TRecoVCandidate*) {}

  virtual void FromReco(TRecoVHit*);
  virtual void ToReco  (TRecoVHit*);

private:
  Char_t  fType;       ///< kTightCandidate, kLooseCandidate, kLooseMaskedCandidate, kUndefinedCandidate
  Short_t fChannel1;   ///< Valid IDs: 101-138, 201-238, 301-338, 401-438
  Short_t fChannel2;   ///< Valid IDs: 151-188, 251-288, 351-388, 451-488; or -1 for loose hits
  Float_t fTime1;      ///< Time of the 1st channel [ns]
  Float_t fTime2;      ///< Time of the 2nd channel [ns]; -999 for loose hits

  ClassDef(TSlimRecoNewCHODCandidate, 1)
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TRECONEWCHODEVENTSLIM_H
#define TRECONEWCHODEVENTSLIM_H

#include <RtypesCore.h>
#include <vector>

#include "TSlimRecoNewCHODCandidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoNewCHODEvent;

class TSlimRecoNewCHODEvent : public TSlimRecoVEvent {

public:
  TSlimRecoNewCHODEvent();
  explicit TSlimRecoNewCHODEvent(TRecoNewCHODEvent*);
  virtual ~TSlimRecoNewCHODEvent() {}

  void Reset();
  void ClearCandidates();

  void AddCandidate(TSlimRecoNewCHODCandidate h) { fCandidates.emplace_back(std::move(h)); }

  Int_t GetNCandidates()                            const { return fCandidates.size(); }
  std::vector<TSlimRecoNewCHODCandidate>& GetCandidates() { return fCandidates;        }
  TSlimRecoVCandidate* GetCandidate(UInt_t iCand)         { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }


  // Conversion functions
  virtual void FromReco(TRecoVEvent*);
  virtual void ToReco(TRecoVEvent*);
private:
  std::vector<TSlimRecoNewCHODCandidate> fCandidates;

  ClassDef(TSlimRecoNewCHODEvent, 1)
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TSLIMRECONEWCHODHIT_H
#define TSLIMRECONEWCHODHIT_H

#include "TSlimRecoVHit.hh"

class TRecoNewCHODHit;

class TSlimRecoNewCHODHit : public TSlimRecoVHit {

public:
  TSlimRecoNewCHODHit()                 {}
  explicit TSlimRecoNewCHODHit(TRecoNewCHODHit*) {}
  virtual ~TSlimRecoNewCHODHit()        {}

  // Conversion functions
  virtual void FromReco(TRecoVHit*) {}
  virtual void ToReco  (TRecoVHit*) {}

private:

  ClassDef(TSlimRecoNewCHODHit, 1)
};

#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoNewCHODCandidate", payloadCode, "@",
"TSlimRecoNewCHODEvent", payloadCode, "@",
"TSlimRecoNewCHODHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libNewCHODSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libNewCHODSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libNewCHODSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libNewCHODSlimPersistency() {
  TriggerDictionaryInitialization_libNewCHODSlimPersistency_Impl();
}
