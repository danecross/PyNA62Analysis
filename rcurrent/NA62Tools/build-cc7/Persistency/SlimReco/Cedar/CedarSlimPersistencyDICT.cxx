// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME CedarSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/Cedar/include/TSlimRecoCedarCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/Cedar/include/TSlimRecoCedarEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/Cedar/include/TSlimRecoCedarHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoCedarCandidate(void *p = 0);
   static void *newArray_TSlimRecoCedarCandidate(Long_t size, void *p);
   static void delete_TSlimRecoCedarCandidate(void *p);
   static void deleteArray_TSlimRecoCedarCandidate(void *p);
   static void destruct_TSlimRecoCedarCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCedarCandidate*)
   {
      ::TSlimRecoCedarCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCedarCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCedarCandidate", ::TSlimRecoCedarCandidate::Class_Version(), "", 16,
                  typeid(::TSlimRecoCedarCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCedarCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCedarCandidate) );
      instance.SetNew(&new_TSlimRecoCedarCandidate);
      instance.SetNewArray(&newArray_TSlimRecoCedarCandidate);
      instance.SetDelete(&delete_TSlimRecoCedarCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCedarCandidate);
      instance.SetDestructor(&destruct_TSlimRecoCedarCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCedarCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCedarCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCedarCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoCedarHit(void *p = 0);
   static void *newArray_TSlimRecoCedarHit(Long_t size, void *p);
   static void delete_TSlimRecoCedarHit(void *p);
   static void deleteArray_TSlimRecoCedarHit(void *p);
   static void destruct_TSlimRecoCedarHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCedarHit*)
   {
      ::TSlimRecoCedarHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCedarHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCedarHit", ::TSlimRecoCedarHit::Class_Version(), "TSlimRecoCedarHit.hh", 10,
                  typeid(::TSlimRecoCedarHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCedarHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCedarHit) );
      instance.SetNew(&new_TSlimRecoCedarHit);
      instance.SetNewArray(&newArray_TSlimRecoCedarHit);
      instance.SetDelete(&delete_TSlimRecoCedarHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCedarHit);
      instance.SetDestructor(&destruct_TSlimRecoCedarHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCedarHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCedarHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCedarHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoCedarEvent(void *p = 0);
   static void *newArray_TSlimRecoCedarEvent(Long_t size, void *p);
   static void delete_TSlimRecoCedarEvent(void *p);
   static void deleteArray_TSlimRecoCedarEvent(void *p);
   static void destruct_TSlimRecoCedarEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCedarEvent*)
   {
      ::TSlimRecoCedarEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCedarEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCedarEvent", ::TSlimRecoCedarEvent::Class_Version(), "", 59,
                  typeid(::TSlimRecoCedarEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCedarEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCedarEvent) );
      instance.SetNew(&new_TSlimRecoCedarEvent);
      instance.SetNewArray(&newArray_TSlimRecoCedarEvent);
      instance.SetDelete(&delete_TSlimRecoCedarEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCedarEvent);
      instance.SetDestructor(&destruct_TSlimRecoCedarEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCedarEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCedarEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCedarEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCedarCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCedarCandidate::Class_Name()
{
   return "TSlimRecoCedarCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoCedarCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCedarCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCedarCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCedarCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCedarHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCedarHit::Class_Name()
{
   return "TSlimRecoCedarHit";
}

//______________________________________________________________________________
const char *TSlimRecoCedarHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCedarHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCedarHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCedarHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCedarEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCedarEvent::Class_Name()
{
   return "TSlimRecoCedarEvent";
}

//______________________________________________________________________________
const char *TSlimRecoCedarEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCedarEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCedarEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCedarEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCedarEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoCedarCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCedarCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCedarCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCedarCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCedarCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoCedarCandidate : new ::TSlimRecoCedarCandidate;
   }
   static void *newArray_TSlimRecoCedarCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCedarCandidate[nElements] : new ::TSlimRecoCedarCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCedarCandidate(void *p) {
      delete ((::TSlimRecoCedarCandidate*)p);
   }
   static void deleteArray_TSlimRecoCedarCandidate(void *p) {
      delete [] ((::TSlimRecoCedarCandidate*)p);
   }
   static void destruct_TSlimRecoCedarCandidate(void *p) {
      typedef ::TSlimRecoCedarCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCedarCandidate

//______________________________________________________________________________
void TSlimRecoCedarHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCedarHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCedarHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCedarHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCedarHit(void *p) {
      return  p ? new(p) ::TSlimRecoCedarHit : new ::TSlimRecoCedarHit;
   }
   static void *newArray_TSlimRecoCedarHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCedarHit[nElements] : new ::TSlimRecoCedarHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCedarHit(void *p) {
      delete ((::TSlimRecoCedarHit*)p);
   }
   static void deleteArray_TSlimRecoCedarHit(void *p) {
      delete [] ((::TSlimRecoCedarHit*)p);
   }
   static void destruct_TSlimRecoCedarHit(void *p) {
      typedef ::TSlimRecoCedarHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCedarHit

//______________________________________________________________________________
void TSlimRecoCedarEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCedarEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCedarEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCedarEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCedarEvent(void *p) {
      return  p ? new(p) ::TSlimRecoCedarEvent : new ::TSlimRecoCedarEvent;
   }
   static void *newArray_TSlimRecoCedarEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCedarEvent[nElements] : new ::TSlimRecoCedarEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCedarEvent(void *p) {
      delete ((::TSlimRecoCedarEvent*)p);
   }
   static void deleteArray_TSlimRecoCedarEvent(void *p) {
      delete [] ((::TSlimRecoCedarEvent*)p);
   }
   static void destruct_TSlimRecoCedarEvent(void *p) {
      typedef ::TSlimRecoCedarEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCedarEvent

namespace ROOT {
   static TClass *vectorlEshortgR_Dictionary();
   static void vectorlEshortgR_TClassManip(TClass*);
   static void *new_vectorlEshortgR(void *p = 0);
   static void *newArray_vectorlEshortgR(Long_t size, void *p);
   static void delete_vectorlEshortgR(void *p);
   static void deleteArray_vectorlEshortgR(void *p);
   static void destruct_vectorlEshortgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<short>*)
   {
      vector<short> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<short>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<short>", -2, "vector", 216,
                  typeid(vector<short>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEshortgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<short>) );
      instance.SetNew(&new_vectorlEshortgR);
      instance.SetNewArray(&newArray_vectorlEshortgR);
      instance.SetDelete(&delete_vectorlEshortgR);
      instance.SetDeleteArray(&deleteArray_vectorlEshortgR);
      instance.SetDestructor(&destruct_vectorlEshortgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<short> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<short>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEshortgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<short>*)0x0)->GetClass();
      vectorlEshortgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEshortgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEshortgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<short> : new vector<short>;
   }
   static void *newArray_vectorlEshortgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<short>[nElements] : new vector<short>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEshortgR(void *p) {
      delete ((vector<short>*)p);
   }
   static void deleteArray_vectorlEshortgR(void *p) {
      delete [] ((vector<short>*)p);
   }
   static void destruct_vectorlEshortgR(void *p) {
      typedef vector<short> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<short>

namespace ROOT {
   static TClass *vectorlETSlimRecoCedarHitgR_Dictionary();
   static void vectorlETSlimRecoCedarHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoCedarHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoCedarHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoCedarHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoCedarHitgR(void *p);
   static void destruct_vectorlETSlimRecoCedarHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoCedarHit>*)
   {
      vector<TSlimRecoCedarHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoCedarHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoCedarHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoCedarHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoCedarHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoCedarHit>) );
      instance.SetNew(&new_vectorlETSlimRecoCedarHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoCedarHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoCedarHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoCedarHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoCedarHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoCedarHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoCedarHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoCedarHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoCedarHit>*)0x0)->GetClass();
      vectorlETSlimRecoCedarHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoCedarHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoCedarHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCedarHit> : new vector<TSlimRecoCedarHit>;
   }
   static void *newArray_vectorlETSlimRecoCedarHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCedarHit>[nElements] : new vector<TSlimRecoCedarHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoCedarHitgR(void *p) {
      delete ((vector<TSlimRecoCedarHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoCedarHitgR(void *p) {
      delete [] ((vector<TSlimRecoCedarHit>*)p);
   }
   static void destruct_vectorlETSlimRecoCedarHitgR(void *p) {
      typedef vector<TSlimRecoCedarHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoCedarHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoCedarCandidategR_Dictionary();
   static void vectorlETSlimRecoCedarCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoCedarCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoCedarCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoCedarCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoCedarCandidategR(void *p);
   static void destruct_vectorlETSlimRecoCedarCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoCedarCandidate>*)
   {
      vector<TSlimRecoCedarCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoCedarCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoCedarCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoCedarCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoCedarCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoCedarCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoCedarCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoCedarCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoCedarCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoCedarCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoCedarCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoCedarCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoCedarCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoCedarCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoCedarCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoCedarCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoCedarCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoCedarCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCedarCandidate> : new vector<TSlimRecoCedarCandidate>;
   }
   static void *newArray_vectorlETSlimRecoCedarCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCedarCandidate>[nElements] : new vector<TSlimRecoCedarCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoCedarCandidategR(void *p) {
      delete ((vector<TSlimRecoCedarCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoCedarCandidategR(void *p) {
      delete [] ((vector<TSlimRecoCedarCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoCedarCandidategR(void *p) {
      typedef vector<TSlimRecoCedarCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoCedarCandidate>

namespace {
  void TriggerDictionaryInitialization_libCedarSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/Cedar/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/Cedar/../../FullReco/Cedar/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/Cedar/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libCedarSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoCedarHit.hh")))  TSlimRecoCedarHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoCedarCandidate;
class TSlimRecoCedarEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libCedarSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOCEDARCANDIDATE_H
#define TSLIMRECOCEDARCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVCandidate.hh"

class TRecoCedarCandidate;

class TSlimRecoCedarCandidate : public TSlimRecoVCandidate {

  public:
    TSlimRecoCedarCandidate();
    explicit TSlimRecoCedarCandidate(TRecoCedarCandidate *candReco);
    virtual ~TSlimRecoCedarCandidate() {};

    // setters for members
    void SetNSectors(Short_t val)  { fNSectors = val;                  }
    void SetTime(Float_t val)      { fTime = val;                      }
    void AddHitIndex(Short_t index){ fHitsIndexes.emplace_back(index); }

    // getters for members
    Short_t GetNSectors()                        const { return fNSectors;           }
    Float_t GetTime()                            const { return fTime;               }
    Short_t GetNHits()                           const { return fHitsIndexes.size(); }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;        }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
  private:
    Short_t fNSectors;
    Float_t fTime;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoCedarCandidate, 1)
};

#endif /* TSLIMRECOCEDARCANDIDATE_H */
#ifndef TRECOCEDAREVENTSLIM_H
#define TRECOCEDAREVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoCedarCandidate.hh"
#include "TSlimRecoCedarHit.hh"

class TRecoCedarEvent;

class TSlimRecoCedarEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoCedarEvent();
    explicit TSlimRecoCedarEvent(TRecoCedarEvent *evReco);
    virtual ~TSlimRecoCedarEvent() {}

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoCedarCandidate c) { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoCedarHit h)             { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                      const { return fHits.size();       }
    std::vector<TSlimRecoCedarHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                          { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                const { return fCandidates.size(); }
    std::vector<TSlimRecoCedarCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)             { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoCedarHit> fHits;
    std::vector<TSlimRecoCedarCandidate> fCandidates;

    ClassDef(TSlimRecoCedarEvent, 1)
};
#endif /* TRECOCEDAREVENTSLIM_H */
#ifndef TSLIMRECOCEDARHIT_H
#define TSLIMRECOCEDARHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoCedarHit;

class TSlimRecoCedarHit : public TSlimRecoVHit
{
public:
    TSlimRecoCedarHit();
    explicit TSlimRecoCedarHit(TRecoCedarHit *hitReco);
    virtual ~TSlimRecoCedarHit() = default;

    // setters for members
    void SetWidth      (Float_t val) { fWidth  = val;    }
    void SetChannelID(Short_t val)   { fChannelID = val; }
    void SetTime(Float_t val)        { fTime = val;      }

    // getters for members
    Float_t GetWidth()       const  { return fWidth;     }
    Short_t GetChannelID()   const  { return fChannelID; }
    Float_t GetTime()        const  { return fTime;      }
    Int_t   GetSectorID()    const;
    Int_t   GetRowID()       const;
    Int_t   GetConeID()      const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Short_t fChannelID;
    Float_t fWidth;
    Float_t fTime;

    ClassDef(TSlimRecoCedarHit, 1)
};

#endif /* TSLIMRECOCEDARHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoCedarCandidate", payloadCode, "@",
"TSlimRecoCedarEvent", payloadCode, "@",
"TSlimRecoCedarHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libCedarSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libCedarSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libCedarSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libCedarSlimPersistency() {
  TriggerDictionaryInitialization_libCedarSlimPersistency_Impl();
}
