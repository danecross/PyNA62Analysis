// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME CHODSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHOD/include/TSlimRecoCHODCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHOD/include/TSlimRecoCHODEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHOD/include/TSlimRecoCHODHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoCHODCandidate(void *p = 0);
   static void *newArray_TSlimRecoCHODCandidate(Long_t size, void *p);
   static void delete_TSlimRecoCHODCandidate(void *p);
   static void deleteArray_TSlimRecoCHODCandidate(void *p);
   static void destruct_TSlimRecoCHODCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCHODCandidate*)
   {
      ::TSlimRecoCHODCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCHODCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCHODCandidate", ::TSlimRecoCHODCandidate::Class_Version(), "", 17,
                  typeid(::TSlimRecoCHODCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCHODCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCHODCandidate) );
      instance.SetNew(&new_TSlimRecoCHODCandidate);
      instance.SetNewArray(&newArray_TSlimRecoCHODCandidate);
      instance.SetDelete(&delete_TSlimRecoCHODCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCHODCandidate);
      instance.SetDestructor(&destruct_TSlimRecoCHODCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCHODCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCHODCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCHODCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoCHODHit(void *p = 0);
   static void *newArray_TSlimRecoCHODHit(Long_t size, void *p);
   static void delete_TSlimRecoCHODHit(void *p);
   static void deleteArray_TSlimRecoCHODHit(void *p);
   static void destruct_TSlimRecoCHODHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCHODHit*)
   {
      ::TSlimRecoCHODHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCHODHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCHODHit", ::TSlimRecoCHODHit::Class_Version(), "TSlimRecoCHODHit.hh", 10,
                  typeid(::TSlimRecoCHODHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCHODHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCHODHit) );
      instance.SetNew(&new_TSlimRecoCHODHit);
      instance.SetNewArray(&newArray_TSlimRecoCHODHit);
      instance.SetDelete(&delete_TSlimRecoCHODHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCHODHit);
      instance.SetDestructor(&destruct_TSlimRecoCHODHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCHODHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCHODHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCHODHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoCHODEvent(void *p = 0);
   static void *newArray_TSlimRecoCHODEvent(Long_t size, void *p);
   static void delete_TSlimRecoCHODEvent(void *p);
   static void deleteArray_TSlimRecoCHODEvent(void *p);
   static void destruct_TSlimRecoCHODEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCHODEvent*)
   {
      ::TSlimRecoCHODEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCHODEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCHODEvent", ::TSlimRecoCHODEvent::Class_Version(), "", 65,
                  typeid(::TSlimRecoCHODEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCHODEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCHODEvent) );
      instance.SetNew(&new_TSlimRecoCHODEvent);
      instance.SetNewArray(&newArray_TSlimRecoCHODEvent);
      instance.SetDelete(&delete_TSlimRecoCHODEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCHODEvent);
      instance.SetDestructor(&destruct_TSlimRecoCHODEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCHODEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCHODEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCHODEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCHODCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCHODCandidate::Class_Name()
{
   return "TSlimRecoCHODCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoCHODCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCHODCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCHODCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCHODCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCHODHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCHODHit::Class_Name()
{
   return "TSlimRecoCHODHit";
}

//______________________________________________________________________________
const char *TSlimRecoCHODHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCHODHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCHODHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCHODHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCHODEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCHODEvent::Class_Name()
{
   return "TSlimRecoCHODEvent";
}

//______________________________________________________________________________
const char *TSlimRecoCHODEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCHODEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCHODEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCHODEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHODEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoCHODCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCHODCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCHODCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCHODCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCHODCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoCHODCandidate : new ::TSlimRecoCHODCandidate;
   }
   static void *newArray_TSlimRecoCHODCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCHODCandidate[nElements] : new ::TSlimRecoCHODCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCHODCandidate(void *p) {
      delete ((::TSlimRecoCHODCandidate*)p);
   }
   static void deleteArray_TSlimRecoCHODCandidate(void *p) {
      delete [] ((::TSlimRecoCHODCandidate*)p);
   }
   static void destruct_TSlimRecoCHODCandidate(void *p) {
      typedef ::TSlimRecoCHODCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCHODCandidate

//______________________________________________________________________________
void TSlimRecoCHODHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCHODHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCHODHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCHODHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCHODHit(void *p) {
      return  p ? new(p) ::TSlimRecoCHODHit : new ::TSlimRecoCHODHit;
   }
   static void *newArray_TSlimRecoCHODHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCHODHit[nElements] : new ::TSlimRecoCHODHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCHODHit(void *p) {
      delete ((::TSlimRecoCHODHit*)p);
   }
   static void deleteArray_TSlimRecoCHODHit(void *p) {
      delete [] ((::TSlimRecoCHODHit*)p);
   }
   static void destruct_TSlimRecoCHODHit(void *p) {
      typedef ::TSlimRecoCHODHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCHODHit

//______________________________________________________________________________
void TSlimRecoCHODEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCHODEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCHODEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCHODEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCHODEvent(void *p) {
      return  p ? new(p) ::TSlimRecoCHODEvent : new ::TSlimRecoCHODEvent;
   }
   static void *newArray_TSlimRecoCHODEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCHODEvent[nElements] : new ::TSlimRecoCHODEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCHODEvent(void *p) {
      delete ((::TSlimRecoCHODEvent*)p);
   }
   static void deleteArray_TSlimRecoCHODEvent(void *p) {
      delete [] ((::TSlimRecoCHODEvent*)p);
   }
   static void destruct_TSlimRecoCHODEvent(void *p) {
      typedef ::TSlimRecoCHODEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCHODEvent

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
   static TClass *vectorlETSlimRecoCHODHitgR_Dictionary();
   static void vectorlETSlimRecoCHODHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoCHODHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoCHODHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoCHODHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoCHODHitgR(void *p);
   static void destruct_vectorlETSlimRecoCHODHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoCHODHit>*)
   {
      vector<TSlimRecoCHODHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoCHODHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoCHODHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoCHODHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoCHODHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoCHODHit>) );
      instance.SetNew(&new_vectorlETSlimRecoCHODHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoCHODHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoCHODHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoCHODHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoCHODHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoCHODHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoCHODHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoCHODHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoCHODHit>*)0x0)->GetClass();
      vectorlETSlimRecoCHODHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoCHODHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoCHODHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHODHit> : new vector<TSlimRecoCHODHit>;
   }
   static void *newArray_vectorlETSlimRecoCHODHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHODHit>[nElements] : new vector<TSlimRecoCHODHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoCHODHitgR(void *p) {
      delete ((vector<TSlimRecoCHODHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoCHODHitgR(void *p) {
      delete [] ((vector<TSlimRecoCHODHit>*)p);
   }
   static void destruct_vectorlETSlimRecoCHODHitgR(void *p) {
      typedef vector<TSlimRecoCHODHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoCHODHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoCHODCandidategR_Dictionary();
   static void vectorlETSlimRecoCHODCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoCHODCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoCHODCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoCHODCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoCHODCandidategR(void *p);
   static void destruct_vectorlETSlimRecoCHODCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoCHODCandidate>*)
   {
      vector<TSlimRecoCHODCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoCHODCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoCHODCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoCHODCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoCHODCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoCHODCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoCHODCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoCHODCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoCHODCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoCHODCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoCHODCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoCHODCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoCHODCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoCHODCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoCHODCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoCHODCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoCHODCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoCHODCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHODCandidate> : new vector<TSlimRecoCHODCandidate>;
   }
   static void *newArray_vectorlETSlimRecoCHODCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHODCandidate>[nElements] : new vector<TSlimRecoCHODCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoCHODCandidategR(void *p) {
      delete ((vector<TSlimRecoCHODCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoCHODCandidategR(void *p) {
      delete [] ((vector<TSlimRecoCHODCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoCHODCandidategR(void *p) {
      typedef vector<TSlimRecoCHODCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoCHODCandidate>

namespace {
  void TriggerDictionaryInitialization_libCHODSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHOD/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHOD/../../FullReco/CHOD/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/CHOD/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libCHODSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoCHODHit.hh")))  TSlimRecoCHODHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoCHODCandidate;
class TSlimRecoCHODEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libCHODSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOCHODCANDIDATE_H
#define TSLIMRECOCHODCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>
#include "TVector2.h"

#include "TSlimRecoVCandidate.hh"

class TRecoCHODCandidate;

class TSlimRecoCHODCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoCHODCandidate() = default;
    explicit TSlimRecoCHODCandidate(TRecoCHODCandidate *candReco);
    virtual ~TSlimRecoCHODCandidate() = default;

    // setters for members
    void SetTime(Float_t time)          { fTime = time;                                         }
    void SetPosition(TVector2 position) { fPositionX = position.X(); fPositionY = position.Y(); }
    void SetNHitPairs(Int_t nhitpairs)  { fNHitPairs = nhitpairs;                               }
    void AddHitIndex(Short_t index)     { fHitsIndexes.emplace_back(index);                     }

    // getters for members
    Int_t    GetNHitsPairs()                     const { return fNHitPairs;                          }
    Float_t  GetTime()                           const { return fTime;                               }
    TVector2 GetPosition()                       const { return TVector2(fPositionX, fPositionY);    }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;                        }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:

    Int_t   fNHitPairs   = -9999;
    Float_t fTime        = -9999;
    Float_t fPositionX   = -9999;
    Float_t fPositionY   = -9999;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoCHODCandidate, 1)
};


#endif /* TSLIMRECOCHODCANDIDATE_H */
#ifndef TRECOCHODEVENTSLIM_H
#define TRECOCHODEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoCHODCandidate.hh"
#include "TSlimRecoCHODHit.hh"

class TRecoCHODEvent;

class TSlimRecoCHODEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoCHODEvent() = default;
    explicit TSlimRecoCHODEvent(TRecoCHODEvent *evReco);
    virtual ~TSlimRecoCHODEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoCHODCandidate c)     { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoCHODHit h)                 { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                         const { return fHits.size();           }
    std::vector<TSlimRecoCHODHit>& GetHits()                       { return fHits;                  }
    TSlimRecoVHit* GetHit(UInt_t iHit)                             { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                   const { return fCandidates.size();     }
    std::vector<TSlimRecoCHODCandidate>& GetCandidates()           { return fCandidates;            }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }
    Int_t GetNQuadrants()                                    const;

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoCHODHit> fHits;
    std::vector<TSlimRecoCHODCandidate> fCandidates;

    ClassDef(TSlimRecoCHODEvent, 1)
};

#endif /* TRECOCHODEVENTSLIM_H */
#ifndef TSLIMRECOCHODHIT_H
#define TSLIMRECOCHODHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoCHODHit;

class TSlimRecoCHODHit : public TSlimRecoVHit
{
public:
    TSlimRecoCHODHit() = default;
    explicit TSlimRecoCHODHit(TRecoCHODHit *hitReco);
    virtual ~TSlimRecoCHODHit() = default;

    // setters for members
    void SetChannelID(Int_t val)   { fChannelID = val; }
    void SetTime(Float_t val)      { fTime = val;      }
    void SetTimeWidth(Float_t val) { fTimeWidth = val; }

    // getters for members
    Int_t   GetPlaneID()    const;
    Int_t   GetQuadrantID() const;
    Int_t   GetCounterID()  const;
    Int_t   GetChannelID()  const { return fChannelID; }
    Float_t GetTime()       const { return fTime;      }
    Float_t GetTimeWidth()  const { return fTimeWidth; }

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:

    Int_t   fChannelID = -1;
    Float_t fTime      = -999;
    Float_t fTimeWidth = -999;

    ClassDef(TSlimRecoCHODHit, 1)
};

#endif /* TSLIMRECOCHODHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoCHODCandidate", payloadCode, "@",
"TSlimRecoCHODEvent", payloadCode, "@",
"TSlimRecoCHODHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libCHODSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libCHODSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libCHODSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libCHODSlimPersistency() {
  TriggerDictionaryInitialization_libCHODSlimPersistency_Impl();
}
