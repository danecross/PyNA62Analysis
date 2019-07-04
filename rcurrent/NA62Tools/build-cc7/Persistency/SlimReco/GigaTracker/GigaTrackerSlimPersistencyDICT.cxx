// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME GigaTrackerSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/GigaTracker/include/TSlimRecoGigaTrackerCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/GigaTracker/include/TSlimRecoGigaTrackerEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/GigaTracker/include/TSlimRecoGigaTrackerHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoGigaTrackerCandidate(void *p = 0);
   static void *newArray_TSlimRecoGigaTrackerCandidate(Long_t size, void *p);
   static void delete_TSlimRecoGigaTrackerCandidate(void *p);
   static void deleteArray_TSlimRecoGigaTrackerCandidate(void *p);
   static void destruct_TSlimRecoGigaTrackerCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoGigaTrackerCandidate*)
   {
      ::TSlimRecoGigaTrackerCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoGigaTrackerCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoGigaTrackerCandidate", ::TSlimRecoGigaTrackerCandidate::Class_Version(), "", 16,
                  typeid(::TSlimRecoGigaTrackerCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoGigaTrackerCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoGigaTrackerCandidate) );
      instance.SetNew(&new_TSlimRecoGigaTrackerCandidate);
      instance.SetNewArray(&newArray_TSlimRecoGigaTrackerCandidate);
      instance.SetDelete(&delete_TSlimRecoGigaTrackerCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoGigaTrackerCandidate);
      instance.SetDestructor(&destruct_TSlimRecoGigaTrackerCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoGigaTrackerCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoGigaTrackerCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoGigaTrackerHit(void *p = 0);
   static void *newArray_TSlimRecoGigaTrackerHit(Long_t size, void *p);
   static void delete_TSlimRecoGigaTrackerHit(void *p);
   static void deleteArray_TSlimRecoGigaTrackerHit(void *p);
   static void destruct_TSlimRecoGigaTrackerHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoGigaTrackerHit*)
   {
      ::TSlimRecoGigaTrackerHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoGigaTrackerHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoGigaTrackerHit", ::TSlimRecoGigaTrackerHit::Class_Version(), "TSlimRecoGigaTrackerHit.hh", 10,
                  typeid(::TSlimRecoGigaTrackerHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoGigaTrackerHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoGigaTrackerHit) );
      instance.SetNew(&new_TSlimRecoGigaTrackerHit);
      instance.SetNewArray(&newArray_TSlimRecoGigaTrackerHit);
      instance.SetDelete(&delete_TSlimRecoGigaTrackerHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoGigaTrackerHit);
      instance.SetDestructor(&destruct_TSlimRecoGigaTrackerHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoGigaTrackerHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoGigaTrackerHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoGigaTrackerEvent(void *p = 0);
   static void *newArray_TSlimRecoGigaTrackerEvent(Long_t size, void *p);
   static void delete_TSlimRecoGigaTrackerEvent(void *p);
   static void deleteArray_TSlimRecoGigaTrackerEvent(void *p);
   static void destruct_TSlimRecoGigaTrackerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoGigaTrackerEvent*)
   {
      ::TSlimRecoGigaTrackerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoGigaTrackerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoGigaTrackerEvent", ::TSlimRecoGigaTrackerEvent::Class_Version(), "", 95,
                  typeid(::TSlimRecoGigaTrackerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoGigaTrackerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoGigaTrackerEvent) );
      instance.SetNew(&new_TSlimRecoGigaTrackerEvent);
      instance.SetNewArray(&newArray_TSlimRecoGigaTrackerEvent);
      instance.SetDelete(&delete_TSlimRecoGigaTrackerEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoGigaTrackerEvent);
      instance.SetDestructor(&destruct_TSlimRecoGigaTrackerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoGigaTrackerEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoGigaTrackerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoGigaTrackerCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoGigaTrackerCandidate::Class_Name()
{
   return "TSlimRecoGigaTrackerCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoGigaTrackerCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoGigaTrackerCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoGigaTrackerCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoGigaTrackerCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoGigaTrackerHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoGigaTrackerHit::Class_Name()
{
   return "TSlimRecoGigaTrackerHit";
}

//______________________________________________________________________________
const char *TSlimRecoGigaTrackerHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoGigaTrackerHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoGigaTrackerHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoGigaTrackerHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoGigaTrackerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoGigaTrackerEvent::Class_Name()
{
   return "TSlimRecoGigaTrackerEvent";
}

//______________________________________________________________________________
const char *TSlimRecoGigaTrackerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoGigaTrackerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoGigaTrackerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoGigaTrackerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoGigaTrackerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoGigaTrackerCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoGigaTrackerCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoGigaTrackerCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoGigaTrackerCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoGigaTrackerCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoGigaTrackerCandidate : new ::TSlimRecoGigaTrackerCandidate;
   }
   static void *newArray_TSlimRecoGigaTrackerCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoGigaTrackerCandidate[nElements] : new ::TSlimRecoGigaTrackerCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoGigaTrackerCandidate(void *p) {
      delete ((::TSlimRecoGigaTrackerCandidate*)p);
   }
   static void deleteArray_TSlimRecoGigaTrackerCandidate(void *p) {
      delete [] ((::TSlimRecoGigaTrackerCandidate*)p);
   }
   static void destruct_TSlimRecoGigaTrackerCandidate(void *p) {
      typedef ::TSlimRecoGigaTrackerCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoGigaTrackerCandidate

//______________________________________________________________________________
void TSlimRecoGigaTrackerHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoGigaTrackerHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoGigaTrackerHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoGigaTrackerHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoGigaTrackerHit(void *p) {
      return  p ? new(p) ::TSlimRecoGigaTrackerHit : new ::TSlimRecoGigaTrackerHit;
   }
   static void *newArray_TSlimRecoGigaTrackerHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoGigaTrackerHit[nElements] : new ::TSlimRecoGigaTrackerHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoGigaTrackerHit(void *p) {
      delete ((::TSlimRecoGigaTrackerHit*)p);
   }
   static void deleteArray_TSlimRecoGigaTrackerHit(void *p) {
      delete [] ((::TSlimRecoGigaTrackerHit*)p);
   }
   static void destruct_TSlimRecoGigaTrackerHit(void *p) {
      typedef ::TSlimRecoGigaTrackerHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoGigaTrackerHit

//______________________________________________________________________________
void TSlimRecoGigaTrackerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoGigaTrackerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoGigaTrackerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoGigaTrackerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoGigaTrackerEvent(void *p) {
      return  p ? new(p) ::TSlimRecoGigaTrackerEvent : new ::TSlimRecoGigaTrackerEvent;
   }
   static void *newArray_TSlimRecoGigaTrackerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoGigaTrackerEvent[nElements] : new ::TSlimRecoGigaTrackerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoGigaTrackerEvent(void *p) {
      delete ((::TSlimRecoGigaTrackerEvent*)p);
   }
   static void deleteArray_TSlimRecoGigaTrackerEvent(void *p) {
      delete [] ((::TSlimRecoGigaTrackerEvent*)p);
   }
   static void destruct_TSlimRecoGigaTrackerEvent(void *p) {
      typedef ::TSlimRecoGigaTrackerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoGigaTrackerEvent

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
   static TClass *vectorlETSlimRecoGigaTrackerHitgR_Dictionary();
   static void vectorlETSlimRecoGigaTrackerHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoGigaTrackerHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoGigaTrackerHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoGigaTrackerHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoGigaTrackerHitgR(void *p);
   static void destruct_vectorlETSlimRecoGigaTrackerHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoGigaTrackerHit>*)
   {
      vector<TSlimRecoGigaTrackerHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoGigaTrackerHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoGigaTrackerHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoGigaTrackerHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoGigaTrackerHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoGigaTrackerHit>) );
      instance.SetNew(&new_vectorlETSlimRecoGigaTrackerHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoGigaTrackerHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoGigaTrackerHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoGigaTrackerHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoGigaTrackerHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoGigaTrackerHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoGigaTrackerHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoGigaTrackerHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoGigaTrackerHit>*)0x0)->GetClass();
      vectorlETSlimRecoGigaTrackerHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoGigaTrackerHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoGigaTrackerHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoGigaTrackerHit> : new vector<TSlimRecoGigaTrackerHit>;
   }
   static void *newArray_vectorlETSlimRecoGigaTrackerHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoGigaTrackerHit>[nElements] : new vector<TSlimRecoGigaTrackerHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoGigaTrackerHitgR(void *p) {
      delete ((vector<TSlimRecoGigaTrackerHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoGigaTrackerHitgR(void *p) {
      delete [] ((vector<TSlimRecoGigaTrackerHit>*)p);
   }
   static void destruct_vectorlETSlimRecoGigaTrackerHitgR(void *p) {
      typedef vector<TSlimRecoGigaTrackerHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoGigaTrackerHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoGigaTrackerCandidategR_Dictionary();
   static void vectorlETSlimRecoGigaTrackerCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoGigaTrackerCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoGigaTrackerCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoGigaTrackerCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoGigaTrackerCandidategR(void *p);
   static void destruct_vectorlETSlimRecoGigaTrackerCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoGigaTrackerCandidate>*)
   {
      vector<TSlimRecoGigaTrackerCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoGigaTrackerCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoGigaTrackerCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoGigaTrackerCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoGigaTrackerCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoGigaTrackerCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoGigaTrackerCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoGigaTrackerCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoGigaTrackerCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoGigaTrackerCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoGigaTrackerCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoGigaTrackerCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoGigaTrackerCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoGigaTrackerCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoGigaTrackerCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoGigaTrackerCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoGigaTrackerCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoGigaTrackerCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoGigaTrackerCandidate> : new vector<TSlimRecoGigaTrackerCandidate>;
   }
   static void *newArray_vectorlETSlimRecoGigaTrackerCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoGigaTrackerCandidate>[nElements] : new vector<TSlimRecoGigaTrackerCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoGigaTrackerCandidategR(void *p) {
      delete ((vector<TSlimRecoGigaTrackerCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoGigaTrackerCandidategR(void *p) {
      delete [] ((vector<TSlimRecoGigaTrackerCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoGigaTrackerCandidategR(void *p) {
      typedef vector<TSlimRecoGigaTrackerCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoGigaTrackerCandidate>

namespace {
  void TriggerDictionaryInitialization_libGigaTrackerSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/GigaTracker/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/GigaTracker/../../FullReco/GigaTracker/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/GigaTracker/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libGigaTrackerSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoGigaTrackerHit.hh")))  TSlimRecoGigaTrackerHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoGigaTrackerCandidate;
class TSlimRecoGigaTrackerEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libGigaTrackerSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOGIGATRACKERCANDIDATE_H
#define TSLIMRECOGIGATRACKERCANDIDATE_H

#include <RtypesCore.h> // ROOT data types, e.g. Float_t
#include <vector>
#include "TVector3.h"
#include "TSlimRecoVCandidate.hh"

class TRecoGigaTrackerCandidate;

class TSlimRecoGigaTrackerCandidate : public TSlimRecoVCandidate
{
public:
  TSlimRecoGigaTrackerCandidate() = default;
  explicit TSlimRecoGigaTrackerCandidate(TRecoGigaTrackerCandidate *candReco);
  virtual ~TSlimRecoGigaTrackerCandidate() = default;
  // setters for members
  void SetTime(Float_t hittime)                           { fTime                 = hittime;     }
  void SetTimeError(Float_t timeerror)                    { fTimeError            = timeerror;   }
  void SetMomentum(TVector3 momentum);
  void SetChi2(Float_t chi2)                              { fChi2                 = chi2;        }
  void SetChi2X(Float_t chi2x)                            { fChi2X                = chi2x;       }
  void SetChi2Y(Float_t chi2y)                            { fChi2Y                = chi2y;       }
  void SetChi2Time(Float_t chi2time)                      { fChi2Time             = chi2time;    }
  void SetTimeStation(Int_t station, Float_t timestation) { fTimeStation[station] = timestation; }
  void SetPosition(Int_t station, TVector3 position);
  void SetCovariance(Int_t i, Int_t j, Double_t value);
  void AddHitIndex(Short_t index)                         { fHitsIndexes.emplace_back(index);    }

  // getters for members
  Float_t GetTime()                            const { return fTime;                 }
  Float_t GetTimeError()                       const { return fTimeError;            }
  TVector3 GetMomentum()                       const;
  Float_t GetChi2()                            const { return fChi2;                 }
  Float_t GetChi2X()                           const { return fChi2X;                }
  Float_t GetChi2Y()                           const { return fChi2Y;                }
  Float_t GetChi2Time()                        const { return fChi2Time;             }
  Float_t GetTimeStation(Int_t station)        const { return fTimeStation[station]; }
  TVector3 GetPosition(Int_t station)          const;
  Int_t GetNHits()                             const { return fHitsIndexes.size();   }
  const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;          }
  Int_t GetType()                              const { return 123;                   }

  Double_t GetCovariance(Int_t i, Int_t j)     const;

  // conversion functions
  virtual void FromReco(TRecoVCandidate *candReco);
  virtual void ToReco(TRecoVCandidate *candReco);

private:
  // utility function for index conversion:
  static Int_t GetArrayIndexCovarianceMatrix(Int_t i, Int_t j);

  static constexpr Int_t kCovarianceDimension = 15;
  Float_t fTime           = 0;
  Float_t fTimeError      = 0;
  Float_t fMomentumX      = 0;
  Float_t fMomentumY      = 0;
  Float_t fMomentumZ      = 0;
  Float_t fChi2           = 0;
  Float_t fChi2X          = 0;
  Float_t fChi2Y          = 0;
  Float_t fChi2Time       = 0;
  Float_t fTimeStation[3] = {0, 0, 0};
  Float_t fPositionX[3]   = {0, 0, 0};
  Float_t fPositionY[3]   = {0, 0, 0};
  // covariance values should be stored in std::array<Double_t, 15>
  // however this is not supported as of ROOT version 6.08 (SLC6)
  // therefore C-style array is used
  Double_t fCovariance[kCovarianceDimension] = {0};
  std::vector<Short_t> fHitsIndexes;

  ClassDef(TSlimRecoGigaTrackerCandidate, 1)
};

#endif /* TSLIMRECoGigaTrackerCANDIDATE_H */
#ifndef TRECOGIGATRACKEREVENTSLIM_H
#define TRECOGIGATRACKEREVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoGigaTrackerCandidate.hh"
#include "TSlimRecoGigaTrackerHit.hh"

class TRecoGigaTrackerEvent;

class TSlimRecoGigaTrackerEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoGigaTrackerEvent() = default;
    explicit TSlimRecoGigaTrackerEvent(TRecoGigaTrackerEvent *evReco);
    virtual ~TSlimRecoGigaTrackerEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddHit(TSlimRecoGigaTrackerHit h)             { fHits.emplace_back(std::move(h));       }
    void AddCandidate(TSlimRecoGigaTrackerCandidate c) { fCandidates.emplace_back(std::move(c)); }

    Int_t GetNHits()                                            const { return fHits.size();       }
    std::vector<TSlimRecoGigaTrackerHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                                { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                      const { return fCandidates.size(); }
    std::vector<TSlimRecoGigaTrackerCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                   { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoGigaTrackerHit> fHits;
    std::vector<TSlimRecoGigaTrackerCandidate> fCandidates;

    ClassDef(TSlimRecoGigaTrackerEvent, 1)
};

#endif /* TRECOGigaTrackerEVENTSLIM_H */
#ifndef TSLIMRECOGIGATRACKERHIT_H
#define TSLIMRECOGIGATRACKERHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include "TVector3.h"
#include "TSlimRecoVHit.hh"

class TRecoGigaTrackerHit;

class TSlimRecoGigaTrackerHit : public TSlimRecoVHit{

public:
  TSlimRecoGigaTrackerHit() = default;
  explicit TSlimRecoGigaTrackerHit(TRecoGigaTrackerHit *hitReco);
  virtual ~TSlimRecoGigaTrackerHit() = default;

  // setters for members
  void SetIsPileUpHit(Bool_t ispileup)  { fIsPileUpHit = ispileup;       }
  void SetChannelID(Int_t channelID)    { fChannelID   = channelID;      }
  void SetRawTime(Float_t rawtime)      { fRawTime     = rawtime;        }
  void SetTime(Float_t time)            { fTime        = time;           }
  void SetToT(Float_t tot)              { fToT         = tot;            }
  void SetPosition(TVector3 position);

  // getters for members
  Bool_t   GetIsPileUpHit() const { return fIsPileUpHit;            }
  Int_t    GetChannelID()   const { return fChannelID;              }
  Float_t  GetRawTime()     const { return fRawTime;                }
  Float_t  GetTime()        const { return fTime;                   }
  Float_t  GetToT()         const { return fToT;                    }
  TVector3 GetPosition()    const;
  Int_t    GetStationNo()   const;
  Int_t    GetChipID()      const;
  Int_t    GetChipPixelID() const;
  UInt_t   GetColumn()      const;
  UInt_t   GetRow()         const;


  // conversion functions
  virtual void FromReco(TRecoVHit *hitReco);
  virtual void ToReco(TRecoVHit *hitReco);
private:

  Bool_t  fIsPileUpHit = 0;
  Int_t   fChannelID   = 0;
  Float_t fRawTime     = 0.;
  Float_t fTime        = 0.;
  Float_t fToT         = 0.;
  Float_t fPositionX   = 0.;
  Float_t fPositionY   = 0.;

  ClassDef(TSlimRecoGigaTrackerHit, 1)
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoGigaTrackerCandidate", payloadCode, "@",
"TSlimRecoGigaTrackerEvent", payloadCode, "@",
"TSlimRecoGigaTrackerHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libGigaTrackerSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libGigaTrackerSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libGigaTrackerSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libGigaTrackerSlimPersistency() {
  TriggerDictionaryInitialization_libGigaTrackerSlimPersistency_Impl();
}
