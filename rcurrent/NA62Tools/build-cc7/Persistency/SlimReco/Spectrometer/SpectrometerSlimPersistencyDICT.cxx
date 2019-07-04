// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME SpectrometerSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Spectrometer/include/TSlimRecoSpectrometerCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Spectrometer/include/TSlimRecoSpectrometerEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Spectrometer/include/TSlimRecoSpectrometerHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoSpectrometerCandidate(void *p = 0);
   static void *newArray_TSlimRecoSpectrometerCandidate(Long_t size, void *p);
   static void delete_TSlimRecoSpectrometerCandidate(void *p);
   static void deleteArray_TSlimRecoSpectrometerCandidate(void *p);
   static void destruct_TSlimRecoSpectrometerCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSpectrometerCandidate*)
   {
      ::TSlimRecoSpectrometerCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSpectrometerCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSpectrometerCandidate", ::TSlimRecoSpectrometerCandidate::Class_Version(), "", 17,
                  typeid(::TSlimRecoSpectrometerCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSpectrometerCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSpectrometerCandidate) );
      instance.SetNew(&new_TSlimRecoSpectrometerCandidate);
      instance.SetNewArray(&newArray_TSlimRecoSpectrometerCandidate);
      instance.SetDelete(&delete_TSlimRecoSpectrometerCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSpectrometerCandidate);
      instance.SetDestructor(&destruct_TSlimRecoSpectrometerCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSpectrometerCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSpectrometerCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoSpectrometerHit(void *p = 0);
   static void *newArray_TSlimRecoSpectrometerHit(Long_t size, void *p);
   static void delete_TSlimRecoSpectrometerHit(void *p);
   static void deleteArray_TSlimRecoSpectrometerHit(void *p);
   static void destruct_TSlimRecoSpectrometerHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSpectrometerHit*)
   {
      ::TSlimRecoSpectrometerHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSpectrometerHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSpectrometerHit", ::TSlimRecoSpectrometerHit::Class_Version(), "TSlimRecoSpectrometerHit.hh", 10,
                  typeid(::TSlimRecoSpectrometerHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSpectrometerHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSpectrometerHit) );
      instance.SetNew(&new_TSlimRecoSpectrometerHit);
      instance.SetNewArray(&newArray_TSlimRecoSpectrometerHit);
      instance.SetDelete(&delete_TSlimRecoSpectrometerHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSpectrometerHit);
      instance.SetDestructor(&destruct_TSlimRecoSpectrometerHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSpectrometerHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSpectrometerHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoSpectrometerEvent(void *p = 0);
   static void *newArray_TSlimRecoSpectrometerEvent(Long_t size, void *p);
   static void delete_TSlimRecoSpectrometerEvent(void *p);
   static void deleteArray_TSlimRecoSpectrometerEvent(void *p);
   static void destruct_TSlimRecoSpectrometerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSpectrometerEvent*)
   {
      ::TSlimRecoSpectrometerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSpectrometerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSpectrometerEvent", ::TSlimRecoSpectrometerEvent::Class_Version(), "", 117,
                  typeid(::TSlimRecoSpectrometerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSpectrometerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSpectrometerEvent) );
      instance.SetNew(&new_TSlimRecoSpectrometerEvent);
      instance.SetNewArray(&newArray_TSlimRecoSpectrometerEvent);
      instance.SetDelete(&delete_TSlimRecoSpectrometerEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSpectrometerEvent);
      instance.SetDestructor(&destruct_TSlimRecoSpectrometerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSpectrometerEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSpectrometerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSpectrometerCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSpectrometerCandidate::Class_Name()
{
   return "TSlimRecoSpectrometerCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoSpectrometerCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSpectrometerCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSpectrometerCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSpectrometerCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSpectrometerHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSpectrometerHit::Class_Name()
{
   return "TSlimRecoSpectrometerHit";
}

//______________________________________________________________________________
const char *TSlimRecoSpectrometerHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSpectrometerHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSpectrometerHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSpectrometerHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSpectrometerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSpectrometerEvent::Class_Name()
{
   return "TSlimRecoSpectrometerEvent";
}

//______________________________________________________________________________
const char *TSlimRecoSpectrometerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSpectrometerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSpectrometerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSpectrometerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSpectrometerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoSpectrometerCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSpectrometerCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSpectrometerCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSpectrometerCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSpectrometerCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoSpectrometerCandidate : new ::TSlimRecoSpectrometerCandidate;
   }
   static void *newArray_TSlimRecoSpectrometerCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSpectrometerCandidate[nElements] : new ::TSlimRecoSpectrometerCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSpectrometerCandidate(void *p) {
      delete ((::TSlimRecoSpectrometerCandidate*)p);
   }
   static void deleteArray_TSlimRecoSpectrometerCandidate(void *p) {
      delete [] ((::TSlimRecoSpectrometerCandidate*)p);
   }
   static void destruct_TSlimRecoSpectrometerCandidate(void *p) {
      typedef ::TSlimRecoSpectrometerCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSpectrometerCandidate

//______________________________________________________________________________
void TSlimRecoSpectrometerHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSpectrometerHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSpectrometerHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSpectrometerHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSpectrometerHit(void *p) {
      return  p ? new(p) ::TSlimRecoSpectrometerHit : new ::TSlimRecoSpectrometerHit;
   }
   static void *newArray_TSlimRecoSpectrometerHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSpectrometerHit[nElements] : new ::TSlimRecoSpectrometerHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSpectrometerHit(void *p) {
      delete ((::TSlimRecoSpectrometerHit*)p);
   }
   static void deleteArray_TSlimRecoSpectrometerHit(void *p) {
      delete [] ((::TSlimRecoSpectrometerHit*)p);
   }
   static void destruct_TSlimRecoSpectrometerHit(void *p) {
      typedef ::TSlimRecoSpectrometerHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSpectrometerHit

//______________________________________________________________________________
void TSlimRecoSpectrometerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSpectrometerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSpectrometerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSpectrometerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSpectrometerEvent(void *p) {
      return  p ? new(p) ::TSlimRecoSpectrometerEvent : new ::TSlimRecoSpectrometerEvent;
   }
   static void *newArray_TSlimRecoSpectrometerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSpectrometerEvent[nElements] : new ::TSlimRecoSpectrometerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSpectrometerEvent(void *p) {
      delete ((::TSlimRecoSpectrometerEvent*)p);
   }
   static void deleteArray_TSlimRecoSpectrometerEvent(void *p) {
      delete [] ((::TSlimRecoSpectrometerEvent*)p);
   }
   static void destruct_TSlimRecoSpectrometerEvent(void *p) {
      typedef ::TSlimRecoSpectrometerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSpectrometerEvent

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
   static TClass *vectorlETSlimRecoSpectrometerHitgR_Dictionary();
   static void vectorlETSlimRecoSpectrometerHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoSpectrometerHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoSpectrometerHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoSpectrometerHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoSpectrometerHitgR(void *p);
   static void destruct_vectorlETSlimRecoSpectrometerHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoSpectrometerHit>*)
   {
      vector<TSlimRecoSpectrometerHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoSpectrometerHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoSpectrometerHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoSpectrometerHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoSpectrometerHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoSpectrometerHit>) );
      instance.SetNew(&new_vectorlETSlimRecoSpectrometerHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoSpectrometerHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoSpectrometerHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoSpectrometerHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoSpectrometerHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoSpectrometerHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoSpectrometerHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoSpectrometerHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoSpectrometerHit>*)0x0)->GetClass();
      vectorlETSlimRecoSpectrometerHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoSpectrometerHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoSpectrometerHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSpectrometerHit> : new vector<TSlimRecoSpectrometerHit>;
   }
   static void *newArray_vectorlETSlimRecoSpectrometerHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSpectrometerHit>[nElements] : new vector<TSlimRecoSpectrometerHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoSpectrometerHitgR(void *p) {
      delete ((vector<TSlimRecoSpectrometerHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoSpectrometerHitgR(void *p) {
      delete [] ((vector<TSlimRecoSpectrometerHit>*)p);
   }
   static void destruct_vectorlETSlimRecoSpectrometerHitgR(void *p) {
      typedef vector<TSlimRecoSpectrometerHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoSpectrometerHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoSpectrometerCandidategR_Dictionary();
   static void vectorlETSlimRecoSpectrometerCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoSpectrometerCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoSpectrometerCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoSpectrometerCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoSpectrometerCandidategR(void *p);
   static void destruct_vectorlETSlimRecoSpectrometerCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoSpectrometerCandidate>*)
   {
      vector<TSlimRecoSpectrometerCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoSpectrometerCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoSpectrometerCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoSpectrometerCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoSpectrometerCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoSpectrometerCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoSpectrometerCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoSpectrometerCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoSpectrometerCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoSpectrometerCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoSpectrometerCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoSpectrometerCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoSpectrometerCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoSpectrometerCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoSpectrometerCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoSpectrometerCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoSpectrometerCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoSpectrometerCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSpectrometerCandidate> : new vector<TSlimRecoSpectrometerCandidate>;
   }
   static void *newArray_vectorlETSlimRecoSpectrometerCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSpectrometerCandidate>[nElements] : new vector<TSlimRecoSpectrometerCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoSpectrometerCandidategR(void *p) {
      delete ((vector<TSlimRecoSpectrometerCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoSpectrometerCandidategR(void *p) {
      delete [] ((vector<TSlimRecoSpectrometerCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoSpectrometerCandidategR(void *p) {
      typedef vector<TSlimRecoSpectrometerCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoSpectrometerCandidate>

namespace {
  void TriggerDictionaryInitialization_libSpectrometerSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Spectrometer/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Spectrometer/../../FullReco/Spectrometer/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Spectrometer/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libSpectrometerSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoSpectrometerHit.hh")))  TSlimRecoSpectrometerHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoSpectrometerCandidate;
class TSlimRecoSpectrometerEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libSpectrometerSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOSPECTROMETERCANDIDATE_H
#define TSLIMRECOSPECTROMETERCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>
#include <TVector3.h>

#include "TSlimRecoVCandidate.hh"

class TRecoSpectrometerCandidate;

class TSlimRecoSpectrometerCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoSpectrometerCandidate() = default;
    explicit TSlimRecoSpectrometerCandidate(TRecoSpectrometerCandidate *candReco);
    virtual ~TSlimRecoSpectrometerCandidate() = default;

    // setters for members
    void SetTime(Float_t time)                       { fTime = time;                      }
    void SetMomentum(Double_t momentum)              { fMomentum = momentum;              }
    void SetMomentumBeforeFit(Double_t momentum)     { fMomentumBeforeFit = momentum;     }
    void SetCharge(Short_t charge)                   { fCharge = charge;                  }
    void SetNChambers(Short_t nchambers)             { fNChambers = nchambers;            }
    void SetMissingChamberID(Short_t ch)             { fMissingChamberID = ch;            }
    void SetSlopeXBeforeMagnet(Double_t slope)       { fSlopeXBeforeMagnet = slope;       }
    void SetSlopeYBeforeMagnet(Double_t slope)       { fSlopeYBeforeMagnet = slope;       }
    void SetPositionXBeforeMagnet(Double_t position) { fPositionXBeforeMagnet = position; }
    void SetPositionYBeforeMagnet(Double_t position) { fPositionYBeforeMagnet = position; }
    void SetSlopeXAfterMagnet(Double_t slope)        { fSlopeXAfterMagnet = slope;        }
    void SetSlopeYAfterMagnet(Double_t slope)        { fSlopeYAfterMagnet = slope;        }
    void SetPositionXAfterMagnet(Double_t position)  { fPositionXAfterMagnet = position;  }
    void SetPositionYAfterMagnet(Double_t position)  { fPositionYAfterMagnet = position;  }
    void SetChi2(Float_t chi2)                       { fChi2 = chi2;                      }
    void SetLeadingTime(Float_t leadingTime)         { fLeadingTime = leadingTime;        }
    void AddHitIndex(Short_t index)                  { fHitsIndexes.emplace_back(index);  }
    void SetCovariance(Int_t i, Int_t j, Double_t value);

    // getters for members
    Float_t GetTime()                            const { return fTime;                  }
    Float_t GetChi2()                            const { return fChi2;                  }
    Float_t GetLeadingTime()                     const { return fLeadingTime;           }
    Double_t GetMomentum()                       const { return fMomentum;              }
    Double_t GetMomentumBeforeFit()              const { return fMomentumBeforeFit;     }
    Short_t GetCharge()                          const { return fCharge;                }
    Short_t GetNChambers()                       const { return fNChambers;             }
    Short_t GetMissingChamberID()                const { return fMissingChamberID;      }
    Double_t GetSlopeXBeforeMagnet()             const { return fSlopeXBeforeMagnet;    }
    Double_t GetSlopeYBeforeMagnet()             const { return fSlopeYBeforeMagnet;    }
    Double_t GetPositionXBeforeMagnet()          const { return fPositionXBeforeMagnet; }
    Double_t GetPositionYBeforeMagnet()          const { return fPositionYBeforeMagnet; }
    Double_t GetSlopeXAfterMagnet()              const { return fSlopeXAfterMagnet;     }
    Double_t GetSlopeYAfterMagnet()              const { return fSlopeYAfterMagnet;     }
    Double_t GetPositionXAfterMagnet()           const { return fPositionXAfterMagnet;  }
    Double_t GetPositionYAfterMagnet()           const { return fPositionYAfterMagnet;  }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;           }
    TVector3 GetPositionBeforeMagnet()           const;
    TVector3 GetPositionAfterMagnet()            const;
    TVector3 GetThreeMomentumBeforeMagnet()      const;
    TVector3 GetThreeMomentumAfterMagnet()       const;

    Double_t GetCovariance(Int_t i, Int_t j)     const;

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:
    // utility function for index conversion:
    static Int_t GetArrayIndexCovarianceMatrix(Int_t i, Int_t j);

    static constexpr Int_t kCovarianceDimension = 15;
    Short_t  fCharge                = -9999;
    Short_t  fNChambers             = -9999; ///< N(chambers) used to reconstruct the candidate
    Short_t  fMissingChamberID      = -9999; ///< ID of the missing chamber for 3-chamber candidates, -1 otherwise
    Float_t  fTime                  = -9999;
    Float_t  fChi2                  = -9999;
    Float_t  fLeadingTime           = -9999;
    Double_t fMomentum              = -9999;
    Double_t fMomentumBeforeFit     = -9999;
    Double_t fSlopeXBeforeMagnet    = -9999;
    Double_t fSlopeYBeforeMagnet    = -9999;
    Double_t fPositionXBeforeMagnet = -9999;
    Double_t fPositionYBeforeMagnet = -9999;
    Double_t fSlopeXAfterMagnet     = -9999;
    Double_t fSlopeYAfterMagnet     = -9999;
    Double_t fPositionXAfterMagnet  = -9999;
    Double_t fPositionYAfterMagnet  = -9999;
    // covariance values should be stored in std::array<Double_t, 15>
    // however this is not supported as of ROOT version 6.08 (SLC6)
    // therefore C-style array is used
    Double_t fCovariance[kCovarianceDimension] = {0};
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoSpectrometerCandidate, 1)
};


#endif /* TSLIMRECOSPECTROMETERCANDIDATE_H */
#ifndef TRECOSPECTROMETEREVENTSLIM_H
#define TRECOSPECTROMETEREVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoSpectrometerCandidate.hh"
#include "TSlimRecoSpectrometerHit.hh"

class TRecoSpectrometerEvent;

class TSlimRecoSpectrometerEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoSpectrometerEvent() = default;
    explicit TSlimRecoSpectrometerEvent(TRecoSpectrometerEvent *evReco);
    virtual ~TSlimRecoSpectrometerEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoSpectrometerCandidate c) { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoSpectrometerHit h)             { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                             const { return fHits.size();       }
    std::vector<TSlimRecoSpectrometerHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                                 { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                       const { return fCandidates.size(); }
    std::vector<TSlimRecoSpectrometerCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                    { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoSpectrometerHit> fHits;
    std::vector<TSlimRecoSpectrometerCandidate> fCandidates;

    ClassDef(TSlimRecoSpectrometerEvent, 1)
};

#endif /* TRECOSPECTROMETEREVENTSLIM_H */
#ifndef TSLIMRECOSPECTROMETERHIT_H
#define TSLIMRECOSPECTROMETERHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoSpectrometerHit;

class TSlimRecoSpectrometerHit : public TSlimRecoVHit
{
public:
    TSlimRecoSpectrometerHit() = default;
    explicit TSlimRecoSpectrometerHit(TRecoSpectrometerHit *hitReco);
    virtual ~TSlimRecoSpectrometerHit() = default;

    // setters for members
    void SetChannelID(Short_t channelID) { fChannelID = channelID; }
    void SetTime(Float_t time)           { fTime = time;           }
    void SetTimeWidth(Float_t width)     { fTimeWidth = width;     }
    void SetWireDistance(Float_t dist)   { fWireDistance = dist;   }

    // getters for members
    Short_t GetChannelID()    const { return fChannelID;    }
    Float_t GetTime()         const { return fTime;         }
    Float_t GetTimeWidth()    const { return fTimeWidth;    }
    Float_t GetWireDistance() const { return fWireDistance; }

    // getters for variables from Standard Persistency interface
    Int_t GetChamberID()    const { return fChannelID/1952;             }
    Int_t GetViewID()       const { return (fChannelID%1952)/488;       }
    Int_t GetHalfViewID()   const { return (fChannelID%488)/244;        }
    Int_t GetPlaneID()      const { return (fChannelID%244)/122;        }
    Int_t GetStrawID()      const { return fChannelID%122;              }
    Int_t GetEdgeStatus()   const { return (Int_t)(fTimeWidth >= -500); }
    Double_t GetDriftTime() const { return fTime;                       }
    Double_t GetRadius()    const { return fWireDistance;               }
    Int_t GetNUsedDigis()   const { return 1;
    }
    // interface for useless variables from Standard Persistency
    Int_t GetSingle()                 const { return 0; }
    Int_t GetID()                     const { return 0; }
    Int_t GetMCID()                   const { return 0; }
    Int_t GetTDCID()                  const { return 0; }
    Int_t GetRecoID()                 const { return 0; }
    Double_t GetEnergy()              const { return 0; }
    Double_t GetWireAverageDistance() const { return 0; }
    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Short_t fChannelID    = -9999;
    Float_t fTime         = -9999;
    Float_t fTimeWidth    = -9999;
    Float_t fWireDistance = -9999;

    ClassDef(TSlimRecoSpectrometerHit, 1)
};

#endif /* TSLIMRECOSPECTROMETERHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoSpectrometerCandidate", payloadCode, "@",
"TSlimRecoSpectrometerEvent", payloadCode, "@",
"TSlimRecoSpectrometerHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libSpectrometerSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libSpectrometerSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libSpectrometerSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libSpectrometerSlimPersistency() {
  TriggerDictionaryInitialization_libSpectrometerSlimPersistency_Impl();
}
