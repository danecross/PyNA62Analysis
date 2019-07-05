// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME RICHSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/RICH/include/TSlimRecoRICHCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/RICH/include/TSlimRecoRICHEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/RICH/include/TSlimRecoRICHHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoRICHCandidate(void *p = 0);
   static void *newArray_TSlimRecoRICHCandidate(Long_t size, void *p);
   static void delete_TSlimRecoRICHCandidate(void *p);
   static void deleteArray_TSlimRecoRICHCandidate(void *p);
   static void destruct_TSlimRecoRICHCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoRICHCandidate*)
   {
      ::TSlimRecoRICHCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoRICHCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoRICHCandidate", ::TSlimRecoRICHCandidate::Class_Version(), "", 17,
                  typeid(::TSlimRecoRICHCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoRICHCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoRICHCandidate) );
      instance.SetNew(&new_TSlimRecoRICHCandidate);
      instance.SetNewArray(&newArray_TSlimRecoRICHCandidate);
      instance.SetDelete(&delete_TSlimRecoRICHCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoRICHCandidate);
      instance.SetDestructor(&destruct_TSlimRecoRICHCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoRICHCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoRICHCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoRICHCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoRICHHit(void *p = 0);
   static void *newArray_TSlimRecoRICHHit(Long_t size, void *p);
   static void delete_TSlimRecoRICHHit(void *p);
   static void deleteArray_TSlimRecoRICHHit(void *p);
   static void destruct_TSlimRecoRICHHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoRICHHit*)
   {
      ::TSlimRecoRICHHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoRICHHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoRICHHit", ::TSlimRecoRICHHit::Class_Version(), "TSlimRecoRICHHit.hh", 11,
                  typeid(::TSlimRecoRICHHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoRICHHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoRICHHit) );
      instance.SetNew(&new_TSlimRecoRICHHit);
      instance.SetNewArray(&newArray_TSlimRecoRICHHit);
      instance.SetDelete(&delete_TSlimRecoRICHHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoRICHHit);
      instance.SetDestructor(&destruct_TSlimRecoRICHHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoRICHHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoRICHHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoRICHHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoRICHEvent(void *p = 0);
   static void *newArray_TSlimRecoRICHEvent(Long_t size, void *p);
   static void delete_TSlimRecoRICHEvent(void *p);
   static void deleteArray_TSlimRecoRICHEvent(void *p);
   static void destruct_TSlimRecoRICHEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoRICHEvent*)
   {
      ::TSlimRecoRICHEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoRICHEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoRICHEvent", ::TSlimRecoRICHEvent::Class_Version(), "", 107,
                  typeid(::TSlimRecoRICHEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoRICHEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoRICHEvent) );
      instance.SetNew(&new_TSlimRecoRICHEvent);
      instance.SetNewArray(&newArray_TSlimRecoRICHEvent);
      instance.SetDelete(&delete_TSlimRecoRICHEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoRICHEvent);
      instance.SetDestructor(&destruct_TSlimRecoRICHEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoRICHEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoRICHEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoRICHEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoRICHCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoRICHCandidate::Class_Name()
{
   return "TSlimRecoRICHCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoRICHCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoRICHCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoRICHCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoRICHCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoRICHHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoRICHHit::Class_Name()
{
   return "TSlimRecoRICHHit";
}

//______________________________________________________________________________
const char *TSlimRecoRICHHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoRICHHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoRICHHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoRICHHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoRICHEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoRICHEvent::Class_Name()
{
   return "TSlimRecoRICHEvent";
}

//______________________________________________________________________________
const char *TSlimRecoRICHEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoRICHEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoRICHEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoRICHEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoRICHEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoRICHCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoRICHCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoRICHCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoRICHCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoRICHCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoRICHCandidate : new ::TSlimRecoRICHCandidate;
   }
   static void *newArray_TSlimRecoRICHCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoRICHCandidate[nElements] : new ::TSlimRecoRICHCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoRICHCandidate(void *p) {
      delete ((::TSlimRecoRICHCandidate*)p);
   }
   static void deleteArray_TSlimRecoRICHCandidate(void *p) {
      delete [] ((::TSlimRecoRICHCandidate*)p);
   }
   static void destruct_TSlimRecoRICHCandidate(void *p) {
      typedef ::TSlimRecoRICHCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoRICHCandidate

//______________________________________________________________________________
void TSlimRecoRICHHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoRICHHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoRICHHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoRICHHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoRICHHit(void *p) {
      return  p ? new(p) ::TSlimRecoRICHHit : new ::TSlimRecoRICHHit;
   }
   static void *newArray_TSlimRecoRICHHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoRICHHit[nElements] : new ::TSlimRecoRICHHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoRICHHit(void *p) {
      delete ((::TSlimRecoRICHHit*)p);
   }
   static void deleteArray_TSlimRecoRICHHit(void *p) {
      delete [] ((::TSlimRecoRICHHit*)p);
   }
   static void destruct_TSlimRecoRICHHit(void *p) {
      typedef ::TSlimRecoRICHHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoRICHHit

//______________________________________________________________________________
void TSlimRecoRICHEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoRICHEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoRICHEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoRICHEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoRICHEvent(void *p) {
      return  p ? new(p) ::TSlimRecoRICHEvent : new ::TSlimRecoRICHEvent;
   }
   static void *newArray_TSlimRecoRICHEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoRICHEvent[nElements] : new ::TSlimRecoRICHEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoRICHEvent(void *p) {
      delete ((::TSlimRecoRICHEvent*)p);
   }
   static void deleteArray_TSlimRecoRICHEvent(void *p) {
      delete [] ((::TSlimRecoRICHEvent*)p);
   }
   static void destruct_TSlimRecoRICHEvent(void *p) {
      typedef ::TSlimRecoRICHEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoRICHEvent

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
   static TClass *vectorlETSlimRecoRICHHitgR_Dictionary();
   static void vectorlETSlimRecoRICHHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoRICHHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoRICHHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoRICHHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoRICHHitgR(void *p);
   static void destruct_vectorlETSlimRecoRICHHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoRICHHit>*)
   {
      vector<TSlimRecoRICHHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoRICHHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoRICHHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoRICHHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoRICHHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoRICHHit>) );
      instance.SetNew(&new_vectorlETSlimRecoRICHHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoRICHHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoRICHHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoRICHHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoRICHHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoRICHHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoRICHHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoRICHHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoRICHHit>*)0x0)->GetClass();
      vectorlETSlimRecoRICHHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoRICHHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoRICHHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoRICHHit> : new vector<TSlimRecoRICHHit>;
   }
   static void *newArray_vectorlETSlimRecoRICHHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoRICHHit>[nElements] : new vector<TSlimRecoRICHHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoRICHHitgR(void *p) {
      delete ((vector<TSlimRecoRICHHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoRICHHitgR(void *p) {
      delete [] ((vector<TSlimRecoRICHHit>*)p);
   }
   static void destruct_vectorlETSlimRecoRICHHitgR(void *p) {
      typedef vector<TSlimRecoRICHHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoRICHHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoRICHCandidategR_Dictionary();
   static void vectorlETSlimRecoRICHCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoRICHCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoRICHCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoRICHCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoRICHCandidategR(void *p);
   static void destruct_vectorlETSlimRecoRICHCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoRICHCandidate>*)
   {
      vector<TSlimRecoRICHCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoRICHCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoRICHCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoRICHCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoRICHCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoRICHCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoRICHCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoRICHCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoRICHCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoRICHCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoRICHCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoRICHCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoRICHCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoRICHCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoRICHCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoRICHCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoRICHCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoRICHCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoRICHCandidate> : new vector<TSlimRecoRICHCandidate>;
   }
   static void *newArray_vectorlETSlimRecoRICHCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoRICHCandidate>[nElements] : new vector<TSlimRecoRICHCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoRICHCandidategR(void *p) {
      delete ((vector<TSlimRecoRICHCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoRICHCandidategR(void *p) {
      delete [] ((vector<TSlimRecoRICHCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoRICHCandidategR(void *p) {
      typedef vector<TSlimRecoRICHCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoRICHCandidate>

namespace {
  void TriggerDictionaryInitialization_libRICHSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/RICH/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/RICH/../../FullReco/RICH/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/RICH/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libRICHSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoRICHHit.hh")))  TSlimRecoRICHHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoRICHCandidate;
class TSlimRecoRICHEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libRICHSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECORICHCANDIDATE_H
#define TSLIMRECORICHCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>
#include "TVector2.h"
#include "TArrayI.h"
#include "TSlimRecoVCandidate.hh"

class TRecoRICHCandidate;

class TSlimRecoRICHCandidate : public TSlimRecoVCandidate {
  public:
    TSlimRecoRICHCandidate() = default;
    explicit TSlimRecoRICHCandidate(TRecoRICHCandidate *candReco);
    virtual ~TSlimRecoRICHCandidate() {};

    // setters for members
    void SetRingCenter(TVector2 value)        { fRingCenter_X = value.X(); fRingCenter_Y = value.Y(); }
    void SetRingRadius(Float_t value)         { fRingRadius = value;                                  }
    void SetRingChi2(Float_t value)           { fRingChi2 = value;                                    }
    void SetRingTime(Float_t value)           { fRingTime = value;                                    }
    void SetTimeCandidateIndex(Float_t value) { fTimeCandidateIndex = value;                          }

    // setters methods for the iteration fit
    void SetRingCenterSingleRing(TVector2 value)      { fRingCenterSingleRing_X = value.X(); fRingCenterSingleRing_Y = value.Y();           }
    void SetRingCenterErrorSingleRing(TVector2 value) { fRingCenterErrorSingleRing_X = value.X(); fRingCenterErrorSingleRing_Y = value.Y(); }
    void SetRingRadiusSingleRing(Float_t value)       { fRingRadiusSingleRing = value;                                                      }
    void SetRingRadiusErrorSingleRing(Float_t value)  { fRingRadiusErrorSingleRing = value;                                                 }
    void SetRingChi2SingleRing(Float_t value)         { fRingChi2SingleRing = value;                                                        }
    void SetRingTimeSingleRing(Float_t value)         { fRingTimeSingleRing = value;                                                        }
    void SetNIterationsSingleRing(Short_t value)      { fNIterationsSingleRing = value;                                                     }
    void AddHitIndex(Short_t index)                   { fHitIndexes.emplace_back(index);                                                    }
    void AddHitIndexSingleRing(Short_t index)         { fHitIndexesSingleRing.emplace_back(index);                                          }

    // getters for members

    TVector2 GetRingCenter()         const { return TVector2(fRingCenter_X, fRingCenter_Y); }
    Float_t  GetRingRadius()         const { return fRingRadius;                            }
    Float_t  GetRingChi2()           const { return fRingChi2;                              }
    Float_t  GetRingTime()           const { return fRingTime;                              }
    Float_t  GetTime()               const { return fTime;                                  }
    Short_t  GetTimeCandidateIndex() const { return fTimeCandidateIndex;                    }

    // getters methods for the iteration fit
    TVector2 GetRingCenterSingleRing()                    const { return TVector2(fRingCenterSingleRing_X, fRingCenterErrorSingleRing_Y);      }
    TVector2 GetRingCenterErrorSingleRing()               const { return TVector2(fRingCenterErrorSingleRing_X, fRingCenterErrorSingleRing_Y); }
    Float_t  GetRingRadiusSingleRing()                    const { return fRingRadiusSingleRing;                                                }
    Float_t  GetRingRadiusErrorSingleRing()               const { return fRingRadiusErrorSingleRing;                                           }
    Float_t  GetRingChi2SingleRing()                      const { return fRingChi2SingleRing;                                                  }
    Float_t  GetRingTimeSingleRing()                      const { return fRingTimeSingleRing;                                                  }
    Float_t  GetNHits()                                   const { return fHitIndexes.size();                                                   }
    Float_t  GetNHitsSingleRing()                         const { return fHitIndexesSingleRing.size();                                         }
    Float_t  GetNIterationsSingleRing()                   const { return fNIterationsSingleRing;                                               }
    const std::vector<Short_t>& GetHitIndexes()           const { return fHitIndexes;                                                          }
    const std::vector<Short_t>& GetHitIndexesSingleRing() const { return fHitIndexesSingleRing;                                                }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);

  private:

    Short_t  fTimeCandidateIndex =    -1;  ///< index of the time candidate from which the ring candidate has been reconstructed
    Float_t  fRingRadius         =  -10.;  ///< radius of the ring as obtained from the fit
    Float_t  fRingChi2           =  -10.;  ///< chi2 of the fit to the ring
    Float_t  fRingTime           = 9999.;  ///< time of the ring candidate
    Float_t  fRingCenter_X       =  500.;  ///< center position of the ring as obtained from the fit
    Float_t  fRingCenter_Y       =  500.;  ///< center position of the ring as obtained from the fit
    Float_t  fTime               = 9999.;

    // variables for the iteration fit
    Short_t  fNIterationsSingleRing       = -1;
    Float_t  fRingRadiusSingleRing        = -9999.;  ///< radius of the single ring ring
    Float_t  fRingRadiusErrorSingleRing   = -9999.;
    Float_t  fRingChi2SingleRing          = -9999.;  ///< chi2 of the single ring fit
    Float_t  fRingTimeSingleRing          = -9999.;  ///< time of the average over all hits in the single ring fit
    Float_t  fRingCenterSingleRing_X      = -9999.;  ///< center position of the ring obtained in the single ring the fit
    Float_t  fRingCenterSingleRing_Y      = -9999.;  ///< center position of the ring obtained in the single ring the fit
    Float_t  fRingCenterErrorSingleRing_X = -9999.;
    Float_t  fRingCenterErrorSingleRing_Y = -9999.;
    std::vector<Short_t> fHitIndexes;
    std::vector<Short_t> fHitIndexesSingleRing;

    ClassDef(TSlimRecoRICHCandidate, 1)
};

#endif /* TSLIMRECORICHCANDIDATE_H */
#ifndef TRECORICHEVENTSLIM_H
#define TRECORICHEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoRICHCandidate.hh"
#include "TSlimRecoRICHHit.hh"

class TRecoRICHEvent;

class TSlimRecoRICHEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoRICHEvent()          = default;
    explicit TSlimRecoRICHEvent(TRecoRICHEvent *evReco);
    virtual ~TSlimRecoRICHEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddHit(TSlimRecoRICHHit h)                   { fHits.emplace_back(std::move(h));           }
    void AddTimeCandidate(TSlimRecoRICHCandidate c)   { fTimeCandidates.emplace_back(std::move(c)); }
    void AddRingCandidate(TSlimRecoRICHCandidate c)   { fRingCandidates.emplace_back(std::move(c)); }

    Int_t GetNHits()                                          const { return fHits.size();                                                                      }
    std::vector<TSlimRecoRICHHit>& GetHits()                        { return fHits;                                                                             }
    TSlimRecoVHit* GetHit(UInt_t iHit)                              { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr;                           }
    Int_t GetNPMTimeCandidates()                              const { return fTimeCandidates.size() - fNSCTimeCandidates;                                       }
    Int_t GetRingCandidates()                                 const { return fRingCandidates.size();                                                            }
    Int_t GetSCTimeCandidates()                               const { return fNSCTimeCandidates;                                                                }
    Int_t GetNTimeCandidates()                                const { return fTimeCandidates.size();                                                            }
    std::vector<TSlimRecoRICHCandidate>& GetTimeCandidates()        { return fTimeCandidates;                                                                   }
    std::vector<TSlimRecoRICHCandidate>& GetRingCandidates()        { return fRingCandidates;                                                                   }
    std::vector<TSlimRecoRICHCandidate> GetPMTimeCandidates()       {
        std::vector<TSlimRecoRICHCandidate> v; std::copy(fTimeCandidates.begin()+fNSCTimeCandidates, fTimeCandidates.end(), std::back_inserter(v)); return v;   }
    std::vector<TSlimRecoRICHCandidate> GetSCTimeCandidates()       {
        std::vector<TSlimRecoRICHCandidate> v; std::copy(fTimeCandidates.begin(), fTimeCandidates.begin()+fNSCTimeCandidates, std::back_inserter(v)); return v; }
    TSlimRecoVCandidate* GetTimeCandidate(UInt_t iCand)             { if(iCand<fTimeCandidates.size()) return &fTimeCandidates[iCand]; else return nullptr;     }
    TSlimRecoVCandidate* GetRingCandidate(UInt_t iCand)             { if(iCand<fRingCandidates.size()) return &fRingCandidates[iCand]; else return nullptr;     }
    TSlimRecoVCandidate* GetSCTimeCandidate(UInt_t iCand)           { if(iCand<fNSCTimeCandidates) return &fTimeCandidates[iCand]; else return nullptr;         }
    TSlimRecoVCandidate* GetPMTimeCandidate(UInt_t iCand)           {
        if(iCand<fTimeCandidates.size()-fNSCTimeCandidates) return &fTimeCandidates[iCand+fNSCTimeCandidates]; else return nullptr;                             }
    Int_t GetNCandidates()                                    const { return fTimeCandidates.size() + fRingCandidates.size();                                   }
    std::vector<TSlimRecoRICHCandidate> GetCandidates();
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                 {
        if(iCand<fTimeCandidates.size()) return &fTimeCandidates[iCand];
        else if(iCand-fTimeCandidates.size() < fRingCandidates.size()) return &fRingCandidates[iCand-fTimeCandidates.size()];
        else return nullptr;
    }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    UShort_t fNSCTimeCandidates = 0;
    std::vector<TSlimRecoRICHHit>       fHits;
    std::vector<TSlimRecoRICHCandidate> fRingCandidates;
    std::vector<TSlimRecoRICHCandidate> fTimeCandidates;

    ClassDef(TSlimRecoRICHEvent, 1)
};
#endif /* TRECORICHEVENTSLIM_H */
#ifndef TSLIMRECORICHHIT_H
#define TSLIMRECORICHHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"
#include "TVector3.h"

class TRecoRICHHit;

class TSlimRecoRICHHit : public TSlimRecoVHit {
  public:
    TSlimRecoRICHHit()          = default;
    explicit TSlimRecoRICHHit(TRecoRICHHit *hitReco);
    virtual ~TSlimRecoRICHHit() = default;

    // setters for members
    void SetTime(Float_t value)         { fTime = value;                                          }
    void SetHitQuality(Short_t value)   { fHitQuality = value;                                    }
    void SetTimeWidth(Float_t value)    { fTimeWidth = value;                                     }
    void SetChannelID(Int_t value)      { fChannelID = value;                                     }
    void SetPtolemy(Float_t value)      { fPtolemy = value;                                       }
    void SetFitPosition(TVector3 value) { fFitPosition_X = value.X(); fFitPosition_Y = value.Y(); }

    // getters for members
    Float_t  GetTime()        const { return fTime;                                        }
    Short_t  GetHitQuality()  const { return fHitQuality;                                  }
    Float_t  GetTimeWidth()   const { return fTimeWidth;                                   }
    Int_t    GetChannelID()   const { return fChannelID;                                   }
    Float_t  GetPtolemy()     const { return fPtolemy;                                     }
    TVector3 GetFitPosition() const { return TVector3(fFitPosition_X, fFitPosition_Y, 0.); }

    //Int_t GetChannelSeqID()                                                { return fChannelSeqID;                 }
    Int_t GetDiskID()        const;
    Int_t GetUpDownDiskID()  const;
    Int_t GetSuperCellID()   const;
    Int_t GetOrSuperCellID() const;
    Int_t GetPmtID()         const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);

  private:

    Int_t   fChannelID     =    -1;
    Float_t fTime          = -999.;
    Short_t fHitQuality    =    -1;
    Float_t fTimeWidth     = -999.;
    Float_t fPtolemy       = -999.;
    Float_t fFitPosition_X =  999.; ///< HitPosition corrected including mirror inclination
    Float_t fFitPosition_Y =  999.; ///< HitPosition corrected including mirror inclination

    ClassDef(TSlimRecoRICHHit, 1)
};

#endif /* TSLIMRECORICHHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoRICHCandidate", payloadCode, "@",
"TSlimRecoRICHEvent", payloadCode, "@",
"TSlimRecoRICHHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libRICHSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libRICHSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libRICHSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libRICHSlimPersistency() {
  TriggerDictionaryInitialization_libRICHSlimPersistency_Impl();
}
