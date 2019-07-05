// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME SAVSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAV/include/TSlimRecoSAVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAV/include/TSlimRecoSAVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAV/include/TSlimRecoSAVHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoSAVCandidate(void *p = 0);
   static void *newArray_TSlimRecoSAVCandidate(Long_t size, void *p);
   static void delete_TSlimRecoSAVCandidate(void *p);
   static void deleteArray_TSlimRecoSAVCandidate(void *p);
   static void destruct_TSlimRecoSAVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSAVCandidate*)
   {
      ::TSlimRecoSAVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSAVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSAVCandidate", ::TSlimRecoSAVCandidate::Class_Version(), "", 16,
                  typeid(::TSlimRecoSAVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSAVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSAVCandidate) );
      instance.SetNew(&new_TSlimRecoSAVCandidate);
      instance.SetNewArray(&newArray_TSlimRecoSAVCandidate);
      instance.SetDelete(&delete_TSlimRecoSAVCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSAVCandidate);
      instance.SetDestructor(&destruct_TSlimRecoSAVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSAVCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSAVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSAVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoSAVHit(void *p = 0);
   static void *newArray_TSlimRecoSAVHit(Long_t size, void *p);
   static void delete_TSlimRecoSAVHit(void *p);
   static void deleteArray_TSlimRecoSAVHit(void *p);
   static void destruct_TSlimRecoSAVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSAVHit*)
   {
      ::TSlimRecoSAVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSAVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSAVHit", ::TSlimRecoSAVHit::Class_Version(), "TSlimRecoSAVHit.hh", 12,
                  typeid(::TSlimRecoSAVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSAVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSAVHit) );
      instance.SetNew(&new_TSlimRecoSAVHit);
      instance.SetNewArray(&newArray_TSlimRecoSAVHit);
      instance.SetDelete(&delete_TSlimRecoSAVHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSAVHit);
      instance.SetDestructor(&destruct_TSlimRecoSAVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSAVHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSAVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSAVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoSAVEvent(void *p = 0);
   static void *newArray_TSlimRecoSAVEvent(Long_t size, void *p);
   static void delete_TSlimRecoSAVEvent(void *p);
   static void deleteArray_TSlimRecoSAVEvent(void *p);
   static void destruct_TSlimRecoSAVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSAVEvent*)
   {
      ::TSlimRecoSAVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSAVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSAVEvent", ::TSlimRecoSAVEvent::Class_Version(), "", 65,
                  typeid(::TSlimRecoSAVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSAVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSAVEvent) );
      instance.SetNew(&new_TSlimRecoSAVEvent);
      instance.SetNewArray(&newArray_TSlimRecoSAVEvent);
      instance.SetDelete(&delete_TSlimRecoSAVEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSAVEvent);
      instance.SetDestructor(&destruct_TSlimRecoSAVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSAVEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSAVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSAVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSAVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSAVCandidate::Class_Name()
{
   return "TSlimRecoSAVCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoSAVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSAVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSAVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSAVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSAVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSAVHit::Class_Name()
{
   return "TSlimRecoSAVHit";
}

//______________________________________________________________________________
const char *TSlimRecoSAVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSAVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSAVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSAVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSAVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSAVEvent::Class_Name()
{
   return "TSlimRecoSAVEvent";
}

//______________________________________________________________________________
const char *TSlimRecoSAVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSAVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSAVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSAVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSAVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoSAVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSAVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSAVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSAVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSAVCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoSAVCandidate : new ::TSlimRecoSAVCandidate;
   }
   static void *newArray_TSlimRecoSAVCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSAVCandidate[nElements] : new ::TSlimRecoSAVCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSAVCandidate(void *p) {
      delete ((::TSlimRecoSAVCandidate*)p);
   }
   static void deleteArray_TSlimRecoSAVCandidate(void *p) {
      delete [] ((::TSlimRecoSAVCandidate*)p);
   }
   static void destruct_TSlimRecoSAVCandidate(void *p) {
      typedef ::TSlimRecoSAVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSAVCandidate

//______________________________________________________________________________
void TSlimRecoSAVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSAVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSAVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSAVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSAVHit(void *p) {
      return  p ? new(p) ::TSlimRecoSAVHit : new ::TSlimRecoSAVHit;
   }
   static void *newArray_TSlimRecoSAVHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSAVHit[nElements] : new ::TSlimRecoSAVHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSAVHit(void *p) {
      delete ((::TSlimRecoSAVHit*)p);
   }
   static void deleteArray_TSlimRecoSAVHit(void *p) {
      delete [] ((::TSlimRecoSAVHit*)p);
   }
   static void destruct_TSlimRecoSAVHit(void *p) {
      typedef ::TSlimRecoSAVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSAVHit

//______________________________________________________________________________
void TSlimRecoSAVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSAVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSAVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSAVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSAVEvent(void *p) {
      return  p ? new(p) ::TSlimRecoSAVEvent : new ::TSlimRecoSAVEvent;
   }
   static void *newArray_TSlimRecoSAVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSAVEvent[nElements] : new ::TSlimRecoSAVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSAVEvent(void *p) {
      delete ((::TSlimRecoSAVEvent*)p);
   }
   static void deleteArray_TSlimRecoSAVEvent(void *p) {
      delete [] ((::TSlimRecoSAVEvent*)p);
   }
   static void destruct_TSlimRecoSAVEvent(void *p) {
      typedef ::TSlimRecoSAVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSAVEvent

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
   static TClass *vectorlETSlimRecoSAVHitgR_Dictionary();
   static void vectorlETSlimRecoSAVHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoSAVHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoSAVHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoSAVHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoSAVHitgR(void *p);
   static void destruct_vectorlETSlimRecoSAVHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoSAVHit>*)
   {
      vector<TSlimRecoSAVHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoSAVHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoSAVHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoSAVHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoSAVHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoSAVHit>) );
      instance.SetNew(&new_vectorlETSlimRecoSAVHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoSAVHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoSAVHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoSAVHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoSAVHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoSAVHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoSAVHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoSAVHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoSAVHit>*)0x0)->GetClass();
      vectorlETSlimRecoSAVHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoSAVHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoSAVHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSAVHit> : new vector<TSlimRecoSAVHit>;
   }
   static void *newArray_vectorlETSlimRecoSAVHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSAVHit>[nElements] : new vector<TSlimRecoSAVHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoSAVHitgR(void *p) {
      delete ((vector<TSlimRecoSAVHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoSAVHitgR(void *p) {
      delete [] ((vector<TSlimRecoSAVHit>*)p);
   }
   static void destruct_vectorlETSlimRecoSAVHitgR(void *p) {
      typedef vector<TSlimRecoSAVHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoSAVHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoSAVCandidategR_Dictionary();
   static void vectorlETSlimRecoSAVCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoSAVCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoSAVCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoSAVCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoSAVCandidategR(void *p);
   static void destruct_vectorlETSlimRecoSAVCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoSAVCandidate>*)
   {
      vector<TSlimRecoSAVCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoSAVCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoSAVCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoSAVCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoSAVCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoSAVCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoSAVCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoSAVCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoSAVCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoSAVCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoSAVCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoSAVCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoSAVCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoSAVCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoSAVCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoSAVCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoSAVCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoSAVCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSAVCandidate> : new vector<TSlimRecoSAVCandidate>;
   }
   static void *newArray_vectorlETSlimRecoSAVCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSAVCandidate>[nElements] : new vector<TSlimRecoSAVCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoSAVCandidategR(void *p) {
      delete ((vector<TSlimRecoSAVCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoSAVCandidategR(void *p) {
      delete [] ((vector<TSlimRecoSAVCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoSAVCandidategR(void *p) {
      typedef vector<TSlimRecoSAVCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoSAVCandidate>

namespace {
  void TriggerDictionaryInitialization_libSAVSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAV/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAV/../../FullReco/SAV/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libSAVSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoSAVHit.hh")))  TSlimRecoSAVHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoSAVCandidate;
class TSlimRecoSAVEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libSAVSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOSAVCANDIDATE_H
#define TSLIMRECOSAVCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVCandidate.hh"
#include <TVector3.h>

class TRecoSAVCandidate;

class TSlimRecoSAVCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoSAVCandidate() = default;
    explicit TSlimRecoSAVCandidate(TRecoSAVCandidate *);
    virtual ~TSlimRecoSAVCandidate() = default;

    // setters for members
    void SetTime(Float_t value)              { fTime=value;                      }
    void SetEnergy(Float_t value)            { fEnergy = value;                  }
    void SetPosition(Float_t x, Double_t y ) { fX = x; fY = y;                   }
    void SetPosition(TVector2 value )        { fX = value.X(); fY = value.Y();   }
    void AddHitIndex(Short_t index)          { fHitsIndexes.emplace_back(index); }

    Int_t GetNHits()                       const { return fHitsIndexes.size(); }
    std::vector<Short_t>& GetHitsIndexes()       { return fHitsIndexes;        }
    Float_t GetTime()                      const { return fTime;               }
    Float_t GetEnergy()                    const { return fEnergy;             }
    Float_t GetX()                         const { return fX;                  }
    Float_t GetY()                         const { return fY;                  }
    TVector2 GetPosition()                 const { return TVector2(fX,fY);     }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candVReco);
    virtual void ToReco(TRecoVCandidate *candVReco);
private:
    Float_t fTime   = -999.;
    Float_t fEnergy = -999.;
    Float_t fX      = -999.;
    Float_t fY      = -999.;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoSAVCandidate, 1)
};

#endif /* TSLIMRECOSAVCANDIDATE_H */
#ifndef TRECOSAVEVENTSLIM_H
#define TRECOSAVEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoSAVCandidate.hh"
#include "TSlimRecoSAVHit.hh"
#include "TSlimRecoVEvent.hh"

class TRecoSAVEvent;

class TSlimRecoSAVEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoSAVEvent() = default;
    explicit TSlimRecoSAVEvent(TRecoSAVEvent *evReco);
    virtual ~TSlimRecoSAVEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoSAVCandidate c) { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoSAVHit h)             { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                    const { return fHits.size();       }
    std::vector<TSlimRecoSAVHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                        { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                              const { return fCandidates.size(); }
    std::vector<TSlimRecoSAVCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoSAVHit> fHits;
    std::vector<TSlimRecoSAVCandidate> fCandidates;

    ClassDef(TSlimRecoSAVEvent, 1)
};

#endif /* TRECOSAVEVENTSLIM_H */
#ifndef TSLIMRECOSAVHIT_H
#define TSLIMRECOSAVHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <TVector2.h>
#include <TVector3.h>

#include "TSlimRecoVHit.hh"

class TRecoSAVHit;

class TSlimRecoSAVHit : public TSlimRecoVHit
{
public:
    TSlimRecoSAVHit() = default;
    explicit TSlimRecoSAVHit(TRecoSAVHit *);
    virtual ~TSlimRecoSAVHit() = default;

    void SetTime(Float_t time)           { fTime = time;           }
    void SetAmplitude(Float_t amplitude) { fAmplitude = amplitude; }
    void SetBaseline(Float_t baseline)   { fBaseline = baseline;   }
    void SetEnergy(Float_t energy)       { fEnergy = energy;       }
    void AddEnergy(Float_t value)        { fEnergy += value;       }

    Float_t  GetTime()            const { return fTime;         }
    Float_t  GetAmplitude()       const { return fAmplitude;    }
    Float_t  GetBaseline()        const { return fBaseline;     }
    Float_t  GetEnergy()          const { return fEnergy;       }
    Int_t    GetChannelID()       const { return fChannelID;    }
    Int_t    GetChannel()         const { return fChannelID;    }
    Int_t    GetDetector()        const;
    Int_t    GetChannelDetector() const;
    TVector2 GetChannelPosition() const;
    TVector3 GetPosition()        const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Int_t   fChannelID = -999 ;
    Float_t fTime      = -999.;
    Float_t fAmplitude = -999.;
    Float_t fBaseline  = -999.;
    Float_t fEnergy    = -999.;

    ClassDef(TSlimRecoSAVHit, 1)
};

#endif /* TSLIMRECOSAVHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoSAVCandidate", payloadCode, "@",
"TSlimRecoSAVEvent", payloadCode, "@",
"TSlimRecoSAVHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libSAVSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libSAVSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libSAVSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libSAVSlimPersistency() {
  TriggerDictionaryInitialization_libSAVSlimPersistency_Impl();
}
