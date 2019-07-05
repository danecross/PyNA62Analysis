// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME LAVSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LAV/include/TSlimRecoLAVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LAV/include/TSlimRecoLAVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LAV/include/TSlimRecoLAVHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoLAVCandidate(void *p = 0);
   static void *newArray_TSlimRecoLAVCandidate(Long_t size, void *p);
   static void delete_TSlimRecoLAVCandidate(void *p);
   static void deleteArray_TSlimRecoLAVCandidate(void *p);
   static void destruct_TSlimRecoLAVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoLAVCandidate*)
   {
      ::TSlimRecoLAVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoLAVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoLAVCandidate", ::TSlimRecoLAVCandidate::Class_Version(), "", 21,
                  typeid(::TSlimRecoLAVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoLAVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoLAVCandidate) );
      instance.SetNew(&new_TSlimRecoLAVCandidate);
      instance.SetNewArray(&newArray_TSlimRecoLAVCandidate);
      instance.SetDelete(&delete_TSlimRecoLAVCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoLAVCandidate);
      instance.SetDestructor(&destruct_TSlimRecoLAVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoLAVCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoLAVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoLAVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoLAVHit(void *p = 0);
   static void *newArray_TSlimRecoLAVHit(Long_t size, void *p);
   static void delete_TSlimRecoLAVHit(void *p);
   static void deleteArray_TSlimRecoLAVHit(void *p);
   static void destruct_TSlimRecoLAVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoLAVHit*)
   {
      ::TSlimRecoLAVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoLAVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoLAVHit", ::TSlimRecoLAVHit::Class_Version(), "TSlimRecoLAVHit.hh", 17,
                  typeid(::TSlimRecoLAVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoLAVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoLAVHit) );
      instance.SetNew(&new_TSlimRecoLAVHit);
      instance.SetNewArray(&newArray_TSlimRecoLAVHit);
      instance.SetDelete(&delete_TSlimRecoLAVHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoLAVHit);
      instance.SetDestructor(&destruct_TSlimRecoLAVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoLAVHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoLAVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoLAVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoLAVEvent(void *p = 0);
   static void *newArray_TSlimRecoLAVEvent(Long_t size, void *p);
   static void delete_TSlimRecoLAVEvent(void *p);
   static void deleteArray_TSlimRecoLAVEvent(void *p);
   static void destruct_TSlimRecoLAVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoLAVEvent*)
   {
      ::TSlimRecoLAVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoLAVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoLAVEvent", ::TSlimRecoLAVEvent::Class_Version(), "", 102,
                  typeid(::TSlimRecoLAVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoLAVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoLAVEvent) );
      instance.SetNew(&new_TSlimRecoLAVEvent);
      instance.SetNewArray(&newArray_TSlimRecoLAVEvent);
      instance.SetDelete(&delete_TSlimRecoLAVEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoLAVEvent);
      instance.SetDestructor(&destruct_TSlimRecoLAVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoLAVEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoLAVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoLAVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoLAVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoLAVCandidate::Class_Name()
{
   return "TSlimRecoLAVCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoLAVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoLAVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoLAVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoLAVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoLAVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoLAVHit::Class_Name()
{
   return "TSlimRecoLAVHit";
}

//______________________________________________________________________________
const char *TSlimRecoLAVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoLAVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoLAVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoLAVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoLAVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoLAVEvent::Class_Name()
{
   return "TSlimRecoLAVEvent";
}

//______________________________________________________________________________
const char *TSlimRecoLAVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoLAVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoLAVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoLAVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLAVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoLAVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoLAVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoLAVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoLAVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoLAVCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoLAVCandidate : new ::TSlimRecoLAVCandidate;
   }
   static void *newArray_TSlimRecoLAVCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoLAVCandidate[nElements] : new ::TSlimRecoLAVCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoLAVCandidate(void *p) {
      delete ((::TSlimRecoLAVCandidate*)p);
   }
   static void deleteArray_TSlimRecoLAVCandidate(void *p) {
      delete [] ((::TSlimRecoLAVCandidate*)p);
   }
   static void destruct_TSlimRecoLAVCandidate(void *p) {
      typedef ::TSlimRecoLAVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoLAVCandidate

//______________________________________________________________________________
void TSlimRecoLAVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoLAVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoLAVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoLAVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoLAVHit(void *p) {
      return  p ? new(p) ::TSlimRecoLAVHit : new ::TSlimRecoLAVHit;
   }
   static void *newArray_TSlimRecoLAVHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoLAVHit[nElements] : new ::TSlimRecoLAVHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoLAVHit(void *p) {
      delete ((::TSlimRecoLAVHit*)p);
   }
   static void deleteArray_TSlimRecoLAVHit(void *p) {
      delete [] ((::TSlimRecoLAVHit*)p);
   }
   static void destruct_TSlimRecoLAVHit(void *p) {
      typedef ::TSlimRecoLAVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoLAVHit

//______________________________________________________________________________
void TSlimRecoLAVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoLAVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoLAVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoLAVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoLAVEvent(void *p) {
      return  p ? new(p) ::TSlimRecoLAVEvent : new ::TSlimRecoLAVEvent;
   }
   static void *newArray_TSlimRecoLAVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoLAVEvent[nElements] : new ::TSlimRecoLAVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoLAVEvent(void *p) {
      delete ((::TSlimRecoLAVEvent*)p);
   }
   static void deleteArray_TSlimRecoLAVEvent(void *p) {
      delete [] ((::TSlimRecoLAVEvent*)p);
   }
   static void destruct_TSlimRecoLAVEvent(void *p) {
      typedef ::TSlimRecoLAVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoLAVEvent

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
   static TClass *vectorlETSlimRecoLAVHitgR_Dictionary();
   static void vectorlETSlimRecoLAVHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoLAVHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoLAVHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoLAVHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoLAVHitgR(void *p);
   static void destruct_vectorlETSlimRecoLAVHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoLAVHit>*)
   {
      vector<TSlimRecoLAVHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoLAVHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoLAVHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoLAVHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoLAVHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoLAVHit>) );
      instance.SetNew(&new_vectorlETSlimRecoLAVHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoLAVHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoLAVHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoLAVHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoLAVHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoLAVHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoLAVHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoLAVHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoLAVHit>*)0x0)->GetClass();
      vectorlETSlimRecoLAVHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoLAVHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoLAVHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLAVHit> : new vector<TSlimRecoLAVHit>;
   }
   static void *newArray_vectorlETSlimRecoLAVHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLAVHit>[nElements] : new vector<TSlimRecoLAVHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoLAVHitgR(void *p) {
      delete ((vector<TSlimRecoLAVHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoLAVHitgR(void *p) {
      delete [] ((vector<TSlimRecoLAVHit>*)p);
   }
   static void destruct_vectorlETSlimRecoLAVHitgR(void *p) {
      typedef vector<TSlimRecoLAVHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoLAVHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoLAVCandidategR_Dictionary();
   static void vectorlETSlimRecoLAVCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoLAVCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoLAVCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoLAVCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoLAVCandidategR(void *p);
   static void destruct_vectorlETSlimRecoLAVCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoLAVCandidate>*)
   {
      vector<TSlimRecoLAVCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoLAVCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoLAVCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoLAVCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoLAVCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoLAVCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoLAVCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoLAVCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoLAVCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoLAVCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoLAVCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoLAVCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoLAVCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoLAVCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoLAVCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoLAVCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoLAVCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoLAVCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLAVCandidate> : new vector<TSlimRecoLAVCandidate>;
   }
   static void *newArray_vectorlETSlimRecoLAVCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLAVCandidate>[nElements] : new vector<TSlimRecoLAVCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoLAVCandidategR(void *p) {
      delete ((vector<TSlimRecoLAVCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoLAVCandidategR(void *p) {
      delete [] ((vector<TSlimRecoLAVCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoLAVCandidategR(void *p) {
      typedef vector<TSlimRecoLAVCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoLAVCandidate>

namespace {
  void TriggerDictionaryInitialization_libLAVSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LAV/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LAV/../../FullReco/LAV/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/LAV/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libLAVSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoLAVHit.hh")))  TSlimRecoLAVHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoLAVCandidate;
class TSlimRecoLAVEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libLAVSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// --------------------------------------------------------------
// History:
//
// 2019-04-12 T. Spadaro (tommaso.spadaro@lnf.infn.it) and S. Martellotti (silvia.martellotti@lnf.infn.it)
//
// --------------------------------------------------------------
#ifndef TSlimRecoLAVCandidate_H
#define TSlimRecoLAVCandidate_H

#include "TVector3.h"
#include "TSlimRecoVCandidate.hh"

class TRecoLAVCandidate;


class TSlimRecoLAVCandidate : public TSlimRecoVCandidate {

public:

  TSlimRecoLAVCandidate();
  explicit TSlimRecoLAVCandidate(TRecoLAVCandidate* candReco);
  virtual ~TSlimRecoLAVCandidate() { fHitsIndexes.clear(); }

  void SetAlgorithm(Short_t val)                 { fAlgorithm = val;                                          }
  void SetClusterType(Short_t val)               { fClusterType = val;                                        } // Classification of the cluster: MIP, Shower, Unknown
  void SetNHitsPerLayer(Int_t ilay, Short_t val) { if (ilay<0 || ilay>=5) return; fNHitsPerLayer[ilay] = val; }
  void SetTime(Float_t val)                      { fTime = val;                                               }
  void SetEnergy(Float_t val)                    { fEnergy = val;                                             }
  void SetZUnweightedError(Float_t val)          { fZUnweightedError = val;                                   }
  void SetPhiUnweightedError(Float_t val)        { fPhiUnweightedError = val;                                 }
  void SetZWeightedError(Float_t val)            { fZWeightedError = val;                                     }
  void SetPhiWeightedError(Float_t val)          { fPhiWeightedError = val;                                   }
  void SetPosition(TVector3 val)                 { fPosition = val;                                           } // Centroid position: unwegithed and energy-weighted (at the moment, in fact, charge-weighted) average
  void SetWeightedPosition(TVector3 val)         { fWeightedPosition = val;                                   } // Centroid position: unwegithed and energy-weighted (at the moment, in fact, charge-weighted) average
  void AddHitIndex(Short_t index)                { fHitsIndexes.emplace_back(index);                          }

  Short_t GetAlgorithm()                       const { return fAlgorithm;                                                                                }
  Short_t GetClusterType()                     const { return fClusterType;                                                                              }
  Short_t GetNHitsPerLayer(Int_t ilay)         const { if (ilay<0 || ilay>=5) return 0; return fNHitsPerLayer[ilay];                                     }
  Float_t GetTime()                            const { return fTime;                                                                                     }
  Float_t GetEnergy()                          const { return fEnergy;                                                                                   }
  Float_t GetZUnweightedError()                const { return fZUnweightedError;                                                                         }
  Float_t GetPhiUnweightedError()              const { return fPhiUnweightedError;                                                                       }
  Float_t GetZWeightedError()                  const { return fZWeightedError;                                                                           }
  Float_t GetPhiWeightedError()                const { return fPhiWeightedError;                                                                         }
  TVector3 GetPosition()                       const { return fPosition;                                                                                 }
  TVector3 GetWeightedPosition()               const { return fWeightedPosition;                                                                         }
  Int_t GetNHits()                             const { return fNHitsPerLayer[0]+fNHitsPerLayer[1]+fNHitsPerLayer[2]+fNHitsPerLayer[3]+fNHitsPerLayer[4]; }
  const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;                                                                              }
  Double_t GetCentroidErrorMatrix(TVector3 position, Double_t sigmaPhi, Double_t sigmaZ, Double_t SigmaR, Int_t i1, Int_t i2) const;

  // Methods not implemented at the moment
  // Bool_t TVCandidate::AddHit(Int_t  )
  // void TRecoLAVCandidate::Clear(Option_t * = "" )
  // TRecoVHit * TRecoVCandidate::GetHit(Int_t iHit )
  // void TVCandidate::Merge(TVCandidate *  )
  // void TVCandidate::RemoveHit(Int_t  )
  // void TVCandidate::SetEvent(TDetectorVEvent * value ) [inline]

  // conversion functions
  virtual void FromReco(TRecoVCandidate *candReco);
  virtual void ToReco(TRecoVCandidate *candReco);
private:
  Short_t fAlgorithm;  ///< =0 if cluster is done by grouping adjacent blocks; =1 if cluster is done by grouping blocks close in phi and time (so-called "tracking"); =2 if no algo is passed
  Short_t fClusterType;  ///<  =1 for MIP cluster (>=2 rows hit with 1 hit per row); =2 for showerLike (>=2 rows hit with > 1 hit per row in at least one row); =0 else
  Short_t fNHitsPerLayer[5];   ///< Number of hits per layer

  Float_t fTime;
  Float_t fEnergy;             ///< Total cluster energy (at the moment, in fact, it is the total charge)
  Float_t fZUnweightedError;   ///< Error on the unweighted average of the z coordinate
  Float_t fPhiUnweightedError; ///< Error on the unweighted average of the azimuthal angle
  Float_t fZWeightedError;     ///< Error on the energy-weighted average of the z coordinate
  Float_t fPhiWeightedError;   ///< Error on the energy-weighted average of the azimuthal angle

  TVector3 fPosition;           ///< Cluster centroid position: no weight is used while averaging fired block positions
  TVector3 fWeightedPosition;   ///< Cluster centroid position: the block energy (in fact, at the moment, the charge) is used while averaging fired block positions

  std::vector<Short_t> fHitsIndexes;

  ClassDef(TSlimRecoLAVCandidate, 1)
};
#endif
#ifndef TSLIMRecoLAVEvent_H
#define TSLIMRecoLAVEvent_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoLAVCandidate.hh"
#include "TSlimRecoLAVHit.hh"

class TRecoLAVEvent;


class TSlimRecoLAVEvent : public TSlimRecoVEvent {

public:

  TSlimRecoLAVEvent();
  explicit TSlimRecoLAVEvent(TRecoLAVEvent *evReco);
  virtual ~TSlimRecoLAVEvent() {}

  void Reset();               // clears the candidate and hit vector
  void ClearHits();
  void ClearCandidates();

  void AddHit(TSlimRecoLAVHit h)             { fHits.emplace_back(std::move(h));       }
  void AddCandidate(TSlimRecoLAVCandidate c) { fCandidates.emplace_back(std::move(c)); }

  Int_t GetNHits()                                   const { return fHits.size();       }
  std::vector<TSlimRecoLAVHit>& GetHits()                  { return fHits;              }
  TSlimRecoVHit* GetHit(UInt_t iHit)                       { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
  Int_t GetNCandidates()                             const { return fCandidates.size(); }
  std::vector<TSlimRecoLAVCandidate>& GetCandidates()      { return fCandidates;        }
  TSlimRecoVCandidate* GetCandidate(UInt_t iCand)          { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

  // conversion functions
  virtual void FromReco(TRecoVEvent *evReco);
  virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoLAVHit> fHits;
    std::vector<TSlimRecoLAVCandidate> fCandidates;

    ClassDef(TSlimRecoLAVEvent, 1)
};
#endif /* TSLIMRecoLAVEvent_H */
// --------------------------------------------------------------
// History:
//
// 2019-04-12 T. Spadaro (tommaso.spadaro@lnf.infn.it) and S. Martellotti (silvia.martellotti@lnf.infn.it)
//
// --------------------------------------------------------------
#ifndef TSLIMRecoLAVHit_H
#define TSLIMRecoLAVHit_H 1

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TVector3.h"
#include "TSlimRecoVHit.hh"

class TRecoLAVHit;

class TSlimRecoLAVHit : public TSlimRecoVHit {

public:
  TSlimRecoLAVHit();
  explicit TSlimRecoLAVHit(TRecoLAVHit *hitReco);
  virtual ~TSlimRecoLAVHit() = default;

  void SetChannelID(Int_t channelID)          { fChannelID = channelID;                       }
  void SetTime(Float_t time)                  { fTime = time;                                 }
  void SetEdgeMask(Short_t edgeMask)          { fEdgeMask = edgeMask;                         }
  void SetLeadingEdgeLow(Double_t edgeTime)   { fLeadingEdgeLow   = edgeTime; fEdgeMask |= 1; }
  void SetLeadingEdgeHigh(Double_t edgeTime)  { fLeadingEdgeHigh  = edgeTime; fEdgeMask |= 2; }
  void SetTrailingEdgeHigh(Double_t edgeTime) { fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4; }
  void SetTrailingEdgeLow(Double_t edgeTime)  { fTrailingEdgeLow  = edgeTime; fEdgeMask |= 8; }

  Int_t GetChannelID()                            const { return fChannelID;                                              }
  Float_t GetTime()                               const { return fTime;                                                   }
  Short_t GetEdgeMask()                           const { return fEdgeMask;                                               }
  Float_t GetLeadingEdgeLow()                     const { if (fEdgeMask & 1) {return fLeadingEdgeLow;}   else {return 0;} }
  Float_t GetLeadingEdgeHigh()                    const { if (fEdgeMask & 2) {return fLeadingEdgeHigh;}  else {return 0;} }
  Float_t GetTrailingEdgeHigh()                   const { if (fEdgeMask & 4) {return fTrailingEdgeHigh;} else {return 0;} }
  Float_t GetTrailingEdgeLow()                    const { if (fEdgeMask & 8) {return fTrailingEdgeLow;}  else {return 0;} }
  TVector3 GetBlockPosition()                     const; ///< Returns block position, expressed in mm
  Int_t GetBlockIDFromPhi(Double_t, Int_t, Int_t) const;
  Double_t GetBlockPhiSpan(Int_t)                 const;
  Int_t GetLAVID()                                const;
  Int_t GetLayerID()                              const;
  Int_t GetBananaID()                             const;
  Int_t GetBlockID()                              const;

  Int_t EncodeChannelID();
  Int_t GetPackedChannelID();

  // Methods not implemented at the moment
  //  void TDetectorVHit::AddEnergy(Double_t value )
  //  void TRecoLAVHit::Clear(Option_t * option = "" )
  //  Int_t TVHit::Compare(const TObject * obj ) const [inline]
  //  TVDigi* TRecoVHit::GetDigi( ) [inline]
  //  Bool_t TRecoVHit::GetDigiOwner( ) [inline]
  //  Bool_t TVHit::GetDirectInteraction( ) [inline]
  //  Int_t TVHit::GetMCTrackID( ) [inline]
  //  Bool_t TVHit::IsSortable( ) const [inline]
  //  void TVHit::Print(Option_t * option = "" ) const
  //  void TRecoVHit::SetDigi(TVDigi * value ) [inline]
  //  void TRecoVHit::SetDigiOwner(Bool_t value ) [inline]
  //  void TVHit::SetDirectInteraction(Bool_t value ) [inline, inherited]
  //  void TDetectorVHit::SetEnergy(Double_t value ) [inline, inherited]
  //  void TVHit::SetMCTrackID(Int_t value ) [inline, inherited]
  //  void TDetectorVHit::SetPosition(TVector3 value ) [inline, inherited]
  //  void TVHit::ShiftMCTrackID(Int_t value ) [inline, inherited]
  //  virtual void TDetectorVHit::UpdateReferenceTime(Double_t value ) [inline, virtual, inherited]
  //

  virtual void FromReco(TRecoVHit*);
  virtual void ToReco(TRecoVHit*);
private:

  Int_t   fChannelID;
  Float_t fTime;
  Short_t fEdgeMask; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow

  Float_t fLeadingEdgeLow; ///< Time of leading low, subtracted of the trigger time only
  Float_t fTrailingEdgeLow;///< Time of leading high, subtracted of the trigger time only
  Float_t fLeadingEdgeHigh;///< Time of trailing high, subtracted of the trigger time only
  Float_t fTrailingEdgeHigh;///< Time of trailing low, subtracted of the trigger time only

  // Private variables not included that cannot be obtained from the above private vars.
  // Double_t fEnergy
  // TVector3 fPosition
  // Bool_t fDirectInteraction
  // Int_t fMCTrackID

  ClassDef(TSlimRecoLAVHit, 1)
};
#endif /* TSLIMRecoLAVHit_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoLAVCandidate", payloadCode, "@",
"TSlimRecoLAVEvent", payloadCode, "@",
"TSlimRecoLAVHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libLAVSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libLAVSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libLAVSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libLAVSlimPersistency() {
  TriggerDictionaryInitialization_libLAVSlimPersistency_Impl();
}
