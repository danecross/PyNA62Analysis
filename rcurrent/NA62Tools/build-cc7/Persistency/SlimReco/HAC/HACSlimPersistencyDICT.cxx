// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME HACSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/HAC/include/TSlimRecoHACCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/HAC/include/TSlimRecoHACEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/HAC/include/TSlimRecoHACHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoHACCandidate(void *p = 0);
   static void *newArray_TSlimRecoHACCandidate(Long_t size, void *p);
   static void delete_TSlimRecoHACCandidate(void *p);
   static void deleteArray_TSlimRecoHACCandidate(void *p);
   static void destruct_TSlimRecoHACCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoHACCandidate*)
   {
      ::TSlimRecoHACCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoHACCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoHACCandidate", ::TSlimRecoHACCandidate::Class_Version(), "", 16,
                  typeid(::TSlimRecoHACCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoHACCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoHACCandidate) );
      instance.SetNew(&new_TSlimRecoHACCandidate);
      instance.SetNewArray(&newArray_TSlimRecoHACCandidate);
      instance.SetDelete(&delete_TSlimRecoHACCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoHACCandidate);
      instance.SetDestructor(&destruct_TSlimRecoHACCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoHACCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoHACCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoHACCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoHACHit(void *p = 0);
   static void *newArray_TSlimRecoHACHit(Long_t size, void *p);
   static void delete_TSlimRecoHACHit(void *p);
   static void deleteArray_TSlimRecoHACHit(void *p);
   static void destruct_TSlimRecoHACHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoHACHit*)
   {
      ::TSlimRecoHACHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoHACHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoHACHit", ::TSlimRecoHACHit::Class_Version(), "TSlimRecoHACHit.hh", 10,
                  typeid(::TSlimRecoHACHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoHACHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoHACHit) );
      instance.SetNew(&new_TSlimRecoHACHit);
      instance.SetNewArray(&newArray_TSlimRecoHACHit);
      instance.SetDelete(&delete_TSlimRecoHACHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoHACHit);
      instance.SetDestructor(&destruct_TSlimRecoHACHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoHACHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoHACHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoHACHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoHACEvent(void *p = 0);
   static void *newArray_TSlimRecoHACEvent(Long_t size, void *p);
   static void delete_TSlimRecoHACEvent(void *p);
   static void deleteArray_TSlimRecoHACEvent(void *p);
   static void destruct_TSlimRecoHACEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoHACEvent*)
   {
      ::TSlimRecoHACEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoHACEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoHACEvent", ::TSlimRecoHACEvent::Class_Version(), "", 58,
                  typeid(::TSlimRecoHACEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoHACEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoHACEvent) );
      instance.SetNew(&new_TSlimRecoHACEvent);
      instance.SetNewArray(&newArray_TSlimRecoHACEvent);
      instance.SetDelete(&delete_TSlimRecoHACEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoHACEvent);
      instance.SetDestructor(&destruct_TSlimRecoHACEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoHACEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoHACEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoHACEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoHACCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoHACCandidate::Class_Name()
{
   return "TSlimRecoHACCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoHACCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoHACCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoHACCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoHACCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoHACHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoHACHit::Class_Name()
{
   return "TSlimRecoHACHit";
}

//______________________________________________________________________________
const char *TSlimRecoHACHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoHACHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoHACHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoHACHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoHACEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoHACEvent::Class_Name()
{
   return "TSlimRecoHACEvent";
}

//______________________________________________________________________________
const char *TSlimRecoHACEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoHACEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoHACEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoHACEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoHACEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoHACCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoHACCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoHACCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoHACCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoHACCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoHACCandidate : new ::TSlimRecoHACCandidate;
   }
   static void *newArray_TSlimRecoHACCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoHACCandidate[nElements] : new ::TSlimRecoHACCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoHACCandidate(void *p) {
      delete ((::TSlimRecoHACCandidate*)p);
   }
   static void deleteArray_TSlimRecoHACCandidate(void *p) {
      delete [] ((::TSlimRecoHACCandidate*)p);
   }
   static void destruct_TSlimRecoHACCandidate(void *p) {
      typedef ::TSlimRecoHACCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoHACCandidate

//______________________________________________________________________________
void TSlimRecoHACHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoHACHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoHACHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoHACHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoHACHit(void *p) {
      return  p ? new(p) ::TSlimRecoHACHit : new ::TSlimRecoHACHit;
   }
   static void *newArray_TSlimRecoHACHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoHACHit[nElements] : new ::TSlimRecoHACHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoHACHit(void *p) {
      delete ((::TSlimRecoHACHit*)p);
   }
   static void deleteArray_TSlimRecoHACHit(void *p) {
      delete [] ((::TSlimRecoHACHit*)p);
   }
   static void destruct_TSlimRecoHACHit(void *p) {
      typedef ::TSlimRecoHACHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoHACHit

//______________________________________________________________________________
void TSlimRecoHACEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoHACEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoHACEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoHACEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoHACEvent(void *p) {
      return  p ? new(p) ::TSlimRecoHACEvent : new ::TSlimRecoHACEvent;
   }
   static void *newArray_TSlimRecoHACEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoHACEvent[nElements] : new ::TSlimRecoHACEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoHACEvent(void *p) {
      delete ((::TSlimRecoHACEvent*)p);
   }
   static void deleteArray_TSlimRecoHACEvent(void *p) {
      delete [] ((::TSlimRecoHACEvent*)p);
   }
   static void destruct_TSlimRecoHACEvent(void *p) {
      typedef ::TSlimRecoHACEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoHACEvent

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
   static TClass *vectorlETSlimRecoHACHitgR_Dictionary();
   static void vectorlETSlimRecoHACHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoHACHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoHACHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoHACHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoHACHitgR(void *p);
   static void destruct_vectorlETSlimRecoHACHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoHACHit>*)
   {
      vector<TSlimRecoHACHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoHACHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoHACHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoHACHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoHACHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoHACHit>) );
      instance.SetNew(&new_vectorlETSlimRecoHACHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoHACHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoHACHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoHACHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoHACHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoHACHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoHACHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoHACHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoHACHit>*)0x0)->GetClass();
      vectorlETSlimRecoHACHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoHACHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoHACHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoHACHit> : new vector<TSlimRecoHACHit>;
   }
   static void *newArray_vectorlETSlimRecoHACHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoHACHit>[nElements] : new vector<TSlimRecoHACHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoHACHitgR(void *p) {
      delete ((vector<TSlimRecoHACHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoHACHitgR(void *p) {
      delete [] ((vector<TSlimRecoHACHit>*)p);
   }
   static void destruct_vectorlETSlimRecoHACHitgR(void *p) {
      typedef vector<TSlimRecoHACHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoHACHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoHACCandidategR_Dictionary();
   static void vectorlETSlimRecoHACCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoHACCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoHACCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoHACCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoHACCandidategR(void *p);
   static void destruct_vectorlETSlimRecoHACCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoHACCandidate>*)
   {
      vector<TSlimRecoHACCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoHACCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoHACCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoHACCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoHACCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoHACCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoHACCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoHACCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoHACCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoHACCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoHACCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoHACCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoHACCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoHACCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoHACCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoHACCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoHACCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoHACCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoHACCandidate> : new vector<TSlimRecoHACCandidate>;
   }
   static void *newArray_vectorlETSlimRecoHACCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoHACCandidate>[nElements] : new vector<TSlimRecoHACCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoHACCandidategR(void *p) {
      delete ((vector<TSlimRecoHACCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoHACCandidategR(void *p) {
      delete [] ((vector<TSlimRecoHACCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoHACCandidategR(void *p) {
      typedef vector<TSlimRecoHACCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoHACCandidate>

namespace {
  void TriggerDictionaryInitialization_libHACSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/HAC/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/HAC/../../FullReco/HAC/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/HAC/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libHACSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class TSlimRecoHACCandidate;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class __attribute__((annotate("$clingAutoload$TSlimRecoHACHit.hh")))  TSlimRecoHACHit;
class TSlimRecoHACEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libHACSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOHACCANDIDATE_H
#define TSLIMRECOHACCANDIDATE_H

#include <vector>
#include "RtypesCore.h"

#include "TSlimRecoVCandidate.hh"

class TRecoHACCandidate;

class TSlimRecoHACCandidate : public TSlimRecoVCandidate {
public:
    TSlimRecoHACCandidate();
    explicit TSlimRecoHACCandidate(TRecoHACCandidate *candReco);
    virtual ~TSlimRecoHACCandidate() { fHitsIndexes.clear(); }

    //Getters and Setters for data members
    void SetTime(Float_t time)      { fTime = time;                     }
    void AddHitIndex(Short_t index) { fHitsIndexes.emplace_back(index); }
    void SetCharge(Float_t charge)  { fCharge = charge;                 }

    Float_t GetTime()                            const { return fTime;        }
    const std::vector<Short_t>& GetHitsIndices() const { return fHitsIndexes; }
    Float_t GetCharge()                          const { return fCharge;      }

    //Methods for standard persistency
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:
    Float_t fTime;
    Float_t fCharge;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoHACCandidate, 1)
};

#endif /* TSLIMRECOHACCANDIDATE_HH */

#ifndef TSLIMRECOHACEVENT_H
#define TSLIMRECOHACEVENT_H

#include "RtypesCore.h"
#include <vector>

#include "TSlimRecoVEvent.hh"

#include "TSlimRecoHACCandidate.hh"
#include "TSlimRecoHACHit.hh"

class TRecoHACEvent;

class TSlimRecoHACEvent : public TSlimRecoVEvent {
public:
    TSlimRecoHACEvent();
    explicit TSlimRecoHACEvent(TRecoHACEvent *evReco);
    virtual ~TSlimRecoHACEvent() {}

    void Reset(); //clears candidates and hits
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoHACCandidate cand) { fCandidates.emplace_back(std::move(cand)); }
    void AddHit(TSlimRecoHACHit hit)              { fHits.emplace_back(std::move(hit));        }

    Int_t GetNCandidates()                        const {return fCandidates.size(); }
    std::vector<TSlimRecoHACCandidate>& GetCandidates() {return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)     { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }
    std::vector<TSlimRecoHACHit>& GetHits()             {return fHits;              }
    Int_t GetNHits()                              const {return fHits.size();       }
    TSlimRecoVHit* GetHit(UInt_t iHit)                  { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }

    virtual void FromReco(TRecoVEvent *evVReco);
    virtual void ToReco(TRecoVEvent *evVReco);
private:
    std::vector<TSlimRecoHACHit> fHits;
    std::vector<TSlimRecoHACCandidate> fCandidates;

    ClassDef(TSlimRecoHACEvent, 1)
};

#endif /* TSLIMRECOHACEVENT_HH */

#ifndef TSLIMRECOHACHIT_H
#define TSLIMRECOHACHIT_H

#include "Rtypes.h"

#include "TSlimRecoVHit.hh"

class TRecoHACHit;

class TSlimRecoHACHit : public TSlimRecoVHit {
public:
    TSlimRecoHACHit();
    explicit TSlimRecoHACHit(TRecoHACHit *hitReco);
    virtual ~TSlimRecoHACHit() = default;

    //Setters
    void SetChannelID(Short_t channelID)             { fChannelID = channelID;        }
    void SetChargeModuleSection(Float_t charge)      { fChargeModuleSection = charge; }
    void SetTime(Float_t time)                       { fTime = time;                  }
    void SetLeadingEdge(Short_t iThr, Float_t time);
    void SetTrailingEdge(Short_t iThr, Float_t time);

    //Getters
    Int_t   GetSiPMID()                  const;
    Int_t   GetModuleID()                const;
    Short_t GetChannelID()               const { return fChannelID;                                                 }
    Float_t GetChargeModuleSection()     const { return fChargeModuleSection;                                       }
    Float_t GetTime()                    const { return fTime;                                                      }
    Float_t GetLeadingEdge(Short_t iThr) const {
        return (iThr >= 0 && iThr < 4) ? fLeadingEdge[iThr] : -999.999;
    }
    Float_t GetTrailingEdge(Short_t iThr) const {
        return (iThr >= 0 && iThr < 4) ? fTrailingEdge[iThr] : -999.999;
    };

    //Getters for standard persistency class
    Int_t GetEdgeMask() const;
    Double_t GetToTSum() const;

    //Methods to convert to and from Standard persistency
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Short_t fChannelID;
    Float_t fTime;
    Float_t fChargeModuleSection; //Cannot be computed outside HACReconstruction
    Float_t fLeadingEdge[4];
    Float_t fTrailingEdge[4];

    ClassDef(TSlimRecoHACHit, 1)
};

#endif /* TSLIMRECOHACHIT_HH */


#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoHACCandidate", payloadCode, "@",
"TSlimRecoHACEvent", payloadCode, "@",
"TSlimRecoHACHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libHACSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libHACSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libHACSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libHACSlimPersistency() {
  TriggerDictionaryInitialization_libHACSlimPersistency_Impl();
}
