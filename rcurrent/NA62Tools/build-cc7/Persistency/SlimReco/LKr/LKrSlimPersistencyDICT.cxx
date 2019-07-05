// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME LKrSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LKr/include/TSlimRecoLKrCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LKr/include/TSlimRecoLKrEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LKr/include/TSlimRecoLKrHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoLKrHit(void *p = 0);
   static void *newArray_TSlimRecoLKrHit(Long_t size, void *p);
   static void delete_TSlimRecoLKrHit(void *p);
   static void deleteArray_TSlimRecoLKrHit(void *p);
   static void destruct_TSlimRecoLKrHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoLKrHit*)
   {
      ::TSlimRecoLKrHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoLKrHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoLKrHit", ::TSlimRecoLKrHit::Class_Version(), "TSlimRecoLKrHit.hh", 10,
                  typeid(::TSlimRecoLKrHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoLKrHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoLKrHit) );
      instance.SetNew(&new_TSlimRecoLKrHit);
      instance.SetNewArray(&newArray_TSlimRecoLKrHit);
      instance.SetDelete(&delete_TSlimRecoLKrHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoLKrHit);
      instance.SetDestructor(&destruct_TSlimRecoLKrHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoLKrHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoLKrHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoLKrHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoLKrCandidate(void *p = 0);
   static void *newArray_TSlimRecoLKrCandidate(Long_t size, void *p);
   static void delete_TSlimRecoLKrCandidate(void *p);
   static void deleteArray_TSlimRecoLKrCandidate(void *p);
   static void destruct_TSlimRecoLKrCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoLKrCandidate*)
   {
      ::TSlimRecoLKrCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoLKrCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoLKrCandidate", ::TSlimRecoLKrCandidate::Class_Version(), "", 15,
                  typeid(::TSlimRecoLKrCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoLKrCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoLKrCandidate) );
      instance.SetNew(&new_TSlimRecoLKrCandidate);
      instance.SetNewArray(&newArray_TSlimRecoLKrCandidate);
      instance.SetDelete(&delete_TSlimRecoLKrCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoLKrCandidate);
      instance.SetDestructor(&destruct_TSlimRecoLKrCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoLKrCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoLKrCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoLKrCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoLKrEvent(void *p = 0);
   static void *newArray_TSlimRecoLKrEvent(Long_t size, void *p);
   static void delete_TSlimRecoLKrEvent(void *p);
   static void deleteArray_TSlimRecoLKrEvent(void *p);
   static void destruct_TSlimRecoLKrEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoLKrEvent*)
   {
      ::TSlimRecoLKrEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoLKrEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoLKrEvent", ::TSlimRecoLKrEvent::Class_Version(), "", 76,
                  typeid(::TSlimRecoLKrEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoLKrEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoLKrEvent) );
      instance.SetNew(&new_TSlimRecoLKrEvent);
      instance.SetNewArray(&newArray_TSlimRecoLKrEvent);
      instance.SetDelete(&delete_TSlimRecoLKrEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoLKrEvent);
      instance.SetDestructor(&destruct_TSlimRecoLKrEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoLKrEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoLKrEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoLKrEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoLKrHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoLKrHit::Class_Name()
{
   return "TSlimRecoLKrHit";
}

//______________________________________________________________________________
const char *TSlimRecoLKrHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoLKrHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoLKrHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoLKrHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoLKrCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoLKrCandidate::Class_Name()
{
   return "TSlimRecoLKrCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoLKrCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoLKrCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoLKrCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoLKrCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoLKrEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoLKrEvent::Class_Name()
{
   return "TSlimRecoLKrEvent";
}

//______________________________________________________________________________
const char *TSlimRecoLKrEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoLKrEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoLKrEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoLKrEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoLKrEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoLKrHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoLKrHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoLKrHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoLKrHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoLKrHit(void *p) {
      return  p ? new(p) ::TSlimRecoLKrHit : new ::TSlimRecoLKrHit;
   }
   static void *newArray_TSlimRecoLKrHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoLKrHit[nElements] : new ::TSlimRecoLKrHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoLKrHit(void *p) {
      delete ((::TSlimRecoLKrHit*)p);
   }
   static void deleteArray_TSlimRecoLKrHit(void *p) {
      delete [] ((::TSlimRecoLKrHit*)p);
   }
   static void destruct_TSlimRecoLKrHit(void *p) {
      typedef ::TSlimRecoLKrHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoLKrHit

//______________________________________________________________________________
void TSlimRecoLKrCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoLKrCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoLKrCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoLKrCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoLKrCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoLKrCandidate : new ::TSlimRecoLKrCandidate;
   }
   static void *newArray_TSlimRecoLKrCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoLKrCandidate[nElements] : new ::TSlimRecoLKrCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoLKrCandidate(void *p) {
      delete ((::TSlimRecoLKrCandidate*)p);
   }
   static void deleteArray_TSlimRecoLKrCandidate(void *p) {
      delete [] ((::TSlimRecoLKrCandidate*)p);
   }
   static void destruct_TSlimRecoLKrCandidate(void *p) {
      typedef ::TSlimRecoLKrCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoLKrCandidate

//______________________________________________________________________________
void TSlimRecoLKrEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoLKrEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoLKrEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoLKrEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoLKrEvent(void *p) {
      return  p ? new(p) ::TSlimRecoLKrEvent : new ::TSlimRecoLKrEvent;
   }
   static void *newArray_TSlimRecoLKrEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoLKrEvent[nElements] : new ::TSlimRecoLKrEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoLKrEvent(void *p) {
      delete ((::TSlimRecoLKrEvent*)p);
   }
   static void deleteArray_TSlimRecoLKrEvent(void *p) {
      delete [] ((::TSlimRecoLKrEvent*)p);
   }
   static void destruct_TSlimRecoLKrEvent(void *p) {
      typedef ::TSlimRecoLKrEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoLKrEvent

namespace ROOT {
   static TClass *vectorlETSlimRecoLKrHitgR_Dictionary();
   static void vectorlETSlimRecoLKrHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoLKrHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoLKrHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoLKrHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoLKrHitgR(void *p);
   static void destruct_vectorlETSlimRecoLKrHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoLKrHit>*)
   {
      vector<TSlimRecoLKrHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoLKrHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoLKrHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoLKrHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoLKrHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoLKrHit>) );
      instance.SetNew(&new_vectorlETSlimRecoLKrHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoLKrHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoLKrHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoLKrHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoLKrHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoLKrHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoLKrHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoLKrHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoLKrHit>*)0x0)->GetClass();
      vectorlETSlimRecoLKrHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoLKrHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoLKrHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLKrHit> : new vector<TSlimRecoLKrHit>;
   }
   static void *newArray_vectorlETSlimRecoLKrHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLKrHit>[nElements] : new vector<TSlimRecoLKrHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoLKrHitgR(void *p) {
      delete ((vector<TSlimRecoLKrHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoLKrHitgR(void *p) {
      delete [] ((vector<TSlimRecoLKrHit>*)p);
   }
   static void destruct_vectorlETSlimRecoLKrHitgR(void *p) {
      typedef vector<TSlimRecoLKrHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoLKrHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoLKrCandidategR_Dictionary();
   static void vectorlETSlimRecoLKrCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoLKrCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoLKrCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoLKrCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoLKrCandidategR(void *p);
   static void destruct_vectorlETSlimRecoLKrCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoLKrCandidate>*)
   {
      vector<TSlimRecoLKrCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoLKrCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoLKrCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoLKrCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoLKrCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoLKrCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoLKrCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoLKrCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoLKrCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoLKrCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoLKrCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoLKrCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoLKrCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoLKrCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoLKrCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoLKrCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoLKrCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoLKrCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLKrCandidate> : new vector<TSlimRecoLKrCandidate>;
   }
   static void *newArray_vectorlETSlimRecoLKrCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoLKrCandidate>[nElements] : new vector<TSlimRecoLKrCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoLKrCandidategR(void *p) {
      delete ((vector<TSlimRecoLKrCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoLKrCandidategR(void *p) {
      delete [] ((vector<TSlimRecoLKrCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoLKrCandidategR(void *p) {
      typedef vector<TSlimRecoLKrCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoLKrCandidate>

namespace {
  void TriggerDictionaryInitialization_libLKrSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LKr/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LKr/../../FullReco/LKr/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/LKr/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libLKrSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoLKrHit.hh")))  TSlimRecoLKrHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoLKrCandidate;
class TSlimRecoLKrEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libLKrSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSlimRecoLKrCandidate_H
#define TSlimRecoLKrCandidate_H
#include <RtypesCore.h>
#include "TSlimRecoVCandidate.hh"
#include "TClonesArray.h"
#include "TSlimRecoLKrHit.hh"

class TRecoLKrCandidate;

class TSlimRecoLKrCandidate : public TSlimRecoVCandidate {

public:

  TSlimRecoLKrCandidate()  = default;
  explicit TSlimRecoLKrCandidate(TRecoLKrCandidate *candReco);
  virtual ~TSlimRecoLKrCandidate() = default;

  void SetIDSeed(Int_t idseed)                         { fIdSeed = idseed;                       }
  void SetNCells(Int_t nCells)                         { fNCells = nCells;                       }
  void SetClusterTime(Float_t time)                    { fTime = time;                           }
  void SetClusterEnergy(Float_t clusterEnergy)         { fClusterEnergy = clusterEnergy;         }
  void SetClusterX(Float_t clusterX)                   { fClusterX = clusterX;                   }
  void SetClusterY(Float_t clusterY)                   { fClusterY = clusterY;                   }
  void SetClusterRMSX(Float_t clusterRmsx)             { fClusterRMSX = clusterRmsx;             }
  void SetClusterRMSY(Float_t clusterRmsy)             { fClusterRMSY = clusterRmsy;             }
  void SetClusterDDeadCell(Float_t clusterDDeadCell)   { fClusterDDeadCell = clusterDDeadCell;   }
  void SetClusterSeedEnergy(Float_t clusterSeedEnergy) { fClusterSeedEnergy = clusterSeedEnergy; }
  void SetCluster77Energy(Float_t cluster77Energy)     { fCluster77Energy = cluster77Energy;     }

  Int_t   GetIdSeed()            const { return fIdSeed;            }
  Int_t   GetNCells()            const { return fNCells;            }
  Float_t GetClusterTime()       const { return fTime;              }
  Float_t GetClusterEnergy()     const { return fClusterEnergy;     }
  Float_t GetClusterX()          const { return fClusterX;          }
  Float_t GetClusterY()          const { return fClusterY;          }
  Float_t GetClusterRMSX()       const { return fClusterRMSX;       }
  Float_t GetClusterRMSY()       const { return fClusterRMSY;       }
  Float_t GetClusterDDeadCell()  const { return fClusterDDeadCell;  }
  Float_t GetClusterSeedEnergy() const { return fClusterSeedEnergy; }
  Float_t GetCluster77Energy()   const { return fCluster77Energy;   }

  // conversion functions
  virtual void FromReco(TRecoVCandidate *candReco);
  virtual void ToReco(TRecoVCandidate *candReco);
private:
  Int_t   fIdSeed            = 0.;
  Int_t   fNCells            = 0;
  Float_t fTime              = 0.;
  Float_t fClusterEnergy     = 0.;
  Float_t fClusterX          = 9999.;
  Float_t fClusterY          = 9999.;
  Float_t fClusterRMSX       = 0.;
  Float_t fClusterRMSY       = 0.;
  Float_t fClusterDDeadCell  = 0.;
  Float_t fClusterSeedEnergy = 0.;
  Float_t fCluster77Energy   = 0.;

  ClassDef(TSlimRecoLKrCandidate, 1)
};
#endif
#ifndef TSlimRecoLKrEvent_H
#define TSlimRecoLKrEvent_H
#include <RtypesCore.h>
#include "TSlimRecoVEvent.hh"
#include "TSlimRecoLKrCandidate.hh"
#include "TSlimRecoLKrHit.hh"

class TRecoLKrEvent;

class TSlimRecoLKrEvent : public TSlimRecoVEvent {

  public:
    TSlimRecoLKrEvent() = default;
    explicit TSlimRecoLKrEvent(TRecoLKrEvent *evVReco);
    virtual ~TSlimRecoLKrEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void SetRecFlag(Int_t val)                 { fRecFlag=val;                           }
    void SetEnergyTotal(Double_t val)          { fEnergyTotal=val;                       }
    void AddHit(TSlimRecoLKrHit h)             { fHits.emplace_back(std::move(h));       }
    void AddCandidate(TSlimRecoLKrCandidate c) { fCandidates.emplace_back(std::move(c)); }

    Int_t GetRecFlag()                                  const { return fRecFlag;           }
    Double_t GetEnergyTotal()                           const { return fEnergyTotal;       }
    Int_t GetNHits()                                    const { return fHits.size();       }
    std::vector<TSlimRecoLKrHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                        { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                              const { return fCandidates.size(); }
    std::vector<TSlimRecoLKrCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    void FromReco(TRecoVEvent *evVReco);
    void ToReco(TRecoVEvent *evVReco);
  private:
    Int_t     fRecFlag     = 0;
    Double_t  fEnergyTotal = 0.;
    std::vector<TSlimRecoLKrHit> fHits;
    std::vector<TSlimRecoLKrCandidate> fCandidates;

    ClassDef(TSlimRecoLKrEvent, 1)
};
#endif
#ifndef TSlimRecoLKrHit_H
#define TSlimRecoLKrHit_H
#include <RtypesCore.h>
#include "TVector3.h"
#include "NA62Global.hh"
#include "TSlimRecoVHit.hh"

class TRecoLKrHit;

class TSlimRecoLKrHit : public TSlimRecoVHit {
    static constexpr Float_t LKrCellSize = 19.7383881; // mm

  public:
    TSlimRecoLKrHit() = default;
    explicit TSlimRecoLKrHit(TRecoLKrHit *hitReco);
    virtual ~TSlimRecoLKrHit() = default;

    void SetChannelID(Int_t val)  { fChannelID = val; }
    void SetTime(Float_t val)     { fTime = val;      }
    void SetPedestal(Float_t val) { fPedestal = val;  }
    void SetEnergy(Double_t val)  { fEnergy = val;    }

    Int_t GetChannelID()  const { return fChannelID; }
    Float_t GetTime()     const { return fTime;      }
    Float_t GetPedestal() const { return fPedestal;  }
    Double_t GetEnergy()  const { return fEnergy;    }

    // variables retrieved from Slim Persistency
    Int_t GetXCellID()      const;
    Int_t GetYCellID()      const;
    Int_t GetCPDID()        const;
    Int_t GetCPDChannelID() const;
    TVector3 GetPosition()  const { return TVector3((GetXCellID()-0.5*127)*LKrCellSize,(GetYCellID()-0.5*127)*LKrCellSize,0);}

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);

  private:
    Int_t     fChannelID  = 0;
    Float_t   fTime       = 0;
    Float_t   fPedestal   = 0;
    Double_t  fEnergy     = 0;

    ClassDef(TSlimRecoLKrHit, 1)
};
#endif





#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoLKrCandidate", payloadCode, "@",
"TSlimRecoLKrEvent", payloadCode, "@",
"TSlimRecoLKrHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libLKrSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libLKrSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libLKrSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libLKrSlimPersistency() {
  TriggerDictionaryInitialization_libLKrSlimPersistency_Impl();
}
