// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME NA62SlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include/TSlimRecoMUVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include/TSlimRecoMUVHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include/TSlimRecoVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include/TSlimRecoVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include/TSlimRecoVHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void delete_TSlimRecoVCandidate(void *p);
   static void deleteArray_TSlimRecoVCandidate(void *p);
   static void destruct_TSlimRecoVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoVCandidate*)
   {
      ::TSlimRecoVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoVCandidate", ::TSlimRecoVCandidate::Class_Version(), "TSlimRecoVCandidate.hh", 8,
                  typeid(::TSlimRecoVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoVCandidate) );
      instance.SetDelete(&delete_TSlimRecoVCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoVCandidate);
      instance.SetDestructor(&destruct_TSlimRecoVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoVCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_TSlimRecoMUVCandidate(void *p);
   static void deleteArray_TSlimRecoMUVCandidate(void *p);
   static void destruct_TSlimRecoMUVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUVCandidate*)
   {
      ::TSlimRecoMUVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUVCandidate", ::TSlimRecoMUVCandidate::Class_Version(), "", 18,
                  typeid(::TSlimRecoMUVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUVCandidate) );
      instance.SetDelete(&delete_TSlimRecoMUVCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUVCandidate);
      instance.SetDestructor(&destruct_TSlimRecoMUVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUVCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_TSlimRecoVHit(void *p);
   static void deleteArray_TSlimRecoVHit(void *p);
   static void destruct_TSlimRecoVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoVHit*)
   {
      ::TSlimRecoVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoVHit", ::TSlimRecoVHit::Class_Version(), "TSlimRecoVHit.hh", 8,
                  typeid(::TSlimRecoVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoVHit) );
      instance.SetDelete(&delete_TSlimRecoVHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoVHit);
      instance.SetDestructor(&destruct_TSlimRecoVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoVHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_TSlimRecoMUVHit(void *p);
   static void deleteArray_TSlimRecoMUVHit(void *p);
   static void destruct_TSlimRecoMUVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUVHit*)
   {
      ::TSlimRecoMUVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUVHit", ::TSlimRecoMUVHit::Class_Version(), "", 123,
                  typeid(::TSlimRecoMUVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUVHit) );
      instance.SetDelete(&delete_TSlimRecoMUVHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUVHit);
      instance.SetDestructor(&destruct_TSlimRecoMUVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUVHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_TSlimRecoVEvent(void *p);
   static void deleteArray_TSlimRecoVEvent(void *p);
   static void destruct_TSlimRecoVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoVEvent*)
   {
      ::TSlimRecoVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoVEvent", ::TSlimRecoVEvent::Class_Version(), "", 241,
                  typeid(::TSlimRecoVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoVEvent) );
      instance.SetDelete(&delete_TSlimRecoVEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoVEvent);
      instance.SetDestructor(&destruct_TSlimRecoVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoVEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoVCandidate::Class_Name()
{
   return "TSlimRecoVCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUVCandidate::Class_Name()
{
   return "TSlimRecoMUVCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoMUVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoVHit::Class_Name()
{
   return "TSlimRecoVHit";
}

//______________________________________________________________________________
const char *TSlimRecoVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUVHit::Class_Name()
{
   return "TSlimRecoMUVHit";
}

//______________________________________________________________________________
const char *TSlimRecoMUVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoVEvent::Class_Name()
{
   return "TSlimRecoVEvent";
}

//______________________________________________________________________________
const char *TSlimRecoVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TSlimRecoVCandidate(void *p) {
      delete ((::TSlimRecoVCandidate*)p);
   }
   static void deleteArray_TSlimRecoVCandidate(void *p) {
      delete [] ((::TSlimRecoVCandidate*)p);
   }
   static void destruct_TSlimRecoVCandidate(void *p) {
      typedef ::TSlimRecoVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoVCandidate

//______________________________________________________________________________
void TSlimRecoMUVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TSlimRecoMUVCandidate(void *p) {
      delete ((::TSlimRecoMUVCandidate*)p);
   }
   static void deleteArray_TSlimRecoMUVCandidate(void *p) {
      delete [] ((::TSlimRecoMUVCandidate*)p);
   }
   static void destruct_TSlimRecoMUVCandidate(void *p) {
      typedef ::TSlimRecoMUVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUVCandidate

//______________________________________________________________________________
void TSlimRecoVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TSlimRecoVHit(void *p) {
      delete ((::TSlimRecoVHit*)p);
   }
   static void deleteArray_TSlimRecoVHit(void *p) {
      delete [] ((::TSlimRecoVHit*)p);
   }
   static void destruct_TSlimRecoVHit(void *p) {
      typedef ::TSlimRecoVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoVHit

//______________________________________________________________________________
void TSlimRecoMUVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TSlimRecoMUVHit(void *p) {
      delete ((::TSlimRecoMUVHit*)p);
   }
   static void deleteArray_TSlimRecoMUVHit(void *p) {
      delete [] ((::TSlimRecoMUVHit*)p);
   }
   static void destruct_TSlimRecoMUVHit(void *p) {
      typedef ::TSlimRecoMUVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUVHit

//______________________________________________________________________________
void TSlimRecoVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TSlimRecoVEvent(void *p) {
      delete ((::TSlimRecoVEvent*)p);
   }
   static void deleteArray_TSlimRecoVEvent(void *p) {
      delete [] ((::TSlimRecoVEvent*)p);
   }
   static void destruct_TSlimRecoVEvent(void *p) {
      typedef ::TSlimRecoVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoVEvent

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

namespace {
  void TriggerDictionaryInitialization_libNA62SlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/../../FullReco/NA62/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/NA62/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libNA62SlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoVCandidate.hh")))  TSlimRecoVCandidate;
class TSlimRecoMUVCandidate;
class __attribute__((annotate("$clingAutoload$TSlimRecoVHit.hh")))  TSlimRecoVHit;
class TSlimRecoMUVHit;
class TSlimRecoVEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libNA62SlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOMUVCANDIDATE_H
#define TSLIMRECOMUVCANDIDATE_H

#include "TSlimRecoVCandidate.hh"
#include "NA62Global.hh"

#include <RtypesCore.h>
#include <TVector2.h>
#include <TMath.h>

#include <vector>

class TSlimRecoMUVCandidate : public TSlimRecoVCandidate {

public:
	TSlimRecoMUVCandidate ()          { ClearBase(); }
	virtual ~TSlimRecoMUVCandidate () {}

	virtual void Clear(Option_t *option="");
	void ClearBase(Option_t *option="");

	//Setters
	void SetChargeH(Float_t chargeH) { fChargeH = chargeH;               }
	void SetChargeV(Float_t chargeV) { fChargeV = chargeV;               }
	void SetEnergy(Float_t energy)   { fEnergy = energy;                 }
	void SetEnergyH(Float_t energyH) { fEnergyH = energyH;               }
	void SetEnergyV(Float_t energyV) { fEnergyV = energyV;               }
	void AddHitIndex(Short_t index)  { fHitsIndexes.emplace_back(index); }
	void SetHitsIndexes(Int_t Nhits, Int_t *hitsIndexes) {
	    fHitsIndexes.clear();
		fHitsIndexes.resize(Nhits);
		for (int i=0; i<Nhits; i++) fHitsIndexes[i] = hitsIndexes[i];
	}

	void SetIndexH(Int_t indexH, Int_t sideH)               { fIndexH = 100*sideH + indexH;                   }
	void SetIndexV(Short_t indexV, Int_t sideV)             { fIndexV = 100*sideV + indexV;                   }
	void SetInnerEnergyH(Float_t innerEnergyH)              { fInnerEnergyH = innerEnergyH;                   }
	void SetInnerEnergyV(Float_t innerEnergyV)              { fInnerEnergyV = innerEnergyV;                   }
	void SetSeedEnergyH(Float_t seedEnergyH)                { fSeedEnergyH = seedEnergyH;                     }
	void SetSeedEnergyV(Float_t seedEnergyV)                { fSeedEnergyV = seedEnergyV;                     }
	void SetSeedIndexes(Int_t seedIndexH, Int_t seedIndexV) { fSeedIndexes = 100*seedIndexV+seedIndexH;       }
	void SetShowerWidthH(Float_t showerWidthH)              { fShowerWidthH = showerWidthH;                   }
	void SetShowerWidthV(Float_t showerWidthV)              { fShowerWidthV = showerWidthV;                   }
	void SetTimeH(Float_t timeH)                            { fTimeH = timeH;                                 }
	void SetTimeV(Float_t timeV)                            { fTimeV = timeV;                                 }
	void SetTimeSigmaH(Float_t timeSigmaH)                  { fTimeSigmaH = timeSigmaH;                       }
	void SetTimeSigmaV(Float_t timeSigmaV)                  { fTimeSigmaV = timeSigmaV;                       }
	void SetPosition (Float_t x, Float_t y)                 { fPosition[0] = x; fPosition[1] = y;             }
	void SetPosition (TVector2 pos)                         { fPosition[0] = pos.X(); fPosition[1] = pos.Y(); }

	//Getters
	Float_t GetCharge()                   const { return 0.5*(fChargeH + fChargeV);                                            }
	Float_t GetChargeH()                  const { return fChargeH;                                                             }
	Float_t GetChargeV()                  const { return fChargeV;                                                             }
	Float_t GetEnergy()                   const { return fEnergy;                                                              }
	Float_t GetEnergyH()                  const { return fEnergyH;                                                             }
	Float_t GetEnergyV()                  const { return fEnergyV;                                                             }
	Int_t GetNHits()                      const { return fHitsIndexes.size();                                                  }
	std::vector<Short_t> GetHitsIndexes() const { return fHitsIndexes;                                                         }
	Short_t GetIndexH()                   const { return fIndexH;                                                              }
	Short_t GetIndexV()                   const { return fIndexV;                                                              }
	Float_t GetInnerEnergy()              const { return 0.5*(fInnerEnergyH+fInnerEnergyV);                                    }
	Float_t GetInnerEnergyH()             const { return fInnerEnergyH;                                                        }
	Float_t GetInnerEnergyV()             const { return fInnerEnergyV;                                                        }
	TVector2 GetPosition()                const { return TVector2(fPosition[0],fPosition[1]);                                  }
	Float_t GetSeedEnergy()               const { return 0.5*(fSeedEnergyH+fSeedEnergyV);                                      }
	Float_t GetSeedEnergyH()              const { return fSeedEnergyH;                                                         }
	Float_t GetSeedEnergyV()              const { return fSeedEnergyV;                                                         }
	Short_t GetSeedIndexH()               const { return fSeedIndexes%100;                                                     }
	Short_t GetSeedIndexV()               const { return static_cast<Short_t>(fSeedIndexes/100);                               }
	Float_t GetShowerWidth()              const { return TMath::Sqrt(fShowerWidthH*fShowerWidthH+fShowerWidthV*fShowerWidthV); }
	Float_t GetShowerWidthH()             const { return fShowerWidthH;                                                        }
	Float_t GetShowerWidthV()             const { return fShowerWidthV;                                                        }
	Float_t GetTime()                     const { return 0.5*(fTimeH+fTimeV);                                                  }
	Float_t GetTimeH()                    const { return fTimeH;                                                               }
	Float_t GetTimeV()                    const { return fTimeV;                                                               }
	Float_t GetTimeSigmaHorizontal()      const { return fTimeSigmaH;                                                          }
	Float_t GetTimeSigmaVertical()        const { return fTimeSigmaV;                                                          }

	//Replicating Standard Persistency
	Int_t GetHorizontalIndex()   const { return fIndexH/100;     }
	Int_t GetVerticalIndex()     const { return fIndexV/100;     }
	Int_t GetHorizontalChannel() const { return fIndexH%100;     }
	Int_t GetVerticalChannel()   const { return fIndexV%100;     }
	Float_t GetPlaneTimeDiff()   const { return fTimeH - fTimeV; }
	Float_t GetX()               const { return fPosition[0];    }
	Float_t GetY()               const { return fPosition[1];    }
	Int_t GetQuadrant()          const;

protected:
	Short_t fIndexH, fIndexV;
	Short_t fSeedIndexes;
	Float_t fTimeH, fTimeV;
	Float_t fEnergy, fEnergyH, fEnergyV;
	Float_t fInnerEnergyH, fInnerEnergyV;
	Float_t fSeedEnergyH, fSeedEnergyV;
	Float_t fChargeH, fChargeV;
	Float_t fShowerWidthH, fShowerWidthV;
	Float_t fTimeSigmaH, fTimeSigmaV;
	Float_t fPosition[2];

	std::vector<Short_t> fHitsIndexes;

	ClassDef(TSlimRecoMUVCandidate, 1)
};


#endif
#ifndef TSLIMRECOMUVHIT_H
#define TSLIMRECOMUVHIT_H

#include "TSlimRecoVHit.hh"
#include "NA62Global.hh"
#include <RtypesCore.h>
#include <TMath.h>

class TSlimRecoMUVHit : public TSlimRecoVHit {

public:
	TSlimRecoMUVHit ()          { ClearBase(); }
	virtual ~TSlimRecoMUVHit () {}

	virtual void Clear (Option_t * option="");
	void ClearBase (Option_t * option="");

	// Setters
	void SetChannelID(Short_t channelId)     { fChannelID = channelId;      }
	void SetPeakAmplitude(Float_t amplitude) { fPeakAmplitude = amplitude;  }
	void SetPeakAmplitudeError(Float_t amplitude, Float_t amplitudeError) {
		fPeakAmplitude = amplitude;
		Float_t val = 1.e+3*amplitudeError/amplitude;
		if (val>65535) val=65535;
		fPeakAmplitudeError = static_cast<UShort_t>(val);
	}
	void SetCharge(Float_t charge)           { fCharge = charge;            }
	void SetChargeError(Float_t charge, Float_t chargeError) {
		fCharge = charge;
		if (charge<10) charge=10.;
		Float_t val = 1.e+3*chargeError/charge;
		if (val>65535) val=65535;
		fChargeError = static_cast<UShort_t>(val);
	}
	void SetSigma(Float_t sigma)             { fSigma = sigma;              }
	void SetSigmaError(Float_t sigma, Float_t sigmaError) {
		fSigma = sigma;
		Float_t val = 1.e+3*sigmaError/sigma;
		if (val>65535) val=65535;
		fSigmaError = static_cast<UShort_t> (val);
	}
	void SetTime(Float_t time)               { fTime = time;                }
	void SetTimeError(Float_t time, Float_t timeError) {
		fTime = time;
		Float_t val = 1.e+3*timeError/10.;
		if (val>65535) val=65535;
		fTimeError = static_cast<UShort_t>(val);
	}

	// Getters
	Short_t GetChannelID()          const { return fChannelID;     }
	Float_t GetPeakAmplitude()      const { return fPeakAmplitude; }
	Float_t GetCharge()             const { return fCharge;        }
	Float_t GetSigma()              const { return fSigma;         }
	Float_t GetTime()               const { return fTime;          }
	Float_t GetPeakAmplitudeError() const {
	    return 1.e-3*static_cast<Float_t>(fPeakAmplitudeError)*fPeakAmplitude;
	}
	Float_t GetChargeError() const {
		Float_t charge=fCharge;
		if (charge<10.) charge=10.;
		return 1.e-3*static_cast<Float_t>(fChargeError)*charge;
	}
	Float_t GetSigmaError() const {
		return 1.e-3*static_cast<float>(fSigmaError)*fSigma;
	}
	Float_t GetTimeError() const {
		return 10.e-3*static_cast<Float_t>(fTimeError);
	}

	//Replicating Standard Persistency
	Int_t GetScintillatorOrientation () const { return (fChannelID%100 > 50 ? 0 : 1);                         }
	Int_t GetPlane ()                   const { return (GetSide()%2==0 ? kHorizontalPlane : kVerticalPlane ); }

	// To be implemented in detector specific classes
	virtual Int_t GetQuadrant ()              const { return 0;  }
	virtual Float_t GetScintillatorPosition() const { return 0.; }
	virtual Float_t GetEnergy ()              const { return 0.; }
    virtual Int_t GetScintillatorNumber ()    const = 0;
    virtual Int_t GetSide ()                  const = 0;


protected:
	Short_t fChannelID;
	UShort_t fTimeError, fPeakAmplitudeError, fSigmaError, fChargeError;
	Float_t fTime;
	Float_t fPeakAmplitude;
	Float_t fCharge;
	Float_t fSigma;

	ClassDef(TSlimRecoMUVHit, 1)
};

#endif
#ifndef TSLIMRECOVCANDIDATE_H
#define TSLIMRECOVCANDIDATE_H

#include <TObject.h>

class TRecoVCandidate;

class TSlimRecoVCandidate : public TObject {
public:
    TSlimRecoVCandidate()          {}
    virtual ~TSlimRecoVCandidate() {}

    virtual void FromReco(TRecoVCandidate *CandReco) = 0;
    virtual void ToReco(TRecoVCandidate *CandReco)   = 0;

    ClassDef(TSlimRecoVCandidate, 1)
};


#endif /* TSLIMRECOVCANDIDATE_H */
#ifndef PERSISTENCY_SLIM_INCLUDE_TSLIMRECOVEVENT_HH_
#define PERSISTENCY_SLIM_INCLUDE_TSLIMRECOVEVENT_HH_

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <TObject.h>

class TRecoVEvent;
class TSlimRecoVHit;
class TSlimRecoVCandidate;

class TSlimRecoVEvent : public TObject{
public:
    TSlimRecoVEvent()          = default;
    virtual ~TSlimRecoVEvent() = default;

    virtual void Reset();
    virtual void ClearHits();
    virtual void ClearCandidates();

    void SetErrorMask(ULong64_t mask)       { fErrorMask = mask; }

    ULong64_t GetErrorMask()          const { return fErrorMask; }
    virtual TSlimRecoVHit* GetHit(UInt_t ) { return nullptr; }
    virtual TSlimRecoVCandidate* GetCandidate(UInt_t ) { return nullptr; }

    virtual void FromReco(TRecoVEvent *evReco) = 0;
    virtual void ToReco(TRecoVEvent *evReco)   = 0;
private:
    ULong64_t fErrorMask = 0;

    ClassDef(TSlimRecoVEvent, 1)
};

#endif /* PERSISTENCY_SLIM_INCLUDE_TSLIMRECOVEVENT_HH_ */
#ifndef TSLIMRECOVHIT_H
#define TSLIMRECOVHIT_H

#include <TObject.h>

class TRecoVHit;

class TSlimRecoVHit : public TObject {
public:
    TSlimRecoVHit()          {}
    virtual ~TSlimRecoVHit() {}

    virtual void FromReco(TRecoVHit *hitReco) = 0;
    virtual void ToReco(TRecoVHit *hitReco)   = 0;

    ClassDef(TSlimRecoVHit, 1)
};


#endif /* TSLIMRECOVHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoMUVCandidate", payloadCode, "@",
"TSlimRecoMUVHit", payloadCode, "@",
"TSlimRecoVCandidate", payloadCode, "@",
"TSlimRecoVEvent", payloadCode, "@",
"TSlimRecoVHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libNA62SlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libNA62SlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libNA62SlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libNA62SlimPersistency() {
  TriggerDictionaryInitialization_libNA62SlimPersistency_Impl();
}
