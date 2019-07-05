// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME HACPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include/HACChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include/THACDigi.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include/THACEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include/THACHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include/TRecoHACCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include/TRecoHACEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include/TRecoHACHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_HACChannelID(void *p = 0);
   static void *newArray_HACChannelID(Long_t size, void *p);
   static void delete_HACChannelID(void *p);
   static void deleteArray_HACChannelID(void *p);
   static void destruct_HACChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::HACChannelID*)
   {
      ::HACChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::HACChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("HACChannelID", ::HACChannelID::Class_Version(), "", 18,
                  typeid(::HACChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::HACChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::HACChannelID) );
      instance.SetNew(&new_HACChannelID);
      instance.SetNewArray(&newArray_HACChannelID);
      instance.SetDelete(&delete_HACChannelID);
      instance.SetDeleteArray(&deleteArray_HACChannelID);
      instance.SetDestructor(&destruct_HACChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::HACChannelID*)
   {
      return GenerateInitInstanceLocal((::HACChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::HACChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_THACDigi(void *p = 0);
   static void *newArray_THACDigi(Long_t size, void *p);
   static void delete_THACDigi(void *p);
   static void deleteArray_THACDigi(void *p);
   static void destruct_THACDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::THACDigi*)
   {
      ::THACDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::THACDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("THACDigi", ::THACDigi::Class_Version(), "", 53,
                  typeid(::THACDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::THACDigi::Dictionary, isa_proxy, 4,
                  sizeof(::THACDigi) );
      instance.SetNew(&new_THACDigi);
      instance.SetNewArray(&newArray_THACDigi);
      instance.SetDelete(&delete_THACDigi);
      instance.SetDeleteArray(&deleteArray_THACDigi);
      instance.SetDestructor(&destruct_THACDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::THACDigi*)
   {
      return GenerateInitInstanceLocal((::THACDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::THACDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_THACEvent(void *p = 0);
   static void *newArray_THACEvent(Long_t size, void *p);
   static void delete_THACEvent(void *p);
   static void deleteArray_THACEvent(void *p);
   static void destruct_THACEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::THACEvent*)
   {
      ::THACEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::THACEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("THACEvent", ::THACEvent::Class_Version(), "", 84,
                  typeid(::THACEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::THACEvent::Dictionary, isa_proxy, 4,
                  sizeof(::THACEvent) );
      instance.SetNew(&new_THACEvent);
      instance.SetNewArray(&newArray_THACEvent);
      instance.SetDelete(&delete_THACEvent);
      instance.SetDeleteArray(&deleteArray_THACEvent);
      instance.SetDestructor(&destruct_THACEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::THACEvent*)
   {
      return GenerateInitInstanceLocal((::THACEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::THACEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_THACHit(void *p = 0);
   static void *newArray_THACHit(Long_t size, void *p);
   static void delete_THACHit(void *p);
   static void deleteArray_THACHit(void *p);
   static void destruct_THACHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::THACHit*)
   {
      ::THACHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::THACHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("THACHit", ::THACHit::Class_Version(), "", 105,
                  typeid(::THACHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::THACHit::Dictionary, isa_proxy, 4,
                  sizeof(::THACHit) );
      instance.SetNew(&new_THACHit);
      instance.SetNewArray(&newArray_THACHit);
      instance.SetDelete(&delete_THACHit);
      instance.SetDeleteArray(&deleteArray_THACHit);
      instance.SetDestructor(&destruct_THACHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::THACHit*)
   {
      return GenerateInitInstanceLocal((::THACHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::THACHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoHACCandidate(void *p = 0);
   static void *newArray_TRecoHACCandidate(Long_t size, void *p);
   static void delete_TRecoHACCandidate(void *p);
   static void deleteArray_TRecoHACCandidate(void *p);
   static void destruct_TRecoHACCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoHACCandidate*)
   {
      ::TRecoHACCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoHACCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoHACCandidate", ::TRecoHACCandidate::Class_Version(), "", 137,
                  typeid(::TRecoHACCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoHACCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoHACCandidate) );
      instance.SetNew(&new_TRecoHACCandidate);
      instance.SetNewArray(&newArray_TRecoHACCandidate);
      instance.SetDelete(&delete_TRecoHACCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoHACCandidate);
      instance.SetDestructor(&destruct_TRecoHACCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoHACCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoHACCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoHACCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoHACHit(void *p = 0);
   static void *newArray_TRecoHACHit(Long_t size, void *p);
   static void delete_TRecoHACHit(void *p);
   static void deleteArray_TRecoHACHit(void *p);
   static void destruct_TRecoHACHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoHACHit*)
   {
      ::TRecoHACHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoHACHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoHACHit", ::TRecoHACHit::Class_Version(), "TRecoHACHit.hh", 6,
                  typeid(::TRecoHACHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoHACHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoHACHit) );
      instance.SetNew(&new_TRecoHACHit);
      instance.SetNewArray(&newArray_TRecoHACHit);
      instance.SetDelete(&delete_TRecoHACHit);
      instance.SetDeleteArray(&deleteArray_TRecoHACHit);
      instance.SetDestructor(&destruct_TRecoHACHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoHACHit*)
   {
      return GenerateInitInstanceLocal((::TRecoHACHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoHACHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoHACEvent(void *p = 0);
   static void *newArray_TRecoHACEvent(Long_t size, void *p);
   static void delete_TRecoHACEvent(void *p);
   static void deleteArray_TRecoHACEvent(void *p);
   static void destruct_TRecoHACEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoHACEvent*)
   {
      ::TRecoHACEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoHACEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoHACEvent", ::TRecoHACEvent::Class_Version(), "", 178,
                  typeid(::TRecoHACEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoHACEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoHACEvent) );
      instance.SetNew(&new_TRecoHACEvent);
      instance.SetNewArray(&newArray_TRecoHACEvent);
      instance.SetDelete(&delete_TRecoHACEvent);
      instance.SetDeleteArray(&deleteArray_TRecoHACEvent);
      instance.SetDestructor(&destruct_TRecoHACEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoHACEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoHACEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoHACEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr HACChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *HACChannelID::Class_Name()
{
   return "HACChannelID";
}

//______________________________________________________________________________
const char *HACChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::HACChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int HACChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::HACChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *HACChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::HACChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *HACChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::HACChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr THACDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *THACDigi::Class_Name()
{
   return "THACDigi";
}

//______________________________________________________________________________
const char *THACDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::THACDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int THACDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::THACDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *THACDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::THACDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *THACDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::THACDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr THACEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *THACEvent::Class_Name()
{
   return "THACEvent";
}

//______________________________________________________________________________
const char *THACEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::THACEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int THACEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::THACEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *THACEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::THACEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *THACEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::THACEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr THACHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *THACHit::Class_Name()
{
   return "THACHit";
}

//______________________________________________________________________________
const char *THACHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::THACHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int THACHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::THACHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *THACHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::THACHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *THACHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::THACHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoHACCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoHACCandidate::Class_Name()
{
   return "TRecoHACCandidate";
}

//______________________________________________________________________________
const char *TRecoHACCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoHACCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoHACCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoHACCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoHACHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoHACHit::Class_Name()
{
   return "TRecoHACHit";
}

//______________________________________________________________________________
const char *TRecoHACHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoHACHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoHACHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoHACHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoHACEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoHACEvent::Class_Name()
{
   return "TRecoHACEvent";
}

//______________________________________________________________________________
const char *TRecoHACEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoHACEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoHACEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoHACEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoHACEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void HACChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class HACChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(HACChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(HACChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_HACChannelID(void *p) {
      return  p ? new(p) ::HACChannelID : new ::HACChannelID;
   }
   static void *newArray_HACChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::HACChannelID[nElements] : new ::HACChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_HACChannelID(void *p) {
      delete ((::HACChannelID*)p);
   }
   static void deleteArray_HACChannelID(void *p) {
      delete [] ((::HACChannelID*)p);
   }
   static void destruct_HACChannelID(void *p) {
      typedef ::HACChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::HACChannelID

//______________________________________________________________________________
void THACDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class THACDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(THACDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(THACDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_THACDigi(void *p) {
      return  p ? new(p) ::THACDigi : new ::THACDigi;
   }
   static void *newArray_THACDigi(Long_t nElements, void *p) {
      return p ? new(p) ::THACDigi[nElements] : new ::THACDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_THACDigi(void *p) {
      delete ((::THACDigi*)p);
   }
   static void deleteArray_THACDigi(void *p) {
      delete [] ((::THACDigi*)p);
   }
   static void destruct_THACDigi(void *p) {
      typedef ::THACDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::THACDigi

//______________________________________________________________________________
void THACEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class THACEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(THACEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(THACEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_THACEvent(void *p) {
      return  p ? new(p) ::THACEvent : new ::THACEvent;
   }
   static void *newArray_THACEvent(Long_t nElements, void *p) {
      return p ? new(p) ::THACEvent[nElements] : new ::THACEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_THACEvent(void *p) {
      delete ((::THACEvent*)p);
   }
   static void deleteArray_THACEvent(void *p) {
      delete [] ((::THACEvent*)p);
   }
   static void destruct_THACEvent(void *p) {
      typedef ::THACEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::THACEvent

//______________________________________________________________________________
void THACHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class THACHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(THACHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(THACHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_THACHit(void *p) {
      return  p ? new(p) ::THACHit : new ::THACHit;
   }
   static void *newArray_THACHit(Long_t nElements, void *p) {
      return p ? new(p) ::THACHit[nElements] : new ::THACHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_THACHit(void *p) {
      delete ((::THACHit*)p);
   }
   static void deleteArray_THACHit(void *p) {
      delete [] ((::THACHit*)p);
   }
   static void destruct_THACHit(void *p) {
      typedef ::THACHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::THACHit

//______________________________________________________________________________
void TRecoHACCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoHACCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoHACCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoHACCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoHACCandidate(void *p) {
      return  p ? new(p) ::TRecoHACCandidate : new ::TRecoHACCandidate;
   }
   static void *newArray_TRecoHACCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoHACCandidate[nElements] : new ::TRecoHACCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoHACCandidate(void *p) {
      delete ((::TRecoHACCandidate*)p);
   }
   static void deleteArray_TRecoHACCandidate(void *p) {
      delete [] ((::TRecoHACCandidate*)p);
   }
   static void destruct_TRecoHACCandidate(void *p) {
      typedef ::TRecoHACCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoHACCandidate

//______________________________________________________________________________
void TRecoHACHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoHACHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoHACHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoHACHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoHACHit(void *p) {
      return  p ? new(p) ::TRecoHACHit : new ::TRecoHACHit;
   }
   static void *newArray_TRecoHACHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoHACHit[nElements] : new ::TRecoHACHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoHACHit(void *p) {
      delete ((::TRecoHACHit*)p);
   }
   static void deleteArray_TRecoHACHit(void *p) {
      delete [] ((::TRecoHACHit*)p);
   }
   static void destruct_TRecoHACHit(void *p) {
      typedef ::TRecoHACHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoHACHit

//______________________________________________________________________________
void TRecoHACEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoHACEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoHACEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoHACEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoHACEvent(void *p) {
      return  p ? new(p) ::TRecoHACEvent : new ::TRecoHACEvent;
   }
   static void *newArray_TRecoHACEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoHACEvent[nElements] : new ::TRecoHACEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoHACEvent(void *p) {
      delete ((::TRecoHACEvent*)p);
   }
   static void deleteArray_TRecoHACEvent(void *p) {
      delete [] ((::TRecoHACEvent*)p);
   }
   static void destruct_TRecoHACEvent(void *p) {
      typedef ::TRecoHACEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoHACEvent

namespace {
  void TriggerDictionaryInitialization_libHACPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/HAC/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libHACPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class HACChannelID;
class THACDigi;
class THACEvent;
class THACHit;
class TRecoHACCandidate;
class __attribute__((annotate("$clingAutoload$TRecoHACHit.hh")))  TRecoHACHit;
class TRecoHACEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libHACPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// ---------------------------------------------------------
// History:
// Modified by Mario Bragadireanu (mario.bragadireanu@cern.ch) 2016-08-31
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-22
//
// ---------------------------------------------------------

#ifndef HAC_PERSISTENCY_INCLUDE_HACCHANNELID_HH_
#define HAC_PERSISTENCY_INCLUDE_HACCHANNELID_HH_

#include "Rtypes.h"

class HACChannelID {
public:
  struct chIDDecoded { Int_t fSiPMID, fModuleID; };

  HACChannelID();
  explicit HACChannelID(Int_t);
  virtual ~HACChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  static struct chIDDecoded DecodeChannelID_Static(Int_t); // converts position ID into PMTID, IsHighThreshold
  void  DecodeChannelID(Int_t); // converts position ID into PMTID, IsHighThreshold
  Int_t GetSiPMID()          const { return fSiPMID;             };
  Int_t GetModuleID()        const { return fModuleID;           };
private:
  Int_t fSiPMID;
  Int_t fModuleID;
  ClassDef(HACChannelID,1);
};

#endif /* HAC_PERSISTENCY_INCLUDE_HACCHANNELID_HH_ */
// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-09-14
//
// --------------------------------------------------------------
#ifndef THACDigi_H
#define THACDigi_H

#include "TDCVHit.hh"
#include "HACChannelID.hh"

class THACDigi : public TDCVHit, public HACChannelID {

  public:

    THACDigi();// : TDCVHit(){}
    explicit THACDigi(Int_t iCh) : TDCVHit(iCh), HACChannelID(iCh%100){}
    explicit THACDigi(TVHit*);
    ~THACDigi() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }
    Int_t GetThresholdType() const { return fChannelID/100; } //Thr types: 0,1,2,3 (0 is the lowest, 3 the highest)

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }

  private:

    ClassDef(THACDigi,1);
};
#endif
#ifndef THACEvent_H
#define THACEvent_H

#include "TDetectorVEvent.hh"

class THACEvent : public TDetectorVEvent {

    public:

        THACEvent();
        ~THACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(THACEvent,1);
};
#endif
#ifndef THACHit_H
#define THACHit_H

#include "TDetectorVHit.hh"
#include "HACChannelID.hh"

class THACHit : public TDetectorVHit, public HACChannelID {

  public:

    THACHit();
    ~THACHit(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  public:

    Int_t                GetScintillatorID()                                { return fScintillatorID;               };
    void                 SetScintillatorID(Int_t value)                     { fScintillatorID = value;              };

  protected:

    Int_t      fScintillatorID;

    ClassDef(THACHit,1);
};
#endif
#ifndef TRecoHACCandidate_H
#define TRecoHACCandidate_H

#include "TRecoVCandidate.hh"

class TRecoHACCandidate : public TRecoVCandidate {

    public:

      TRecoHACCandidate();
      ~TRecoHACCandidate(){};

      void Clear(Option_t* = "");

      void UpdateTime();   
      void UpdateTime(Double_t);

      void SetIsSelected(Bool_t val) {fIsSelected = val;}
      void SetDeltaTimeClosestCandidate(Double_t val) { fDeltaTimeClosestCandidate = val;}
      void SetNHitsClosestCandidate(Int_t val) { fNHitsClosestCandidate = val;}
      void SetCharge(Double_t val) { fCharge = val;}

      Bool_t GetIsSelected() {return fIsSelected;}
      Double_t GetDeltaTimeClosestCandidate() {return fDeltaTimeClosestCandidate;}
      Int_t GetNHitsClosestCandidate() {return fNHitsClosestCandidate;}
      Double_t GetCharge() {return fCharge;}
    
    private:

	Bool_t fIsSelected;
  	Double_t fDeltaTimeClosestCandidate;
  	Int_t fNHitsClosestCandidate;
  	Double_t fCharge;

        ClassDef(TRecoHACCandidate,1);
};
#endif

#ifndef TRecoHACEvent_H
#define TRecoHACEvent_H

#include "TRecoVEvent.hh"
#include "TRecoHACCandidate.hh"
#include "TRecoHACHit.hh"

class TRecoHACEvent : public TRecoVEvent {

    public:

        TRecoHACEvent();
        ~TRecoHACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoHACEvent,1);
};
#endif
#ifndef TRecoHACHit_H
#define TRecoHACHit_H

#include "TRecoVHit.hh"
#include "HACChannelID.hh"
class TRecoHACHit : public TRecoVHit , public HACChannelID {

public:

  TRecoHACHit();
  ~TRecoHACHit(){};
  
  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

public:

  void SetEdgeMask(Int_t val) {fEdgeMask =val;} 
  
  void SetLeadingEdge0(Double_t LedgeTDC){fLeadingEdge0 = LedgeTDC; fEdgeMask |= 0x1;}     // Saves T0 subtracted Leading edge for thr0  and set the mask bit 0
  void SetLeadingEdge1(Double_t LedgeTDC){fLeadingEdge1 = LedgeTDC; /*(fLeadingEdge1==-999.)?(fEdgeMask &= ~(1<<1)):(*/fEdgeMask |= 0x2;} // Saves T0 subtracted Leading edge for thr1  and set the mask bit 1
  void SetLeadingEdge2(Double_t LedgeTDC){fLeadingEdge2 = LedgeTDC; /*(fLeadingEdge2==-999.)?(fEdgeMask &= ~(1<<2)):(*/fEdgeMask |= 0x4;} // Saves T0 subtracted Leading edge for thr2  and set the mask bit 2
  void SetLeadingEdge3(Double_t LedgeTDC){fLeadingEdge3 = LedgeTDC; /*(fLeadingEdge3==-999.)?(fEdgeMask &= ~(1<<3)):(*/fEdgeMask |= 0x8;} // Saves T0 subtracted Leading edge for thr3  and set the mask bit 3
  
  Double_t GetLeadingEdge0()const{return (fLeadingEdge0);}
  Double_t GetLeadingEdge1()const{return (fLeadingEdge1);}
  Double_t GetLeadingEdge2()const{return (fLeadingEdge2);}
  Double_t GetLeadingEdge3()const{return (fLeadingEdge3);}
  
    
  void SetTrailingEdge0(Double_t LedgeTDC){fTrailingEdge0 = LedgeTDC; fEdgeMask |= 0x10;}  // Saves T0 subtracted Trailing edge for thr0 and set the mask bit 4
  void SetTrailingEdge1(Double_t LedgeTDC){fTrailingEdge1 = LedgeTDC; /*(fTrailingEdge1==-999.)?(fEdgeMask &= ~(1<<5)):(*/fEdgeMask |= 0x20;}  // Saves T0 subtracted Trailing edge for thr1 and set the mask bit 5 
  void SetTrailingEdge2(Double_t LedgeTDC){fTrailingEdge2 = LedgeTDC; /*(fTrailingEdge2==-999.)?(fEdgeMask &= ~(1<<6)):(*/fEdgeMask |= 0x40;}  // Saves T0 subtracted Trailing edge for thr2 and set the mask bit 6
  void SetTrailingEdge3(Double_t LedgeTDC){fTrailingEdge3 = LedgeTDC; /*(fTrailingEdge3==-999.)?(fEdgeMask &= ~(1<<7)):(*/fEdgeMask |= 0x80;} // Saves T0 subtracted Trailing edge for thr3 and set the mask bit 7
  
  Double_t GetTrailingEdge0()const{return (fTrailingEdge0);}
  Double_t GetTrailingEdge1()const{return (fTrailingEdge1);}
  Double_t GetTrailingEdge2()const{return (fTrailingEdge2);}
  Double_t GetTrailingEdge3()const{return (fTrailingEdge3);}
  
  Bool_t IsGrade0()const{return (fEdgeMask&0x11) == 0x11;} // 1st threshold passed
  Bool_t IsGrade1()const{return (fEdgeMask&0x22) == 0x22;} // 2nd threshold passed
  Bool_t IsGrade2()const{return (fEdgeMask&0x44) == 0x44;} // 3rd threshold passed
  Bool_t IsGrade3()const{return (fEdgeMask&0x88) == 0x88;} // 4th threshold passed
  Bool_t IsGrade(Int_t ithr);
  
  Int_t GetEdgeMask()const{return fEdgeMask;}
  
  Bool_t HasTDCsInOrder() const; // when FALSE there is a tdc arrival exception - Channel Time and Charge are calculated accordingly in HASCReconstruction
  
  Double_t GetChargeModuleSection()             { return fChargeModuleSection;} // [pC] See HACReconstruction.cc
  void     SetChargeModuleSection(Double_t val) { fChargeModuleSection = val; } 
  
  Double_t GetToTsumm()             { return fToTsumm;} // [ns] this is the Summ of all dt's corresponding to each surpassed threshold
  void     SetToTsumm(Double_t val) { fToTsumm = val; } 
  
  Double_t GetToT(Int_t ithr);
  Double_t GetLeadingEdge(Int_t ithr);
  Double_t GetTrailingEdge(Int_t ithr);
  Int_t    GetHighestThreshold();
  
  void SetLeadingEdge(Int_t ithr, Double_t fLeadTime);
  void SetTrailingEdge(Int_t ithr, Double_t fTrailTime);
  
private:
  
  Int_t fEdgeMask; // Mask for the edges present: bit 0-3 Leading, 4-7-Trailing
  Double_t fLeadingEdge0, fLeadingEdge1, fLeadingEdge2, fLeadingEdge3;
  Double_t fTrailingEdge0, fTrailingEdge1, fTrailingEdge2, fTrailingEdge3;
  Double_t fChargeModuleSection, fToTsumm; // See HACReconstruction.cc
  ClassDef(TRecoHACHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"HACChannelID", payloadCode, "@",
"THACDigi", payloadCode, "@",
"THACEvent", payloadCode, "@",
"THACHit", payloadCode, "@",
"TRecoHACCandidate", payloadCode, "@",
"TRecoHACEvent", payloadCode, "@",
"TRecoHACHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libHACPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libHACPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libHACPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libHACPersistency() {
  TriggerDictionaryInitialization_libHACPersistency_Impl();
}
