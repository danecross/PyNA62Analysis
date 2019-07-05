// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME RICHPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/RICHChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/TPMTHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/TRICHDigi.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/TRICHEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/TRICHHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/TRecoRICHCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/TRecoRICHEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include/TRecoRICHHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_RICHChannelID(void *p = 0);
   static void *newArray_RICHChannelID(Long_t size, void *p);
   static void delete_RICHChannelID(void *p);
   static void deleteArray_RICHChannelID(void *p);
   static void destruct_RICHChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::RICHChannelID*)
   {
      ::RICHChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::RICHChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("RICHChannelID", ::RICHChannelID::Class_Version(), "", 17,
                  typeid(::RICHChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::RICHChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::RICHChannelID) );
      instance.SetNew(&new_RICHChannelID);
      instance.SetNewArray(&newArray_RICHChannelID);
      instance.SetDelete(&delete_RICHChannelID);
      instance.SetDeleteArray(&deleteArray_RICHChannelID);
      instance.SetDestructor(&destruct_RICHChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::RICHChannelID*)
   {
      return GenerateInitInstanceLocal((::RICHChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::RICHChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRICHDigi(void *p = 0);
   static void *newArray_TRICHDigi(Long_t size, void *p);
   static void delete_TRICHDigi(void *p);
   static void deleteArray_TRICHDigi(void *p);
   static void destruct_TRICHDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRICHDigi*)
   {
      ::TRICHDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRICHDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRICHDigi", ::TRICHDigi::Class_Version(), "", 115,
                  typeid(::TRICHDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRICHDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TRICHDigi) );
      instance.SetNew(&new_TRICHDigi);
      instance.SetNewArray(&newArray_TRICHDigi);
      instance.SetDelete(&delete_TRICHDigi);
      instance.SetDeleteArray(&deleteArray_TRICHDigi);
      instance.SetDestructor(&destruct_TRICHDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRICHDigi*)
   {
      return GenerateInitInstanceLocal((::TRICHDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRICHDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRICHEvent(void *p = 0);
   static void *newArray_TRICHEvent(Long_t size, void *p);
   static void delete_TRICHEvent(void *p);
   static void deleteArray_TRICHEvent(void *p);
   static void destruct_TRICHEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRICHEvent*)
   {
      ::TRICHEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRICHEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRICHEvent", ::TRICHEvent::Class_Version(), "", 154,
                  typeid(::TRICHEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRICHEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRICHEvent) );
      instance.SetNew(&new_TRICHEvent);
      instance.SetNewArray(&newArray_TRICHEvent);
      instance.SetDelete(&delete_TRICHEvent);
      instance.SetDeleteArray(&deleteArray_TRICHEvent);
      instance.SetDestructor(&destruct_TRICHEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRICHEvent*)
   {
      return GenerateInitInstanceLocal((::TRICHEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRICHEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRICHHit(void *p = 0);
   static void *newArray_TRICHHit(Long_t size, void *p);
   static void delete_TRICHHit(void *p);
   static void deleteArray_TRICHHit(void *p);
   static void destruct_TRICHHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRICHHit*)
   {
      ::TRICHHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRICHHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRICHHit", ::TRICHHit::Class_Version(), "", 181,
                  typeid(::TRICHHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRICHHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRICHHit) );
      instance.SetNew(&new_TRICHHit);
      instance.SetNewArray(&newArray_TRICHHit);
      instance.SetDelete(&delete_TRICHHit);
      instance.SetDeleteArray(&deleteArray_TRICHHit);
      instance.SetDestructor(&destruct_TRICHHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRICHHit*)
   {
      return GenerateInitInstanceLocal((::TRICHHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRICHHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoRICHCandidate(void *p = 0);
   static void *newArray_TRecoRICHCandidate(Long_t size, void *p);
   static void delete_TRecoRICHCandidate(void *p);
   static void deleteArray_TRecoRICHCandidate(void *p);
   static void destruct_TRecoRICHCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoRICHCandidate*)
   {
      ::TRecoRICHCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoRICHCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoRICHCandidate", ::TRecoRICHCandidate::Class_Version(), "", 219,
                  typeid(::TRecoRICHCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoRICHCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoRICHCandidate) );
      instance.SetNew(&new_TRecoRICHCandidate);
      instance.SetNewArray(&newArray_TRecoRICHCandidate);
      instance.SetDelete(&delete_TRecoRICHCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoRICHCandidate);
      instance.SetDestructor(&destruct_TRecoRICHCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoRICHCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoRICHCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoRICHCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoRICHHit(void *p = 0);
   static void *newArray_TRecoRICHHit(Long_t size, void *p);
   static void delete_TRecoRICHHit(void *p);
   static void deleteArray_TRecoRICHHit(void *p);
   static void destruct_TRecoRICHHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoRICHHit*)
   {
      ::TRecoRICHHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoRICHHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoRICHHit", ::TRecoRICHHit::Class_Version(), "TRecoRICHHit.hh", 14,
                  typeid(::TRecoRICHHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoRICHHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoRICHHit) );
      instance.SetNew(&new_TRecoRICHHit);
      instance.SetNewArray(&newArray_TRecoRICHHit);
      instance.SetDelete(&delete_TRecoRICHHit);
      instance.SetDeleteArray(&deleteArray_TRecoRICHHit);
      instance.SetDestructor(&destruct_TRecoRICHHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoRICHHit*)
   {
      return GenerateInitInstanceLocal((::TRecoRICHHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoRICHHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoRICHEvent(void *p = 0);
   static void *newArray_TRecoRICHEvent(Long_t size, void *p);
   static void delete_TRecoRICHEvent(void *p);
   static void deleteArray_TRecoRICHEvent(void *p);
   static void destruct_TRecoRICHEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoRICHEvent*)
   {
      ::TRecoRICHEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoRICHEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoRICHEvent", ::TRecoRICHEvent::Class_Version(), "", 313,
                  typeid(::TRecoRICHEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoRICHEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoRICHEvent) );
      instance.SetNew(&new_TRecoRICHEvent);
      instance.SetNewArray(&newArray_TRecoRICHEvent);
      instance.SetDelete(&delete_TRecoRICHEvent);
      instance.SetDeleteArray(&deleteArray_TRecoRICHEvent);
      instance.SetDestructor(&destruct_TRecoRICHEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoRICHEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoRICHEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoRICHEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr RICHChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *RICHChannelID::Class_Name()
{
   return "RICHChannelID";
}

//______________________________________________________________________________
const char *RICHChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::RICHChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int RICHChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::RICHChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *RICHChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::RICHChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *RICHChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::RICHChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRICHDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRICHDigi::Class_Name()
{
   return "TRICHDigi";
}

//______________________________________________________________________________
const char *TRICHDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRICHDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRICHDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRICHDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRICHDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRICHDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRICHDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRICHDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRICHEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRICHEvent::Class_Name()
{
   return "TRICHEvent";
}

//______________________________________________________________________________
const char *TRICHEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRICHEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRICHEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRICHEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRICHEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRICHEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRICHEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRICHEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRICHHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRICHHit::Class_Name()
{
   return "TRICHHit";
}

//______________________________________________________________________________
const char *TRICHHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRICHHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRICHHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRICHHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRICHHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRICHHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRICHHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRICHHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoRICHCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoRICHCandidate::Class_Name()
{
   return "TRecoRICHCandidate";
}

//______________________________________________________________________________
const char *TRecoRICHCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoRICHCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoRICHCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoRICHCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoRICHHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoRICHHit::Class_Name()
{
   return "TRecoRICHHit";
}

//______________________________________________________________________________
const char *TRecoRICHHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoRICHHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoRICHHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoRICHHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoRICHEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoRICHEvent::Class_Name()
{
   return "TRecoRICHEvent";
}

//______________________________________________________________________________
const char *TRecoRICHEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoRICHEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoRICHEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoRICHEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoRICHEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void RICHChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class RICHChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(RICHChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(RICHChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_RICHChannelID(void *p) {
      return  p ? new(p) ::RICHChannelID : new ::RICHChannelID;
   }
   static void *newArray_RICHChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::RICHChannelID[nElements] : new ::RICHChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_RICHChannelID(void *p) {
      delete ((::RICHChannelID*)p);
   }
   static void deleteArray_RICHChannelID(void *p) {
      delete [] ((::RICHChannelID*)p);
   }
   static void destruct_RICHChannelID(void *p) {
      typedef ::RICHChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::RICHChannelID

//______________________________________________________________________________
void TRICHDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRICHDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRICHDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRICHDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRICHDigi(void *p) {
      return  p ? new(p) ::TRICHDigi : new ::TRICHDigi;
   }
   static void *newArray_TRICHDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TRICHDigi[nElements] : new ::TRICHDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRICHDigi(void *p) {
      delete ((::TRICHDigi*)p);
   }
   static void deleteArray_TRICHDigi(void *p) {
      delete [] ((::TRICHDigi*)p);
   }
   static void destruct_TRICHDigi(void *p) {
      typedef ::TRICHDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRICHDigi

//______________________________________________________________________________
void TRICHEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRICHEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRICHEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRICHEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRICHEvent(void *p) {
      return  p ? new(p) ::TRICHEvent : new ::TRICHEvent;
   }
   static void *newArray_TRICHEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRICHEvent[nElements] : new ::TRICHEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRICHEvent(void *p) {
      delete ((::TRICHEvent*)p);
   }
   static void deleteArray_TRICHEvent(void *p) {
      delete [] ((::TRICHEvent*)p);
   }
   static void destruct_TRICHEvent(void *p) {
      typedef ::TRICHEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRICHEvent

//______________________________________________________________________________
void TRICHHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRICHHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRICHHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRICHHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRICHHit(void *p) {
      return  p ? new(p) ::TRICHHit : new ::TRICHHit;
   }
   static void *newArray_TRICHHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRICHHit[nElements] : new ::TRICHHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRICHHit(void *p) {
      delete ((::TRICHHit*)p);
   }
   static void deleteArray_TRICHHit(void *p) {
      delete [] ((::TRICHHit*)p);
   }
   static void destruct_TRICHHit(void *p) {
      typedef ::TRICHHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRICHHit

//______________________________________________________________________________
void TRecoRICHCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoRICHCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoRICHCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoRICHCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoRICHCandidate(void *p) {
      return  p ? new(p) ::TRecoRICHCandidate : new ::TRecoRICHCandidate;
   }
   static void *newArray_TRecoRICHCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoRICHCandidate[nElements] : new ::TRecoRICHCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoRICHCandidate(void *p) {
      delete ((::TRecoRICHCandidate*)p);
   }
   static void deleteArray_TRecoRICHCandidate(void *p) {
      delete [] ((::TRecoRICHCandidate*)p);
   }
   static void destruct_TRecoRICHCandidate(void *p) {
      typedef ::TRecoRICHCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoRICHCandidate

//______________________________________________________________________________
void TRecoRICHHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoRICHHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoRICHHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoRICHHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoRICHHit(void *p) {
      return  p ? new(p) ::TRecoRICHHit : new ::TRecoRICHHit;
   }
   static void *newArray_TRecoRICHHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoRICHHit[nElements] : new ::TRecoRICHHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoRICHHit(void *p) {
      delete ((::TRecoRICHHit*)p);
   }
   static void deleteArray_TRecoRICHHit(void *p) {
      delete [] ((::TRecoRICHHit*)p);
   }
   static void destruct_TRecoRICHHit(void *p) {
      typedef ::TRecoRICHHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoRICHHit

//______________________________________________________________________________
void TRecoRICHEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoRICHEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoRICHEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoRICHEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoRICHEvent(void *p) {
      return  p ? new(p) ::TRecoRICHEvent : new ::TRecoRICHEvent;
   }
   static void *newArray_TRecoRICHEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoRICHEvent[nElements] : new ::TRecoRICHEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoRICHEvent(void *p) {
      delete ((::TRecoRICHEvent*)p);
   }
   static void deleteArray_TRecoRICHEvent(void *p) {
      delete [] ((::TRecoRICHEvent*)p);
   }
   static void destruct_TRecoRICHEvent(void *p) {
      typedef ::TRecoRICHEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoRICHEvent

namespace {
  void TriggerDictionaryInitialization_libRICHPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/RICH/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libRICHPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class RICHChannelID;
class TRICHDigi;
class TRICHEvent;
class TRICHHit;
class TRecoRICHCandidate;
class __attribute__((annotate("$clingAutoload$TRecoRICHHit.hh")))  TRecoRICHHit;
class TRecoRICHEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libRICHPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// --------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (francesca.bucci@cern.ch) 2014-06-19
//
// --------------------------------------------------------------

#ifndef RICHChannelID_H
#define RICHChannelID_H
#include "Rtypes.h"

class RICHChannelID {

    public:

  struct chIDDecoded{ Int_t DiskID, UpDownDiskID, SuperCellID, OrSuperCellID, PmtID;};
  RICHChannelID();
  virtual ~RICHChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  static chIDDecoded DecodeChannelID_Static(int ChannelID); // converts channel ID into disk ID, PMT ID, etc...
  void DecodeChannelID(Int_t);
  Int_t GetChannelSeqID();

    public:

  Int_t                GetDiskID()                                        { return fDiskID;                       };
  void                 SetDiskID(Int_t value)                             { fDiskID = value;                      };

  Int_t                GetUpDownDiskID()                                  { return fUpDownDiskID;                 };
  void                 SetUpDownDiskID(Int_t value)                       { fUpDownDiskID = value;                };

  Int_t                GetSuperCellID()                                   { return fSuperCellID;                  };
  void                 SetSuperCellID(Int_t value)                        { fSuperCellID = value;                 };

  Int_t                GetOrSuperCellID()                                 { return fOrSuperCellID;                };
  void                 SetOrSuperCellID(Int_t value)                      { fOrSuperCellID = value;               };

  Int_t                GetPmtID()                                         { return fPmtID;                        };
  void                 SetPmtID(Int_t value)                              { fPmtID = value;                       };


    protected:

  Int_t     fDiskID;   ///< 0 if hit position on Jura, 1 if on Saleve
  Int_t     fUpDownDiskID;  ///< 0 if hit position on the Up half of the disk, 1 if on the Down half of the disk     
  Int_t     fSuperCellID;  ///<  ID number of the SuperCell to whom the hit belongs (from 0 to 121)
  Int_t     fOrSuperCellID;  ///< 0 if hit coming from a single PM, 1 if coming from a SuperCell (logic OR of 8 adjacent PMs)
  Int_t     fPmtID;       ///< ID number of the hit PM inside the SuperCell (from 0 to 7)

        ClassDef(RICHChannelID,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TPMTHIT_H
#define TPMTHIT_H

#include "TObject.h"
#include "TVector3.h"

class TPMTHit : public TObject {

public:

  TPMTHit() : fPosition(), fEnergy(0.),fTime(0.),fMCTrackID(-1) {}

  void Clear(Option_t* = "");

  TVector3             GetPosition()                                      { return fPosition;                     };
  void                 SetPosition(TVector3 value)                        { fPosition = value;                    };
  Double_t             GetEnergy()                                        { return fEnergy;                       };
  void                 SetEnergy(Double_t value)                          { fEnergy = value;                      };
  Double_t             GetTime()                                          { return fTime;                         };
  void                 SetTime(Double_t value)                            { fTime = value;                        };
  Int_t                GetMCTrackID()                                     { return fMCTrackID;                    };
  void                 SetMCTrackID(Int_t value)                          { fMCTrackID = value;                   };

private:

  TVector3   fPosition;
  Double_t   fEnergy;
  Double_t   fTime;
  Int_t      fMCTrackID; // For MCTruth Association

  ClassDef(TPMTHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TRICHDigi_H
#define TRICHDigi_H

#include "TDCVHit.hh"
#include "RICHChannelID.hh"


class TRICHDigi : public TDCVHit, public RICHChannelID {

public:

  TRICHDigi() : TDCVHit(), RICHChannelID() {}
  explicit TRICHDigi(Int_t iCh) : TDCVHit(iCh), RICHChannelID(){}
  ~TRICHDigi(){}

  void Clear(Option_t* = "");

  Int_t Compare(const TObject *obj) const;
  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return GetOrSuperCellID(); }
    


public:


private:


  ClassDef(TRICHDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TRICHEvent_H
#define TRICHEvent_H

#include "TDetectorVEvent.hh"

class TRICHEvent : public TDetectorVEvent {

    public:

        TRICHEvent();
        ~TRICHEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRICHEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TRICHHit_H
#define TRICHHit_H

#include "TDetectorVHit.hh"
#include "RICHChannelID.hh"

class TRICHHit : public TDetectorVHit, public RICHChannelID {

    public:

  TRICHHit();
  explicit TRICHHit(Int_t iCh);
  ~TRICHHit(){};
  
  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID();

  Int_t GetStationID() { return 0; }



    public:


    protected:

        ClassDef(TRICHHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoRICHCandidate_H
#define TRecoRICHCandidate_H

#include "TRecoVCandidate.hh"
#include "TArrayI.h"

class TRecoRICHCandidate : public TRecoVCandidate {

  public:

    TRecoRICHCandidate();
    ~TRecoRICHCandidate(){};

    void Clear(Option_t* = "");

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }

    void UpdateTime();
    void UpdateTime(Double_t);

    Bool_t               GetIsSelected()                                    { return fIsSelected;                   };
    void                 SetIsSelected(Bool_t value)                        { fIsSelected = value;                  };
    Double_t             GetDeltaTimeClosestCandidate()                     { return fDeltaTimeClosestCandidate;    };
    void                 SetDeltaTimeClosestCandidate(Double_t val)         { fDeltaTimeClosestCandidate = val;     };
    Int_t                GetNHitsClosestCandidate()                         { return fNHitsClosestCandidate;        };
    void                 SetNHitsClosestCandidate(Int_t val)                { fNHitsClosestCandidate = val;         };

    TVector2             GetRingCenter()                                    { return fRingCenter;                   };
    void                 SetRingCenter(TVector2 value)                      { fRingCenter = value;                  };
    Double_t             GetRingRadius()                                    { return fRingRadius;                   };
    void                 SetRingRadius(Double_t value)                      { fRingRadius = value;                  };
    Double_t             GetRingChi2()                                      { return fRingChi2;                     };
    void                 SetRingChi2(Double_t value)                        { fRingChi2 = value;                    };
    Double_t             GetRingTime()                                      { return fRingTime;                     };
    void                 SetRingTime(Double_t value)                        { fRingTime = value;                    };
    Int_t                GetTimeCandidateIndex()                            { return fTimeCandidateIndex;           };
    void                 SetTimeCandidateIndex(Double_t value)              { fTimeCandidateIndex = value;          };

    // methods for the iteration fit
    TVector2             GetRingCenterSingleRing()                                    { return fRingCenterSingleRing;                };
    void                 SetRingCenterSingleRing(TVector2 value)                      { fRingCenterSingleRing = value;               };
    TVector2             GetRingCenterErrorSingleRing()                               { return fRingCenterErrorSingleRing;           };
    void                 SetRingCenterErrorSingleRing(TVector2 value)                 { fRingCenterErrorSingleRing = value;          };
    Double_t             GetRingRadiusSingleRing()                                    { return fRingRadiusSingleRing;                };
    void                 SetRingRadiusSingleRing(Double_t value)                      { fRingRadiusSingleRing = value;               };
    Double_t             GetRingRadiusErrorSingleRing()                               { return fRingRadiusErrorSingleRing;           };
    void                 SetRingRadiusErrorSingleRing(Double_t value)                 { fRingRadiusErrorSingleRing = value;          };
    Double_t             GetRingChi2SingleRing()                                      { return fRingChi2SingleRing;                  };
    void                 SetRingChi2SingleRing(Double_t value)                        { fRingChi2SingleRing = value;                 };
    Double_t             GetRingTimeSingleRing()                                      { return fRingTimeSingleRing;                  };
    void                 SetRingTimeSingleRing(Double_t value)                        { fRingTimeSingleRing = value;                 };
    Int_t*               GetHitIndexesSingleRing()                                    { return fHitIndexesSingleRing.GetArray();     };
    void                 SetHitIndexesSingleRing(Int_t *value, Int_t NHits)           
    { fHitIndexesSingleRing.Set(NHits); for(Int_t iHit=0; iHit<NHits; iHit++) fHitIndexesSingleRing[iHit] = value[iHit]; };
    Double_t             GetNHitsSingleRing()                                         { return fHitIndexesSingleRing.GetSize();      };
    void                 SetNHitsSingleRing(Int_t value)                              { fHitIndexesSingleRing.Set(value);            };
    Double_t             GetNIterationsSingleRing()                                   { return fNIterationsSingleRing;               };
    void                 SetNIterationsSingleRing(Double_t value)                     { fNIterationsSingleRing = value;              };

  private:

    Bool_t fIsSelected; //!  Transient data member
    Double_t fDeltaTimeClosestCandidate; //!  Transient data member
    Int_t fNHitsClosestCandidate; //!  Transient data member

    TVector2 fRingCenter;             ///< center position of the ring as obtained from the fit
    Double_t fRingRadius;             ///< radius of the ring as obtained from the fit
    Double_t fRingChi2;               ///< chi2 of the fit to the ring
    Double_t fRingTime;               ///< time of the ring candidate
    Int_t    fTimeCandidateIndex;     ///< index of the time candidate from which the ring candidate has been reconstructed 

    // variables for the iteration fit
    TVector2 fRingCenterSingleRing;             ///< center position of the ring obtained in the single ring the fit
    TVector2 fRingCenterErrorSingleRing; 
    Double_t fRingRadiusSingleRing;             ///< radius of the single ring ring
    Double_t fRingRadiusErrorSingleRing;
    Double_t fRingChi2SingleRing;               ///< chi2 of the single ring fit   
    Double_t fRingTimeSingleRing;               ///< time of the average over all hits in the single ring fit
    Int_t fNHitsSingleRing;    // this is redundant, it should be removed!
    Int_t fNIterationsSingleRing;
    TArrayI fHitIndexesSingleRing;

    ClassDef(TRecoRICHCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoRICHEvent_H
#define TRecoRICHEvent_H

#include "TRecoVEvent.hh"
#include "TRecoRICHCandidate.hh"
#include "TRecoRICHHit.hh"

class TRecoRICHEvent : public TRecoVEvent {

public:

  TRecoRICHEvent();
  ~TRecoRICHEvent();

  void Clear(Option_t* = "");

  TRecoRICHCandidate * GetTimeCandidate(Int_t);
  TRecoRICHCandidate * GetRingCandidate(Int_t);
  TRecoRICHCandidate * GetSCTimeCandidate(Int_t);
  TRecoRICHCandidate * GetPMTimeCandidate(Int_t);

  Int_t    GetNTimeCandidates()              { return fNTimeCandidates;    }
  void     SetNTimeCandidates(Int_t value)   { fNTimeCandidates = value;   }
  Int_t    GetNRingCandidates()              { return fNRingCandidates;    }
  void     SetNRingCandidates(Int_t value)   { fNRingCandidates = value;   }
  Int_t    GetNPMTimeCandidates()            { return fNPMTimeCandidates;  }
  void     SetNPMTimeCandidates(Int_t value) { fNPMTimeCandidates = value; }
  Int_t    GetNSCTimeCandidates()            { return fNSCTimeCandidates;  }
  void     SetNSCTimeCandidates(Int_t value) { fNSCTimeCandidates = value; }

private:

  Int_t fNTimeCandidates; ///< number of Time Candidates
  Int_t fNRingCandidates; ///< number of Ring Candidates
  Int_t fNPMTimeCandidates; ///< number of PM Time Candidates 
  Int_t fNSCTimeCandidates; ///< number of SuperCell Time Candidates

  ClassDef(TRecoRICHEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoRICHHit_H
#define TRecoRICHHit_H

#include "TRecoVHit.hh"
#include "TRICHDigi.hh"
#include "RICHChannelID.hh"

class TRecoRICHHit : public TRecoVHit, public RICHChannelID {

  public:

    TRecoRICHHit();
    explicit TRecoRICHHit(Int_t iCh);
    explicit TRecoRICHHit(TRICHDigi *);
    ~TRecoRICHHit(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();


  public:
    Double_t             GetHitQuality()                                    { return fHitQuality;                   };
    void                 SetHitQuality(Double_t value)                      { fHitQuality = value;                  };
    Double_t             GetTimeWidth()                                     { return fTimeWidth;                    };
    void                 SetTimeWidth(Double_t value)                       { fTimeWidth = value;                   };
    Int_t                GetROChannelID()                                   { return fROChannelID;                 };
    void                 SetROChannelID(Int_t value)                        { fROChannelID = value;                };
    Double_t             GetPtolemy()                                       { return fPtolemy;                      };
    void                 SetPtolemy(Double_t value)                         { fPtolemy = value;                     };
    Bool_t               GetIsOnCircle()                                    { return fIsOnCircle;                   };
    void                 SetIsOnCircle(Bool_t value)                        { fIsOnCircle = value;                  };
    TVector3             GetFitPosition()                                   { return fFitPosition;                  };        
    void                 SetFitPosition(TVector3 value)                     { fFitPosition = value;                 };

  private:

    Double_t fHitQuality;
    Double_t fTimeWidth;
    Int_t    fROChannelID;
    Double_t fPtolemy;
    Bool_t fIsOnCircle; //!  Transient data member
    TVector3 fFitPosition; ///< HitPosition corrected including mirror inclination

    ClassDef(TRecoRICHHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"RICHChannelID", payloadCode, "@",
"TRICHDigi", payloadCode, "@",
"TRICHEvent", payloadCode, "@",
"TRICHHit", payloadCode, "@",
"TRecoRICHCandidate", payloadCode, "@",
"TRecoRICHEvent", payloadCode, "@",
"TRecoRICHHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libRICHPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libRICHPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libRICHPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libRICHPersistency() {
  TriggerDictionaryInitialization_libRICHPersistency_Impl();
}
