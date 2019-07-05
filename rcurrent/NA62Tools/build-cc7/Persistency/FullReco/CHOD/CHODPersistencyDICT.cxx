// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME CHODPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include/CHODChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include/TCHODDigi.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include/TCHODEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include/TCHODHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include/TRecoCHODCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include/TRecoCHODEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include/TRecoCHODHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_CHODChannelID(void *p = 0);
   static void *newArray_CHODChannelID(Long_t size, void *p);
   static void delete_CHODChannelID(void *p);
   static void deleteArray_CHODChannelID(void *p);
   static void destruct_CHODChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::CHODChannelID*)
   {
      ::CHODChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::CHODChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("CHODChannelID", ::CHODChannelID::Class_Version(), "", 19,
                  typeid(::CHODChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::CHODChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::CHODChannelID) );
      instance.SetNew(&new_CHODChannelID);
      instance.SetNewArray(&newArray_CHODChannelID);
      instance.SetDelete(&delete_CHODChannelID);
      instance.SetDeleteArray(&deleteArray_CHODChannelID);
      instance.SetDestructor(&destruct_CHODChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::CHODChannelID*)
   {
      return GenerateInitInstanceLocal((::CHODChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::CHODChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCHODDigi(void *p = 0);
   static void *newArray_TCHODDigi(Long_t size, void *p);
   static void delete_TCHODDigi(void *p);
   static void deleteArray_TCHODDigi(void *p);
   static void destruct_TCHODDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCHODDigi*)
   {
      ::TCHODDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCHODDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCHODDigi", ::TCHODDigi::Class_Version(), "", 63,
                  typeid(::TCHODDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCHODDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TCHODDigi) );
      instance.SetNew(&new_TCHODDigi);
      instance.SetNewArray(&newArray_TCHODDigi);
      instance.SetDelete(&delete_TCHODDigi);
      instance.SetDeleteArray(&deleteArray_TCHODDigi);
      instance.SetDestructor(&destruct_TCHODDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCHODDigi*)
   {
      return GenerateInitInstanceLocal((::TCHODDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TCHODDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCHODEvent(void *p = 0);
   static void *newArray_TCHODEvent(Long_t size, void *p);
   static void delete_TCHODEvent(void *p);
   static void deleteArray_TCHODEvent(void *p);
   static void destruct_TCHODEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCHODEvent*)
   {
      ::TCHODEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCHODEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCHODEvent", ::TCHODEvent::Class_Version(), "", 95,
                  typeid(::TCHODEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCHODEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TCHODEvent) );
      instance.SetNew(&new_TCHODEvent);
      instance.SetNewArray(&newArray_TCHODEvent);
      instance.SetDelete(&delete_TCHODEvent);
      instance.SetDeleteArray(&deleteArray_TCHODEvent);
      instance.SetDestructor(&destruct_TCHODEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCHODEvent*)
   {
      return GenerateInitInstanceLocal((::TCHODEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TCHODEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCHODHit(void *p = 0);
   static void *newArray_TCHODHit(Long_t size, void *p);
   static void delete_TCHODHit(void *p);
   static void deleteArray_TCHODHit(void *p);
   static void destruct_TCHODHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCHODHit*)
   {
      ::TCHODHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCHODHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCHODHit", ::TCHODHit::Class_Version(), "", 127,
                  typeid(::TCHODHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCHODHit::Dictionary, isa_proxy, 4,
                  sizeof(::TCHODHit) );
      instance.SetNew(&new_TCHODHit);
      instance.SetNewArray(&newArray_TCHODHit);
      instance.SetDelete(&delete_TCHODHit);
      instance.SetDeleteArray(&deleteArray_TCHODHit);
      instance.SetDestructor(&destruct_TCHODHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCHODHit*)
   {
      return GenerateInitInstanceLocal((::TCHODHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TCHODHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCHODCandidate(void *p = 0);
   static void *newArray_TRecoCHODCandidate(Long_t size, void *p);
   static void delete_TRecoCHODCandidate(void *p);
   static void deleteArray_TRecoCHODCandidate(void *p);
   static void destruct_TRecoCHODCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCHODCandidate*)
   {
      ::TRecoCHODCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCHODCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCHODCandidate", ::TRecoCHODCandidate::Class_Version(), "", 158,
                  typeid(::TRecoCHODCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCHODCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCHODCandidate) );
      instance.SetNew(&new_TRecoCHODCandidate);
      instance.SetNewArray(&newArray_TRecoCHODCandidate);
      instance.SetDelete(&delete_TRecoCHODCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoCHODCandidate);
      instance.SetDestructor(&destruct_TRecoCHODCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCHODCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoCHODCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoCHODCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCHODHit(void *p = 0);
   static void *newArray_TRecoCHODHit(Long_t size, void *p);
   static void delete_TRecoCHODHit(void *p);
   static void deleteArray_TRecoCHODHit(void *p);
   static void destruct_TRecoCHODHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCHODHit*)
   {
      ::TRecoCHODHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCHODHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCHODHit", ::TRecoCHODHit::Class_Version(), "TRecoCHODHit.hh", 14,
                  typeid(::TRecoCHODHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCHODHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCHODHit) );
      instance.SetNew(&new_TRecoCHODHit);
      instance.SetNewArray(&newArray_TRecoCHODHit);
      instance.SetDelete(&delete_TRecoCHODHit);
      instance.SetDeleteArray(&deleteArray_TRecoCHODHit);
      instance.SetDestructor(&destruct_TRecoCHODHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCHODHit*)
   {
      return GenerateInitInstanceLocal((::TRecoCHODHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoCHODHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCHODEvent(void *p = 0);
   static void *newArray_TRecoCHODEvent(Long_t size, void *p);
   static void delete_TRecoCHODEvent(void *p);
   static void deleteArray_TRecoCHODEvent(void *p);
   static void destruct_TRecoCHODEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCHODEvent*)
   {
      ::TRecoCHODEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCHODEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCHODEvent", ::TRecoCHODEvent::Class_Version(), "", 195,
                  typeid(::TRecoCHODEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCHODEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCHODEvent) );
      instance.SetNew(&new_TRecoCHODEvent);
      instance.SetNewArray(&newArray_TRecoCHODEvent);
      instance.SetDelete(&delete_TRecoCHODEvent);
      instance.SetDeleteArray(&deleteArray_TRecoCHODEvent);
      instance.SetDestructor(&destruct_TRecoCHODEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCHODEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoCHODEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoCHODEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr CHODChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *CHODChannelID::Class_Name()
{
   return "CHODChannelID";
}

//______________________________________________________________________________
const char *CHODChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CHODChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int CHODChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CHODChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *CHODChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CHODChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *CHODChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CHODChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCHODDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCHODDigi::Class_Name()
{
   return "TCHODDigi";
}

//______________________________________________________________________________
const char *TCHODDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHODDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCHODDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHODDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCHODDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHODDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCHODDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHODDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCHODEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCHODEvent::Class_Name()
{
   return "TCHODEvent";
}

//______________________________________________________________________________
const char *TCHODEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHODEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCHODEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHODEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCHODEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHODEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCHODEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHODEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCHODHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCHODHit::Class_Name()
{
   return "TCHODHit";
}

//______________________________________________________________________________
const char *TCHODHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHODHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCHODHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHODHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCHODHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHODHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCHODHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHODHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCHODCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCHODCandidate::Class_Name()
{
   return "TRecoCHODCandidate";
}

//______________________________________________________________________________
const char *TRecoCHODCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCHODCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCHODCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCHODCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCHODHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCHODHit::Class_Name()
{
   return "TRecoCHODHit";
}

//______________________________________________________________________________
const char *TRecoCHODHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCHODHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCHODHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCHODHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCHODEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCHODEvent::Class_Name()
{
   return "TRecoCHODEvent";
}

//______________________________________________________________________________
const char *TRecoCHODEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCHODEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCHODEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCHODEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHODEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void CHODChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class CHODChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(CHODChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(CHODChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_CHODChannelID(void *p) {
      return  p ? new(p) ::CHODChannelID : new ::CHODChannelID;
   }
   static void *newArray_CHODChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::CHODChannelID[nElements] : new ::CHODChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_CHODChannelID(void *p) {
      delete ((::CHODChannelID*)p);
   }
   static void deleteArray_CHODChannelID(void *p) {
      delete [] ((::CHODChannelID*)p);
   }
   static void destruct_CHODChannelID(void *p) {
      typedef ::CHODChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::CHODChannelID

//______________________________________________________________________________
void TCHODDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCHODDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCHODDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCHODDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCHODDigi(void *p) {
      return  p ? new(p) ::TCHODDigi : new ::TCHODDigi;
   }
   static void *newArray_TCHODDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TCHODDigi[nElements] : new ::TCHODDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCHODDigi(void *p) {
      delete ((::TCHODDigi*)p);
   }
   static void deleteArray_TCHODDigi(void *p) {
      delete [] ((::TCHODDigi*)p);
   }
   static void destruct_TCHODDigi(void *p) {
      typedef ::TCHODDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCHODDigi

//______________________________________________________________________________
void TCHODEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCHODEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCHODEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCHODEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCHODEvent(void *p) {
      return  p ? new(p) ::TCHODEvent : new ::TCHODEvent;
   }
   static void *newArray_TCHODEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TCHODEvent[nElements] : new ::TCHODEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCHODEvent(void *p) {
      delete ((::TCHODEvent*)p);
   }
   static void deleteArray_TCHODEvent(void *p) {
      delete [] ((::TCHODEvent*)p);
   }
   static void destruct_TCHODEvent(void *p) {
      typedef ::TCHODEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCHODEvent

//______________________________________________________________________________
void TCHODHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCHODHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCHODHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCHODHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCHODHit(void *p) {
      return  p ? new(p) ::TCHODHit : new ::TCHODHit;
   }
   static void *newArray_TCHODHit(Long_t nElements, void *p) {
      return p ? new(p) ::TCHODHit[nElements] : new ::TCHODHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCHODHit(void *p) {
      delete ((::TCHODHit*)p);
   }
   static void deleteArray_TCHODHit(void *p) {
      delete [] ((::TCHODHit*)p);
   }
   static void destruct_TCHODHit(void *p) {
      typedef ::TCHODHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCHODHit

//______________________________________________________________________________
void TRecoCHODCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCHODCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCHODCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCHODCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCHODCandidate(void *p) {
      return  p ? new(p) ::TRecoCHODCandidate : new ::TRecoCHODCandidate;
   }
   static void *newArray_TRecoCHODCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCHODCandidate[nElements] : new ::TRecoCHODCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCHODCandidate(void *p) {
      delete ((::TRecoCHODCandidate*)p);
   }
   static void deleteArray_TRecoCHODCandidate(void *p) {
      delete [] ((::TRecoCHODCandidate*)p);
   }
   static void destruct_TRecoCHODCandidate(void *p) {
      typedef ::TRecoCHODCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCHODCandidate

//______________________________________________________________________________
void TRecoCHODHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCHODHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCHODHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCHODHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCHODHit(void *p) {
      return  p ? new(p) ::TRecoCHODHit : new ::TRecoCHODHit;
   }
   static void *newArray_TRecoCHODHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCHODHit[nElements] : new ::TRecoCHODHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCHODHit(void *p) {
      delete ((::TRecoCHODHit*)p);
   }
   static void deleteArray_TRecoCHODHit(void *p) {
      delete [] ((::TRecoCHODHit*)p);
   }
   static void destruct_TRecoCHODHit(void *p) {
      typedef ::TRecoCHODHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCHODHit

//______________________________________________________________________________
void TRecoCHODEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCHODEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCHODEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCHODEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCHODEvent(void *p) {
      return  p ? new(p) ::TRecoCHODEvent : new ::TRecoCHODEvent;
   }
   static void *newArray_TRecoCHODEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCHODEvent[nElements] : new ::TRecoCHODEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCHODEvent(void *p) {
      delete ((::TRecoCHODEvent*)p);
   }
   static void deleteArray_TRecoCHODEvent(void *p) {
      delete [] ((::TRecoCHODEvent*)p);
   }
   static void destruct_TRecoCHODEvent(void *p) {
      typedef ::TRecoCHODEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCHODEvent

namespace {
  void TriggerDictionaryInitialization_libCHODPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/CHOD/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libCHODPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class CHODChannelID;
class TCHODDigi;
class TCHODEvent;
class TCHODHit;
class TRecoCHODCandidate;
class __attribute__((annotate("$clingAutoload$TRecoCHODHit.hh")))  TRecoCHODHit;
class TRecoCHODEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libCHODPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------

#ifndef CHODChannelID_H
#define CHODChannelID_H

#include "Rtypes.h"
#include "TVChannelID.hh"

class CHODChannelID {

public:
  struct chIDDecoded{ Int_t PlaneID,QuadrantID,CounterID;};

  CHODChannelID();
  virtual ~CHODChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); // returns position ID
  static chIDDecoded DecodeChannelID_Static(int ChannelID); // converts position ID into plane, quadrant, counter IDs (Struct)
  void DecodeChannelID(int ChannelID);

  Int_t GetPlaneID()             { return fPlaneID;    }
  void  SetPlaneID(Int_t val)    { fPlaneID = val;     }
  Int_t GetQuadrantID()          { return fQuadrantID; }
  void  SetQuadrantID(Int_t val) { fQuadrantID = val;  }
  Int_t GetCounterID()           { return fCounterID;  }
  void  SetCounterID(Int_t val)  { fCounterID = val;   }

private:

  Int_t fPlaneID;
  Int_t fQuadrantID;
  Int_t fCounterID;

  ClassDef(CHODChannelID,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------
#ifndef TCHODDigi_H
#define TCHODDigi_H

#include "TDCVHit.hh"
#include "CHODChannelID.hh"

class TCHODDigi : public TDCVHit, public CHODChannelID {

public:

  TCHODDigi();
  ~TCHODDigi() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

  Int_t GetThresholdType() const { return fChannelID/1000; } //Low Threshold: 0, High Threshold: 1

private:

  ClassDef(TCHODDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TCHODEvent_H
#define TCHODEvent_H

#include "TDetectorVEvent.hh"

class TCHODEvent : public TDetectorVEvent {

    public:

        TCHODEvent();
        ~TCHODEvent();

        void Clear(Option_t* = "");

    public:

    private:

        ClassDef(TCHODEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------

#ifndef TCHODHit_H
#define TCHODHit_H

#include "TDetectorVHit.hh"
#include "CHODChannelID.hh"

class TCHODHit : public TDetectorVHit, public CHODChannelID {

public:

  TCHODHit();
  ~TCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

protected:
  
  ClassDef(TCHODHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------
#ifndef TRecoCHODCandidate_H
#define TRecoCHODCandidate_H

#include "TRecoVCandidate.hh"

class TRecoCHODCandidate : public TRecoVCandidate {

public:

  TRecoCHODCandidate();
  ~TRecoCHODCandidate(){}

  void Clear(Option_t* = "");

  TVector2 GetHitPosition()               { return fHitPosition;  }
  void     SetHitPosition(TVector2 value) { fHitPosition = value; }
  Int_t    GetNHitPairs()                 { return fNHitPairs;      }
  void     SetNHitPairs(Int_t value)      { fNHitPairs = value;     }

private:

  Double_t fHitTime;  // useless, should be removed
  TVector2 fHitPosition;
  Int_t fNHitPairs;

  ClassDef(TRecoCHODCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoCHODEvent_H
#define TRecoCHODEvent_H

#include "TRecoVEvent.hh"
#include "TRecoCHODCandidate.hh"
#include "TRecoCHODHit.hh"

class TRecoCHODEvent : public TRecoVEvent {

    public:

      TRecoCHODEvent();
      ~TRecoCHODEvent();

      void Clear(Option_t* = "");

      TRecoCHODCandidate * GetTimeCandidate();

    public:
      Int_t             GetNTimeCandidates()                          { return fNTimeCandidates;         };
      void              SetNTimeCandidates(Int_t value)               { fNTimeCandidates = value;        };

      Int_t             GetNQuadrants()                               { return fNQuadrants;              };
      void              SetNQuadrants(Int_t value)                    { fNQuadrants = value;             };

    private:
      Int_t fNTimeCandidates;
      Int_t fNQuadrants;

      ClassDef(TRecoCHODEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------
#ifndef TRecoCHODHit_H
#define TRecoCHODHit_H

#include "TRecoVHit.hh"
#include "CHODChannelID.hh"

class TRecoCHODHit : public TRecoVHit, public CHODChannelID {

public:

  TRecoCHODHit();
  ~TRecoCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Double_t GetTimeWidth()             { return fTimeWidth; }
  void     SetTimeWidth(Double_t val) { fTimeWidth = val;  }

private:
    
  Double_t fTimeWidth;

  ClassDef(TRecoCHODHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"CHODChannelID", payloadCode, "@",
"TCHODDigi", payloadCode, "@",
"TCHODEvent", payloadCode, "@",
"TCHODHit", payloadCode, "@",
"TRecoCHODCandidate", payloadCode, "@",
"TRecoCHODEvent", payloadCode, "@",
"TRecoCHODHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libCHODPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libCHODPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libCHODPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libCHODPersistency() {
  TriggerDictionaryInitialization_libCHODPersistency_Impl();
}
