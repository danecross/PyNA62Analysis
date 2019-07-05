// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME IRCPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include/IRCChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include/TIRCDigi.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include/TIRCEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include/TIRCHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include/TRecoIRCCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include/TRecoIRCEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include/TRecoIRCHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_IRCChannelID(void *p = 0);
   static void *newArray_IRCChannelID(Long_t size, void *p);
   static void delete_IRCChannelID(void *p);
   static void deleteArray_IRCChannelID(void *p);
   static void destruct_IRCChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::IRCChannelID*)
   {
      ::IRCChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::IRCChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("IRCChannelID", ::IRCChannelID::Class_Version(), "", 18,
                  typeid(::IRCChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::IRCChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::IRCChannelID) );
      instance.SetNew(&new_IRCChannelID);
      instance.SetNewArray(&newArray_IRCChannelID);
      instance.SetDelete(&delete_IRCChannelID);
      instance.SetDeleteArray(&deleteArray_IRCChannelID);
      instance.SetDestructor(&destruct_IRCChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::IRCChannelID*)
   {
      return GenerateInitInstanceLocal((::IRCChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::IRCChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TIRCDigi(void *p = 0);
   static void *newArray_TIRCDigi(Long_t size, void *p);
   static void delete_TIRCDigi(void *p);
   static void deleteArray_TIRCDigi(void *p);
   static void destruct_TIRCDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TIRCDigi*)
   {
      ::TIRCDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TIRCDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TIRCDigi", ::TIRCDigi::Class_Version(), "", 51,
                  typeid(::TIRCDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TIRCDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TIRCDigi) );
      instance.SetNew(&new_TIRCDigi);
      instance.SetNewArray(&newArray_TIRCDigi);
      instance.SetDelete(&delete_TIRCDigi);
      instance.SetDeleteArray(&deleteArray_TIRCDigi);
      instance.SetDestructor(&destruct_TIRCDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TIRCDigi*)
   {
      return GenerateInitInstanceLocal((::TIRCDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TIRCDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TIRCEvent(void *p = 0);
   static void *newArray_TIRCEvent(Long_t size, void *p);
   static void delete_TIRCEvent(void *p);
   static void deleteArray_TIRCEvent(void *p);
   static void destruct_TIRCEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TIRCEvent*)
   {
      ::TIRCEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TIRCEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TIRCEvent", ::TIRCEvent::Class_Version(), "", 97,
                  typeid(::TIRCEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TIRCEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TIRCEvent) );
      instance.SetNew(&new_TIRCEvent);
      instance.SetNewArray(&newArray_TIRCEvent);
      instance.SetDelete(&delete_TIRCEvent);
      instance.SetDeleteArray(&deleteArray_TIRCEvent);
      instance.SetDestructor(&destruct_TIRCEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TIRCEvent*)
   {
      return GenerateInitInstanceLocal((::TIRCEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TIRCEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TIRCHit(void *p = 0);
   static void *newArray_TIRCHit(Long_t size, void *p);
   static void delete_TIRCHit(void *p);
   static void deleteArray_TIRCHit(void *p);
   static void destruct_TIRCHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TIRCHit*)
   {
      ::TIRCHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TIRCHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TIRCHit", ::TIRCHit::Class_Version(), "", 124,
                  typeid(::TIRCHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TIRCHit::Dictionary, isa_proxy, 4,
                  sizeof(::TIRCHit) );
      instance.SetNew(&new_TIRCHit);
      instance.SetNewArray(&newArray_TIRCHit);
      instance.SetDelete(&delete_TIRCHit);
      instance.SetDeleteArray(&deleteArray_TIRCHit);
      instance.SetDestructor(&destruct_TIRCHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TIRCHit*)
   {
      return GenerateInitInstanceLocal((::TIRCHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TIRCHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoIRCCandidate(void *p = 0);
   static void *newArray_TRecoIRCCandidate(Long_t size, void *p);
   static void delete_TRecoIRCCandidate(void *p);
   static void deleteArray_TRecoIRCCandidate(void *p);
   static void destruct_TRecoIRCCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoIRCCandidate*)
   {
      ::TRecoIRCCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoIRCCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoIRCCandidate", ::TRecoIRCCandidate::Class_Version(), "", 162,
                  typeid(::TRecoIRCCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoIRCCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoIRCCandidate) );
      instance.SetNew(&new_TRecoIRCCandidate);
      instance.SetNewArray(&newArray_TRecoIRCCandidate);
      instance.SetDelete(&delete_TRecoIRCCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoIRCCandidate);
      instance.SetDestructor(&destruct_TRecoIRCCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoIRCCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoIRCCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoIRCCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoIRCHit(void *p = 0);
   static void *newArray_TRecoIRCHit(Long_t size, void *p);
   static void delete_TRecoIRCHit(void *p);
   static void deleteArray_TRecoIRCHit(void *p);
   static void destruct_TRecoIRCHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoIRCHit*)
   {
      ::TRecoIRCHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoIRCHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoIRCHit", ::TRecoIRCHit::Class_Version(), "TRecoIRCHit.hh", 13,
                  typeid(::TRecoIRCHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoIRCHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoIRCHit) );
      instance.SetNew(&new_TRecoIRCHit);
      instance.SetNewArray(&newArray_TRecoIRCHit);
      instance.SetDelete(&delete_TRecoIRCHit);
      instance.SetDeleteArray(&deleteArray_TRecoIRCHit);
      instance.SetDestructor(&destruct_TRecoIRCHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoIRCHit*)
   {
      return GenerateInitInstanceLocal((::TRecoIRCHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoIRCHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoIRCEvent(void *p = 0);
   static void *newArray_TRecoIRCEvent(Long_t size, void *p);
   static void delete_TRecoIRCEvent(void *p);
   static void deleteArray_TRecoIRCEvent(void *p);
   static void destruct_TRecoIRCEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoIRCEvent*)
   {
      ::TRecoIRCEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoIRCEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoIRCEvent", ::TRecoIRCEvent::Class_Version(), "", 190,
                  typeid(::TRecoIRCEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoIRCEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoIRCEvent) );
      instance.SetNew(&new_TRecoIRCEvent);
      instance.SetNewArray(&newArray_TRecoIRCEvent);
      instance.SetDelete(&delete_TRecoIRCEvent);
      instance.SetDeleteArray(&deleteArray_TRecoIRCEvent);
      instance.SetDestructor(&destruct_TRecoIRCEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoIRCEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoIRCEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoIRCEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr IRCChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *IRCChannelID::Class_Name()
{
   return "IRCChannelID";
}

//______________________________________________________________________________
const char *IRCChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::IRCChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int IRCChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::IRCChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *IRCChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::IRCChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *IRCChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::IRCChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TIRCDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TIRCDigi::Class_Name()
{
   return "TIRCDigi";
}

//______________________________________________________________________________
const char *TIRCDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TIRCDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TIRCDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TIRCDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TIRCDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TIRCDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TIRCDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TIRCDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TIRCEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TIRCEvent::Class_Name()
{
   return "TIRCEvent";
}

//______________________________________________________________________________
const char *TIRCEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TIRCEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TIRCEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TIRCEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TIRCEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TIRCEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TIRCEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TIRCEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TIRCHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TIRCHit::Class_Name()
{
   return "TIRCHit";
}

//______________________________________________________________________________
const char *TIRCHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TIRCHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TIRCHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TIRCHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TIRCHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TIRCHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TIRCHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TIRCHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoIRCCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoIRCCandidate::Class_Name()
{
   return "TRecoIRCCandidate";
}

//______________________________________________________________________________
const char *TRecoIRCCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoIRCCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoIRCCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoIRCCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoIRCHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoIRCHit::Class_Name()
{
   return "TRecoIRCHit";
}

//______________________________________________________________________________
const char *TRecoIRCHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoIRCHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoIRCHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoIRCHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoIRCEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoIRCEvent::Class_Name()
{
   return "TRecoIRCEvent";
}

//______________________________________________________________________________
const char *TRecoIRCEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoIRCEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoIRCEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoIRCEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoIRCEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void IRCChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class IRCChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(IRCChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(IRCChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_IRCChannelID(void *p) {
      return  p ? new(p) ::IRCChannelID : new ::IRCChannelID;
   }
   static void *newArray_IRCChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::IRCChannelID[nElements] : new ::IRCChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_IRCChannelID(void *p) {
      delete ((::IRCChannelID*)p);
   }
   static void deleteArray_IRCChannelID(void *p) {
      delete [] ((::IRCChannelID*)p);
   }
   static void destruct_IRCChannelID(void *p) {
      typedef ::IRCChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::IRCChannelID

//______________________________________________________________________________
void TIRCDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TIRCDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TIRCDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TIRCDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TIRCDigi(void *p) {
      return  p ? new(p) ::TIRCDigi : new ::TIRCDigi;
   }
   static void *newArray_TIRCDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TIRCDigi[nElements] : new ::TIRCDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TIRCDigi(void *p) {
      delete ((::TIRCDigi*)p);
   }
   static void deleteArray_TIRCDigi(void *p) {
      delete [] ((::TIRCDigi*)p);
   }
   static void destruct_TIRCDigi(void *p) {
      typedef ::TIRCDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TIRCDigi

//______________________________________________________________________________
void TIRCEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TIRCEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TIRCEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TIRCEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TIRCEvent(void *p) {
      return  p ? new(p) ::TIRCEvent : new ::TIRCEvent;
   }
   static void *newArray_TIRCEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TIRCEvent[nElements] : new ::TIRCEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TIRCEvent(void *p) {
      delete ((::TIRCEvent*)p);
   }
   static void deleteArray_TIRCEvent(void *p) {
      delete [] ((::TIRCEvent*)p);
   }
   static void destruct_TIRCEvent(void *p) {
      typedef ::TIRCEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TIRCEvent

//______________________________________________________________________________
void TIRCHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TIRCHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TIRCHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TIRCHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TIRCHit(void *p) {
      return  p ? new(p) ::TIRCHit : new ::TIRCHit;
   }
   static void *newArray_TIRCHit(Long_t nElements, void *p) {
      return p ? new(p) ::TIRCHit[nElements] : new ::TIRCHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TIRCHit(void *p) {
      delete ((::TIRCHit*)p);
   }
   static void deleteArray_TIRCHit(void *p) {
      delete [] ((::TIRCHit*)p);
   }
   static void destruct_TIRCHit(void *p) {
      typedef ::TIRCHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TIRCHit

//______________________________________________________________________________
void TRecoIRCCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoIRCCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoIRCCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoIRCCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoIRCCandidate(void *p) {
      return  p ? new(p) ::TRecoIRCCandidate : new ::TRecoIRCCandidate;
   }
   static void *newArray_TRecoIRCCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoIRCCandidate[nElements] : new ::TRecoIRCCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoIRCCandidate(void *p) {
      delete ((::TRecoIRCCandidate*)p);
   }
   static void deleteArray_TRecoIRCCandidate(void *p) {
      delete [] ((::TRecoIRCCandidate*)p);
   }
   static void destruct_TRecoIRCCandidate(void *p) {
      typedef ::TRecoIRCCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoIRCCandidate

//______________________________________________________________________________
void TRecoIRCHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoIRCHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoIRCHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoIRCHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoIRCHit(void *p) {
      return  p ? new(p) ::TRecoIRCHit : new ::TRecoIRCHit;
   }
   static void *newArray_TRecoIRCHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoIRCHit[nElements] : new ::TRecoIRCHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoIRCHit(void *p) {
      delete ((::TRecoIRCHit*)p);
   }
   static void deleteArray_TRecoIRCHit(void *p) {
      delete [] ((::TRecoIRCHit*)p);
   }
   static void destruct_TRecoIRCHit(void *p) {
      typedef ::TRecoIRCHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoIRCHit

//______________________________________________________________________________
void TRecoIRCEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoIRCEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoIRCEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoIRCEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoIRCEvent(void *p) {
      return  p ? new(p) ::TRecoIRCEvent : new ::TRecoIRCEvent;
   }
   static void *newArray_TRecoIRCEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoIRCEvent[nElements] : new ::TRecoIRCEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoIRCEvent(void *p) {
      delete ((::TRecoIRCEvent*)p);
   }
   static void deleteArray_TRecoIRCEvent(void *p) {
      delete [] ((::TRecoIRCEvent*)p);
   }
   static void destruct_TRecoIRCEvent(void *p) {
      typedef ::TRecoIRCEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoIRCEvent

namespace {
  void TriggerDictionaryInitialization_libIRCPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/IRC/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libIRCPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class IRCChannelID;
class TIRCDigi;
class TIRCEvent;
class TIRCHit;
class TRecoIRCCandidate;
class __attribute__((annotate("$clingAutoload$TRecoIRCHit.hh")))  TRecoIRCHit;
class TRecoIRCEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libIRCPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
/*
 * IRCChannelID.hh
 *
 *  Created on: Sep 26, 2015
 *      Author: veni
 */

#ifndef IRC_PERSISTENCY_INCLUDE_IRCCHANNELID_HH_
#define IRC_PERSISTENCY_INCLUDE_IRCCHANNELID_HH_

#include "Rtypes.h"

class IRCChannelID {
public:
  IRCChannelID();
  explicit IRCChannelID(Int_t);
  virtual ~IRCChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  static Int_t DecodeChannelID_Static(Int_t); // converts position ID into PMTID, IsHighThreshold
  void  DecodeChannelID(Int_t); // converts position ID into PMTID, IsHighThreshold
  Int_t GetPMTID()           const { return fPMTID;              };

private:
  Int_t fPMTID;

 ClassDef(IRCChannelID,1);
};

#endif /* IRC_PERSISTENCY_INCLUDE_IRCCHANNELID_HH_ */
// --------------------------------------------------------------
// History:
//
// Created by T Spadaro (tommaso.spadaro@cern.ch) 2015-05-14
//
// --------------------------------------------------------------
#ifndef TIRCDigi_H
#define TIRCDigi_H

#include "TDCVHit.hh"
#include "IRCChannelID.hh"

class TIRCDigi : public TDCVHit, public IRCChannelID {

  public:

    TIRCDigi();// : TDCVHit(){}
    explicit TIRCDigi(Int_t iCh) : TDCVHit(iCh), IRCChannelID(iCh%1000){}
    explicit TIRCDigi(TVHit*);
    ~TIRCDigi() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return (fChannelID%1000)/100; }

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }
    //Int_t      GetPMTID   ()   {  return   IRCChannelID::EncodeChannelID();}
    //Int_t      GetChannelID            ()const  {return  fChannelID;}
    Int_t      GetCorrespondingLowHighChannelId()const;

    Int_t GetThresholdType() const { return fChannelID/1000; } //Low Threshold: 0, High Threshold: 1

    Bool_t     HasLeadingEdge          ()       {return  GetDetectedEdge()&1;}
    Bool_t     HasTrailingEdge         ()       {return  GetDetectedEdge()&2;}

  private:
    //  Int_t fPMTID;
    //  Bool_t fIsLowThresholdChannel;

    ClassDef(TIRCDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TIRCEvent_H
#define TIRCEvent_H

#include "TDetectorVEvent.hh"

class TIRCEvent : public TDetectorVEvent {

    public:

        TIRCEvent();
        ~TIRCEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TIRCEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TIRCHit_H
#define TIRCHit_H

#include "TDetectorVHit.hh"
#include "IRCChannelID.hh"

class TIRCHit : public TDetectorVHit, public IRCChannelID {

  public:

    TIRCHit();
    ~TIRCHit(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  public:

    Int_t                GetScintillatorID()                                { return fScintillatorID;               };
    void                 SetScintillatorID(Int_t value)                     { fScintillatorID = value;              };

  protected:

    Int_t      fScintillatorID;

    ClassDef(TIRCHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoIRCCandidate_H
#define TRecoIRCCandidate_H

#include "TRecoVCandidate.hh"

class TRecoIRCCandidate : public TRecoVCandidate {

    public:

        TRecoIRCCandidate();
        ~TRecoIRCCandidate(){};

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoIRCCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoIRCEvent_H
#define TRecoIRCEvent_H

#include "TRecoVEvent.hh"
#include "TRecoIRCCandidate.hh"
#include "TRecoIRCHit.hh"

class TRecoIRCEvent : public TRecoVEvent {

    public:

        TRecoIRCEvent();
        ~TRecoIRCEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoIRCEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoIRCHit_H
#define TRecoIRCHit_H

#include "TRecoVHit.hh"
#include "IRCChannelID.hh"

class TRecoIRCHit : public TRecoVHit, public IRCChannelID {

    public:

        TRecoIRCHit();
        ~TRecoIRCHit(){};

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void  DecodeChannelID();

    public:

        void SetLeadingEdgeLow(Double_t edgeTime){fLeadingEdgeLow = edgeTime; fEdgeMask |= 1;}
        void SetLeadingEdgeHigh(Double_t edgeTime){fLeadingEdgeHigh = edgeTime; fEdgeMask |= 2;}
        void SetTrailingEdgeHigh(Double_t edgeTime){fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4;}
        void SetTrailingEdgeLow(Double_t edgeTime){fTrailingEdgeLow = edgeTime; fEdgeMask |= 8;}
        void SetTimeNoT0(Double_t val)         { fTimeNoT0 = val;          }

        Double_t GetLeadingEdgeLow(){if (fEdgeMask & 1) {return fLeadingEdgeLow;} else {return 0;}}
        Double_t GetLeadingEdgeHigh(){if (fEdgeMask & 2) {return fLeadingEdgeHigh;} else {return 0;}}
        Double_t GetTrailingEdgeHigh(){if (fEdgeMask & 4) {return fTrailingEdgeHigh;} else {return 0;}}
        Double_t GetTrailingEdgeLow(){if (fEdgeMask & 8) {return fTrailingEdgeLow;} else {return 0;}}

        Double_t GetTimeNoT0                ()const{return fTimeNoT0;}
        //Double_t GetTimeOverThreshold       ()const{return fTimeOvThr;}
        void SetTimeOverThresholdLowThr (Double_t val){fTimeOvThrLow=val;}
        void SetTimeOverThresholdHighThr(Double_t val){fTimeOvThrHigh=val;}
        void SetLowThresholdROChannelID (Int_t val){fLowThresholdROChannelID =val;}
        void SetHighThresholdROChannelID(Int_t val){fHighThresholdROChannelID=val;}
        void SetLeadingESlewingSlope (Double_t val){fLeadingESlewingSlope =val;}
        void SetTrailingESlewingSlope(Double_t val){fTrailingESlewingSlope=val;}

        Double_t GetLeadingEdgeLow          ()const{return ((fEdgeMask & 0x1)? fLeadingEdgeLow  :0);}
        Double_t GetLeadingEdgeHigh         ()const{return ((fEdgeMask & 0x2)? fLeadingEdgeHigh :0);}
        Double_t GetTrailingEdgeHigh        ()const{return ((fEdgeMask & 0x4)? fTrailingEdgeHigh:0);}
        Double_t GetTrailingEdgeLow         ()const{return ((fEdgeMask & 0x8)? fTrailingEdgeLow :0);}


        Double_t GetTimeOverThresholdLowThr ()const{return fTimeOvThrLow;}
        Double_t GetTimeOverThresholdHighThr()const{return fTimeOvThrHigh;}
        Int_t    GetLowThresholdROChannelID ()const{return fLowThresholdROChannelID; }
        Int_t    GetHighThresholdROChannelID()const{return fHighThresholdROChannelID;}
        Double_t GetLeadingESlewingSlope    ()const{return fLeadingESlewingSlope ;}
        Double_t GetTrailingESlewingSlope   ()const{return fTrailingESlewingSlope;}
        Double_t GetSlewingCorrection(Double_t, Double_t);
        // overloaded because TVChannelID::GetChannelID() is not const
        Int_t    GetChannelID               ()const{return fChannelID;}


        Bool_t HasLeadingEdgeLow   ()const{return fEdgeMask & 0x1;}
        Bool_t HasLeadingEdgeHigh  ()const{return fEdgeMask & 0x2;}
        Bool_t HasTrailingEdgeHigh ()const{return fEdgeMask & 0x4;}
        Bool_t HasTrailingEdgeLow  ()const{return fEdgeMask & 0x8;}
        Bool_t HasAll4EdgesDetected()const{return fEdgeMask==0xF;}
        Bool_t HasAllTimesInOrder()const;

        Int_t GetEdgeMask() const {return fEdgeMask;}

    private:
        Double_t fTimeNoT0;

        Int_t fEdgeMask; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow
        Double_t fTimeOvThrLow;
        Double_t fTimeOvThrHigh;
        Double_t fLeadingESlewingSlope;
        Double_t fTrailingESlewingSlope;

        Double_t fLeadingEdgeLow; ///< Time of leading low, subtracted of the trigger time only
        Double_t fTrailingEdgeLow;///< Time of leading high, subtracted of the trigger time only
        Double_t fLeadingEdgeHigh;///< Time of trailing high, subtracted of the trigger time only
        Double_t fTrailingEdgeHigh;///< Time of trailing low, subtracted of the trigger time only
        Int_t fLowThresholdROChannelID;
        Int_t fHighThresholdROChannelID;

        ClassDef(TRecoIRCHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"IRCChannelID", payloadCode, "@",
"TIRCDigi", payloadCode, "@",
"TIRCEvent", payloadCode, "@",
"TIRCHit", payloadCode, "@",
"TRecoIRCCandidate", payloadCode, "@",
"TRecoIRCEvent", payloadCode, "@",
"TRecoIRCHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libIRCPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libIRCPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libIRCPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libIRCPersistency() {
  TriggerDictionaryInitialization_libIRCPersistency_Impl();
}
