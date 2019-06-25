// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME CHANTIPersistencyDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/CHANTIChannelID.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/Pair.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/TCHANTIDigi.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/TCHANTIEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/TCHANTIHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/TRecoCHANTICandidate.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/TRecoCHANTIEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include/TRecoCHANTIHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_CHANTIChannelID(void *p = 0);
   static void *newArray_CHANTIChannelID(Long_t size, void *p);
   static void delete_CHANTIChannelID(void *p);
   static void deleteArray_CHANTIChannelID(void *p);
   static void destruct_CHANTIChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::CHANTIChannelID*)
   {
      ::CHANTIChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::CHANTIChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("CHANTIChannelID", ::CHANTIChannelID::Class_Version(), "", 25,
                  typeid(::CHANTIChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::CHANTIChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::CHANTIChannelID) );
      instance.SetNew(&new_CHANTIChannelID);
      instance.SetNewArray(&newArray_CHANTIChannelID);
      instance.SetDelete(&delete_CHANTIChannelID);
      instance.SetDeleteArray(&deleteArray_CHANTIChannelID);
      instance.SetDestructor(&destruct_CHANTIChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::CHANTIChannelID*)
   {
      return GenerateInitInstanceLocal((::CHANTIChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::CHANTIChannelID*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCHANTIDigi(void *p = 0);
   static void *newArray_TCHANTIDigi(Long_t size, void *p);
   static void delete_TCHANTIDigi(void *p);
   static void deleteArray_TCHANTIDigi(void *p);
   static void destruct_TCHANTIDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCHANTIDigi*)
   {
      ::TCHANTIDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCHANTIDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCHANTIDigi", ::TCHANTIDigi::Class_Version(), "", 109,
                  typeid(::TCHANTIDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCHANTIDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TCHANTIDigi) );
      instance.SetNew(&new_TCHANTIDigi);
      instance.SetNewArray(&newArray_TCHANTIDigi);
      instance.SetDelete(&delete_TCHANTIDigi);
      instance.SetDeleteArray(&deleteArray_TCHANTIDigi);
      instance.SetDestructor(&destruct_TCHANTIDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCHANTIDigi*)
   {
      return GenerateInitInstanceLocal((::TCHANTIDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TCHANTIDigi*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCHANTIEvent(void *p = 0);
   static void *newArray_TCHANTIEvent(Long_t size, void *p);
   static void delete_TCHANTIEvent(void *p);
   static void deleteArray_TCHANTIEvent(void *p);
   static void destruct_TCHANTIEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCHANTIEvent*)
   {
      ::TCHANTIEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCHANTIEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCHANTIEvent", ::TCHANTIEvent::Class_Version(), "", 156,
                  typeid(::TCHANTIEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCHANTIEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TCHANTIEvent) );
      instance.SetNew(&new_TCHANTIEvent);
      instance.SetNewArray(&newArray_TCHANTIEvent);
      instance.SetDelete(&delete_TCHANTIEvent);
      instance.SetDeleteArray(&deleteArray_TCHANTIEvent);
      instance.SetDestructor(&destruct_TCHANTIEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCHANTIEvent*)
   {
      return GenerateInitInstanceLocal((::TCHANTIEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TCHANTIEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCHANTIHit(void *p = 0);
   static void *newArray_TCHANTIHit(Long_t size, void *p);
   static void delete_TCHANTIHit(void *p);
   static void deleteArray_TCHANTIHit(void *p);
   static void destruct_TCHANTIHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCHANTIHit*)
   {
      ::TCHANTIHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCHANTIHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCHANTIHit", ::TCHANTIHit::Class_Version(), "", 182,
                  typeid(::TCHANTIHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCHANTIHit::Dictionary, isa_proxy, 4,
                  sizeof(::TCHANTIHit) );
      instance.SetNew(&new_TCHANTIHit);
      instance.SetNewArray(&newArray_TCHANTIHit);
      instance.SetDelete(&delete_TCHANTIHit);
      instance.SetDeleteArray(&deleteArray_TCHANTIHit);
      instance.SetDestructor(&destruct_TCHANTIHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCHANTIHit*)
   {
      return GenerateInitInstanceLocal((::TCHANTIHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TCHANTIHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCHANTICandidate(void *p = 0);
   static void *newArray_TRecoCHANTICandidate(Long_t size, void *p);
   static void delete_TRecoCHANTICandidate(void *p);
   static void deleteArray_TRecoCHANTICandidate(void *p);
   static void destruct_TRecoCHANTICandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCHANTICandidate*)
   {
      ::TRecoCHANTICandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCHANTICandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCHANTICandidate", ::TRecoCHANTICandidate::Class_Version(), "", 213,
                  typeid(::TRecoCHANTICandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCHANTICandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCHANTICandidate) );
      instance.SetNew(&new_TRecoCHANTICandidate);
      instance.SetNewArray(&newArray_TRecoCHANTICandidate);
      instance.SetDelete(&delete_TRecoCHANTICandidate);
      instance.SetDeleteArray(&deleteArray_TRecoCHANTICandidate);
      instance.SetDestructor(&destruct_TRecoCHANTICandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCHANTICandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoCHANTICandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoCHANTICandidate*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCHANTIHit(void *p = 0);
   static void *newArray_TRecoCHANTIHit(Long_t size, void *p);
   static void delete_TRecoCHANTIHit(void *p);
   static void deleteArray_TRecoCHANTIHit(void *p);
   static void destruct_TRecoCHANTIHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCHANTIHit*)
   {
      ::TRecoCHANTIHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCHANTIHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCHANTIHit", ::TRecoCHANTIHit::Class_Version(), "TRecoCHANTIHit.hh", 13,
                  typeid(::TRecoCHANTIHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCHANTIHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCHANTIHit) );
      instance.SetNew(&new_TRecoCHANTIHit);
      instance.SetNewArray(&newArray_TRecoCHANTIHit);
      instance.SetDelete(&delete_TRecoCHANTIHit);
      instance.SetDeleteArray(&deleteArray_TRecoCHANTIHit);
      instance.SetDestructor(&destruct_TRecoCHANTIHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCHANTIHit*)
   {
      return GenerateInitInstanceLocal((::TRecoCHANTIHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoCHANTIHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCHANTIEvent(void *p = 0);
   static void *newArray_TRecoCHANTIEvent(Long_t size, void *p);
   static void delete_TRecoCHANTIEvent(void *p);
   static void deleteArray_TRecoCHANTIEvent(void *p);
   static void destruct_TRecoCHANTIEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCHANTIEvent*)
   {
      ::TRecoCHANTIEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCHANTIEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCHANTIEvent", ::TRecoCHANTIEvent::Class_Version(), "", 258,
                  typeid(::TRecoCHANTIEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCHANTIEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCHANTIEvent) );
      instance.SetNew(&new_TRecoCHANTIEvent);
      instance.SetNewArray(&newArray_TRecoCHANTIEvent);
      instance.SetDelete(&delete_TRecoCHANTIEvent);
      instance.SetDeleteArray(&deleteArray_TRecoCHANTIEvent);
      instance.SetDestructor(&destruct_TRecoCHANTIEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCHANTIEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoCHANTIEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoCHANTIEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr CHANTIChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *CHANTIChannelID::Class_Name()
{
   return "CHANTIChannelID";
}

//______________________________________________________________________________
const char *CHANTIChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CHANTIChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int CHANTIChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CHANTIChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *CHANTIChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CHANTIChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *CHANTIChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CHANTIChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCHANTIDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCHANTIDigi::Class_Name()
{
   return "TCHANTIDigi";
}

//______________________________________________________________________________
const char *TCHANTIDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCHANTIDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCHANTIDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCHANTIDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCHANTIEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCHANTIEvent::Class_Name()
{
   return "TCHANTIEvent";
}

//______________________________________________________________________________
const char *TCHANTIEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCHANTIEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCHANTIEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCHANTIEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCHANTIHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCHANTIHit::Class_Name()
{
   return "TCHANTIHit";
}

//______________________________________________________________________________
const char *TCHANTIHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCHANTIHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCHANTIHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCHANTIHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCHANTIHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCHANTICandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCHANTICandidate::Class_Name()
{
   return "TRecoCHANTICandidate";
}

//______________________________________________________________________________
const char *TRecoCHANTICandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTICandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCHANTICandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTICandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCHANTICandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTICandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCHANTICandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTICandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCHANTIHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCHANTIHit::Class_Name()
{
   return "TRecoCHANTIHit";
}

//______________________________________________________________________________
const char *TRecoCHANTIHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCHANTIHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCHANTIHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCHANTIHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCHANTIEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCHANTIEvent::Class_Name()
{
   return "TRecoCHANTIEvent";
}

//______________________________________________________________________________
const char *TRecoCHANTIEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCHANTIEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCHANTIEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCHANTIEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCHANTIEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void CHANTIChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class CHANTIChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(CHANTIChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(CHANTIChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_CHANTIChannelID(void *p) {
      return  p ? new(p) ::CHANTIChannelID : new ::CHANTIChannelID;
   }
   static void *newArray_CHANTIChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::CHANTIChannelID[nElements] : new ::CHANTIChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_CHANTIChannelID(void *p) {
      delete ((::CHANTIChannelID*)p);
   }
   static void deleteArray_CHANTIChannelID(void *p) {
      delete [] ((::CHANTIChannelID*)p);
   }
   static void destruct_CHANTIChannelID(void *p) {
      typedef ::CHANTIChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::CHANTIChannelID

//______________________________________________________________________________
void TCHANTIDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCHANTIDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCHANTIDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCHANTIDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCHANTIDigi(void *p) {
      return  p ? new(p) ::TCHANTIDigi : new ::TCHANTIDigi;
   }
   static void *newArray_TCHANTIDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TCHANTIDigi[nElements] : new ::TCHANTIDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCHANTIDigi(void *p) {
      delete ((::TCHANTIDigi*)p);
   }
   static void deleteArray_TCHANTIDigi(void *p) {
      delete [] ((::TCHANTIDigi*)p);
   }
   static void destruct_TCHANTIDigi(void *p) {
      typedef ::TCHANTIDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCHANTIDigi

//______________________________________________________________________________
void TCHANTIEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCHANTIEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCHANTIEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCHANTIEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCHANTIEvent(void *p) {
      return  p ? new(p) ::TCHANTIEvent : new ::TCHANTIEvent;
   }
   static void *newArray_TCHANTIEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TCHANTIEvent[nElements] : new ::TCHANTIEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCHANTIEvent(void *p) {
      delete ((::TCHANTIEvent*)p);
   }
   static void deleteArray_TCHANTIEvent(void *p) {
      delete [] ((::TCHANTIEvent*)p);
   }
   static void destruct_TCHANTIEvent(void *p) {
      typedef ::TCHANTIEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCHANTIEvent

//______________________________________________________________________________
void TCHANTIHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCHANTIHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCHANTIHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCHANTIHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCHANTIHit(void *p) {
      return  p ? new(p) ::TCHANTIHit : new ::TCHANTIHit;
   }
   static void *newArray_TCHANTIHit(Long_t nElements, void *p) {
      return p ? new(p) ::TCHANTIHit[nElements] : new ::TCHANTIHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCHANTIHit(void *p) {
      delete ((::TCHANTIHit*)p);
   }
   static void deleteArray_TCHANTIHit(void *p) {
      delete [] ((::TCHANTIHit*)p);
   }
   static void destruct_TCHANTIHit(void *p) {
      typedef ::TCHANTIHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCHANTIHit

//______________________________________________________________________________
void TRecoCHANTICandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCHANTICandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCHANTICandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCHANTICandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCHANTICandidate(void *p) {
      return  p ? new(p) ::TRecoCHANTICandidate : new ::TRecoCHANTICandidate;
   }
   static void *newArray_TRecoCHANTICandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCHANTICandidate[nElements] : new ::TRecoCHANTICandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCHANTICandidate(void *p) {
      delete ((::TRecoCHANTICandidate*)p);
   }
   static void deleteArray_TRecoCHANTICandidate(void *p) {
      delete [] ((::TRecoCHANTICandidate*)p);
   }
   static void destruct_TRecoCHANTICandidate(void *p) {
      typedef ::TRecoCHANTICandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCHANTICandidate

//______________________________________________________________________________
void TRecoCHANTIHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCHANTIHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCHANTIHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCHANTIHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCHANTIHit(void *p) {
      return  p ? new(p) ::TRecoCHANTIHit : new ::TRecoCHANTIHit;
   }
   static void *newArray_TRecoCHANTIHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCHANTIHit[nElements] : new ::TRecoCHANTIHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCHANTIHit(void *p) {
      delete ((::TRecoCHANTIHit*)p);
   }
   static void deleteArray_TRecoCHANTIHit(void *p) {
      delete [] ((::TRecoCHANTIHit*)p);
   }
   static void destruct_TRecoCHANTIHit(void *p) {
      typedef ::TRecoCHANTIHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCHANTIHit

//______________________________________________________________________________
void TRecoCHANTIEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCHANTIEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCHANTIEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCHANTIEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCHANTIEvent(void *p) {
      return  p ? new(p) ::TRecoCHANTIEvent : new ::TRecoCHANTIEvent;
   }
   static void *newArray_TRecoCHANTIEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCHANTIEvent[nElements] : new ::TRecoCHANTIEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCHANTIEvent(void *p) {
      delete ((::TRecoCHANTIEvent*)p);
   }
   static void deleteArray_TRecoCHANTIEvent(void *p) {
      delete [] ((::TRecoCHANTIEvent*)p);
   }
   static void destruct_TRecoCHANTIEvent(void *p) {
      typedef ::TRecoCHANTIEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCHANTIEvent

namespace {
  void TriggerDictionaryInitialization_libCHANTIPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4",
"/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include/boost-1_62",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CHANTI/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libCHANTIPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class CHANTIChannelID;
class TCHANTIDigi;
class TCHANTIEvent;
class TCHANTIHit;
class TRecoCHANTICandidate;
class __attribute__((annotate("$clingAutoload$TRecoCHANTIHit.hh")))  TRecoCHANTIHit;
class TRecoCHANTIEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libCHANTIPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif
#ifndef G4_STORE_TRAJECTORY
  #define G4_STORE_TRAJECTORY 1
#endif
#ifndef G4VERBOSE
  #define G4VERBOSE 1
#endif
#ifndef G4MULTITHREADED
  #define G4MULTITHREADED 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-01-31
//
// --------------------------------------------------------------
#ifndef CHANTIChannelID_H
#define CHANTIChannelID_H
#include "Rtypes.h"

class CHANTIChannelID {

    public:

  CHANTIChannelID();
  virtual ~CHANTIChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID(Int_t);

    public:

  Int_t    GetStationID()                   { return 0;              }
  Int_t    GetPlaneID()                     { return fPlaneID;       }
  void     SetPlaneID(Int_t value)          { fPlaneID = value;      }
  Int_t    GetRingType()                    { return fRingType;      }
  void     SetRingType(Int_t value)         { fRingType = value;     }
  Int_t    GetRingID()                      { return fRingType + 2*fPlaneID; }
  Int_t    GetSideID()                      { return fSideID;        }
  void     SetSideID(Int_t value)           { fSideID = value;       }
  Int_t    GetBarID()                       { return fBarID;         }
  void     SetBarID(Int_t value)            { fBarID = value;        }

    private:

  Int_t      fPlaneID;
  Int_t      fRingType;
  Int_t      fSideID;
  Int_t      fBarID;

  ClassDef(CHANTIChannelID,1);
};
#endif
//
// Created by Vito Palladino 19.3.2009
//

#ifndef Pair_H
#define Pair_H 1


#include <iostream>
#include <vector>


class Pair{

public:

  Pair();
  Pair(int ValueX, int ValueY);
  //bool &operator==(const Pair);
  
  void Clear(Option_t* = "");

  void SetXY(int ValueX, int ValueY);

  int GetX()   { return X; };
  int GetY()   { return Y; };

private:

  int X, Y; 

};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Antonio Cassese (Antonio.Cassese@cern.ch) 2012-10-19
//
// ---------------------------------------------------------------

#ifndef TCHANTIDigi_H
#define TCHANTIDigi_H

#include "TDCVHit.hh"
#include "CHANTIChannelID.hh"

class TCHANTIDigi : public TDCVHit, public CHANTIChannelID {

public:

  TCHANTIDigi() : TDCVHit(), fThresholdType(-1), fDigiSortFlag(0) {}
  explicit TCHANTIDigi(Int_t iCh) : TDCVHit(iCh), fThresholdType(-1), fDigiSortFlag(0) {}

  void Clear(Option_t* = "");

  Bool_t IsSortable() const { return kTRUE; }
  Int_t CompareChannel(const TObject *obj) const;

  Int_t Compare(const TObject *obj) const {Int_t res = CompareChannel(obj); if(res == 0){ return TDCVHit::Compare(obj);
    } else {return res;}}
  
  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return CHANTIChannelID::GetStationID();}
  Int_t GetPlaneID()   { return CHANTIChannelID::GetPlaneID();}

public:
  
  Int_t  GetThresholdType()              { return fThresholdType;  };
  void   SetThresholdType(Int_t value)   { fThresholdType = value; };  
  void   SetSortFlag(Int_t value)        { fDigiSortFlag = value; };  

private:
  
  Int_t fThresholdType;
  Int_t fDigiSortFlag; //!   
  ClassDef(TCHANTIDigi,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TCHANTIEvent_H
#define TCHANTIEvent_H

#include "TDetectorVEvent.hh"

class TCHANTIEvent : public TDetectorVEvent {

    public:

        TCHANTIEvent();
        ~TCHANTIEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TCHANTIEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TCHANTIHit_H
#define TCHANTIHit_H

#include "TDetectorVHit.hh"
#include "CHANTIChannelID.hh"
class TCHANTIHit : public TDetectorVHit, public CHANTIChannelID {

public:
  
  TCHANTIHit();
  void Clear(Option_t* = "");
  Int_t EncodeChannelID();
  void DecodeChannelID();
  Double_t GetDistanceFromSiPM();
  Int_t GetStationID() { return CHANTIChannelID::GetStationID();}
  Int_t GetPlaneID()   { return CHANTIChannelID::GetPlaneID();}

private:

protected:
  
  ClassDef(TCHANTIHit,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Modified by Domenico Di Filippo (Domenico.DiFilippo@cern.ch) 2015-04-10
//
// --------------------------------------------------------------
#ifndef TRecoCHANTICandidate_H
#define TRecoCHANTICandidate_H

#include "TRecoVCandidate.hh"
class TRecoCHANTICandidate : public TRecoVCandidate {

    public:

        TRecoCHANTICandidate();
        ~TRecoCHANTICandidate(){};

        void Clear(Option_t* = "");

        void SetXYMult(Int_t g) {fXYMult = g;};
        Int_t GetXYMult() {return fXYMult;}
        void SetXPCharge(Double_t g) {fXPCharge = g;};
        Double_t GetXPCharge() {return fXPCharge;}
        void SetYPCharge(Double_t g) {fYPCharge = g;};
        Double_t GetYPCharge() {return fYPCharge;}
        void SetXPos(Double_t g) {fXPos = g;};
        Double_t GetXPos() {return fXPos;}
        void SetYPos(Double_t g) {fYPos = g;};
        Double_t GetYPos() {return fYPos;}

    private:

        Double_t fXPCharge; // Sum of the charges collected in the X cluster
        Double_t fYPCharge; // Sum of the charges collected in the Y cluster
        Double_t fXPos; // Sum of the charges collected in the X cluster
        Double_t fYPos; // Sum of the charges collected in the Y cluster
        Int_t fXYMult; // Type of cluster flag
  
  ClassDef(TRecoCHANTICandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoCHANTIEvent_H
#define TRecoCHANTIEvent_H

#include "TRecoVEvent.hh"
#include "TRecoCHANTICandidate.hh"
#include "TRecoCHANTIHit.hh"

class TRecoCHANTIEvent : public TRecoVEvent {

    public:

        TRecoCHANTIEvent();
        ~TRecoCHANTIEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoCHANTIEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoCHANTIHit_H
#define TRecoCHANTIHit_H

#include "TRecoVHit.hh"
#include "CHANTIChannelID.hh"

class TRecoCHANTIHit : public TRecoVHit, public CHANTIChannelID {

public:
  
  TRecoCHANTIHit();
  ~TRecoCHANTIHit(){};

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID();
  
public:
  
  void        SetTimeWidth(Double_t value)       { fTimeWidth = value;     };
  Double_t    GetTimeWidth()                     { return fTimeWidth;      };
  void        SetDeltaTime(Double_t value)       { fDeltaTime = value;     };
  Double_t    GetDeltaTime()                     { return fDeltaTime;      }; 
  void        SetDeltaWidth(Double_t value)      { fDeltaWidth = value;    };
  Double_t    GetDeltaWidth()                    { return fDeltaWidth;     }; 
  void        SetQualityFlag(Int_t value)        { fQualityFlag = value;   }; 
  Int_t       GetQualityFlag()                   { return fQualityFlag;    }; 

  Double_t    GetX()                             { return fX;              };
  void        SetX(Double_t value)               { fX = value;             };
  Double_t    GetY()                             { return fY;              };
  void        SetY(Double_t value)               { fY = value;             };
  Double_t    GetZ()                             { return fZ;              };
  void        SetZ(Double_t value)               { fZ = value;             };
  Int_t       GetThresholdFlag()                 {return fThresholdFlag;   };
  void        SetThresholdFlag(Int_t value)      {fThresholdFlag = value;  };
  Int_t       GetConnectorID();
  Double_t    GetXYTimeCorrection(Double_t Position);
  Int_t       GetMult()                         { return fMult;            };
  void        SetMult(Int_t value)              { fMult = value;           };


    private:

  Double_t fX;
  Double_t fY;
  Double_t fZ;
  Int_t fThresholdFlag; ///<    This is the flag that describe the quality of Reco Hit:
                        ///<    2: for physical hit with double thresholds crossed   
                        ///<    1: for physical hit with only low thresholds crossed   
                        ///<    0: for physical hit with only low threshold crossed   
  Double_t fTimeWidth; 
  Int_t fConnectorID;	///<	This is the connector index (from 1 to 18) corresponding to the Channel cable  
  Int_t fMult;		///<	This is the multilicity of the physics channel in a single event
  Double_t fDeltaTime;	///<	This is the difference between the Low and High threshold leading time
  Double_t fDeltaWidth;	///<	This is the difference between the Low and High threshold ToT
  Int_t    fQualityFlag;///<	This is the flag that describe the quality of the Reco Hit :
                        ///<    0 both leading and trealing edge(for physical hit with single and double thresholds crossed)
                        ///<    1 high threshold trailing edge missing (for physical hit with double thresholds crossed)
                        ///<    2 low threshold trailing edge missing (for physical hit with double thresholds crossed)
                        ///<    3 low and hig threshold trailing edge missing (for physical hit with double thresholds crossed)
                        ///<    4 trailing edge missing (for physical hit with single threshold crossed) 
                        ///<    5 leading edge missing (for physical hit with single threshold crossed)


  
  ClassDef(TRecoCHANTIHit,1);

};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"CHANTIChannelID", payloadCode, "@",
"TCHANTIDigi", payloadCode, "@",
"TCHANTIEvent", payloadCode, "@",
"TCHANTIHit", payloadCode, "@",
"TRecoCHANTICandidate", payloadCode, "@",
"TRecoCHANTIEvent", payloadCode, "@",
"TRecoCHANTIHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libCHANTIPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libCHANTIPersistency_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libCHANTIPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libCHANTIPersistency() {
  TriggerDictionaryInitialization_libCHANTIPersistency_Impl();
}
