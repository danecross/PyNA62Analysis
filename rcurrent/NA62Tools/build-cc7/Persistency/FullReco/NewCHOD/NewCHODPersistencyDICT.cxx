// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME NewCHODPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include/NewCHODChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include/TNewCHODDigi.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include/TNewCHODEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include/TNewCHODHit.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include/TRecoNewCHODCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include/TRecoNewCHODEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include/TRecoNewCHODHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_NewCHODChannelID(void *p = 0);
   static void *newArray_NewCHODChannelID(Long_t size, void *p);
   static void delete_NewCHODChannelID(void *p);
   static void deleteArray_NewCHODChannelID(void *p);
   static void destruct_NewCHODChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::NewCHODChannelID*)
   {
      ::NewCHODChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::NewCHODChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("NewCHODChannelID", ::NewCHODChannelID::Class_Version(), "", 18,
                  typeid(::NewCHODChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::NewCHODChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::NewCHODChannelID) );
      instance.SetNew(&new_NewCHODChannelID);
      instance.SetNewArray(&newArray_NewCHODChannelID);
      instance.SetDelete(&delete_NewCHODChannelID);
      instance.SetDeleteArray(&deleteArray_NewCHODChannelID);
      instance.SetDestructor(&destruct_NewCHODChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::NewCHODChannelID*)
   {
      return GenerateInitInstanceLocal((::NewCHODChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::NewCHODChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TNewCHODDigi(void *p = 0);
   static void *newArray_TNewCHODDigi(Long_t size, void *p);
   static void delete_TNewCHODDigi(void *p);
   static void deleteArray_TNewCHODDigi(void *p);
   static void destruct_TNewCHODDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TNewCHODDigi*)
   {
      ::TNewCHODDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TNewCHODDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TNewCHODDigi", ::TNewCHODDigi::Class_Version(), "", 59,
                  typeid(::TNewCHODDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TNewCHODDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TNewCHODDigi) );
      instance.SetNew(&new_TNewCHODDigi);
      instance.SetNewArray(&newArray_TNewCHODDigi);
      instance.SetDelete(&delete_TNewCHODDigi);
      instance.SetDeleteArray(&deleteArray_TNewCHODDigi);
      instance.SetDestructor(&destruct_TNewCHODDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TNewCHODDigi*)
   {
      return GenerateInitInstanceLocal((::TNewCHODDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TNewCHODDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TNewCHODEvent(void *p = 0);
   static void *newArray_TNewCHODEvent(Long_t size, void *p);
   static void delete_TNewCHODEvent(void *p);
   static void deleteArray_TNewCHODEvent(void *p);
   static void destruct_TNewCHODEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TNewCHODEvent*)
   {
      ::TNewCHODEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TNewCHODEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TNewCHODEvent", ::TNewCHODEvent::Class_Version(), "", 94,
                  typeid(::TNewCHODEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TNewCHODEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TNewCHODEvent) );
      instance.SetNew(&new_TNewCHODEvent);
      instance.SetNewArray(&newArray_TNewCHODEvent);
      instance.SetDelete(&delete_TNewCHODEvent);
      instance.SetDeleteArray(&deleteArray_TNewCHODEvent);
      instance.SetDestructor(&destruct_TNewCHODEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TNewCHODEvent*)
   {
      return GenerateInitInstanceLocal((::TNewCHODEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TNewCHODEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TNewCHODHit(void *p = 0);
   static void *newArray_TNewCHODHit(Long_t size, void *p);
   static void delete_TNewCHODHit(void *p);
   static void deleteArray_TNewCHODHit(void *p);
   static void destruct_TNewCHODHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TNewCHODHit*)
   {
      ::TNewCHODHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TNewCHODHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TNewCHODHit", ::TNewCHODHit::Class_Version(), "", 121,
                  typeid(::TNewCHODHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TNewCHODHit::Dictionary, isa_proxy, 4,
                  sizeof(::TNewCHODHit) );
      instance.SetNew(&new_TNewCHODHit);
      instance.SetNewArray(&newArray_TNewCHODHit);
      instance.SetDelete(&delete_TNewCHODHit);
      instance.SetDeleteArray(&deleteArray_TNewCHODHit);
      instance.SetDestructor(&destruct_TNewCHODHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TNewCHODHit*)
   {
      return GenerateInitInstanceLocal((::TNewCHODHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TNewCHODHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoNewCHODCandidate(void *p = 0);
   static void *newArray_TRecoNewCHODCandidate(Long_t size, void *p);
   static void delete_TRecoNewCHODCandidate(void *p);
   static void deleteArray_TRecoNewCHODCandidate(void *p);
   static void destruct_TRecoNewCHODCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoNewCHODCandidate*)
   {
      ::TRecoNewCHODCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoNewCHODCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoNewCHODCandidate", ::TRecoNewCHODCandidate::Class_Version(), "", 152,
                  typeid(::TRecoNewCHODCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoNewCHODCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoNewCHODCandidate) );
      instance.SetNew(&new_TRecoNewCHODCandidate);
      instance.SetNewArray(&newArray_TRecoNewCHODCandidate);
      instance.SetDelete(&delete_TRecoNewCHODCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoNewCHODCandidate);
      instance.SetDestructor(&destruct_TRecoNewCHODCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoNewCHODCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoNewCHODCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoNewCHODCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoNewCHODHit(void *p = 0);
   static void *newArray_TRecoNewCHODHit(Long_t size, void *p);
   static void delete_TRecoNewCHODHit(void *p);
   static void deleteArray_TRecoNewCHODHit(void *p);
   static void destruct_TRecoNewCHODHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoNewCHODHit*)
   {
      ::TRecoNewCHODHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoNewCHODHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoNewCHODHit", ::TRecoNewCHODHit::Class_Version(), "TRecoNewCHODHit.hh", 14,
                  typeid(::TRecoNewCHODHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoNewCHODHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoNewCHODHit) );
      instance.SetNew(&new_TRecoNewCHODHit);
      instance.SetNewArray(&newArray_TRecoNewCHODHit);
      instance.SetDelete(&delete_TRecoNewCHODHit);
      instance.SetDeleteArray(&deleteArray_TRecoNewCHODHit);
      instance.SetDestructor(&destruct_TRecoNewCHODHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoNewCHODHit*)
   {
      return GenerateInitInstanceLocal((::TRecoNewCHODHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoNewCHODHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoNewCHODEvent(void *p = 0);
   static void *newArray_TRecoNewCHODEvent(Long_t size, void *p);
   static void delete_TRecoNewCHODEvent(void *p);
   static void deleteArray_TRecoNewCHODEvent(void *p);
   static void destruct_TRecoNewCHODEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoNewCHODEvent*)
   {
      ::TRecoNewCHODEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoNewCHODEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoNewCHODEvent", ::TRecoNewCHODEvent::Class_Version(), "", 180,
                  typeid(::TRecoNewCHODEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoNewCHODEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoNewCHODEvent) );
      instance.SetNew(&new_TRecoNewCHODEvent);
      instance.SetNewArray(&newArray_TRecoNewCHODEvent);
      instance.SetDelete(&delete_TRecoNewCHODEvent);
      instance.SetDeleteArray(&deleteArray_TRecoNewCHODEvent);
      instance.SetDestructor(&destruct_TRecoNewCHODEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoNewCHODEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoNewCHODEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoNewCHODEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr NewCHODChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *NewCHODChannelID::Class_Name()
{
   return "NewCHODChannelID";
}

//______________________________________________________________________________
const char *NewCHODChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::NewCHODChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int NewCHODChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::NewCHODChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *NewCHODChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::NewCHODChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *NewCHODChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::NewCHODChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TNewCHODDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TNewCHODDigi::Class_Name()
{
   return "TNewCHODDigi";
}

//______________________________________________________________________________
const char *TNewCHODDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TNewCHODDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TNewCHODDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TNewCHODDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TNewCHODEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TNewCHODEvent::Class_Name()
{
   return "TNewCHODEvent";
}

//______________________________________________________________________________
const char *TNewCHODEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TNewCHODEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TNewCHODEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TNewCHODEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TNewCHODHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TNewCHODHit::Class_Name()
{
   return "TNewCHODHit";
}

//______________________________________________________________________________
const char *TNewCHODHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TNewCHODHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TNewCHODHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TNewCHODHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNewCHODHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoNewCHODCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoNewCHODCandidate::Class_Name()
{
   return "TRecoNewCHODCandidate";
}

//______________________________________________________________________________
const char *TRecoNewCHODCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoNewCHODCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoNewCHODCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoNewCHODCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoNewCHODHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoNewCHODHit::Class_Name()
{
   return "TRecoNewCHODHit";
}

//______________________________________________________________________________
const char *TRecoNewCHODHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoNewCHODHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoNewCHODHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoNewCHODHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoNewCHODEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoNewCHODEvent::Class_Name()
{
   return "TRecoNewCHODEvent";
}

//______________________________________________________________________________
const char *TRecoNewCHODEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoNewCHODEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoNewCHODEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoNewCHODEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoNewCHODEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void NewCHODChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class NewCHODChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(NewCHODChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(NewCHODChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_NewCHODChannelID(void *p) {
      return  p ? new(p) ::NewCHODChannelID : new ::NewCHODChannelID;
   }
   static void *newArray_NewCHODChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::NewCHODChannelID[nElements] : new ::NewCHODChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_NewCHODChannelID(void *p) {
      delete ((::NewCHODChannelID*)p);
   }
   static void deleteArray_NewCHODChannelID(void *p) {
      delete [] ((::NewCHODChannelID*)p);
   }
   static void destruct_NewCHODChannelID(void *p) {
      typedef ::NewCHODChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::NewCHODChannelID

//______________________________________________________________________________
void TNewCHODDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TNewCHODDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TNewCHODDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TNewCHODDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TNewCHODDigi(void *p) {
      return  p ? new(p) ::TNewCHODDigi : new ::TNewCHODDigi;
   }
   static void *newArray_TNewCHODDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TNewCHODDigi[nElements] : new ::TNewCHODDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TNewCHODDigi(void *p) {
      delete ((::TNewCHODDigi*)p);
   }
   static void deleteArray_TNewCHODDigi(void *p) {
      delete [] ((::TNewCHODDigi*)p);
   }
   static void destruct_TNewCHODDigi(void *p) {
      typedef ::TNewCHODDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TNewCHODDigi

//______________________________________________________________________________
void TNewCHODEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TNewCHODEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TNewCHODEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TNewCHODEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TNewCHODEvent(void *p) {
      return  p ? new(p) ::TNewCHODEvent : new ::TNewCHODEvent;
   }
   static void *newArray_TNewCHODEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TNewCHODEvent[nElements] : new ::TNewCHODEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TNewCHODEvent(void *p) {
      delete ((::TNewCHODEvent*)p);
   }
   static void deleteArray_TNewCHODEvent(void *p) {
      delete [] ((::TNewCHODEvent*)p);
   }
   static void destruct_TNewCHODEvent(void *p) {
      typedef ::TNewCHODEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TNewCHODEvent

//______________________________________________________________________________
void TNewCHODHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TNewCHODHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TNewCHODHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TNewCHODHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TNewCHODHit(void *p) {
      return  p ? new(p) ::TNewCHODHit : new ::TNewCHODHit;
   }
   static void *newArray_TNewCHODHit(Long_t nElements, void *p) {
      return p ? new(p) ::TNewCHODHit[nElements] : new ::TNewCHODHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TNewCHODHit(void *p) {
      delete ((::TNewCHODHit*)p);
   }
   static void deleteArray_TNewCHODHit(void *p) {
      delete [] ((::TNewCHODHit*)p);
   }
   static void destruct_TNewCHODHit(void *p) {
      typedef ::TNewCHODHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TNewCHODHit

//______________________________________________________________________________
void TRecoNewCHODCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoNewCHODCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoNewCHODCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoNewCHODCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoNewCHODCandidate(void *p) {
      return  p ? new(p) ::TRecoNewCHODCandidate : new ::TRecoNewCHODCandidate;
   }
   static void *newArray_TRecoNewCHODCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoNewCHODCandidate[nElements] : new ::TRecoNewCHODCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoNewCHODCandidate(void *p) {
      delete ((::TRecoNewCHODCandidate*)p);
   }
   static void deleteArray_TRecoNewCHODCandidate(void *p) {
      delete [] ((::TRecoNewCHODCandidate*)p);
   }
   static void destruct_TRecoNewCHODCandidate(void *p) {
      typedef ::TRecoNewCHODCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoNewCHODCandidate

//______________________________________________________________________________
void TRecoNewCHODHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoNewCHODHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoNewCHODHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoNewCHODHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoNewCHODHit(void *p) {
      return  p ? new(p) ::TRecoNewCHODHit : new ::TRecoNewCHODHit;
   }
   static void *newArray_TRecoNewCHODHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoNewCHODHit[nElements] : new ::TRecoNewCHODHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoNewCHODHit(void *p) {
      delete ((::TRecoNewCHODHit*)p);
   }
   static void deleteArray_TRecoNewCHODHit(void *p) {
      delete [] ((::TRecoNewCHODHit*)p);
   }
   static void destruct_TRecoNewCHODHit(void *p) {
      typedef ::TRecoNewCHODHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoNewCHODHit

//______________________________________________________________________________
void TRecoNewCHODEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoNewCHODEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoNewCHODEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoNewCHODEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoNewCHODEvent(void *p) {
      return  p ? new(p) ::TRecoNewCHODEvent : new ::TRecoNewCHODEvent;
   }
   static void *newArray_TRecoNewCHODEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoNewCHODEvent[nElements] : new ::TRecoNewCHODEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoNewCHODEvent(void *p) {
      delete ((::TRecoNewCHODEvent*)p);
   }
   static void deleteArray_TRecoNewCHODEvent(void *p) {
      delete [] ((::TRecoNewCHODEvent*)p);
   }
   static void destruct_TRecoNewCHODEvent(void *p) {
      typedef ::TRecoNewCHODEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoNewCHODEvent

namespace {
  void TriggerDictionaryInitialization_libNewCHODPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/NewCHOD/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libNewCHODPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class NewCHODChannelID;
class TNewCHODDigi;
class TNewCHODEvent;
class TNewCHODHit;
class TRecoNewCHODCandidate;
class __attribute__((annotate("$clingAutoload$TRecoNewCHODHit.hh")))  TRecoNewCHODHit;
class TRecoNewCHODEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libNewCHODPersistency dictionary payload"

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

#ifndef NewCHODChannelID_H
#define NewCHODChannelID_H

#include "Rtypes.h"

class NewCHODChannelID {

public:
  struct chIDDecoded { Int_t fTileID, fSeqTileID, fQuadrantID; };

  NewCHODChannelID();
  virtual ~NewCHODChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); // returns position ID
  static struct chIDDecoded DecodeChannelID_Static(Int_t);
  void  DecodeChannelID(Int_t);

  Int_t  GetTileID()     { return fTileID;     }
  Int_t  GetSeqTileID()  { return fSeqTileID;  }
  Int_t  GetQuadrantID() { return fQuadrantID; }

private:

  Int_t  fTileID;     ///< ID of the physical tile (101-138, 201-238, 301-338, 401-438)
  Int_t  fSeqTileID;  ///< Sequential ID of the tile (1-151)
  Int_t  fQuadrantID; ///< ID of the quadrant

  ClassDef(NewCHODChannelID,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TNewCHODDigi_H
#define TNewCHODDigi_H

#include "TDCVHit.hh"
#include "NewCHODChannelID.hh"

class TNewCHODDigi : public TDCVHit, public NewCHODChannelID {

public:

  TNewCHODDigi();
  ~TNewCHODDigi() {}

  void Clear(Option_t* = "");

  Int_t  EncodeChannelID();
  void   DecodeChannelID();
  Int_t  GetStationID() { return 0;       }
  Bool_t IsHigh()       { return fIsHigh; }

private:

  Bool_t fIsHigh;

  ClassDef(TNewCHODDigi,1);
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TNewCHODEvent_H
#define TNewCHODEvent_H

#include "TDetectorVEvent.hh"

class TNewCHODEvent : public TDetectorVEvent {

public:
  TNewCHODEvent();
  ~TNewCHODEvent();

  void Clear(Option_t* = "");

private:

  ClassDef(TNewCHODEvent,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TNewCHODHit_H
#define TNewCHODHit_H

#include "TDetectorVHit.hh"
#include "NewCHODChannelID.hh"

class TNewCHODHit : public TDetectorVHit, public NewCHODChannelID {

public:

  TNewCHODHit();
  ~TNewCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

protected:

  ClassDef(TNewCHODHit,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TRecoNewCHODCandidate_H
#define TRecoNewCHODCandidate_H

#include "TRecoVCandidate.hh"

class TRecoNewCHODCandidate : public TRecoVCandidate {

public:

  TRecoNewCHODCandidate();
  ~TRecoNewCHODCandidate() {}

  void Clear(Option_t* = "");

private:

  ClassDef(TRecoNewCHODCandidate,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------
#ifndef TRecoNewCHODEvent_H
#define TRecoNewCHODEvent_H

#include "TRecoVEvent.hh"
#include "TRecoNewCHODCandidate.hh"
#include "TRecoNewCHODHit.hh"

class TRecoNewCHODEvent : public TRecoVEvent {

public:
  TRecoNewCHODEvent();
  ~TRecoNewCHODEvent();

  void Clear(Option_t* = "");

private:
  ClassDef(TRecoNewCHODEvent,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TRecoNewCHODHit_H
#define TRecoNewCHODHit_H

#include "TRecoVHit.hh"
#include "NewCHODChannelID.hh"

class TRecoNewCHODHit : public TRecoVHit, public NewCHODChannelID {

public:

  TRecoNewCHODHit();
  ~TRecoNewCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Double_t GetX ()                    { return fX;                 }
  void     SetX (Double_t x)          { fX = x;                    }
  Double_t GetY ()                    { return fY;                 }
  void     SetY (Double_t y)          { fY = y;                    }
  Double_t GetZ ()                    { return fZ;                 }
  void     SetZ (Double_t z)          { fZ = z;                    }
  TVector3 GetPosition()              { return TVector3(fX,fY,fZ); }

  void     SetChannel1(Int_t val)     { fChannel1 = val;    }
  void     SetChannel2(Int_t val)     { fChannel2 = val;    }
  Int_t    GetChannel1()              { return fChannel1;   }
  Int_t    GetChannel2()              { return fChannel2;   }
  void     SetROChannel1(Int_t val)   { fROChannel1 = val;  }
  void     SetROChannel2(Int_t val)   { fROChannel2 = val;  }
  Int_t    GetROChannel1()            { return fROChannel1; }
  Int_t    GetROChannel2()            { return fROChannel2; }

  void     SetTime1(Double_t val)     { fTime1 = val;       }
  void     SetTime2(Double_t val)     { fTime2 = val;       }
  Double_t GetTime1()                 { return fTime1;      }
  Double_t GetTime2()                 { return fTime2;      }
  void     SetTime1NoT0(Double_t val) { fTime1NoT0 = val;   }
  void     SetTime2NoT0(Double_t val) { fTime2NoT0 = val;   }
  void     SetTimeNoT0 (Double_t val) { fTimeNoT0  = val;   }
  Double_t GetTime1NoT0()             { return fTime1NoT0;  }
  Double_t GetTime2NoT0()             { return fTime2NoT0;  }
  Double_t GetTimeNoT0()              { return fTimeNoT0;   }
  Double_t GetEarliestTime();
  Double_t GetDeltaTime();

  void     SetType(Int_t val)         { fType  = val;       }
  Int_t    GetType()                  { return fType;       }

private:

  Int_t fType;     ///< Tight, loose, or loose masked; see NewCHOD.conf for documentation  
  Int_t fChannel1; ///< "geometric" ID of the first channel
  Int_t fChannel2; ///< "geometric" ID of the second channel
  Int_t fROChannel1;
  Int_t fROChannel2;
  Double_t fX, fY, fZ;
  Double_t fTime1, fTime2;
  Double_t fTime1NoT0, fTime2NoT0, fTimeNoT0;

  ClassDef(TRecoNewCHODHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"NewCHODChannelID", payloadCode, "@",
"TNewCHODDigi", payloadCode, "@",
"TNewCHODEvent", payloadCode, "@",
"TNewCHODHit", payloadCode, "@",
"TRecoNewCHODCandidate", payloadCode, "@",
"TRecoNewCHODEvent", payloadCode, "@",
"TRecoNewCHODHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libNewCHODPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libNewCHODPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libNewCHODPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libNewCHODPersistency() {
  TriggerDictionaryInitialization_libNewCHODPersistency_Impl();
}
