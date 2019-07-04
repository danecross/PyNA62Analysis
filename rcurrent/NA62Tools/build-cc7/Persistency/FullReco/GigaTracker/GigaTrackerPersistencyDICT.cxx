// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME GigaTrackerPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/GigaTrackerChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TDigiGigaTrackerError.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TGigaTrackerDigi.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TGigaTrackerDigiEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TGigaTrackerEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TGigaTrackerHit.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TGigaTrackerSpecialTriggerEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TRecoGigaTrackerCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TRecoGigaTrackerEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include/TRecoGigaTrackerHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_GigaTrackerChannelID(void *p = 0);
   static void *newArray_GigaTrackerChannelID(Long_t size, void *p);
   static void delete_GigaTrackerChannelID(void *p);
   static void deleteArray_GigaTrackerChannelID(void *p);
   static void destruct_GigaTrackerChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::GigaTrackerChannelID*)
   {
      ::GigaTrackerChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::GigaTrackerChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("GigaTrackerChannelID", ::GigaTrackerChannelID::Class_Version(), "", 20,
                  typeid(::GigaTrackerChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::GigaTrackerChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::GigaTrackerChannelID) );
      instance.SetNew(&new_GigaTrackerChannelID);
      instance.SetNewArray(&newArray_GigaTrackerChannelID);
      instance.SetDelete(&delete_GigaTrackerChannelID);
      instance.SetDeleteArray(&deleteArray_GigaTrackerChannelID);
      instance.SetDestructor(&destruct_GigaTrackerChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::GigaTrackerChannelID*)
   {
      return GenerateInitInstanceLocal((::GigaTrackerChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::GigaTrackerChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDigiGigaTrackerError(void *p = 0);
   static void *newArray_TDigiGigaTrackerError(Long_t size, void *p);
   static void delete_TDigiGigaTrackerError(void *p);
   static void deleteArray_TDigiGigaTrackerError(void *p);
   static void destruct_TDigiGigaTrackerError(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDigiGigaTrackerError*)
   {
      ::TDigiGigaTrackerError *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDigiGigaTrackerError >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDigiGigaTrackerError", ::TDigiGigaTrackerError::Class_Version(), "", 72,
                  typeid(::TDigiGigaTrackerError), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDigiGigaTrackerError::Dictionary, isa_proxy, 4,
                  sizeof(::TDigiGigaTrackerError) );
      instance.SetNew(&new_TDigiGigaTrackerError);
      instance.SetNewArray(&newArray_TDigiGigaTrackerError);
      instance.SetDelete(&delete_TDigiGigaTrackerError);
      instance.SetDeleteArray(&deleteArray_TDigiGigaTrackerError);
      instance.SetDestructor(&destruct_TDigiGigaTrackerError);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDigiGigaTrackerError*)
   {
      return GenerateInitInstanceLocal((::TDigiGigaTrackerError*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDigiGigaTrackerError*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TGigaTrackerDigi(void *p = 0);
   static void *newArray_TGigaTrackerDigi(Long_t size, void *p);
   static void delete_TGigaTrackerDigi(void *p);
   static void deleteArray_TGigaTrackerDigi(void *p);
   static void destruct_TGigaTrackerDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TGigaTrackerDigi*)
   {
      ::TGigaTrackerDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TGigaTrackerDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TGigaTrackerDigi", ::TGigaTrackerDigi::Class_Version(), "", 139,
                  typeid(::TGigaTrackerDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TGigaTrackerDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TGigaTrackerDigi) );
      instance.SetNew(&new_TGigaTrackerDigi);
      instance.SetNewArray(&newArray_TGigaTrackerDigi);
      instance.SetDelete(&delete_TGigaTrackerDigi);
      instance.SetDeleteArray(&deleteArray_TGigaTrackerDigi);
      instance.SetDestructor(&destruct_TGigaTrackerDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TGigaTrackerDigi*)
   {
      return GenerateInitInstanceLocal((::TGigaTrackerDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TGigaTrackerDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TGigaTrackerEvent(void *p = 0);
   static void *newArray_TGigaTrackerEvent(Long_t size, void *p);
   static void delete_TGigaTrackerEvent(void *p);
   static void deleteArray_TGigaTrackerEvent(void *p);
   static void destruct_TGigaTrackerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TGigaTrackerEvent*)
   {
      ::TGigaTrackerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TGigaTrackerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TGigaTrackerEvent", ::TGigaTrackerEvent::Class_Version(), "", 272,
                  typeid(::TGigaTrackerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TGigaTrackerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TGigaTrackerEvent) );
      instance.SetNew(&new_TGigaTrackerEvent);
      instance.SetNewArray(&newArray_TGigaTrackerEvent);
      instance.SetDelete(&delete_TGigaTrackerEvent);
      instance.SetDeleteArray(&deleteArray_TGigaTrackerEvent);
      instance.SetDestructor(&destruct_TGigaTrackerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TGigaTrackerEvent*)
   {
      return GenerateInitInstanceLocal((::TGigaTrackerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TGigaTrackerEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TGigaTrackerHit(void *p = 0);
   static void *newArray_TGigaTrackerHit(Long_t size, void *p);
   static void delete_TGigaTrackerHit(void *p);
   static void deleteArray_TGigaTrackerHit(void *p);
   static void destruct_TGigaTrackerHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TGigaTrackerHit*)
   {
      ::TGigaTrackerHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TGigaTrackerHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TGigaTrackerHit", ::TGigaTrackerHit::Class_Version(), "", 299,
                  typeid(::TGigaTrackerHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TGigaTrackerHit::Dictionary, isa_proxy, 4,
                  sizeof(::TGigaTrackerHit) );
      instance.SetNew(&new_TGigaTrackerHit);
      instance.SetNewArray(&newArray_TGigaTrackerHit);
      instance.SetDelete(&delete_TGigaTrackerHit);
      instance.SetDeleteArray(&deleteArray_TGigaTrackerHit);
      instance.SetDestructor(&destruct_TGigaTrackerHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TGigaTrackerHit*)
   {
      return GenerateInitInstanceLocal((::TGigaTrackerHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TGigaTrackerHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TGigaTrackerSpecialTriggerEvent(void *p = 0);
   static void *newArray_TGigaTrackerSpecialTriggerEvent(Long_t size, void *p);
   static void delete_TGigaTrackerSpecialTriggerEvent(void *p);
   static void deleteArray_TGigaTrackerSpecialTriggerEvent(void *p);
   static void destruct_TGigaTrackerSpecialTriggerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TGigaTrackerSpecialTriggerEvent*)
   {
      ::TGigaTrackerSpecialTriggerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TGigaTrackerSpecialTriggerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TGigaTrackerSpecialTriggerEvent", ::TGigaTrackerSpecialTriggerEvent::Class_Version(), "", 326,
                  typeid(::TGigaTrackerSpecialTriggerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TGigaTrackerSpecialTriggerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TGigaTrackerSpecialTriggerEvent) );
      instance.SetNew(&new_TGigaTrackerSpecialTriggerEvent);
      instance.SetNewArray(&newArray_TGigaTrackerSpecialTriggerEvent);
      instance.SetDelete(&delete_TGigaTrackerSpecialTriggerEvent);
      instance.SetDeleteArray(&deleteArray_TGigaTrackerSpecialTriggerEvent);
      instance.SetDestructor(&destruct_TGigaTrackerSpecialTriggerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TGigaTrackerSpecialTriggerEvent*)
   {
      return GenerateInitInstanceLocal((::TGigaTrackerSpecialTriggerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TGigaTrackerSpecialTriggerEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoGigaTrackerCandidate(void *p = 0);
   static void *newArray_TRecoGigaTrackerCandidate(Long_t size, void *p);
   static void delete_TRecoGigaTrackerCandidate(void *p);
   static void deleteArray_TRecoGigaTrackerCandidate(void *p);
   static void destruct_TRecoGigaTrackerCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoGigaTrackerCandidate*)
   {
      ::TRecoGigaTrackerCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoGigaTrackerCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoGigaTrackerCandidate", ::TRecoGigaTrackerCandidate::Class_Version(), "", 370,
                  typeid(::TRecoGigaTrackerCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoGigaTrackerCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoGigaTrackerCandidate) );
      instance.SetNew(&new_TRecoGigaTrackerCandidate);
      instance.SetNewArray(&newArray_TRecoGigaTrackerCandidate);
      instance.SetDelete(&delete_TRecoGigaTrackerCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoGigaTrackerCandidate);
      instance.SetDestructor(&destruct_TRecoGigaTrackerCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoGigaTrackerCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoGigaTrackerCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoGigaTrackerCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoGigaTrackerHit(void *p = 0);
   static void *newArray_TRecoGigaTrackerHit(Long_t size, void *p);
   static void delete_TRecoGigaTrackerHit(void *p);
   static void deleteArray_TRecoGigaTrackerHit(void *p);
   static void destruct_TRecoGigaTrackerHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoGigaTrackerHit*)
   {
      ::TRecoGigaTrackerHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoGigaTrackerHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoGigaTrackerHit", ::TRecoGigaTrackerHit::Class_Version(), "TRecoGigaTrackerHit.hh", 15,
                  typeid(::TRecoGigaTrackerHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoGigaTrackerHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoGigaTrackerHit) );
      instance.SetNew(&new_TRecoGigaTrackerHit);
      instance.SetNewArray(&newArray_TRecoGigaTrackerHit);
      instance.SetDelete(&delete_TRecoGigaTrackerHit);
      instance.SetDeleteArray(&deleteArray_TRecoGigaTrackerHit);
      instance.SetDestructor(&destruct_TRecoGigaTrackerHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoGigaTrackerHit*)
   {
      return GenerateInitInstanceLocal((::TRecoGigaTrackerHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoGigaTrackerHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoGigaTrackerEvent(void *p = 0);
   static void *newArray_TRecoGigaTrackerEvent(Long_t size, void *p);
   static void delete_TRecoGigaTrackerEvent(void *p);
   static void deleteArray_TRecoGigaTrackerEvent(void *p);
   static void destruct_TRecoGigaTrackerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoGigaTrackerEvent*)
   {
      ::TRecoGigaTrackerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoGigaTrackerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoGigaTrackerEvent", ::TRecoGigaTrackerEvent::Class_Version(), "", 453,
                  typeid(::TRecoGigaTrackerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoGigaTrackerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoGigaTrackerEvent) );
      instance.SetNew(&new_TRecoGigaTrackerEvent);
      instance.SetNewArray(&newArray_TRecoGigaTrackerEvent);
      instance.SetDelete(&delete_TRecoGigaTrackerEvent);
      instance.SetDeleteArray(&deleteArray_TRecoGigaTrackerEvent);
      instance.SetDestructor(&destruct_TRecoGigaTrackerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoGigaTrackerEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoGigaTrackerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoGigaTrackerEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr GigaTrackerChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *GigaTrackerChannelID::Class_Name()
{
   return "GigaTrackerChannelID";
}

//______________________________________________________________________________
const char *GigaTrackerChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::GigaTrackerChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int GigaTrackerChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::GigaTrackerChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *GigaTrackerChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::GigaTrackerChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *GigaTrackerChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::GigaTrackerChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDigiGigaTrackerError::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDigiGigaTrackerError::Class_Name()
{
   return "TDigiGigaTrackerError";
}

//______________________________________________________________________________
const char *TDigiGigaTrackerError::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiGigaTrackerError*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDigiGigaTrackerError::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiGigaTrackerError*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDigiGigaTrackerError::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiGigaTrackerError*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDigiGigaTrackerError::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiGigaTrackerError*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TGigaTrackerDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TGigaTrackerDigi::Class_Name()
{
   return "TGigaTrackerDigi";
}

//______________________________________________________________________________
const char *TGigaTrackerDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TGigaTrackerDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TGigaTrackerDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TGigaTrackerDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TGigaTrackerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TGigaTrackerEvent::Class_Name()
{
   return "TGigaTrackerEvent";
}

//______________________________________________________________________________
const char *TGigaTrackerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TGigaTrackerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TGigaTrackerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TGigaTrackerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TGigaTrackerHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TGigaTrackerHit::Class_Name()
{
   return "TGigaTrackerHit";
}

//______________________________________________________________________________
const char *TGigaTrackerHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TGigaTrackerHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TGigaTrackerHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TGigaTrackerHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TGigaTrackerSpecialTriggerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TGigaTrackerSpecialTriggerEvent::Class_Name()
{
   return "TGigaTrackerSpecialTriggerEvent";
}

//______________________________________________________________________________
const char *TGigaTrackerSpecialTriggerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerSpecialTriggerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TGigaTrackerSpecialTriggerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerSpecialTriggerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TGigaTrackerSpecialTriggerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerSpecialTriggerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TGigaTrackerSpecialTriggerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TGigaTrackerSpecialTriggerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoGigaTrackerCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoGigaTrackerCandidate::Class_Name()
{
   return "TRecoGigaTrackerCandidate";
}

//______________________________________________________________________________
const char *TRecoGigaTrackerCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoGigaTrackerCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoGigaTrackerCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoGigaTrackerCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoGigaTrackerHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoGigaTrackerHit::Class_Name()
{
   return "TRecoGigaTrackerHit";
}

//______________________________________________________________________________
const char *TRecoGigaTrackerHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoGigaTrackerHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoGigaTrackerHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoGigaTrackerHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoGigaTrackerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoGigaTrackerEvent::Class_Name()
{
   return "TRecoGigaTrackerEvent";
}

//______________________________________________________________________________
const char *TRecoGigaTrackerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoGigaTrackerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoGigaTrackerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoGigaTrackerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoGigaTrackerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void GigaTrackerChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class GigaTrackerChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(GigaTrackerChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(GigaTrackerChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_GigaTrackerChannelID(void *p) {
      return  p ? new(p) ::GigaTrackerChannelID : new ::GigaTrackerChannelID;
   }
   static void *newArray_GigaTrackerChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::GigaTrackerChannelID[nElements] : new ::GigaTrackerChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_GigaTrackerChannelID(void *p) {
      delete ((::GigaTrackerChannelID*)p);
   }
   static void deleteArray_GigaTrackerChannelID(void *p) {
      delete [] ((::GigaTrackerChannelID*)p);
   }
   static void destruct_GigaTrackerChannelID(void *p) {
      typedef ::GigaTrackerChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::GigaTrackerChannelID

//______________________________________________________________________________
void TDigiGigaTrackerError::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDigiGigaTrackerError.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDigiGigaTrackerError::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDigiGigaTrackerError::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDigiGigaTrackerError(void *p) {
      return  p ? new(p) ::TDigiGigaTrackerError : new ::TDigiGigaTrackerError;
   }
   static void *newArray_TDigiGigaTrackerError(Long_t nElements, void *p) {
      return p ? new(p) ::TDigiGigaTrackerError[nElements] : new ::TDigiGigaTrackerError[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDigiGigaTrackerError(void *p) {
      delete ((::TDigiGigaTrackerError*)p);
   }
   static void deleteArray_TDigiGigaTrackerError(void *p) {
      delete [] ((::TDigiGigaTrackerError*)p);
   }
   static void destruct_TDigiGigaTrackerError(void *p) {
      typedef ::TDigiGigaTrackerError current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDigiGigaTrackerError

//______________________________________________________________________________
void TGigaTrackerDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TGigaTrackerDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TGigaTrackerDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TGigaTrackerDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TGigaTrackerDigi(void *p) {
      return  p ? new(p) ::TGigaTrackerDigi : new ::TGigaTrackerDigi;
   }
   static void *newArray_TGigaTrackerDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TGigaTrackerDigi[nElements] : new ::TGigaTrackerDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TGigaTrackerDigi(void *p) {
      delete ((::TGigaTrackerDigi*)p);
   }
   static void deleteArray_TGigaTrackerDigi(void *p) {
      delete [] ((::TGigaTrackerDigi*)p);
   }
   static void destruct_TGigaTrackerDigi(void *p) {
      typedef ::TGigaTrackerDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TGigaTrackerDigi

//______________________________________________________________________________
void TGigaTrackerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TGigaTrackerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TGigaTrackerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TGigaTrackerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TGigaTrackerEvent(void *p) {
      return  p ? new(p) ::TGigaTrackerEvent : new ::TGigaTrackerEvent;
   }
   static void *newArray_TGigaTrackerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TGigaTrackerEvent[nElements] : new ::TGigaTrackerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TGigaTrackerEvent(void *p) {
      delete ((::TGigaTrackerEvent*)p);
   }
   static void deleteArray_TGigaTrackerEvent(void *p) {
      delete [] ((::TGigaTrackerEvent*)p);
   }
   static void destruct_TGigaTrackerEvent(void *p) {
      typedef ::TGigaTrackerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TGigaTrackerEvent

//______________________________________________________________________________
void TGigaTrackerHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TGigaTrackerHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TGigaTrackerHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TGigaTrackerHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TGigaTrackerHit(void *p) {
      return  p ? new(p) ::TGigaTrackerHit : new ::TGigaTrackerHit;
   }
   static void *newArray_TGigaTrackerHit(Long_t nElements, void *p) {
      return p ? new(p) ::TGigaTrackerHit[nElements] : new ::TGigaTrackerHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TGigaTrackerHit(void *p) {
      delete ((::TGigaTrackerHit*)p);
   }
   static void deleteArray_TGigaTrackerHit(void *p) {
      delete [] ((::TGigaTrackerHit*)p);
   }
   static void destruct_TGigaTrackerHit(void *p) {
      typedef ::TGigaTrackerHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TGigaTrackerHit

//______________________________________________________________________________
void TGigaTrackerSpecialTriggerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TGigaTrackerSpecialTriggerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TGigaTrackerSpecialTriggerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TGigaTrackerSpecialTriggerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TGigaTrackerSpecialTriggerEvent(void *p) {
      return  p ? new(p) ::TGigaTrackerSpecialTriggerEvent : new ::TGigaTrackerSpecialTriggerEvent;
   }
   static void *newArray_TGigaTrackerSpecialTriggerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TGigaTrackerSpecialTriggerEvent[nElements] : new ::TGigaTrackerSpecialTriggerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TGigaTrackerSpecialTriggerEvent(void *p) {
      delete ((::TGigaTrackerSpecialTriggerEvent*)p);
   }
   static void deleteArray_TGigaTrackerSpecialTriggerEvent(void *p) {
      delete [] ((::TGigaTrackerSpecialTriggerEvent*)p);
   }
   static void destruct_TGigaTrackerSpecialTriggerEvent(void *p) {
      typedef ::TGigaTrackerSpecialTriggerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TGigaTrackerSpecialTriggerEvent

//______________________________________________________________________________
void TRecoGigaTrackerCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoGigaTrackerCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoGigaTrackerCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoGigaTrackerCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoGigaTrackerCandidate(void *p) {
      return  p ? new(p) ::TRecoGigaTrackerCandidate : new ::TRecoGigaTrackerCandidate;
   }
   static void *newArray_TRecoGigaTrackerCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoGigaTrackerCandidate[nElements] : new ::TRecoGigaTrackerCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoGigaTrackerCandidate(void *p) {
      delete ((::TRecoGigaTrackerCandidate*)p);
   }
   static void deleteArray_TRecoGigaTrackerCandidate(void *p) {
      delete [] ((::TRecoGigaTrackerCandidate*)p);
   }
   static void destruct_TRecoGigaTrackerCandidate(void *p) {
      typedef ::TRecoGigaTrackerCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoGigaTrackerCandidate

//______________________________________________________________________________
void TRecoGigaTrackerHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoGigaTrackerHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoGigaTrackerHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoGigaTrackerHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoGigaTrackerHit(void *p) {
      return  p ? new(p) ::TRecoGigaTrackerHit : new ::TRecoGigaTrackerHit;
   }
   static void *newArray_TRecoGigaTrackerHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoGigaTrackerHit[nElements] : new ::TRecoGigaTrackerHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoGigaTrackerHit(void *p) {
      delete ((::TRecoGigaTrackerHit*)p);
   }
   static void deleteArray_TRecoGigaTrackerHit(void *p) {
      delete [] ((::TRecoGigaTrackerHit*)p);
   }
   static void destruct_TRecoGigaTrackerHit(void *p) {
      typedef ::TRecoGigaTrackerHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoGigaTrackerHit

//______________________________________________________________________________
void TRecoGigaTrackerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoGigaTrackerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoGigaTrackerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoGigaTrackerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoGigaTrackerEvent(void *p) {
      return  p ? new(p) ::TRecoGigaTrackerEvent : new ::TRecoGigaTrackerEvent;
   }
   static void *newArray_TRecoGigaTrackerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoGigaTrackerEvent[nElements] : new ::TRecoGigaTrackerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoGigaTrackerEvent(void *p) {
      delete ((::TRecoGigaTrackerEvent*)p);
   }
   static void deleteArray_TRecoGigaTrackerEvent(void *p) {
      delete [] ((::TRecoGigaTrackerEvent*)p);
   }
   static void destruct_TRecoGigaTrackerEvent(void *p) {
      typedef ::TRecoGigaTrackerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoGigaTrackerEvent

namespace {
  void TriggerDictionaryInitialization_libGigaTrackerPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/GigaTracker/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libGigaTrackerPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class GigaTrackerChannelID;
class TDigiGigaTrackerError;
class TGigaTrackerDigi;
class TGigaTrackerEvent;
class TGigaTrackerHit;
class TGigaTrackerSpecialTriggerEvent;
class TRecoGigaTrackerCandidate;
class __attribute__((annotate("$clingAutoload$TRecoGigaTrackerHit.hh")))  TRecoGigaTrackerHit;
class TRecoGigaTrackerEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libGigaTrackerPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// --------------------------------------------------------------
// History:
//
// Bob Velghe (bob.velghe@cern.ch) 2014-11-13
//  - Add UID field
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-26
//
// --------------------------------------------------------------
#ifndef GigaTrackerChannelID_H
#define GigaTrackerChannelID_H
#include "Rtypes.h"
#include "TVector3.h"

class GigaTrackerChannelID {
public:
  struct chIDDecoded {  Int_t fStationNo, fChipPixelID; UInt_t fChipID; };

  GigaTrackerChannelID();
  GigaTrackerChannelID(const GigaTrackerChannelID &);
  virtual ~GigaTrackerChannelID();
  void Clear(Option_t* = "");
  Double_t GetPixelXPosition();
  Double_t GetPixelYPosition();
  TVector3 GetRawPosition();
  static TVector3 GetRawPosition(Int_t stationNo, Double_t pixelX, Double_t pixelY);
  
public:
  Int_t                GetStationNo() const;
  void                 SetStationNo(Int_t value);
  
  Int_t                GetPixelID() const;
  void                 SetPixelID(Int_t value);
  
  Int_t                GetChipID() const;
  void                 SetChipID(UInt_t value)           {fChipID = value;        };

  UInt_t               GetqChipID();
  
  Int_t                GetChipPixelID() const           {return fChipPixelID;    };
  void                 SetChipPixelID(Int_t value)      {fChipPixelID = value;   };
  
  static chIDDecoded   DecodeChannelID_Static(Long_t);
  void                 DecodeChannelID(Long_t);
  UInt_t               EncodeChannelID();

  UInt_t               GetColumn();
  UInt_t               GetRow();
  static UInt_t        GetColumn(Int_t ChipID, Int_t ChipPixelID);
  static UInt_t        GetRow(Int_t ChipID, Int_t ChipPixelID);


private:
  Int_t fStationNo;
  Int_t fChipPixelID; 

  UInt_t fChipID;

  ClassDef(GigaTrackerChannelID,1);
};
#endif
#ifndef TDigiGigaTrackerError_H
#define TDigiGigaTrackerError_H
#include "TDigiVError.hh"

class TDigiGigaTrackerError : public TDigiVError {

public:

  //  TDigiGigaTrackerError();
  // ~TDigiGigaTrackerError(){};



public:
  
  static int GetErrorType( std::string );
  static std::string GetErrorName( int );


private:

  enum error_types {
    e_GigaTrackerDAQBoardTimeStamp_DataStatus = 1,
    e_GigaTrackerDAQBoardTimeStamp_ErrorChip,
    e_GigaTrackerDAQBoardTimeStamp_ErrorFlag,
    e_GigaTrackerDAQBoardTimeStamp_ErrorIncFcL1,
    e_GigaTrackerDAQBoardTimeStamp_ErrorqChip,
    e_GigaTrackerDAQBoardTimeStamp_ErrorStation,
    e_GigaTrackerDAQBoardTimeStamp_HitArbiter,
    e_GigaTrackerDAQBoardTimeStamp_PixAddress,
    e_GigaTrackerDAQBoardTimeStamp_PUAddress,
    e_GigaTrackerDAQBoardTrailerOne_ErrorFlag,
    e_GigaTrackerDAQBoardTrailerOne_ErrorIncNHitLength,
    e_GigaTrackerDAQBoardTrailerTwo_Flag,
    e_GigaTrackerNa62HeaderL0_ErrorFlag,
    e_GigaTrackerNa62HeaderL1_Flag,
    e_GigaTrackerNa62HeaderL1_SubId,
    e_GTKRawDecoder_DuplicatedHits,
    e_GTKRawDecoder_ErrorIncNTrigg,
    e_GTKRawDecoder_ErrorIncTSHitHeader,
    e_GTKRawDecoder_EvtNb,
    e_GTKRawDecoder_FCLSB,
    e_GTKRawDecoder_IncChip,
    e_GTKRawDecoder_Limiter,
    e_GTKRawDecoder_SubId,
    e_GTKRawDecoder_Trigger,
    e_GTKRawDecoder_TS,
    e_GTKRawDecoder_MissingChip,
    e_GigaTrackerDAQBoardTrailerOne_Chip,
    e_GigaTrackerDAQBoardTrailerTwo_Chip,
    e_GigaTrackerDAQBoardTrailerThree_Chip
  };

  ClassDef(TDigiGigaTrackerError,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// --------------------------------------------------------------
#ifndef TGigaTrackerDigi_H
#define TGigaTrackerDigi_H

#include "TDCVHit.hh"
#include "GigaTrackerChannelID.hh"
#include "TVector3.h"

class TGigaTrackerDigi : public TDCVHit, public GigaTrackerChannelID {

public:
  
  TGigaTrackerDigi();
  ~TGigaTrackerDigi(){}
  
  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); //must be here because of TDCVHit
  void  DecodeChannelID(); //must be here because of TDCVHit

  Int_t GetStationID() { return GetStationNo(); } //must be here because of TDCVHit


  UInt_t         GetSourceId()                              { return fSourceId;          };
  void           SetSourceId(UInt_t value)                  { fSourceId = value;         };

  UInt_t         GetFrameCounter()                          { return fFrameCounter;      };
  void           SetFrameCounter(UInt_t value)              { fFrameCounter = value;     };

  UInt_t         GetPixelAddress()                          { return fPixelAddress;      };
  void           SetPixelAddress(UInt_t value)              { fPixelAddress = value;     };

  UInt_t         GetHitArbiterAddress()                     { return fHitArbiterAddress; };
  void           SetHitArbiterAddress(UInt_t value)         { fHitArbiterAddress = value;};

  UInt_t         GetPileUpAddress()                         { return fPileUpAddress;     };
  void           SetPileUpAddress(UInt_t value)             { fPileUpAddress = value;    };

  Bool_t         GetIsPileUp()                              { return fIsPileUp;     };
  void           SetIsPileUp(Bool_t value)                  { fIsPileUp = value;    };

  UInt_t         GetLeadingSelector()                       { return fLeadingSelector;   };
  void           SetLeadingSelector(UInt_t value)           { fLeadingSelector = value;  };

  UInt_t         GetLeadingCoarse()                         { return fLeadingCoarse;     };
  void           SetLeadingCoarse(UInt_t value)             { fLeadingCoarse = value;    };

  UInt_t         GetLeadingFine()                           { return fLeadingFine;       };
  void           SetLeadingFine(UInt_t value)               { fLeadingFine = value;      };

  UInt_t         GetTotSelector()                           { return fTotSelector;       };
  void           SetTotSelector(UInt_t value)               { fTotSelector = value;      };

  UInt_t         GetTotCoarse()                             { return fTotCoarse;         };
  void           SetTotCoarse(UInt_t value)                 { fTotCoarse = value;        };

  UInt_t         GetTotFine()                               { return fTotFine;           };
  void           SetTotFine(UInt_t value)                   { fTotFine = value;          };

  Float_t        GetDelay()                                 { return fDelay;             };
  void           SetDelay(Float_t value)                    { fDelay = value;            };

  Float_t        GetAbsLeadingEdge()                        { return fAbsLeadingEdge;    };
  void           SetAbsLeadingEdge(Float_t value)           { fAbsLeadingEdge = value;   };

  Float_t        GetAbsTrailingEdge()                       { return fAbsTrailingEdge;   };
  void           SetAbsTrailingEdge(Float_t value)          { fAbsTrailingEdge = value;  };

  Double_t       GetTime()                                  { return fTime;              };
  void           SetTime(Double_t value)                    { fTime = value;             };



private:

  Float_t fDelay;
  Float_t fAbsLeadingEdge;
  Float_t fAbsTrailingEdge;
  UInt_t  fSourceId;
  UInt_t  fFrameCounter;
  
  UInt_t  fPixelAddress;
  UInt_t  fHitArbiterAddress;
  UInt_t  fPileUpAddress;
  
  UInt_t  fLeadingSelector;
  UInt_t  fLeadingCoarse;
  UInt_t  fLeadingFine;
  
  UInt_t  fTotSelector;
  UInt_t  fTotCoarse;
  UInt_t  fTotFine;

  Double_t fTime;

  // This member is last in the list: it has been introduced in v2 of the class
  Bool_t fIsPileUp;
  
  ClassDef(TGigaTrackerDigi,2);

};
#endif
#ifndef TGigaTrackerDigiEvent_HH_
#define TGigaTrackerDigiEvent_HH_
#include "TDCEvent.hh"

class TGigaTrackerDigiEvent : public TDCEvent
{
public:
  TGigaTrackerDigiEvent();
  explicit TGigaTrackerDigiEvent(TClass *);
  virtual ~TGigaTrackerDigiEvent();
  void SetHitNbFromPrevL0(int gtk, int chip, int half, int n);
  int GetHitNbFromPrevL0(int gtk, int chip, int half = -1);
  void Clear(Option_t* = "");

private:
  TGigaTrackerDigiEvent(const TGigaTrackerDigiEvent &);
  TGigaTrackerDigiEvent & operator=(const TGigaTrackerDigiEvent &);
  int fHitNbFromPrevL0[3][10][2];

protected:
};



#endif//~TGigaTrackerDigiEvent_HH_

// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TGigaTrackerEvent_H
#define TGigaTrackerEvent_H

#include "TDetectorVEvent.hh"

class TGigaTrackerEvent : public TDetectorVEvent {

    public:
  
        TGigaTrackerEvent();
        ~TGigaTrackerEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TGigaTrackerEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TGigaTrackerHit_H
#define TGigaTrackerHit_H

#include "TDetectorVHit.hh"
#include "GigaTrackerChannelID.hh"

class TGigaTrackerHit : public TDetectorVHit, public GigaTrackerChannelID  {
//class TGigaTrackerHit : public TDCVHit, public GigaTrackerChannelID  {

    public:

        TGigaTrackerHit();
        ~TGigaTrackerHit(){};

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void  DecodeChannelID();
        Int_t GetStationID() { return GetStationNo(); }

    public:

    protected:

        ClassDef(TGigaTrackerHit,1);
};
#endif
#ifndef TGigaTrackerSpecialTriggerEvent_HH_
#define TGigaTrackerSpecialTriggerEvent_HH_
#include "TSpecialTriggerEvent.hh"
#include "TClass.h"

class TGigaTrackerSpecialTriggerEvent: public TSpecialTriggerEvent
{
public:
  TGigaTrackerSpecialTriggerEvent();
  explicit TGigaTrackerSpecialTriggerEvent(TClass* );
  virtual ~TGigaTrackerSpecialTriggerEvent();

  UInt_t GetNHits(int gtk, int chip, int half = -1);  
  void SetNHits(int gtk, int chip, int half, UInt_t n);

  UInt_t GetFW(int gtk, int chip, int half = -1);  
  void SetFW(int gtk, int chip, int half,  UInt_t n);

  void Clear(Option_t* t = "");

private:
  TGigaTrackerSpecialTriggerEvent(const TGigaTrackerSpecialTriggerEvent &);
  TGigaTrackerSpecialTriggerEvent & operator=(const TGigaTrackerSpecialTriggerEvent &);

  UInt_t fNHits[3][10][2];
  UInt_t fFW[3][10][2];

  ClassDef(TGigaTrackerSpecialTriggerEvent,1);
};



#endif//~TGigaTrackerSpecialTriggerEvent_HH_

// --------------------------------------------------------------
// History:
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-05-04
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoGigaTrackerCandidate_H
#define TRecoGigaTrackerCandidate_H

#include "TRecoVCandidate.hh"
#include "TLorentzVector.h"

class TRecoGigaTrackerCandidate : public TRecoVCandidate {

public:
  
  TRecoGigaTrackerCandidate();
  TRecoGigaTrackerCandidate(const TRecoGigaTrackerCandidate &);
  ~TRecoGigaTrackerCandidate(){};
  
  void Clear(Option_t* = "");

  TVector3       GetMomentum(){                                    return fMomentum;                 }
  void           SetMomentum(TVector3 value){                      fMomentum = value;                }

  Double_t       GetTimeError(){                                   return fTimeError;                }
  void           SetTimeError(Double_t value){                     fTimeError = value;               }

  Double_t       GetTimeStation(Int_t StationNo);
  void           SetTimeStation(Int_t StationNo,Double_t value);

  TVector3       GetPosition(Int_t StationNo);
  void           SetPosition(Int_t StationNo,TVector3 value);

  Double_t       GetChi2X(){                                       return fChi2X;                    }
  void           SetChi2X(Double_t value){                         fChi2X = value;                   }

  Double_t       GetChi2Y(){                                       return fChi2Y;                    }
  void           SetChi2Y(Double_t value){                         fChi2Y = value;                   }

  Double_t       GetChi2Time(){                                    return fChi2Time;                 }
  void           SetChi2Time(Double_t value){                      fChi2Time = value;                }

  Double_t       GetChi2(){                                        return fChi2;                     }
  void           SetChi2(Double_t value){                          fChi2 = value;                    }

  Int_t          GetType(){                                        return fType;                     }
  void           SetType(Int_t value){                             fType = value;                    }
 
  Double_t       GetCovariance(Int_t i, Int_t j)               { return fCovariance[i][j];           }
  void           SetCovariance(Int_t i, Int_t j, Double_t val) { fCovariance[i][j]=val;              }

public:
  
  
private:
  
  TVector3 fMomentum;
  Double_t fTimeError;
  Double_t fTimeStation[3];
  TVector3 fPosition[3];

  Int_t fType;

  Double_t fChi2X;
  Double_t fChi2Y;
  Double_t fChi2Time;
  Double_t fChi2;

  Double_t fCovariance[5][5];          ///< Covariance matrix provided by the fit (slopes, positions, momentum -> thetaXY, XY, P)
                                       ///< s2(thetaX)  s(thetaX)s(thetaY)  s(thetaX)s(X)  s(thetaX)s(Y)  s(thetaX)s(1/P)
                                       ///<                s2(thetaY)       s(thetaY)s(X)  s(thetaY)s(Y)  s(thetaY)s(1/P)
                                       ///<                                     s2(X)        s(X)s(Y)       s(X)s(1/P)
                                       ///<                                                   s2(Y)         s(Y)s(1/P)
                                       ///<                                                                   s2(1/P)

  // From v2 fTime1,2,3 and corresponding functions are removed
  // From v3 fTime and corresponding functions are removed, fCovariance Matrix is added
  ClassDef(TRecoGigaTrackerCandidate,3);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoGigaTrackerEvent_H
#define TRecoGigaTrackerEvent_H

#include "TRecoVEvent.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoGigaTrackerHit.hh"

class TRecoGigaTrackerEvent : public TRecoVEvent {

    public:

        TRecoGigaTrackerEvent();
        TRecoGigaTrackerEvent(const TRecoGigaTrackerEvent &);
        ~TRecoGigaTrackerEvent();

        void SetHitNbFromPrevL0(int gtk, int chip, int half, int n);
        int GetHitNbFromPrevL0(int gtk, int chip, int half = -1);

        void Clear(Option_t* = "");

    private:

        int fHitNbFromPrevL0[3][10][2];
        ClassDef(TRecoGigaTrackerEvent,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoGigaTrackerHit_H
#define TRecoGigaTrackerHit_H

#include "TRecoVHit.hh"
#include "GigaTrackerChannelID.hh"

class TRecoGigaTrackerHit : public TRecoVHit, public GigaTrackerChannelID {
  
public:
  
  TRecoGigaTrackerHit();
  TRecoGigaTrackerHit(const TRecoGigaTrackerHit &);
  ~TRecoGigaTrackerHit(){};

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  
  Double_t             GetToT()                         { return fToT;            };
  void                 SetToT(Double_t value)           { fToT = value;           };

  Double_t             GetRawTime()                     { return fRawTime;        };
  void                 SetRawTime(Double_t value)       { fRawTime = value;       };

  Bool_t               GetIsPileUpHit()                 { return fIsPileUpHit;    };
  void                 SetIsPileUpHit(Bool_t value)     { fIsPileUpHit = value;   };
private:
  
  Double_t fToT;
  Double_t fRawTime;

  // This member is last in the list: it has been introduced in v2 of the class
  Bool_t fIsPileUpHit;
  
  ClassDef(TRecoGigaTrackerHit,2);

};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"GigaTrackerChannelID", payloadCode, "@",
"TDigiGigaTrackerError", payloadCode, "@",
"TGigaTrackerDigi", payloadCode, "@",
"TGigaTrackerEvent", payloadCode, "@",
"TGigaTrackerHit", payloadCode, "@",
"TGigaTrackerSpecialTriggerEvent", payloadCode, "@",
"TRecoGigaTrackerCandidate", payloadCode, "@",
"TRecoGigaTrackerEvent", payloadCode, "@",
"TRecoGigaTrackerHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libGigaTrackerPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libGigaTrackerPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libGigaTrackerPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libGigaTrackerPersistency() {
  TriggerDictionaryInitialization_libGigaTrackerPersistency_Impl();
}
