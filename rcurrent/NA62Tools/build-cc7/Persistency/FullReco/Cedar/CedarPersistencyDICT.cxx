// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME CedarPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/CedarChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/TCedarDigi.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/TCedarEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/TCedarHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/TCedarSpecialTriggerEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/TRecoCedarCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/TRecoCedarEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include/TRecoCedarHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_CedarChannelID(void *p = 0);
   static void *newArray_CedarChannelID(Long_t size, void *p);
   static void delete_CedarChannelID(void *p);
   static void deleteArray_CedarChannelID(void *p);
   static void destruct_CedarChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::CedarChannelID*)
   {
      ::CedarChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::CedarChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("CedarChannelID", ::CedarChannelID::Class_Version(), "", 18,
                  typeid(::CedarChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::CedarChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::CedarChannelID) );
      instance.SetNew(&new_CedarChannelID);
      instance.SetNewArray(&newArray_CedarChannelID);
      instance.SetDelete(&delete_CedarChannelID);
      instance.SetDeleteArray(&deleteArray_CedarChannelID);
      instance.SetDestructor(&destruct_CedarChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::CedarChannelID*)
   {
      return GenerateInitInstanceLocal((::CedarChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::CedarChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCedarDigi(void *p = 0);
   static void *newArray_TCedarDigi(Long_t size, void *p);
   static void delete_TCedarDigi(void *p);
   static void deleteArray_TCedarDigi(void *p);
   static void destruct_TCedarDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCedarDigi*)
   {
      ::TCedarDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCedarDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCedarDigi", ::TCedarDigi::Class_Version(), "", 62,
                  typeid(::TCedarDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCedarDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TCedarDigi) );
      instance.SetNew(&new_TCedarDigi);
      instance.SetNewArray(&newArray_TCedarDigi);
      instance.SetDelete(&delete_TCedarDigi);
      instance.SetDeleteArray(&deleteArray_TCedarDigi);
      instance.SetDestructor(&destruct_TCedarDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCedarDigi*)
   {
      return GenerateInitInstanceLocal((::TCedarDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TCedarDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCedarEvent(void *p = 0);
   static void *newArray_TCedarEvent(Long_t size, void *p);
   static void delete_TCedarEvent(void *p);
   static void deleteArray_TCedarEvent(void *p);
   static void destruct_TCedarEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCedarEvent*)
   {
      ::TCedarEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCedarEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCedarEvent", ::TCedarEvent::Class_Version(), "", 100,
                  typeid(::TCedarEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCedarEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TCedarEvent) );
      instance.SetNew(&new_TCedarEvent);
      instance.SetNewArray(&newArray_TCedarEvent);
      instance.SetDelete(&delete_TCedarEvent);
      instance.SetDeleteArray(&deleteArray_TCedarEvent);
      instance.SetDestructor(&destruct_TCedarEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCedarEvent*)
   {
      return GenerateInitInstanceLocal((::TCedarEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TCedarEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCedarHit(void *p = 0);
   static void *newArray_TCedarHit(Long_t size, void *p);
   static void delete_TCedarHit(void *p);
   static void deleteArray_TCedarHit(void *p);
   static void destruct_TCedarHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCedarHit*)
   {
      ::TCedarHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCedarHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCedarHit", ::TCedarHit::Class_Version(), "", 128,
                  typeid(::TCedarHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCedarHit::Dictionary, isa_proxy, 4,
                  sizeof(::TCedarHit) );
      instance.SetNew(&new_TCedarHit);
      instance.SetNewArray(&newArray_TCedarHit);
      instance.SetDelete(&delete_TCedarHit);
      instance.SetDeleteArray(&deleteArray_TCedarHit);
      instance.SetDestructor(&destruct_TCedarHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCedarHit*)
   {
      return GenerateInitInstanceLocal((::TCedarHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TCedarHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_CedarDIMInfo(void *p = 0);
   static void *newArray_CedarDIMInfo(Long_t size, void *p);
   static void delete_CedarDIMInfo(void *p);
   static void deleteArray_CedarDIMInfo(void *p);
   static void destruct_CedarDIMInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::CedarDIMInfo*)
   {
      ::CedarDIMInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::CedarDIMInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("CedarDIMInfo", ::CedarDIMInfo::Class_Version(), "", 167,
                  typeid(::CedarDIMInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::CedarDIMInfo::Dictionary, isa_proxy, 4,
                  sizeof(::CedarDIMInfo) );
      instance.SetNew(&new_CedarDIMInfo);
      instance.SetNewArray(&newArray_CedarDIMInfo);
      instance.SetDelete(&delete_CedarDIMInfo);
      instance.SetDeleteArray(&deleteArray_CedarDIMInfo);
      instance.SetDestructor(&destruct_CedarDIMInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::CedarDIMInfo*)
   {
      return GenerateInitInstanceLocal((::CedarDIMInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::CedarDIMInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TCedarSpecialTriggerEvent(void *p = 0);
   static void *newArray_TCedarSpecialTriggerEvent(Long_t size, void *p);
   static void delete_TCedarSpecialTriggerEvent(void *p);
   static void deleteArray_TCedarSpecialTriggerEvent(void *p);
   static void destruct_TCedarSpecialTriggerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TCedarSpecialTriggerEvent*)
   {
      ::TCedarSpecialTriggerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TCedarSpecialTriggerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TCedarSpecialTriggerEvent", ::TCedarSpecialTriggerEvent::Class_Version(), "", 215,
                  typeid(::TCedarSpecialTriggerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TCedarSpecialTriggerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TCedarSpecialTriggerEvent) );
      instance.SetNew(&new_TCedarSpecialTriggerEvent);
      instance.SetNewArray(&newArray_TCedarSpecialTriggerEvent);
      instance.SetDelete(&delete_TCedarSpecialTriggerEvent);
      instance.SetDeleteArray(&deleteArray_TCedarSpecialTriggerEvent);
      instance.SetDestructor(&destruct_TCedarSpecialTriggerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TCedarSpecialTriggerEvent*)
   {
      return GenerateInitInstanceLocal((::TCedarSpecialTriggerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TCedarSpecialTriggerEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCedarCandidate(void *p = 0);
   static void *newArray_TRecoCedarCandidate(Long_t size, void *p);
   static void delete_TRecoCedarCandidate(void *p);
   static void deleteArray_TRecoCedarCandidate(void *p);
   static void destruct_TRecoCedarCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCedarCandidate*)
   {
      ::TRecoCedarCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCedarCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCedarCandidate", ::TRecoCedarCandidate::Class_Version(), "", 286,
                  typeid(::TRecoCedarCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCedarCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCedarCandidate) );
      instance.SetNew(&new_TRecoCedarCandidate);
      instance.SetNewArray(&newArray_TRecoCedarCandidate);
      instance.SetDelete(&delete_TRecoCedarCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoCedarCandidate);
      instance.SetDestructor(&destruct_TRecoCedarCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCedarCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoCedarCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoCedarCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCedarHit(void *p = 0);
   static void *newArray_TRecoCedarHit(Long_t size, void *p);
   static void delete_TRecoCedarHit(void *p);
   static void deleteArray_TRecoCedarHit(void *p);
   static void destruct_TRecoCedarHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCedarHit*)
   {
      ::TRecoCedarHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCedarHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCedarHit", ::TRecoCedarHit::Class_Version(), "TRecoCedarHit.hh", 14,
                  typeid(::TRecoCedarHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCedarHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCedarHit) );
      instance.SetNew(&new_TRecoCedarHit);
      instance.SetNewArray(&newArray_TRecoCedarHit);
      instance.SetDelete(&delete_TRecoCedarHit);
      instance.SetDeleteArray(&deleteArray_TRecoCedarHit);
      instance.SetDestructor(&destruct_TRecoCedarHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCedarHit*)
   {
      return GenerateInitInstanceLocal((::TRecoCedarHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoCedarHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoCedarEvent(void *p = 0);
   static void *newArray_TRecoCedarEvent(Long_t size, void *p);
   static void delete_TRecoCedarEvent(void *p);
   static void deleteArray_TRecoCedarEvent(void *p);
   static void destruct_TRecoCedarEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoCedarEvent*)
   {
      ::TRecoCedarEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoCedarEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoCedarEvent", ::TRecoCedarEvent::Class_Version(), "", 334,
                  typeid(::TRecoCedarEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoCedarEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoCedarEvent) );
      instance.SetNew(&new_TRecoCedarEvent);
      instance.SetNewArray(&newArray_TRecoCedarEvent);
      instance.SetDelete(&delete_TRecoCedarEvent);
      instance.SetDeleteArray(&deleteArray_TRecoCedarEvent);
      instance.SetDestructor(&destruct_TRecoCedarEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoCedarEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoCedarEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoCedarEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr CedarChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *CedarChannelID::Class_Name()
{
   return "CedarChannelID";
}

//______________________________________________________________________________
const char *CedarChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CedarChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int CedarChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CedarChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *CedarChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CedarChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *CedarChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CedarChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCedarDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCedarDigi::Class_Name()
{
   return "TCedarDigi";
}

//______________________________________________________________________________
const char *TCedarDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCedarDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCedarDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCedarDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCedarEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCedarEvent::Class_Name()
{
   return "TCedarEvent";
}

//______________________________________________________________________________
const char *TCedarEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCedarEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCedarEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCedarEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCedarHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCedarHit::Class_Name()
{
   return "TCedarHit";
}

//______________________________________________________________________________
const char *TCedarHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCedarHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCedarHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCedarHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr CedarDIMInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *CedarDIMInfo::Class_Name()
{
   return "CedarDIMInfo";
}

//______________________________________________________________________________
const char *CedarDIMInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CedarDIMInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int CedarDIMInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CedarDIMInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *CedarDIMInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CedarDIMInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *CedarDIMInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CedarDIMInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TCedarSpecialTriggerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TCedarSpecialTriggerEvent::Class_Name()
{
   return "TCedarSpecialTriggerEvent";
}

//______________________________________________________________________________
const char *TCedarSpecialTriggerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarSpecialTriggerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TCedarSpecialTriggerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TCedarSpecialTriggerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TCedarSpecialTriggerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarSpecialTriggerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TCedarSpecialTriggerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TCedarSpecialTriggerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCedarCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCedarCandidate::Class_Name()
{
   return "TRecoCedarCandidate";
}

//______________________________________________________________________________
const char *TRecoCedarCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCedarCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCedarCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCedarCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCedarHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCedarHit::Class_Name()
{
   return "TRecoCedarHit";
}

//______________________________________________________________________________
const char *TRecoCedarHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCedarHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCedarHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCedarHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoCedarEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoCedarEvent::Class_Name()
{
   return "TRecoCedarEvent";
}

//______________________________________________________________________________
const char *TRecoCedarEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoCedarEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoCedarEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoCedarEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoCedarEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void CedarChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class CedarChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(CedarChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(CedarChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_CedarChannelID(void *p) {
      return  p ? new(p) ::CedarChannelID : new ::CedarChannelID;
   }
   static void *newArray_CedarChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::CedarChannelID[nElements] : new ::CedarChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_CedarChannelID(void *p) {
      delete ((::CedarChannelID*)p);
   }
   static void deleteArray_CedarChannelID(void *p) {
      delete [] ((::CedarChannelID*)p);
   }
   static void destruct_CedarChannelID(void *p) {
      typedef ::CedarChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::CedarChannelID

//______________________________________________________________________________
void TCedarDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCedarDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCedarDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCedarDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCedarDigi(void *p) {
      return  p ? new(p) ::TCedarDigi : new ::TCedarDigi;
   }
   static void *newArray_TCedarDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TCedarDigi[nElements] : new ::TCedarDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCedarDigi(void *p) {
      delete ((::TCedarDigi*)p);
   }
   static void deleteArray_TCedarDigi(void *p) {
      delete [] ((::TCedarDigi*)p);
   }
   static void destruct_TCedarDigi(void *p) {
      typedef ::TCedarDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCedarDigi

//______________________________________________________________________________
void TCedarEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCedarEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCedarEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCedarEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCedarEvent(void *p) {
      return  p ? new(p) ::TCedarEvent : new ::TCedarEvent;
   }
   static void *newArray_TCedarEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TCedarEvent[nElements] : new ::TCedarEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCedarEvent(void *p) {
      delete ((::TCedarEvent*)p);
   }
   static void deleteArray_TCedarEvent(void *p) {
      delete [] ((::TCedarEvent*)p);
   }
   static void destruct_TCedarEvent(void *p) {
      typedef ::TCedarEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCedarEvent

//______________________________________________________________________________
void TCedarHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCedarHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCedarHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCedarHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCedarHit(void *p) {
      return  p ? new(p) ::TCedarHit : new ::TCedarHit;
   }
   static void *newArray_TCedarHit(Long_t nElements, void *p) {
      return p ? new(p) ::TCedarHit[nElements] : new ::TCedarHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCedarHit(void *p) {
      delete ((::TCedarHit*)p);
   }
   static void deleteArray_TCedarHit(void *p) {
      delete [] ((::TCedarHit*)p);
   }
   static void destruct_TCedarHit(void *p) {
      typedef ::TCedarHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCedarHit

//______________________________________________________________________________
void CedarDIMInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class CedarDIMInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(CedarDIMInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(CedarDIMInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_CedarDIMInfo(void *p) {
      return  p ? new(p) ::CedarDIMInfo : new ::CedarDIMInfo;
   }
   static void *newArray_CedarDIMInfo(Long_t nElements, void *p) {
      return p ? new(p) ::CedarDIMInfo[nElements] : new ::CedarDIMInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_CedarDIMInfo(void *p) {
      delete ((::CedarDIMInfo*)p);
   }
   static void deleteArray_CedarDIMInfo(void *p) {
      delete [] ((::CedarDIMInfo*)p);
   }
   static void destruct_CedarDIMInfo(void *p) {
      typedef ::CedarDIMInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::CedarDIMInfo

//______________________________________________________________________________
void TCedarSpecialTriggerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TCedarSpecialTriggerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TCedarSpecialTriggerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TCedarSpecialTriggerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TCedarSpecialTriggerEvent(void *p) {
      return  p ? new(p) ::TCedarSpecialTriggerEvent : new ::TCedarSpecialTriggerEvent;
   }
   static void *newArray_TCedarSpecialTriggerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TCedarSpecialTriggerEvent[nElements] : new ::TCedarSpecialTriggerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TCedarSpecialTriggerEvent(void *p) {
      delete ((::TCedarSpecialTriggerEvent*)p);
   }
   static void deleteArray_TCedarSpecialTriggerEvent(void *p) {
      delete [] ((::TCedarSpecialTriggerEvent*)p);
   }
   static void destruct_TCedarSpecialTriggerEvent(void *p) {
      typedef ::TCedarSpecialTriggerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TCedarSpecialTriggerEvent

//______________________________________________________________________________
void TRecoCedarCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCedarCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCedarCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCedarCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCedarCandidate(void *p) {
      return  p ? new(p) ::TRecoCedarCandidate : new ::TRecoCedarCandidate;
   }
   static void *newArray_TRecoCedarCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCedarCandidate[nElements] : new ::TRecoCedarCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCedarCandidate(void *p) {
      delete ((::TRecoCedarCandidate*)p);
   }
   static void deleteArray_TRecoCedarCandidate(void *p) {
      delete [] ((::TRecoCedarCandidate*)p);
   }
   static void destruct_TRecoCedarCandidate(void *p) {
      typedef ::TRecoCedarCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCedarCandidate

//______________________________________________________________________________
void TRecoCedarHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCedarHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCedarHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCedarHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCedarHit(void *p) {
      return  p ? new(p) ::TRecoCedarHit : new ::TRecoCedarHit;
   }
   static void *newArray_TRecoCedarHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCedarHit[nElements] : new ::TRecoCedarHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCedarHit(void *p) {
      delete ((::TRecoCedarHit*)p);
   }
   static void deleteArray_TRecoCedarHit(void *p) {
      delete [] ((::TRecoCedarHit*)p);
   }
   static void destruct_TRecoCedarHit(void *p) {
      typedef ::TRecoCedarHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCedarHit

//______________________________________________________________________________
void TRecoCedarEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoCedarEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoCedarEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoCedarEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoCedarEvent(void *p) {
      return  p ? new(p) ::TRecoCedarEvent : new ::TRecoCedarEvent;
   }
   static void *newArray_TRecoCedarEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoCedarEvent[nElements] : new ::TRecoCedarEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoCedarEvent(void *p) {
      delete ((::TRecoCedarEvent*)p);
   }
   static void deleteArray_TRecoCedarEvent(void *p) {
      delete [] ((::TRecoCedarEvent*)p);
   }
   static void destruct_TRecoCedarEvent(void *p) {
      typedef ::TRecoCedarEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoCedarEvent

namespace {
  void TriggerDictionaryInitialization_libCedarPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/Cedar/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libCedarPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class CedarChannelID;
class TCedarDigi;
class TCedarEvent;
class TCedarHit;
class CedarDIMInfo;
class TCedarSpecialTriggerEvent;
class TRecoCedarCandidate;
class __attribute__((annotate("$clingAutoload$TRecoCedarHit.hh")))  TRecoCedarHit;
class TRecoCedarEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libCedarPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-04-10
//
// ---------------------------------------------------------------

#ifndef CedarChannelID_H
#define CedarChannelID_H

#include "Rtypes.h"

class CedarChannelID {

public:
  struct chIDDecoded{ Int_t SectorID,RowID,ConeID;};

  CedarChannelID();
  virtual ~CedarChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  static chIDDecoded DecodeChannelID_Static(int ChannelID); // converts position ID into sector, row, cone IDs (Struct)
  void DecodeChannelID(int ChannelID);

  Int_t GetSectorID()          { return fSectorID; }
  void  SetSectorID(Int_t val) { fSectorID = val;  }
  Int_t GetRowID()             { return fRowID;    }
  void  SetRowID(Int_t val)    { fRowID = val;     }
  Int_t GetConeID()            { return fConeID;   }
  void  SetConeID(Int_t val)   { fConeID = val;    }

private:

  Int_t fSectorID;
  Int_t fRowID;
  Int_t fConeID;

  ClassDef(CedarChannelID,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-19
//
// ---------------------------------------------------------------

#ifndef TCedarDigi_H
#define TCedarDigi_H

#include "TDCVHit.hh"
#include "CedarChannelID.hh"

class TCedarDigi : public TDCVHit, public CedarChannelID {

public:

  TCedarDigi();
  explicit TCedarDigi(TVHit*);
  ~TCedarDigi() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return 0; }

  //  TCedarDigi(Int_t iCh) : TDCVHit(iCh) {}

  Int_t Compare(const TObject *obj) const;
  Bool_t IsSortable() const { return kTRUE; }

private:

  ClassDef(TCedarDigi,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TCedarEvent_H
#define TCedarEvent_H

#include "TDetectorVEvent.hh"

class TCedarEvent : public TDetectorVEvent {

public:

  TCedarEvent();
  ~TCedarEvent();

  void Clear(Option_t* = "");

private:

  ClassDef(TCedarEvent,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TCedarHit_H
#define TCedarHit_H

#include "TDetectorVHit.hh"
#include "CedarChannelID.hh"

class TCedarHit : public TDetectorVHit, public CedarChannelID {

public:

  TCedarHit();
  ~TCedarHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return 0; }

  void  SetPMType (Int_t type) { iPMType = type; }
  Int_t GetPMType()            { return iPMType; }

protected:
  ClassDef(TCedarHit,1);

private:
  Int_t iPMType;

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-24
//
// --------------------------------------------------------------
#ifndef TCedarSpecialTriggerEvent_H
#define TCedarSpecialTriggerEvent_H
#include "TClass.h"
#include "TSpecialTriggerEvent.hh"

struct CedarDIMInfoStruct;

class CedarDIMInfo : public TObject {
  public:
    CedarDIMInfo();
    ~CedarDIMInfo(){};
    void SetInfo(CedarDIMInfoStruct);
    void Clear(Option_t* = "");
    UInt_t  GetTimeStamp()          { return fTimeStamp;          }
    Float_t GetTempRear()           { return fTempRear;           }
    Float_t GetTempFront()          { return fTempFront;          }
    Float_t GetTempDiaph()          { return fTempDiaph;          }
    Float_t GetMotorPosX()          { return fMotorPosX;          }
    Float_t GetMotorPosY()          { return fMotorPosY;          }
    Float_t GetDiaphragmAperture()  { return fDiaphragmAperture;  }
    UInt_t  GetPressureState()      { return fPressureState;      }
    Float_t GetPressure()           { return fPressure;           }
    Float_t GetPressureSetPoint()   { return fPressureSetPoint;   }
    UInt_t  GetHVStatus()           { return fHVStatus;           }
    UInt_t  GetFEStatus()           { return fFEStatus;           }
    UInt_t  GetCedarStatus()        { return fCedarStatus;        }
    UInt_t  GetAlignStatus()        { return fAlignStatus;        }
    UInt_t  GetKTAGEnvStatus()      { return fKTAGEnvStatus;      }
    UInt_t  GetKTAGStatus()         { return fKTAGStatus;         }
    UInt_t  GetWienerStatus()       { return fWienerStatus;       }

    CedarDIMInfo& operator=(const CedarDIMInfo& obj);

  private:
    UInt_t  fTimeStamp;
    Float_t fTempRear;
    Float_t fTempFront;
    Float_t fTempDiaph;
    Float_t fMotorPosX;
    Float_t fMotorPosY;
    Float_t fDiaphragmAperture;
    UInt_t  fPressureState;
    Float_t fPressure;
    Float_t fPressureSetPoint;
    UInt_t  fHVStatus;
    UInt_t  fFEStatus;
    UInt_t  fCedarStatus;
    UInt_t  fAlignStatus;
    UInt_t  fKTAGEnvStatus;
    UInt_t  fKTAGStatus;
    UInt_t  fWienerStatus;

    ClassDef(CedarDIMInfo,1);
};

class TCedarSpecialTriggerEvent : public TSpecialTriggerEvent {

  public:
    TCedarSpecialTriggerEvent();
    explicit TCedarSpecialTriggerEvent(TClass *);
    ~TCedarSpecialTriggerEvent(){};
    void Clear(Option_t* = "");
    void SetDIMInfo(UInt_t * value);
    CedarDIMInfo GetDIMInfo()        { return fDIMInfo;                        }
    UInt_t  GetTimeStamp()           { return fDIMInfo.GetTimeStamp();         }
    Float_t GetTempRear()            { return fDIMInfo.GetTempRear();          }
    Float_t GetTempFront()           { return fDIMInfo.GetTempFront();         }
    Float_t GetTempDiaph()           { return fDIMInfo.GetTempDiaph();         }
    Float_t GetMotorPosX()           { return fDIMInfo.GetMotorPosX();         }
    Float_t GetMotorPosY()           { return fDIMInfo.GetMotorPosY();         }
    Float_t GetDiaphragmAperture()   { return fDIMInfo.GetDiaphragmAperture(); }
    UInt_t  GetPressureState()       { return fDIMInfo.GetPressureState();     }
    Float_t GetPressure()            { return fDIMInfo.GetPressure();          }
    Float_t GetPressureSetPoint()    { return fDIMInfo.GetPressureSetPoint();  }
    UInt_t  GetHVStatus()            { return fDIMInfo.GetHVStatus();          }
    UInt_t  GetFEStatus()            { return fDIMInfo.GetFEStatus();          }
    UInt_t  GetCedarStatus()         { return fDIMInfo.GetCedarStatus();       }
    UInt_t  GetAlignStatus()         { return fDIMInfo.GetAlignStatus();       }
    UInt_t  GetKTAGEnvStatus()       { return fDIMInfo.GetKTAGEnvStatus();     }
    UInt_t  GetKTAGStatus()          { return fDIMInfo.GetKTAGStatus();        }
    UInt_t  GetWienerStatus()        { return fDIMInfo.GetWienerStatus();      }

  private:

    CedarDIMInfo fDIMInfo;

    ClassDef(TCedarSpecialTriggerEvent,1);

};

// --- structure definitions [needed to extract values from buffer --- //

struct CedarDIMInfoStruct {
  uint32_t TimeStamp;           //unix time (s)
  uint32_t TempRear;            //(temperature in degrees C)*1000 [rear      sensor]
  uint32_t TempFront;           //(temperature in degrees C)*1000 [front     sensor]
  uint32_t TempDiaph;           //(temperature in degrees C)*1000 [diaphragm sensor]
  uint32_t MotorPosX;           //(x motor position in mm)*1000
  uint32_t MotorPosY;           //(y motor position in mm)*1000
  uint32_t DiaphragmAperture;   //(diaphragm aperture in mm)*1000
  uint32_t PressureState;       //pressure state (1: stable, 2: ramping up, 3: ramping down, 4: stabilising, 5: unknown, 6: error)
  uint32_t Pressure;            //(Cedar pressure in bar)*1000
  uint32_t PressureSetPoint;    //(Cedar pressure set point in bar)*1000
  uint32_t HVStatus;            //High Voltage Status (1: OK, 0: Error)
  uint32_t FEStatus;            //Front-End    Status (1: OK, 0: Error)
  uint32_t CedarStatus;         //Cedar        Status (1: OK, 0: Error)
  uint32_t AlignStatus;         //Alignment    Status (1: OK, 0: Error)
  uint32_t KTAGEnvStatus;       //Environment  Status (1: OK, 0: Error)
  uint32_t KTAGStatus;          //KTAG         Status (1: OK, 0: Error)
  uint32_t WienerStatus;        //Wiener       Status (1: OK, 0: Error)
};
// ------------------------------------------------------------------- //
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------

#ifndef TRecoCedarCandidate_H
#define TRecoCedarCandidate_H

#include "TRecoVCandidate.hh"

class TRecoCedarCandidate : public TRecoVCandidate {

public:

  TRecoCedarCandidate();
  ~TRecoCedarCandidate() {}

  void       Clear(Option_t* = "");
  void       UpdateTime();
  void       UpdateTime(Double_t);

  Int_t      GetNSectors()              { return fNSectors;  }
  void       SetNSectors(Double_t val)  { fNSectors = val;   }
  Bool_t     GetIsSelected()            { return fIsSelected;}
  void       SetIsSelected(Double_t val){ fIsSelected = val; }

  Double_t   GetDeltaTimeClosestCandidate()             { return fDeltaTimeClosestCandidate; }
  void       SetDeltaTimeClosestCandidate(Double_t val) { fDeltaTimeClosestCandidate = val;  }
  Double_t   GetNHitsClosestCandidate()                 { return fNHitsClosestCandidate;     }
  void       SetNHitsClosestCandidate(Double_t val)     { fNHitsClosestCandidate = val;      }

private:
  // fNHits and fTime already defined in TRecoVCandidate:
  // it is essential to use those ones.

  Int_t fNPMTs, fNSectors; // fNPMTs it's redundant, it should be removed!
  Bool_t fIsSelected;
  Double_t fDeltaTimeClosestCandidate, fNHitsClosestCandidate;

  ClassDef(TRecoCedarCandidate,1);
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------

#ifndef TRecoCedarEvent_H
#define TRecoCedarEvent_H

#include "TRecoVEvent.hh"
#include "TRecoCedarCandidate.hh"
#include "TRecoCedarHit.hh"

class TRecoCedarEvent : public TRecoVEvent {

public:

  TRecoCedarEvent();
  ~TRecoCedarEvent();

  void       Clear(Option_t* = "");

private:

  ClassDef(TRecoCedarEvent,1);
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------
 
#ifndef TRecoCedarHit_H
#define TRecoCedarHit_H

#include "TRecoVHit.hh"
#include "CedarChannelID.hh"

class TRecoCedarHit : public TRecoVHit, public CedarChannelID {

public:

  TRecoCedarHit();
  ~TRecoCedarHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  void SetWidth      (Double_t val) { fWidth  = val;       }
  void SetROChannelID(Int_t val)    { fROChannelID = val;  }

  Double_t GetWidth()               { return fWidth;       }
  Int_t    GetROChannelID()         { return fROChannelID; }
  
private:

  Double_t fWidth;
  Int_t    fROChannelID;

  ClassDef(TRecoCedarHit,1);
};

#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"CedarChannelID", payloadCode, "@",
"CedarDIMInfo", payloadCode, "@",
"TCedarDigi", payloadCode, "@",
"TCedarEvent", payloadCode, "@",
"TCedarHit", payloadCode, "@",
"TCedarSpecialTriggerEvent", payloadCode, "@",
"TRecoCedarCandidate", payloadCode, "@",
"TRecoCedarEvent", payloadCode, "@",
"TRecoCedarHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libCedarPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libCedarPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libCedarPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libCedarPersistency() {
  TriggerDictionaryInitialization_libCedarPersistency_Impl();
}
