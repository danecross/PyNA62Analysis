// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME LKrPersistencyDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/LKrChannelID.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TDigiLKrEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrDigi.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrMicroCellHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TRecoLKrCandidate.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TRecoLKrEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TRecoLKrHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_LKrChannelID(void *p = 0);
   static void *newArray_LKrChannelID(Long_t size, void *p);
   static void delete_LKrChannelID(void *p);
   static void deleteArray_LKrChannelID(void *p);
   static void destruct_LKrChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::LKrChannelID*)
   {
      ::LKrChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::LKrChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("LKrChannelID", ::LKrChannelID::Class_Version(), "", 25,
                  typeid(::LKrChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::LKrChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::LKrChannelID) );
      instance.SetNew(&new_LKrChannelID);
      instance.SetNewArray(&newArray_LKrChannelID);
      instance.SetDelete(&delete_LKrChannelID);
      instance.SetDeleteArray(&deleteArray_LKrChannelID);
      instance.SetDestructor(&destruct_LKrChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::LKrChannelID*)
   {
      return GenerateInitInstanceLocal((::LKrChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::LKrChannelID*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDigiLKrEvent(void *p = 0);
   static void *newArray_TDigiLKrEvent(Long_t size, void *p);
   static void delete_TDigiLKrEvent(void *p);
   static void deleteArray_TDigiLKrEvent(void *p);
   static void destruct_TDigiLKrEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDigiLKrEvent*)
   {
      ::TDigiLKrEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDigiLKrEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDigiLKrEvent", ::TDigiLKrEvent::Class_Version(), "", 69,
                  typeid(::TDigiLKrEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDigiLKrEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TDigiLKrEvent) );
      instance.SetNew(&new_TDigiLKrEvent);
      instance.SetNewArray(&newArray_TDigiLKrEvent);
      instance.SetDelete(&delete_TDigiLKrEvent);
      instance.SetDeleteArray(&deleteArray_TDigiLKrEvent);
      instance.SetDestructor(&destruct_TDigiLKrEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDigiLKrEvent*)
   {
      return GenerateInitInstanceLocal((::TDigiLKrEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TDigiLKrEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TLKrDigi(void *p = 0);
   static void *newArray_TLKrDigi(Long_t size, void *p);
   static void delete_TLKrDigi(void *p);
   static void deleteArray_TLKrDigi(void *p);
   static void destruct_TLKrDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TLKrDigi*)
   {
      ::TLKrDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TLKrDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TLKrDigi", ::TLKrDigi::Class_Version(), "", 104,
                  typeid(::TLKrDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TLKrDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TLKrDigi) );
      instance.SetNew(&new_TLKrDigi);
      instance.SetNewArray(&newArray_TLKrDigi);
      instance.SetDelete(&delete_TLKrDigi);
      instance.SetDeleteArray(&deleteArray_TLKrDigi);
      instance.SetDestructor(&destruct_TLKrDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TLKrDigi*)
   {
      return GenerateInitInstanceLocal((::TLKrDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TLKrDigi*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TLKrEvent(void *p = 0);
   static void *newArray_TLKrEvent(Long_t size, void *p);
   static void delete_TLKrEvent(void *p);
   static void deleteArray_TLKrEvent(void *p);
   static void destruct_TLKrEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TLKrEvent*)
   {
      ::TLKrEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TLKrEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TLKrEvent", ::TLKrEvent::Class_Version(), "", 136,
                  typeid(::TLKrEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TLKrEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TLKrEvent) );
      instance.SetNew(&new_TLKrEvent);
      instance.SetNewArray(&newArray_TLKrEvent);
      instance.SetDelete(&delete_TLKrEvent);
      instance.SetDeleteArray(&deleteArray_TLKrEvent);
      instance.SetDestructor(&destruct_TLKrEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TLKrEvent*)
   {
      return GenerateInitInstanceLocal((::TLKrEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TLKrEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TLKrHit(void *p = 0);
   static void *newArray_TLKrHit(Long_t size, void *p);
   static void delete_TLKrHit(void *p);
   static void deleteArray_TLKrHit(void *p);
   static void destruct_TLKrHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TLKrHit*)
   {
      ::TLKrHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TLKrHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TLKrHit", ::TLKrHit::Class_Version(), "", 172,
                  typeid(::TLKrHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TLKrHit::Dictionary, isa_proxy, 4,
                  sizeof(::TLKrHit) );
      instance.SetNew(&new_TLKrHit);
      instance.SetNewArray(&newArray_TLKrHit);
      instance.SetDelete(&delete_TLKrHit);
      instance.SetDeleteArray(&deleteArray_TLKrHit);
      instance.SetDestructor(&destruct_TLKrHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TLKrHit*)
   {
      return GenerateInitInstanceLocal((::TLKrHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TLKrHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TLKrMicroCellHit(void *p = 0);
   static void *newArray_TLKrMicroCellHit(Long_t size, void *p);
   static void delete_TLKrMicroCellHit(void *p);
   static void deleteArray_TLKrMicroCellHit(void *p);
   static void destruct_TLKrMicroCellHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TLKrMicroCellHit*)
   {
      ::TLKrMicroCellHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TLKrMicroCellHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TLKrMicroCellHit", ::TLKrMicroCellHit::Class_Version(), "", 226,
                  typeid(::TLKrMicroCellHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TLKrMicroCellHit::Dictionary, isa_proxy, 4,
                  sizeof(::TLKrMicroCellHit) );
      instance.SetNew(&new_TLKrMicroCellHit);
      instance.SetNewArray(&newArray_TLKrMicroCellHit);
      instance.SetDelete(&delete_TLKrMicroCellHit);
      instance.SetDeleteArray(&deleteArray_TLKrMicroCellHit);
      instance.SetDestructor(&destruct_TLKrMicroCellHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TLKrMicroCellHit*)
   {
      return GenerateInitInstanceLocal((::TLKrMicroCellHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TLKrMicroCellHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoLKrHit(void *p = 0);
   static void *newArray_TRecoLKrHit(Long_t size, void *p);
   static void delete_TRecoLKrHit(void *p);
   static void deleteArray_TRecoLKrHit(void *p);
   static void destruct_TRecoLKrHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoLKrHit*)
   {
      ::TRecoLKrHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoLKrHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoLKrHit", ::TRecoLKrHit::Class_Version(), "TRecoLKrHit.hh", 13,
                  typeid(::TRecoLKrHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoLKrHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoLKrHit) );
      instance.SetNew(&new_TRecoLKrHit);
      instance.SetNewArray(&newArray_TRecoLKrHit);
      instance.SetDelete(&delete_TRecoLKrHit);
      instance.SetDeleteArray(&deleteArray_TRecoLKrHit);
      instance.SetDestructor(&destruct_TRecoLKrHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoLKrHit*)
   {
      return GenerateInitInstanceLocal((::TRecoLKrHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoLKrHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoLKrCandidate(void *p = 0);
   static void *newArray_TRecoLKrCandidate(Long_t size, void *p);
   static void delete_TRecoLKrCandidate(void *p);
   static void deleteArray_TRecoLKrCandidate(void *p);
   static void destruct_TRecoLKrCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoLKrCandidate*)
   {
      ::TRecoLKrCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoLKrCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoLKrCandidate", ::TRecoLKrCandidate::Class_Version(), "", 263,
                  typeid(::TRecoLKrCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoLKrCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoLKrCandidate) );
      instance.SetNew(&new_TRecoLKrCandidate);
      instance.SetNewArray(&newArray_TRecoLKrCandidate);
      instance.SetDelete(&delete_TRecoLKrCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoLKrCandidate);
      instance.SetDestructor(&destruct_TRecoLKrCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoLKrCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoLKrCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoLKrCandidate*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoLKrEvent(void *p = 0);
   static void *newArray_TRecoLKrEvent(Long_t size, void *p);
   static void delete_TRecoLKrEvent(void *p);
   static void deleteArray_TRecoLKrEvent(void *p);
   static void destruct_TRecoLKrEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoLKrEvent*)
   {
      ::TRecoLKrEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoLKrEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoLKrEvent", ::TRecoLKrEvent::Class_Version(), "", 373,
                  typeid(::TRecoLKrEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoLKrEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoLKrEvent) );
      instance.SetNew(&new_TRecoLKrEvent);
      instance.SetNewArray(&newArray_TRecoLKrEvent);
      instance.SetDelete(&delete_TRecoLKrEvent);
      instance.SetDeleteArray(&deleteArray_TRecoLKrEvent);
      instance.SetDestructor(&destruct_TRecoLKrEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoLKrEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoLKrEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoLKrEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr LKrChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *LKrChannelID::Class_Name()
{
   return "LKrChannelID";
}

//______________________________________________________________________________
const char *LKrChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::LKrChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int LKrChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::LKrChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *LKrChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::LKrChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *LKrChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::LKrChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDigiLKrEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDigiLKrEvent::Class_Name()
{
   return "TDigiLKrEvent";
}

//______________________________________________________________________________
const char *TDigiLKrEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiLKrEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDigiLKrEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiLKrEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDigiLKrEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiLKrEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDigiLKrEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiLKrEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TLKrDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TLKrDigi::Class_Name()
{
   return "TLKrDigi";
}

//______________________________________________________________________________
const char *TLKrDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TLKrDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TLKrDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TLKrDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TLKrEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TLKrEvent::Class_Name()
{
   return "TLKrEvent";
}

//______________________________________________________________________________
const char *TLKrEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TLKrEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TLKrEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TLKrEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TLKrHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TLKrHit::Class_Name()
{
   return "TLKrHit";
}

//______________________________________________________________________________
const char *TLKrHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TLKrHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TLKrHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TLKrHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TLKrMicroCellHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TLKrMicroCellHit::Class_Name()
{
   return "TLKrMicroCellHit";
}

//______________________________________________________________________________
const char *TLKrMicroCellHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrMicroCellHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TLKrMicroCellHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLKrMicroCellHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TLKrMicroCellHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrMicroCellHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TLKrMicroCellHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLKrMicroCellHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoLKrHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoLKrHit::Class_Name()
{
   return "TRecoLKrHit";
}

//______________________________________________________________________________
const char *TRecoLKrHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoLKrHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoLKrHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoLKrHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoLKrCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoLKrCandidate::Class_Name()
{
   return "TRecoLKrCandidate";
}

//______________________________________________________________________________
const char *TRecoLKrCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoLKrCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoLKrCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoLKrCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoLKrEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoLKrEvent::Class_Name()
{
   return "TRecoLKrEvent";
}

//______________________________________________________________________________
const char *TRecoLKrEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoLKrEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoLKrEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoLKrEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLKrEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void LKrChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class LKrChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(LKrChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(LKrChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_LKrChannelID(void *p) {
      return  p ? new(p) ::LKrChannelID : new ::LKrChannelID;
   }
   static void *newArray_LKrChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::LKrChannelID[nElements] : new ::LKrChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_LKrChannelID(void *p) {
      delete ((::LKrChannelID*)p);
   }
   static void deleteArray_LKrChannelID(void *p) {
      delete [] ((::LKrChannelID*)p);
   }
   static void destruct_LKrChannelID(void *p) {
      typedef ::LKrChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::LKrChannelID

//______________________________________________________________________________
void TDigiLKrEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDigiLKrEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDigiLKrEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDigiLKrEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDigiLKrEvent(void *p) {
      return  p ? new(p) ::TDigiLKrEvent : new ::TDigiLKrEvent;
   }
   static void *newArray_TDigiLKrEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TDigiLKrEvent[nElements] : new ::TDigiLKrEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDigiLKrEvent(void *p) {
      delete ((::TDigiLKrEvent*)p);
   }
   static void deleteArray_TDigiLKrEvent(void *p) {
      delete [] ((::TDigiLKrEvent*)p);
   }
   static void destruct_TDigiLKrEvent(void *p) {
      typedef ::TDigiLKrEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDigiLKrEvent

//______________________________________________________________________________
void TLKrDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TLKrDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TLKrDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TLKrDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TLKrDigi(void *p) {
      return  p ? new(p) ::TLKrDigi : new ::TLKrDigi;
   }
   static void *newArray_TLKrDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TLKrDigi[nElements] : new ::TLKrDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TLKrDigi(void *p) {
      delete ((::TLKrDigi*)p);
   }
   static void deleteArray_TLKrDigi(void *p) {
      delete [] ((::TLKrDigi*)p);
   }
   static void destruct_TLKrDigi(void *p) {
      typedef ::TLKrDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TLKrDigi

//______________________________________________________________________________
void TLKrEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TLKrEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TLKrEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TLKrEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TLKrEvent(void *p) {
      return  p ? new(p) ::TLKrEvent : new ::TLKrEvent;
   }
   static void *newArray_TLKrEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TLKrEvent[nElements] : new ::TLKrEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TLKrEvent(void *p) {
      delete ((::TLKrEvent*)p);
   }
   static void deleteArray_TLKrEvent(void *p) {
      delete [] ((::TLKrEvent*)p);
   }
   static void destruct_TLKrEvent(void *p) {
      typedef ::TLKrEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TLKrEvent

//______________________________________________________________________________
void TLKrHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TLKrHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TLKrHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TLKrHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TLKrHit(void *p) {
      return  p ? new(p) ::TLKrHit : new ::TLKrHit;
   }
   static void *newArray_TLKrHit(Long_t nElements, void *p) {
      return p ? new(p) ::TLKrHit[nElements] : new ::TLKrHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TLKrHit(void *p) {
      delete ((::TLKrHit*)p);
   }
   static void deleteArray_TLKrHit(void *p) {
      delete [] ((::TLKrHit*)p);
   }
   static void destruct_TLKrHit(void *p) {
      typedef ::TLKrHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TLKrHit

//______________________________________________________________________________
void TLKrMicroCellHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TLKrMicroCellHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TLKrMicroCellHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TLKrMicroCellHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TLKrMicroCellHit(void *p) {
      return  p ? new(p) ::TLKrMicroCellHit : new ::TLKrMicroCellHit;
   }
   static void *newArray_TLKrMicroCellHit(Long_t nElements, void *p) {
      return p ? new(p) ::TLKrMicroCellHit[nElements] : new ::TLKrMicroCellHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TLKrMicroCellHit(void *p) {
      delete ((::TLKrMicroCellHit*)p);
   }
   static void deleteArray_TLKrMicroCellHit(void *p) {
      delete [] ((::TLKrMicroCellHit*)p);
   }
   static void destruct_TLKrMicroCellHit(void *p) {
      typedef ::TLKrMicroCellHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TLKrMicroCellHit

//______________________________________________________________________________
void TRecoLKrHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoLKrHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoLKrHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoLKrHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoLKrHit(void *p) {
      return  p ? new(p) ::TRecoLKrHit : new ::TRecoLKrHit;
   }
   static void *newArray_TRecoLKrHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoLKrHit[nElements] : new ::TRecoLKrHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoLKrHit(void *p) {
      delete ((::TRecoLKrHit*)p);
   }
   static void deleteArray_TRecoLKrHit(void *p) {
      delete [] ((::TRecoLKrHit*)p);
   }
   static void destruct_TRecoLKrHit(void *p) {
      typedef ::TRecoLKrHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoLKrHit

//______________________________________________________________________________
void TRecoLKrCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoLKrCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoLKrCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoLKrCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoLKrCandidate(void *p) {
      return  p ? new(p) ::TRecoLKrCandidate : new ::TRecoLKrCandidate;
   }
   static void *newArray_TRecoLKrCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoLKrCandidate[nElements] : new ::TRecoLKrCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoLKrCandidate(void *p) {
      delete ((::TRecoLKrCandidate*)p);
   }
   static void deleteArray_TRecoLKrCandidate(void *p) {
      delete [] ((::TRecoLKrCandidate*)p);
   }
   static void destruct_TRecoLKrCandidate(void *p) {
      typedef ::TRecoLKrCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoLKrCandidate

//______________________________________________________________________________
void TRecoLKrEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoLKrEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoLKrEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoLKrEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoLKrEvent(void *p) {
      return  p ? new(p) ::TRecoLKrEvent : new ::TRecoLKrEvent;
   }
   static void *newArray_TRecoLKrEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoLKrEvent[nElements] : new ::TRecoLKrEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoLKrEvent(void *p) {
      delete ((::TRecoLKrEvent*)p);
   }
   static void deleteArray_TRecoLKrEvent(void *p) {
      delete [] ((::TRecoLKrEvent*)p);
   }
   static void destruct_TRecoLKrEvent(void *p) {
      typedef ::TRecoLKrEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoLKrEvent

namespace {
  void TriggerDictionaryInitialization_libLKrPersistency_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LKr/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libLKrPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class LKrChannelID;
class TDigiLKrEvent;
class TLKrDigi;
class TLKrEvent;
class TLKrHit;
class TLKrMicroCellHit;
class __attribute__((annotate("$clingAutoload$TRecoLKrHit.hh")))  TRecoLKrHit;
class TRecoLKrCandidate;
class TRecoLKrEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libLKrPersistency dictionary payload"

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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef LKrChannelID_H
#define LKrChannelID_H
#include "Rtypes.h"

class LKrChannelID {

  public:

    LKrChannelID();
    LKrChannelID(Int_t, Int_t); //old, it should be removed eventually
    virtual ~LKrChannelID() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();      // returns position ID
    void  DecodeChannelID(Int_t); // converts position ID into XCellIDs, YCellIDs

    Int_t GetCPDID()                   { return fCPDID;         } //old, it should be removed eventually
    void  SetCPDID(Int_t value)        { fCPDID = value;        } //old, it should be removed eventually
    Int_t GetCPDChannelID()            { return fCPDChannelID;  } //old, it should be removed eventually
    void  SetCPDChannelID(Int_t value) { fCPDChannelID = value; } //old, it should be removed eventually
    Int_t GetXCellID()          { return fXCellID; }
    void  SetXCellID(Int_t val) { fXCellID = val;  }
    Int_t GetYCellID()          { return fYCellID; }
    void  SetYCellID(Int_t val) { fYCellID = val;  }

  private:

    Int_t fCPDID;        //old, it should be removed eventually
    Int_t fCPDChannelID; //old, it should be removed eventually
    Int_t fXCellID;
    Int_t fYCellID;

    ClassDef(LKrChannelID,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (Tommaso.Spadaro@cern.ch) 2017-01-11
//
// --------------------------------------------------------------
#ifndef TDigiLKrEvent_H
#define TDigiLKrEvent_H

#include "FADCEvent.hh"

class TDigiLKrEvent : public FADCEvent {

    public:

        TDigiLKrEvent();
        explicit TDigiLKrEvent(TClass*);
        TDigiLKrEvent(TClass*,Int_t);
        ~TDigiLKrEvent();
        void Init();

        void Clear(Option_t* = "");

    public:
        void SetTimePhase(Double_t val) { fTimePhase=val; };
        Double_t GetTimePhase() { return fTimePhase; };

    private:
        Double_t fTimePhase;

        ClassDef(TDigiLKrEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//            Evelina Gersabeck (Evelina.Gersabeck@cern.ch)
// --------------------------------------------------------------
#ifndef TLKrDigi_H
#define TLKrDigi_H

#include "FADCVHit.hh"
#include "LKrChannelID.hh"

class TLKrDigi : public FADCVHit, public LKrChannelID {
  public:

    TLKrDigi(Int_t iCh, Int_t CPDID, Int_t CPDChannelID) : FADCVHit(iCh), LKrChannelID(CPDID, CPDChannelID){} //old, it should be removed eventually
    TLKrDigi() : FADCVHit(), LKrChannelID(){}
    explicit TLKrDigi(Int_t iCh) : FADCVHit(iCh), LKrChannelID(){}
    ~TLKrDigi(){}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  private:

    ClassDef(TLKrDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#ifndef TLKrEvent_H
#define TLKrEvent_H

#include "TDetectorVEvent.hh"

class TLKrEvent : public TDetectorVEvent {

    public:

      TLKrEvent();
      void Clear(Option_t* = "");
      // void AddSeed(Int_t );

    public:
      //  Int_t *              GetSeedIndexes()                                   { return fSeedIndexes;                  };

      //  Int_t                GetNSeeds()                                        { return fNSeeds;                       };
      //  void                 SetNSeeds(Int_t value)                             { fNSeeds = value;                      };

    private:

      Int_t fSeedIndexes[16384]; //index of the hit recognized as seed
      Int_t fNSeeds; //totalnumber of seeds

      ClassDef(TLKrEvent,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//            Evelina Marinova (Evelina.Marinova@cern.ch)
// --------------------------------------------------------------
#ifndef TLKrHit_H
#define TLKrHit_H

#include "TDetectorVHit.hh"
#include "LKrChannelID.hh"

class TLKrHit : public TDetectorVHit, public LKrChannelID  {

    public:

        TLKrHit();
        ~TLKrHit();

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void  DecodeChannelID();

        Int_t GetStationID() { return 0; }


    public:

        Int_t                GetNSlices()                                       { return fNSlices;                      };
        void                 SetNSlices(Int_t value)                            { fNSlices = value;                     };
        Int_t                GetSlicesInX()                                     { return fSlicesInX;                    };
        void                 SetSlicesInX(Int_t value)                          { fSlicesInX = value;                   };
        Int_t                GetSlicesInY()                                     { return fSlicesInY;                    };
        void                 SetSlicesInY(Int_t value)                          { fSlicesInY = value;                   };

////<<        TClonesArray *       GetMicroCellData()                                 { return fMicroCellData;                };
////<<        void                 SetMicroCellData(TClonesArray * value)             { fMicroCellData = value;               };

        Double_t             GetCurrent() { return fCurrent; };
        void                 SetCurrent(Double_t value) { fCurrent = value; }; 

    protected:

        Int_t fNSlices ;
        Int_t fSlicesInX ;
        Int_t fSlicesInY ;
        Double_t fCurrent ; 

////<<        TClonesArray * fMicroCellData;

        ClassDef(TLKrHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//            Evelina Marinova (Evelina.Marinova@cern.ch)
// --------------------------------------------------------------
#ifndef TLKrMicroCellHit_H
#define TLKrMicroCellHit_H
#include "TObject.h"


class TLKrMicroCellHit : public TObject {

    public:

        TLKrMicroCellHit();
        ~TLKrMicroCellHit();

        void Clear(Option_t* = "");

        Int_t                GetXIndex()                                        { return fXIndex;                       };
        void                 SetXIndex(Int_t value)                             { fXIndex = value;                      };
        Int_t                GetYIndex()                                        { return fYIndex;                       };
        void                 SetYIndex(Int_t value)                             { fYIndex = value;                      };
        Int_t                GetZIndex()                                        { return fZIndex;                       };
        void                 SetZIndex(Int_t value)                             { fZIndex = value;                      };
        Float_t              GetEnergyFraction()                                { return fEnergyFraction;               };
        void                 SetEnergyFraction(Float_t value)                   { fEnergyFraction = value;              };

    protected:

        Int_t fXIndex;
        Int_t fYIndex;
        Int_t fZIndex;
        Float_t fEnergyFraction;

        ClassDef(TLKrMicroCellHit,1);
};
#endif
#ifndef TRecoLKrCandidate_H
#define TRecoLKrCandidate_H

#include "TRecoVCandidate.hh"
#include "TClonesArray.h"
#include "TRecoLKrHit.hh"
#include "TLKrHit.hh"

class TRecoLKrCandidate : public TRecoVCandidate {

public:

  TRecoLKrCandidate();
  ~TRecoLKrCandidate(){}

  void Clear(Option_t* = "");

  Int_t GetId() const { return fId; }
  void SetId(Int_t val) { fId=val; }
  Int_t GetNCells() const { return fNCells; }
  void SetNCells(Int_t val) { fNCells=val; }
  Int_t GetIdSeed() const { return fIdSeed; }
  void SetIdSeed(Int_t val) { fIdSeed=val; }
  Double_t GetClusterEnergy() const { return fClusterEnergy; }
  void SetClusterEnergy(Double_t val) { fClusterEnergy=val; }
  Double_t GetClusterEnergyError() const { return fClusterEnergyError; }
  void SetClusterEnergyError(Double_t val) { fClusterEnergyError=val; }
  Int_t GetClusterStatus() const { return fClusterStatus; }
  void SetClusterStatus(Int_t val) { fClusterStatus=val; }
  Double_t GetClusterX() const { return fClusterX; }
  void SetClusterX(Double_t val) { fClusterX=val; }
  Double_t GetClusterY() const { return fClusterY; }
  void SetClusterY(Double_t val) { fClusterY=val; }
  Double_t GetClusterRMSX() const { return fClusterRMSX; }
  void SetClusterRMSX(Double_t val) { fClusterRMSX=val; }
  Double_t GetClusterRMSY() const { return fClusterRMSY; }
  void SetClusterRMSY(Double_t val) { fClusterRMSY=val; }
  Double_t GetClusterTime() const { return fClusterTime; }
  void SetClusterTime(Double_t val) { fClusterTime=val; }
  Double_t GetClusterChi2RMS() const { return fClusterChi2RMS; }
  void SetClusterChi2RMS(Double_t val) { fClusterChi2RMS=val; }
  Double_t GetClusterTimeLateralCell() const { return fClusterTimeLateralCell; }
  void SetClusterTimeLateralCell(Double_t val) { fClusterTimeLateralCell=val; }
  Double_t GetClusterDDeadCell() const { return fClusterDDeadCell; }
  void SetClusterDDeadCell(Double_t val) { fClusterDDeadCell=val; }
  Double_t GetClusterUEnergy() const { return fClusterUEnergy; }
  void SetClusterUEnergy(Double_t val) { fClusterUEnergy=val; }
  Double_t GetClusterEnoise() const { return fClusterEnoise; }
  void SetClusterEnoise(Double_t val) { fClusterEnoise=val; }
  Double_t GetCluster77Energy() const { return fCluster77Energy; }
  void SetCluster77Energy(Double_t val) { fCluster77Energy=val; }
  Double_t GetSpaceChargeCorr() const { return fSpaceChargeCorr; }
  void SetSpaceChargeCorr(Double_t val) { fSpaceChargeCorr=val; }
  Double_t GetClusterKe3Energy() const { return fClusterKe3Energy; }
  void SetClusterKe3Energy(Double_t val) { fClusterKe3Energy=val; }
  Double_t GetCluster77Enoise() const { return fCluster77Enoise; }
  void SetCluster77Enoise(Double_t val) { fCluster77Enoise=val; }
  Double_t GetClusterUTime() const { return fClusterUTime; }
  void SetClusterUTime(Double_t val) { fClusterUTime=val; }
  Int_t GetN77Cells() const { return fN77Cells; }
  void SetN77Cells(Int_t val) { fN77Cells=val; }
  Int_t GetId77Cell(Int_t i) const { return fId77Cell[i]; }
  void SetId77Cell(Int_t i,Int_t val) { fId77Cell[i]=val; }
  Int_t GetFlag77Cell(Int_t i) const { return fFlag77Cell[i]; }
  void SetFlag77Cell(Int_t i,Int_t val) { fFlag77Cell[i]=val; }
  Double_t GetEnergy77Cell(Int_t i) const { return fEnergy77Cell[i]; }
  void SetEnergy77Cell(Int_t i,Double_t val) { fEnergy77Cell[i]=val; }
  Double_t GetTime77Cell(Int_t i) const { return fTime77Cell[i]; }
  void SetTime77Cell(Int_t i,Double_t val) { fTime77Cell[i]=val; }
  Double_t GetClusterSeedEnergy() const { return fClusterSeedEnergy; }
  void SetClusterSeedEnergy(Double_t val) { fClusterSeedEnergy=val; }

private:
  Int_t    fId;
  Int_t    fNCells; 
  Int_t    fIdSeed; 
  Double_t fClusterEnergy;
  Double_t fClusterEnergyError;
  Int_t    fClusterStatus;
  Double_t fClusterX;
  Double_t fClusterY;
  Double_t fClusterRMSX;
  Double_t fClusterRMSY;
  Double_t fClusterTime;
  Double_t fClusterChi2RMS;
  Double_t fClusterTimeLateralCell;
  Double_t fClusterDDeadCell;
  Double_t fClusterUEnergy;
  Double_t fClusterEnoise;
  Double_t fCluster77Energy;
  Double_t fSpaceChargeCorr;
  Double_t fClusterKe3Energy;
  Double_t fCluster77Enoise;
  Double_t fClusterUTime;
  Int_t    fN77Cells;
  Int_t    fId77Cell[50];
  Int_t    fFlag77Cell[50];
  Double_t fEnergy77Cell[50];
  Double_t fTime77Cell[50];
  Double_t fClusterSeedEnergy;

  ClassDef(TRecoLKrCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLKrEvent_H
#define TRecoLKrEvent_H

#include "TRecoVEvent.hh"
#include "TRecoLKrCandidate.hh"
#include "TRecoLKrHit.hh"

class TRecoLKrEvent : public TRecoVEvent {

    public:

        TRecoLKrEvent();
        ~TRecoLKrEvent();
        void Init();

        void Clear(Option_t* = "");

    public:
        void SetEnergyTotal(Double_t val) { fEnergyTotal=val; };
        Double_t GetEnergyTotal() { return fEnergyTotal; };
        void SetRecFlag(Int_t val) { fRecFlag=val; };
        Int_t GetRecFlag() { return fRecFlag; };

    private:
        Double_t fEnergyTotal;
        Int_t fRecFlag;

        ClassDef(TRecoLKrEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLKrHit_H
#define TRecoLKrHit_H

#include "TRecoVHit.hh"
#include "LKrChannelID.hh"

class TRecoLKrHit : public TRecoVHit, public LKrChannelID {

    public:

        TRecoLKrHit();
        ~TRecoLKrHit(){};

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void  DecodeChannelID();

    public:
        Double_t GetPedestal() {return fPedestal;};
        void SetPedestal(Double_t val) {fPedestal=val;};
        Int_t GetGain() {return fGain;};
        void SetGain(Int_t val) {fGain=val;};

    private:
        Int_t fGain;
        Double_t fPedestal;

        ClassDef(TRecoLKrHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"LKrChannelID", payloadCode, "@",
"TDigiLKrEvent", payloadCode, "@",
"TLKrDigi", payloadCode, "@",
"TLKrEvent", payloadCode, "@",
"TLKrHit", payloadCode, "@",
"TLKrMicroCellHit", payloadCode, "@",
"TRecoLKrCandidate", payloadCode, "@",
"TRecoLKrEvent", payloadCode, "@",
"TRecoLKrHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libLKrPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libLKrPersistency_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libLKrPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libLKrPersistency() {
  TriggerDictionaryInitialization_libLKrPersistency_Impl();
}
