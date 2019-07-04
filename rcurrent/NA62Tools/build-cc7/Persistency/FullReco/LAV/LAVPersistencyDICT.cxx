// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME LAVPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/LAVChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/LAVDefinitions.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/TLAVDigi.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/TLAVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/TLAVHit.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/TRecoLAVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/TRecoLAVDigi.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/TRecoLAVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include/TRecoLAVHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_LAVChannelID(void *p = 0);
   static void *newArray_LAVChannelID(Long_t size, void *p);
   static void delete_LAVChannelID(void *p);
   static void deleteArray_LAVChannelID(void *p);
   static void destruct_LAVChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::LAVChannelID*)
   {
      ::LAVChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::LAVChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("LAVChannelID", ::LAVChannelID::Class_Version(), "", 17,
                  typeid(::LAVChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::LAVChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::LAVChannelID) );
      instance.SetNew(&new_LAVChannelID);
      instance.SetNewArray(&newArray_LAVChannelID);
      instance.SetDelete(&delete_LAVChannelID);
      instance.SetDeleteArray(&deleteArray_LAVChannelID);
      instance.SetDestructor(&destruct_LAVChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::LAVChannelID*)
   {
      return GenerateInitInstanceLocal((::LAVChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::LAVChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TLAVDigi(void *p = 0);
   static void *newArray_TLAVDigi(Long_t size, void *p);
   static void delete_TLAVDigi(void *p);
   static void deleteArray_TLAVDigi(void *p);
   static void destruct_TLAVDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TLAVDigi*)
   {
      ::TLAVDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TLAVDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TLAVDigi", ::TLAVDigi::Class_Version(), "", 85,
                  typeid(::TLAVDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TLAVDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TLAVDigi) );
      instance.SetNew(&new_TLAVDigi);
      instance.SetNewArray(&newArray_TLAVDigi);
      instance.SetDelete(&delete_TLAVDigi);
      instance.SetDeleteArray(&deleteArray_TLAVDigi);
      instance.SetDestructor(&destruct_TLAVDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TLAVDigi*)
   {
      return GenerateInitInstanceLocal((::TLAVDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TLAVDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TLAVEvent(void *p = 0);
   static void *newArray_TLAVEvent(Long_t size, void *p);
   static void delete_TLAVEvent(void *p);
   static void deleteArray_TLAVEvent(void *p);
   static void destruct_TLAVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TLAVEvent*)
   {
      ::TLAVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TLAVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TLAVEvent", ::TLAVEvent::Class_Version(), "", 120,
                  typeid(::TLAVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TLAVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TLAVEvent) );
      instance.SetNew(&new_TLAVEvent);
      instance.SetNewArray(&newArray_TLAVEvent);
      instance.SetDelete(&delete_TLAVEvent);
      instance.SetDeleteArray(&deleteArray_TLAVEvent);
      instance.SetDestructor(&destruct_TLAVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TLAVEvent*)
   {
      return GenerateInitInstanceLocal((::TLAVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TLAVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TLAVHit(void *p = 0);
   static void *newArray_TLAVHit(Long_t size, void *p);
   static void delete_TLAVHit(void *p);
   static void deleteArray_TLAVHit(void *p);
   static void destruct_TLAVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TLAVHit*)
   {
      ::TLAVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TLAVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TLAVHit", ::TLAVHit::Class_Version(), "", 155,
                  typeid(::TLAVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TLAVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TLAVHit) );
      instance.SetNew(&new_TLAVHit);
      instance.SetNewArray(&newArray_TLAVHit);
      instance.SetDelete(&delete_TLAVHit);
      instance.SetDeleteArray(&deleteArray_TLAVHit);
      instance.SetDestructor(&destruct_TLAVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TLAVHit*)
   {
      return GenerateInitInstanceLocal((::TLAVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TLAVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoLAVCandidate(void *p = 0);
   static void *newArray_TRecoLAVCandidate(Long_t size, void *p);
   static void delete_TRecoLAVCandidate(void *p);
   static void deleteArray_TRecoLAVCandidate(void *p);
   static void destruct_TRecoLAVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoLAVCandidate*)
   {
      ::TRecoLAVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoLAVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoLAVCandidate", ::TRecoLAVCandidate::Class_Version(), "", 224,
                  typeid(::TRecoLAVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoLAVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoLAVCandidate) );
      instance.SetNew(&new_TRecoLAVCandidate);
      instance.SetNewArray(&newArray_TRecoLAVCandidate);
      instance.SetDelete(&delete_TRecoLAVCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoLAVCandidate);
      instance.SetDestructor(&destruct_TRecoLAVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoLAVCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoLAVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoLAVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoLAVDigi(void *p = 0);
   static void *newArray_TRecoLAVDigi(Long_t size, void *p);
   static void delete_TRecoLAVDigi(void *p);
   static void deleteArray_TRecoLAVDigi(void *p);
   static void destruct_TRecoLAVDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoLAVDigi*)
   {
      ::TRecoLAVDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoLAVDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoLAVDigi", ::TRecoLAVDigi::Class_Version(), "", 310,
                  typeid(::TRecoLAVDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoLAVDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoLAVDigi) );
      instance.SetNew(&new_TRecoLAVDigi);
      instance.SetNewArray(&newArray_TRecoLAVDigi);
      instance.SetDelete(&delete_TRecoLAVDigi);
      instance.SetDeleteArray(&deleteArray_TRecoLAVDigi);
      instance.SetDestructor(&destruct_TRecoLAVDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoLAVDigi*)
   {
      return GenerateInitInstanceLocal((::TRecoLAVDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoLAVDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoLAVHit(void *p = 0);
   static void *newArray_TRecoLAVHit(Long_t size, void *p);
   static void delete_TRecoLAVHit(void *p);
   static void deleteArray_TRecoLAVHit(void *p);
   static void destruct_TRecoLAVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoLAVHit*)
   {
      ::TRecoLAVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoLAVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoLAVHit", ::TRecoLAVHit::Class_Version(), "TRecoLAVHit.hh", 29,
                  typeid(::TRecoLAVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoLAVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoLAVHit) );
      instance.SetNew(&new_TRecoLAVHit);
      instance.SetNewArray(&newArray_TRecoLAVHit);
      instance.SetDelete(&delete_TRecoLAVHit);
      instance.SetDeleteArray(&deleteArray_TRecoLAVHit);
      instance.SetDestructor(&destruct_TRecoLAVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoLAVHit*)
   {
      return GenerateInitInstanceLocal((::TRecoLAVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoLAVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoLAVEvent(void *p = 0);
   static void *newArray_TRecoLAVEvent(Long_t size, void *p);
   static void delete_TRecoLAVEvent(void *p);
   static void deleteArray_TRecoLAVEvent(void *p);
   static void destruct_TRecoLAVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoLAVEvent*)
   {
      ::TRecoLAVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoLAVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoLAVEvent", ::TRecoLAVEvent::Class_Version(), "", 373,
                  typeid(::TRecoLAVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoLAVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoLAVEvent) );
      instance.SetNew(&new_TRecoLAVEvent);
      instance.SetNewArray(&newArray_TRecoLAVEvent);
      instance.SetDelete(&delete_TRecoLAVEvent);
      instance.SetDeleteArray(&deleteArray_TRecoLAVEvent);
      instance.SetDestructor(&destruct_TRecoLAVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoLAVEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoLAVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoLAVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr LAVChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *LAVChannelID::Class_Name()
{
   return "LAVChannelID";
}

//______________________________________________________________________________
const char *LAVChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::LAVChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int LAVChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::LAVChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *LAVChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::LAVChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *LAVChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::LAVChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TLAVDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TLAVDigi::Class_Name()
{
   return "TLAVDigi";
}

//______________________________________________________________________________
const char *TLAVDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLAVDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TLAVDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLAVDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TLAVDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLAVDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TLAVDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLAVDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TLAVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TLAVEvent::Class_Name()
{
   return "TLAVEvent";
}

//______________________________________________________________________________
const char *TLAVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLAVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TLAVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLAVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TLAVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLAVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TLAVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLAVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TLAVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TLAVHit::Class_Name()
{
   return "TLAVHit";
}

//______________________________________________________________________________
const char *TLAVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLAVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TLAVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TLAVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TLAVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLAVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TLAVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TLAVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoLAVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoLAVCandidate::Class_Name()
{
   return "TRecoLAVCandidate";
}

//______________________________________________________________________________
const char *TRecoLAVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoLAVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoLAVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoLAVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoLAVDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoLAVDigi::Class_Name()
{
   return "TRecoLAVDigi";
}

//______________________________________________________________________________
const char *TRecoLAVDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoLAVDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoLAVDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoLAVDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoLAVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoLAVHit::Class_Name()
{
   return "TRecoLAVHit";
}

//______________________________________________________________________________
const char *TRecoLAVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoLAVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoLAVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoLAVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoLAVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoLAVEvent::Class_Name()
{
   return "TRecoLAVEvent";
}

//______________________________________________________________________________
const char *TRecoLAVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoLAVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoLAVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoLAVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoLAVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void LAVChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class LAVChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(LAVChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(LAVChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_LAVChannelID(void *p) {
      return  p ? new(p) ::LAVChannelID : new ::LAVChannelID;
   }
   static void *newArray_LAVChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::LAVChannelID[nElements] : new ::LAVChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_LAVChannelID(void *p) {
      delete ((::LAVChannelID*)p);
   }
   static void deleteArray_LAVChannelID(void *p) {
      delete [] ((::LAVChannelID*)p);
   }
   static void destruct_LAVChannelID(void *p) {
      typedef ::LAVChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::LAVChannelID

//______________________________________________________________________________
void TLAVDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TLAVDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TLAVDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TLAVDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TLAVDigi(void *p) {
      return  p ? new(p) ::TLAVDigi : new ::TLAVDigi;
   }
   static void *newArray_TLAVDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TLAVDigi[nElements] : new ::TLAVDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TLAVDigi(void *p) {
      delete ((::TLAVDigi*)p);
   }
   static void deleteArray_TLAVDigi(void *p) {
      delete [] ((::TLAVDigi*)p);
   }
   static void destruct_TLAVDigi(void *p) {
      typedef ::TLAVDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TLAVDigi

//______________________________________________________________________________
void TLAVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TLAVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TLAVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TLAVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TLAVEvent(void *p) {
      return  p ? new(p) ::TLAVEvent : new ::TLAVEvent;
   }
   static void *newArray_TLAVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TLAVEvent[nElements] : new ::TLAVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TLAVEvent(void *p) {
      delete ((::TLAVEvent*)p);
   }
   static void deleteArray_TLAVEvent(void *p) {
      delete [] ((::TLAVEvent*)p);
   }
   static void destruct_TLAVEvent(void *p) {
      typedef ::TLAVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TLAVEvent

//______________________________________________________________________________
void TLAVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TLAVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TLAVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TLAVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TLAVHit(void *p) {
      return  p ? new(p) ::TLAVHit : new ::TLAVHit;
   }
   static void *newArray_TLAVHit(Long_t nElements, void *p) {
      return p ? new(p) ::TLAVHit[nElements] : new ::TLAVHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TLAVHit(void *p) {
      delete ((::TLAVHit*)p);
   }
   static void deleteArray_TLAVHit(void *p) {
      delete [] ((::TLAVHit*)p);
   }
   static void destruct_TLAVHit(void *p) {
      typedef ::TLAVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TLAVHit

//______________________________________________________________________________
void TRecoLAVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoLAVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoLAVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoLAVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoLAVCandidate(void *p) {
      return  p ? new(p) ::TRecoLAVCandidate : new ::TRecoLAVCandidate;
   }
   static void *newArray_TRecoLAVCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoLAVCandidate[nElements] : new ::TRecoLAVCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoLAVCandidate(void *p) {
      delete ((::TRecoLAVCandidate*)p);
   }
   static void deleteArray_TRecoLAVCandidate(void *p) {
      delete [] ((::TRecoLAVCandidate*)p);
   }
   static void destruct_TRecoLAVCandidate(void *p) {
      typedef ::TRecoLAVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoLAVCandidate

//______________________________________________________________________________
void TRecoLAVDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoLAVDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoLAVDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoLAVDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoLAVDigi(void *p) {
      return  p ? new(p) ::TRecoLAVDigi : new ::TRecoLAVDigi;
   }
   static void *newArray_TRecoLAVDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoLAVDigi[nElements] : new ::TRecoLAVDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoLAVDigi(void *p) {
      delete ((::TRecoLAVDigi*)p);
   }
   static void deleteArray_TRecoLAVDigi(void *p) {
      delete [] ((::TRecoLAVDigi*)p);
   }
   static void destruct_TRecoLAVDigi(void *p) {
      typedef ::TRecoLAVDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoLAVDigi

//______________________________________________________________________________
void TRecoLAVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoLAVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoLAVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoLAVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoLAVHit(void *p) {
      return  p ? new(p) ::TRecoLAVHit : new ::TRecoLAVHit;
   }
   static void *newArray_TRecoLAVHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoLAVHit[nElements] : new ::TRecoLAVHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoLAVHit(void *p) {
      delete ((::TRecoLAVHit*)p);
   }
   static void deleteArray_TRecoLAVHit(void *p) {
      delete [] ((::TRecoLAVHit*)p);
   }
   static void destruct_TRecoLAVHit(void *p) {
      typedef ::TRecoLAVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoLAVHit

//______________________________________________________________________________
void TRecoLAVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoLAVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoLAVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoLAVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoLAVEvent(void *p) {
      return  p ? new(p) ::TRecoLAVEvent : new ::TRecoLAVEvent;
   }
   static void *newArray_TRecoLAVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoLAVEvent[nElements] : new ::TRecoLAVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoLAVEvent(void *p) {
      delete ((::TRecoLAVEvent*)p);
   }
   static void deleteArray_TRecoLAVEvent(void *p) {
      delete [] ((::TRecoLAVEvent*)p);
   }
   static void destruct_TRecoLAVEvent(void *p) {
      typedef ::TRecoLAVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoLAVEvent

namespace {
  void TriggerDictionaryInitialization_libLAVPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/LAV/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libLAVPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class LAVChannelID;
class TLAVDigi;
class TLAVEvent;
class TLAVHit;
class TRecoLAVCandidate;
class TRecoLAVDigi;
class __attribute__((annotate("$clingAutoload$TRecoLAVHit.hh")))  TRecoLAVHit;
class TRecoLAVEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libLAVPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-05-02
//
// First implementation and revision by T. Spadaro and E. Leonardi
// --------------------------------------------------------------
#ifndef LAVChannelID_H
#define LAVChannelID_H
#include "Rtypes.h"

class LAVChannelID {

public:

  LAVChannelID();
  LAVChannelID(Int_t, Int_t, Int_t, Int_t);
  virtual ~LAVChannelID() {}
  void Clear(Option_t* = "");
  Int_t EncodeChannelID();      
  static Int_t EncodeChannelIDFromInfo(Int_t LAVID, Int_t LayerID, Int_t BananaID, Int_t BlockID){return LAVID*10000 + LayerID*1000 + BananaID*10 + BlockID;}

  void  DecodeChannelID(Int_t);

  Int_t GetPackedChannelID();
  static Int_t GetPackedChannelIDFromCh(Int_t channelID);
public:

  Int_t GetLAVID() { return fLAVID;};
  void  SetLAVID(Int_t value){ fLAVID = value;};
  static Int_t GetLAVIDFromCh(Int_t ChannelID){return ChannelID/10000;}

  Int_t GetLayerID(){ return fLayerID;};
  void  SetLayerID(Int_t value){ fLayerID = value;};
  static Int_t GetLayerIDFromCh(Int_t ChannelID){return (ChannelID-10000*GetLAVIDFromCh(ChannelID))/1000;}

  Int_t GetBananaID(){ return fBananaID;};
  void  SetBananaID(Int_t value){ fBananaID = value;};
  static Int_t GetBananaIDFromCh(Int_t ChannelID){return (ChannelID-10000*GetLAVIDFromCh(ChannelID)-1000*GetLayerIDFromCh(ChannelID))/10;}


  Int_t GetBlockID(){ return fBlockID;};
  void  SetBlockID(Int_t value){ fBlockID = value;};
  static Int_t GetBlockIDFromCh(Int_t ChannelID){return ChannelID-10000*GetLAVIDFromCh(ChannelID)-1000*GetLayerIDFromCh(ChannelID)-10*GetBananaIDFromCh(ChannelID);}
private:

  Int_t fLAVID;
  Int_t fLayerID;
  Int_t fBananaID;
  Int_t fBlockID;

 ClassDef(LAVChannelID,1);
};
#endif

#ifndef LAV_DEFINITIONS_H
#define LAV_DEFINITIONS_H 1

#define MAX_EDGES 20

#endif
// --------------------------------------------------------------
// History:
//
// Created by Vito Palladno (vito.palladino@cern.ch) 2011-10-11
//
// Totally Modified by Tommaso.Spadaro@cern.ch and Emanuele Leonardi
//
// --------------------------------------------------------------
#ifndef TLAVDigi_H
#define TLAVDigi_H 1

#include "TDCVHit.hh"
#include "LAVChannelID.hh"

using namespace std;

class TLAVDigi : public TDCVHit, public LAVChannelID {

public:

  TLAVDigi() : TDCVHit(), LAVChannelID(){} 
  explicit TLAVDigi(Int_t iCh) : TDCVHit(iCh), LAVChannelID(){}
  ~TLAVDigi(){}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return GetLAVID(); }

  Int_t GetThresholdType() { return fChannelID/1000000; }
  Int_t GetElectronicChannelID();

  void SetEdges(Int_t, Double_t, Double_t);
   
private:
  ClassDef(TLAVDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TLAVEvent_H
#define TLAVEvent_H

#include "TDetectorVEvent.hh"

class TLAVEvent : public TDetectorVEvent {

    public:

        TLAVEvent();
        ~TLAVEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TLAVEvent,1);
};
#endif
// TLAVHit.hh
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - add LAV specific hit information
// 2010-03-15 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Optical photons on photocatode
//
// First implementation of LAVChannelID and revision by T. Spadaro and E. Leonardi
// --------------------------------------------------------------
#ifndef TLAVHit_H
#define TLAVHit_H

#include "TDetectorVHit.hh"
#include "LAVChannelID.hh"

#define __TLAVHit_MAX_PHOTONS__ 10000

class TLAVHit : public TDetectorVHit, public LAVChannelID {

public:

        TLAVHit();
        ~TLAVHit(){};

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void DecodeChannelID();
        Int_t GetStationID() { return GetLAVID(); }

        TVector3 GetLocalPosition()                { return fLocalPosition; };
        void     SetLocalPosition(TVector3 value)  { fLocalPosition = value; };
        TVector3 GetLocalDirection()               { return fLocalDirection; };
        void     SetLocalDirection(TVector3 value) { fLocalDirection = value; };
        Double_t GetBeta()                         { return fBeta; };
        void     SetBeta(Double_t value)           { fBeta = value; };
        Double_t GetStepLength()                   { return fStepLength; };
        void     SetStepLength(Double_t value)     { fStepLength = value; };

        void      SetPhotonsNumber(Int_t value)    { fPhotonsNumber = value;
                                                     if (fPhotonsNumber > __TLAVHit_MAX_PHOTONS__)
                                                        fPhotonsNumber = __TLAVHit_MAX_PHOTONS__;
                                                     if (fPhotonsNumber < 0)
                                                        fPhotonsNumber = 0; };
        Int_t     GetPhotonsNumber()               { return fPhotonsNumber; };
        Float_t* GetPhotonsEnergy()                { return fPhotonsEnergy; };
        Float_t* GetPhotonsTime()                  { return fPhotonsTime; };

        void Print(Option_t * option = "" )  const;

    private:
        
    protected:

        TVector3   fLocalPosition;
        TVector3   fLocalDirection;
        Double_t   fBeta;
        Double_t   fStepLength;

        Int_t      fPhotonsNumber;
        Float_t    fPhotonsEnergy[__TLAVHit_MAX_PHOTONS__];
        Float_t    fPhotonsTime[__TLAVHit_MAX_PHOTONS__];

        ClassDef(TLAVHit,2);

};
#endif
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - Added methods to set and retrieve cluster properties
// - Doxygen-compliant documentation added
// 2015-01-22 Totally modified and revised by T. Spadaro and E. Leonardi
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLAVCandidate_H
#define TRecoLAVCandidate_H

#include "TRecoVCandidate.hh"

using namespace std;

class TRecoLAVCandidate : public TRecoVCandidate {

    public:

       TRecoLAVCandidate();
       ~TRecoLAVCandidate(){};
       void Clear(Option_t* = "");

       void SetEnergy(Double_t val){fEnergy = val;}
       Double_t GetEnergy(){return fEnergy;}

// Number of Hits per layer

       void SetNHitsPerLayer(Int_t ilay, Int_t val){if (ilay<0 || ilay>=5) return; fNHitsPerLayer[ilay] = val;}
       Int_t GetNHitsPerLayer(Int_t ilay){if (ilay<0 || ilay>=5) return 0; return fNHitsPerLayer[ilay];}

// Algorithm Type

       void  SetAlgorithm(Int_t val) {fAlgorithm = val;}
       Int_t GetAlgorithm() {return fAlgorithm;}

// Classification of the cluster: MIP, Shower, Unknown

       void  SetClusterType(Int_t val) {fClusterType = val;}
       Int_t GetClusterType() {return fClusterType;}

// Centroid position: unwegithed and energy-weighted (at the moment, in fact, charge-weighted) average

       void SetPosition(TVector3 val){fPosition = val;}
       TVector3 GetPosition(){return fPosition;}
       void SetWeightedPosition(TVector3 val){fWeightedPosition = val;}
       TVector3 GetWeightedPosition(){return fWeightedPosition;}

// Methods to set/retrieve errors on the averages 

       void SetZUnweightedError(Double_t val){fZUnweightedError = val;}     
       Double_t GetZUnweightedError(){return fZUnweightedError;}            
     
       void SetPhiUnweightedError(Double_t val){fPhiUnweightedError = val;} 
       Double_t GetPhiUnweightedError(){return fPhiUnweightedError;}        
     
       void SetZWeightedError(Double_t val){fZWeightedError = val;}         
       Double_t GetZWeightedError(){return fZWeightedError;}                
     
       void SetPhiWeightedError(Double_t val){fPhiWeightedError = val;}     
       Double_t GetPhiWeightedError(){return fPhiWeightedError;}            

  // Method to retrieve the error matrix of x,y,z centroid coordinate estimates

  static Double_t GetCentroidErrorMatrix(TVector3, Double_t, Double_t, Double_t, Int_t, Int_t);
  
    private:

       Double_t fEnergy;             ///< Total cluster energy (at the moment, in fact, it is the total charge)
       TVector3 fPosition;           ///< Cluster centroid position: no weight is used while averaging fired block positions
       TVector3 fWeightedPosition;   ///< Cluster centroid position: the block energy (in fact, at the moment, the charge) is used while averaging fired block positions
       Double_t fZUnweightedError;   ///< Error on the unweighted average of the z coordinate
       Double_t fPhiUnweightedError; ///< Error on the unweighted average of the azimuthal angle
       Double_t fZWeightedError;     ///< Error on the energy-weighted average of the z coordinate   
       Double_t fPhiWeightedError;   ///< Error on the energy-weighted average of the azimuthal angle

       Int_t fNHitsPerLayer[5];   ///< Number of hits per layer
       Int_t fAlgorithm;  ///< =0 if cluster is done by grouping adjacent blocks; =1 if cluster is done by grouping blocks close in phi and time (so-called "tracking"); =2 if no algo is passed
       Int_t fClusterType;  ///<  =1 for MIP cluster (>=2 rows hit with 1 hit per row); =2 for showerLike (>=2 rows hit with > 1 hit per row in at least one row); =0 else
       ClassDef(TRecoLAVCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro and E. Leonardi: this class is obsolete and will be removed
// Created by Vito Palladno and Tommaso Spadaro (vito.palladino@cern.ch tommaso.spadaro@cern.ch) 2011-10-11
//
// 
//
// --------------------------------------------------------------


#ifndef TRecoLAVDigi_H
#define TRecoLAVDigi_H 1

#include "TObject.h"

#include "LAVDefinitions.hh"

class TRecoLAVDigi : public TObject {

public:

  TRecoLAVDigi();
  ~TRecoLAVDigi(){};

  void Clear(Option_t* = "");

  void  SetThType( Int_t ThType ) { fThType = ThType; }  // 1 High -1 Low 0 not assigned 
  Int_t GetThType()               { return fThType; }

  void SetNToT(Int_t NToT)       { 
    if(NToT>MAX_EDGES)
      fNToT = MAX_EDGES;
    else
      fNToT = NToT; }
  
  Int_t GetNToT()                { return fNToT; }

  void SetToT(Double_t* ToT)    { for(Int_t i=0; i<MAX_EDGES; i++) fToT[i] = ToT[i]; }
  Double_t* GetToT()            { return fToT; }
  
  void SetCharge(Double_t* Q)   { for(Int_t i=0; i<MAX_EDGES; i++) fQ[i] = Q[i]; }
  Double_t* GetCharge()         { return fQ; }
  
  void SetTime(Double_t* Time)  { for(Int_t i=0; i<MAX_EDGES; i++) fTime[i] = Time[i]; }
  Double_t* GetTime()           { return fTime; }

  void SetStatus(Int_t* Status)  { for(Int_t i=0; i<MAX_EDGES; i++) fStatus[i] = Status[i]; }
  Int_t* GetStatus()             { return fStatus; }

private:

  Int_t fThType;

  Int_t fNToT;

  Double_t fToT[MAX_EDGES];
  Double_t fTime[MAX_EDGES];
  Double_t fQ[MAX_EDGES];
  Int_t fStatus[MAX_EDGES];

  ClassDef(TRecoLAVDigi,1);

};
#endif


// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLAVEvent_H
#define TRecoLAVEvent_H

#include "TRecoVEvent.hh"
#include "TRecoLAVCandidate.hh"
#include "TRecoLAVHit.hh"

class TRecoLAVEvent : public TRecoVEvent {

    public:

        TRecoLAVEvent();
        ~TRecoLAVEvent();

        void Clear(Option_t* = "");
  
    private:

        ClassDef(TRecoLAVEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - add doxygen-compliant documentation
// - add method to retrieve span in azimuth of a given block
// 2015-01-22 Totally modified and revised by T. Spadaro and E. Leonardi
// 2012-01-04 Modified by Vito Palladino
//        RecoHit contains now a TClonesArray with all the RecoDigis
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// 
// --------------------------------------------------------------

#ifndef TRecoLAVHit_H
#define TRecoLAVHit_H 1

#include "TClonesArray.h"

#include "TRecoVHit.hh"
#include "LAVDefinitions.hh"
#include "LAVChannelID.hh"
#include "TVector3.h"
#include "TMath.h"

using namespace std;

class TRecoLAVHit : public TRecoVHit, public LAVChannelID {
  
public:
  
  TRecoLAVHit();
  ~TRecoLAVHit();
  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  void SetLeadingEdgeLow(Double_t edgeTime){fLeadingEdgeLow = edgeTime; fEdgeMask |= 1;}
  void SetLeadingEdgeHigh(Double_t edgeTime){fLeadingEdgeHigh = edgeTime; fEdgeMask |= 2;}
  void SetTrailingEdgeHigh(Double_t edgeTime){fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4;}
  void SetTrailingEdgeLow(Double_t edgeTime){fTrailingEdgeLow = edgeTime; fEdgeMask |= 8;}
  
  Double_t GetLeadingEdgeLow(){if (fEdgeMask & 1) {return fLeadingEdgeLow;} else {return 0;}}
  Double_t GetLeadingEdgeHigh(){if (fEdgeMask & 2) {return fLeadingEdgeHigh;} else {return 0;}} 
  Double_t GetTrailingEdgeHigh(){if (fEdgeMask & 4) {return fTrailingEdgeHigh;} else {return 0;}}
  Double_t GetTrailingEdgeLow(){if (fEdgeMask & 8) {return fTrailingEdgeLow;} else {return 0;}}

  Int_t GetEdgeMask(){return fEdgeMask;}

  void GetBlockPosition(TVector3 &); ///< Returns block position, expressed in mm
  static void GetBlockPositionFromCh(Int_t, TVector3 &);
  static Int_t  GetBlockIDFromPhi(Double_t, Int_t, Int_t); 
  static Double_t  GetBlockPhiSpan(Int_t);
private:

  Int_t fEdgeMask; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow

  Double_t fLeadingEdgeLow; ///< Time of leading low, subtracted of the trigger time only 
  Double_t fTrailingEdgeLow;///< Time of leading high, subtracted of the trigger time only 
  Double_t fLeadingEdgeHigh;///< Time of trailing high, subtracted of the trigger time only 
  Double_t fTrailingEdgeHigh;///< Time of trailing low, subtracted of the trigger time only 
  
  ClassDef(TRecoLAVHit,1);
  
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"LAVChannelID", payloadCode, "@",
"TLAVDigi", payloadCode, "@",
"TLAVEvent", payloadCode, "@",
"TLAVHit", payloadCode, "@",
"TRecoLAVCandidate", payloadCode, "@",
"TRecoLAVDigi", payloadCode, "@",
"TRecoLAVEvent", payloadCode, "@",
"TRecoLAVHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libLAVPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libLAVPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libLAVPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libLAVPersistency() {
  TriggerDictionaryInitialization_libLAVPersistency_Impl();
}
