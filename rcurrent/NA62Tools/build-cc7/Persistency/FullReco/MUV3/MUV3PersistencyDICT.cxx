// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV3PersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include/MUV3ChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include/TMUV3Digi.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include/TMUV3Event.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include/TMUV3Hit.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include/TRecoMUV3Candidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include/TRecoMUV3Event.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include/TRecoMUV3Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_MUV3ChannelID(void *p = 0);
   static void *newArray_MUV3ChannelID(Long_t size, void *p);
   static void delete_MUV3ChannelID(void *p);
   static void deleteArray_MUV3ChannelID(void *p);
   static void destruct_MUV3ChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MUV3ChannelID*)
   {
      ::MUV3ChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::MUV3ChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("MUV3ChannelID", ::MUV3ChannelID::Class_Version(), "", 19,
                  typeid(::MUV3ChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::MUV3ChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::MUV3ChannelID) );
      instance.SetNew(&new_MUV3ChannelID);
      instance.SetNewArray(&newArray_MUV3ChannelID);
      instance.SetDelete(&delete_MUV3ChannelID);
      instance.SetDeleteArray(&deleteArray_MUV3ChannelID);
      instance.SetDestructor(&destruct_MUV3ChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MUV3ChannelID*)
   {
      return GenerateInitInstanceLocal((::MUV3ChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::MUV3ChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV3Digi(void *p = 0);
   static void *newArray_TMUV3Digi(Long_t size, void *p);
   static void delete_TMUV3Digi(void *p);
   static void deleteArray_TMUV3Digi(void *p);
   static void destruct_TMUV3Digi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV3Digi*)
   {
      ::TMUV3Digi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV3Digi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV3Digi", ::TMUV3Digi::Class_Version(), "", 62,
                  typeid(::TMUV3Digi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV3Digi::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV3Digi) );
      instance.SetNew(&new_TMUV3Digi);
      instance.SetNewArray(&newArray_TMUV3Digi);
      instance.SetDelete(&delete_TMUV3Digi);
      instance.SetDeleteArray(&deleteArray_TMUV3Digi);
      instance.SetDestructor(&destruct_TMUV3Digi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV3Digi*)
   {
      return GenerateInitInstanceLocal((::TMUV3Digi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV3Digi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV3Event(void *p = 0);
   static void *newArray_TMUV3Event(Long_t size, void *p);
   static void delete_TMUV3Event(void *p);
   static void deleteArray_TMUV3Event(void *p);
   static void destruct_TMUV3Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV3Event*)
   {
      ::TMUV3Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV3Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV3Event", ::TMUV3Event::Class_Version(), "", 96,
                  typeid(::TMUV3Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV3Event::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV3Event) );
      instance.SetNew(&new_TMUV3Event);
      instance.SetNewArray(&newArray_TMUV3Event);
      instance.SetDelete(&delete_TMUV3Event);
      instance.SetDeleteArray(&deleteArray_TMUV3Event);
      instance.SetDestructor(&destruct_TMUV3Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV3Event*)
   {
      return GenerateInitInstanceLocal((::TMUV3Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV3Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV3Hit(void *p = 0);
   static void *newArray_TMUV3Hit(Long_t size, void *p);
   static void delete_TMUV3Hit(void *p);
   static void deleteArray_TMUV3Hit(void *p);
   static void destruct_TMUV3Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV3Hit*)
   {
      ::TMUV3Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV3Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV3Hit", ::TMUV3Hit::Class_Version(), "", 126,
                  typeid(::TMUV3Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV3Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV3Hit) );
      instance.SetNew(&new_TMUV3Hit);
      instance.SetNewArray(&newArray_TMUV3Hit);
      instance.SetDelete(&delete_TMUV3Hit);
      instance.SetDeleteArray(&deleteArray_TMUV3Hit);
      instance.SetDestructor(&destruct_TMUV3Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV3Hit*)
   {
      return GenerateInitInstanceLocal((::TMUV3Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV3Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV3Candidate(void *p = 0);
   static void *newArray_TRecoMUV3Candidate(Long_t size, void *p);
   static void delete_TRecoMUV3Candidate(void *p);
   static void deleteArray_TRecoMUV3Candidate(void *p);
   static void destruct_TRecoMUV3Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV3Candidate*)
   {
      ::TRecoMUV3Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV3Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV3Candidate", ::TRecoMUV3Candidate::Class_Version(), "", 163,
                  typeid(::TRecoMUV3Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV3Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV3Candidate) );
      instance.SetNew(&new_TRecoMUV3Candidate);
      instance.SetNewArray(&newArray_TRecoMUV3Candidate);
      instance.SetDelete(&delete_TRecoMUV3Candidate);
      instance.SetDeleteArray(&deleteArray_TRecoMUV3Candidate);
      instance.SetDestructor(&destruct_TRecoMUV3Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV3Candidate*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV3Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV3Candidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV3Hit(void *p = 0);
   static void *newArray_TRecoMUV3Hit(Long_t size, void *p);
   static void delete_TRecoMUV3Hit(void *p);
   static void deleteArray_TRecoMUV3Hit(void *p);
   static void destruct_TRecoMUV3Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV3Hit*)
   {
      ::TRecoMUV3Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV3Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV3Hit", ::TRecoMUV3Hit::Class_Version(), "TRecoMUV3Hit.hh", 15,
                  typeid(::TRecoMUV3Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV3Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV3Hit) );
      instance.SetNew(&new_TRecoMUV3Hit);
      instance.SetNewArray(&newArray_TRecoMUV3Hit);
      instance.SetDelete(&delete_TRecoMUV3Hit);
      instance.SetDeleteArray(&deleteArray_TRecoMUV3Hit);
      instance.SetDestructor(&destruct_TRecoMUV3Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV3Hit*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV3Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV3Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV3Event(void *p = 0);
   static void *newArray_TRecoMUV3Event(Long_t size, void *p);
   static void delete_TRecoMUV3Event(void *p);
   static void deleteArray_TRecoMUV3Event(void *p);
   static void destruct_TRecoMUV3Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV3Event*)
   {
      ::TRecoMUV3Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV3Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV3Event", ::TRecoMUV3Event::Class_Version(), "", 244,
                  typeid(::TRecoMUV3Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV3Event::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV3Event) );
      instance.SetNew(&new_TRecoMUV3Event);
      instance.SetNewArray(&newArray_TRecoMUV3Event);
      instance.SetDelete(&delete_TRecoMUV3Event);
      instance.SetDeleteArray(&deleteArray_TRecoMUV3Event);
      instance.SetDestructor(&destruct_TRecoMUV3Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV3Event*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV3Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV3Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr MUV3ChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *MUV3ChannelID::Class_Name()
{
   return "MUV3ChannelID";
}

//______________________________________________________________________________
const char *MUV3ChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV3ChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int MUV3ChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV3ChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *MUV3ChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV3ChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *MUV3ChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV3ChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV3Digi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV3Digi::Class_Name()
{
   return "TMUV3Digi";
}

//______________________________________________________________________________
const char *TMUV3Digi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Digi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV3Digi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Digi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV3Digi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Digi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV3Digi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Digi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV3Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV3Event::Class_Name()
{
   return "TMUV3Event";
}

//______________________________________________________________________________
const char *TMUV3Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV3Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV3Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV3Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV3Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV3Hit::Class_Name()
{
   return "TMUV3Hit";
}

//______________________________________________________________________________
const char *TMUV3Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV3Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV3Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV3Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV3Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV3Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV3Candidate::Class_Name()
{
   return "TRecoMUV3Candidate";
}

//______________________________________________________________________________
const char *TRecoMUV3Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV3Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV3Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV3Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV3Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV3Hit::Class_Name()
{
   return "TRecoMUV3Hit";
}

//______________________________________________________________________________
const char *TRecoMUV3Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV3Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV3Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV3Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV3Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV3Event::Class_Name()
{
   return "TRecoMUV3Event";
}

//______________________________________________________________________________
const char *TRecoMUV3Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV3Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV3Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV3Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV3Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void MUV3ChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class MUV3ChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(MUV3ChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(MUV3ChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MUV3ChannelID(void *p) {
      return  p ? new(p) ::MUV3ChannelID : new ::MUV3ChannelID;
   }
   static void *newArray_MUV3ChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::MUV3ChannelID[nElements] : new ::MUV3ChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_MUV3ChannelID(void *p) {
      delete ((::MUV3ChannelID*)p);
   }
   static void deleteArray_MUV3ChannelID(void *p) {
      delete [] ((::MUV3ChannelID*)p);
   }
   static void destruct_MUV3ChannelID(void *p) {
      typedef ::MUV3ChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MUV3ChannelID

//______________________________________________________________________________
void TMUV3Digi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV3Digi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV3Digi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV3Digi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV3Digi(void *p) {
      return  p ? new(p) ::TMUV3Digi : new ::TMUV3Digi;
   }
   static void *newArray_TMUV3Digi(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV3Digi[nElements] : new ::TMUV3Digi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV3Digi(void *p) {
      delete ((::TMUV3Digi*)p);
   }
   static void deleteArray_TMUV3Digi(void *p) {
      delete [] ((::TMUV3Digi*)p);
   }
   static void destruct_TMUV3Digi(void *p) {
      typedef ::TMUV3Digi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV3Digi

//______________________________________________________________________________
void TMUV3Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV3Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV3Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV3Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV3Event(void *p) {
      return  p ? new(p) ::TMUV3Event : new ::TMUV3Event;
   }
   static void *newArray_TMUV3Event(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV3Event[nElements] : new ::TMUV3Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV3Event(void *p) {
      delete ((::TMUV3Event*)p);
   }
   static void deleteArray_TMUV3Event(void *p) {
      delete [] ((::TMUV3Event*)p);
   }
   static void destruct_TMUV3Event(void *p) {
      typedef ::TMUV3Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV3Event

//______________________________________________________________________________
void TMUV3Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV3Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV3Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV3Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV3Hit(void *p) {
      return  p ? new(p) ::TMUV3Hit : new ::TMUV3Hit;
   }
   static void *newArray_TMUV3Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV3Hit[nElements] : new ::TMUV3Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV3Hit(void *p) {
      delete ((::TMUV3Hit*)p);
   }
   static void deleteArray_TMUV3Hit(void *p) {
      delete [] ((::TMUV3Hit*)p);
   }
   static void destruct_TMUV3Hit(void *p) {
      typedef ::TMUV3Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV3Hit

//______________________________________________________________________________
void TRecoMUV3Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV3Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV3Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV3Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV3Candidate(void *p) {
      return  p ? new(p) ::TRecoMUV3Candidate : new ::TRecoMUV3Candidate;
   }
   static void *newArray_TRecoMUV3Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV3Candidate[nElements] : new ::TRecoMUV3Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV3Candidate(void *p) {
      delete ((::TRecoMUV3Candidate*)p);
   }
   static void deleteArray_TRecoMUV3Candidate(void *p) {
      delete [] ((::TRecoMUV3Candidate*)p);
   }
   static void destruct_TRecoMUV3Candidate(void *p) {
      typedef ::TRecoMUV3Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV3Candidate

//______________________________________________________________________________
void TRecoMUV3Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV3Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV3Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV3Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV3Hit(void *p) {
      return  p ? new(p) ::TRecoMUV3Hit : new ::TRecoMUV3Hit;
   }
   static void *newArray_TRecoMUV3Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV3Hit[nElements] : new ::TRecoMUV3Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV3Hit(void *p) {
      delete ((::TRecoMUV3Hit*)p);
   }
   static void deleteArray_TRecoMUV3Hit(void *p) {
      delete [] ((::TRecoMUV3Hit*)p);
   }
   static void destruct_TRecoMUV3Hit(void *p) {
      typedef ::TRecoMUV3Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV3Hit

//______________________________________________________________________________
void TRecoMUV3Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV3Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV3Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV3Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV3Event(void *p) {
      return  p ? new(p) ::TRecoMUV3Event : new ::TRecoMUV3Event;
   }
   static void *newArray_TRecoMUV3Event(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV3Event[nElements] : new ::TRecoMUV3Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV3Event(void *p) {
      delete ((::TRecoMUV3Event*)p);
   }
   static void deleteArray_TRecoMUV3Event(void *p) {
      delete [] ((::TRecoMUV3Event*)p);
   }
   static void destruct_TRecoMUV3Event(void *p) {
      typedef ::TRecoMUV3Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV3Event

namespace {
  void TriggerDictionaryInitialization_libMUV3Persistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV3/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV3Persistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class MUV3ChannelID;
class TMUV3Digi;
class TMUV3Event;
class TMUV3Hit;
class TRecoMUV3Candidate;
class __attribute__((annotate("$clingAutoload$TRecoMUV3Hit.hh")))  TRecoMUV3Hit;
class TRecoMUV3Event;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV3Persistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-10
//
// ---------------------------------------------------------------

#ifndef MUV3ChannelID_H
#define MUV3ChannelID_H

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV3ChannelID {

public:

  MUV3ChannelID();
  virtual ~MUV3ChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); // returns position ID
  void  DecodeChannelID(Int_t);

  Int_t  GetTileID()      { return fTileID;       }
  Bool_t IsHigh()         { return fIsHigh;       }
  Bool_t IsInnerTile()    { return fIsInnerTile;  }
  Bool_t IsOuterTile()    { return !fIsInnerTile; }
  Int_t  GetPMTLocation() { return fPMTLocation;  }

private:

  Int_t  fTileID;
  Bool_t fIsHigh; ///< Channel with the higher ID in tile (N+200 rather than N)?
  Bool_t fIsInnerTile; ///< Outer (0-143) or inner (144-151) tile?
  Int_t  fPMTLocation; ///< Type of PMT location relative to the tile centre: kBottom, kTop, kJura, kSaleve

  ClassDef(MUV3ChannelID,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Antonio Cassese (Antonio.Cassese@cern.ch) 2012-10-19
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)         2015-11-02
//
// ---------------------------------------------------------------

#ifndef TMUV3Digi_H
#define TMUV3Digi_H

#include "TDCVHit.hh"
#include "MUV3ChannelID.hh"

class TMUV3Digi : public TDCVHit, public MUV3ChannelID {

public:

  TMUV3Digi();
  ~TMUV3Digi() {}

  void Clear(Option_t* = "");

  Int_t  EncodeChannelID();
  void   DecodeChannelID();
  Int_t  GetStationID()    { return 0;                    }
  Bool_t HasLeadingEdge()  { return  GetDetectedEdge()&1; }
  Bool_t HasTrailingEdge() { return  GetDetectedEdge()&2; }

private:

  ClassDef(TMUV3Digi,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------

#ifndef TMUV3Event_H
#define TMUV3Event_H

#include "TDetectorVEvent.hh"

class TMUV3Event : public TDetectorVEvent {

public:

  TMUV3Event();
  ~TMUV3Event();

  void Clear(Option_t* = "");

private:

  ClassDef(TMUV3Event,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)     2015-11-02
//
// --------------------------------------------------------------

#ifndef TMUV3Hit_H
#define TMUV3Hit_H

#include "TDetectorVHit.hh"
#include "MUV3ChannelID.hh"

class TMUV3Hit : public TDetectorVHit, public MUV3ChannelID {

public:

  TMUV3Hit();
  ~TMUV3Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t  GetStationID()         { return 0;          }
  Bool_t IsMuonHit()            { return fIsMuonHit; }
  void   SetMuonHit(Bool_t val) { fIsMuonHit = val;  }

protected:
  ClassDef(TMUV3Hit,1);

private:
  Bool_t fIsMuonHit;

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------

#ifndef TRecoMUV3Candidate_H
#define TRecoMUV3Candidate_H

#include "TRecoVCandidate.hh"

class TRecoMUV3Candidate : public TRecoVCandidate {

public:

  TRecoMUV3Candidate();
  ~TRecoMUV3Candidate() {}

  void Clear(Option_t* = "");

  Double_t GetX ()                       { return fX;                 }
  void     SetX (Double_t x)             { fX=x;                      }
  Double_t GetY ()                       { return fY;                 }
  void     SetY (Double_t y)             { fY=y;                      }
  Double_t GetZ ()                       { return fZ;                 }
  void     SetZ (Double_t z)             { fZ=z;                      }
  TVector3 GetPosition()                 { return TVector3(fX,fY,fZ); }

  Int_t    GetTileID()                   { return fTileID;       }
  void     SetTileID(Int_t val)          { fTileID = val;        }
  Int_t    GetChannel1()                 { return fChannel1;     }
  Int_t    GetChannel2()                 { return fChannel2;     }
  void     SetChannel1(Double_t val)     { fChannel1 = val;      }
  void     SetChannel2(Double_t val)     { fChannel2 = val;      }
  Int_t    GetROChannel1()               { return fROChannel1;   }
  Int_t    GetROChannel2()               { return fROChannel2;   }
  void     SetROChannel1(Double_t val)   { fROChannel1 = val;    }
  void     SetROChannel2(Double_t val)   { fROChannel2 = val;    }

  Double_t GetTime1()                    { return fTime1;        }
  Double_t GetTime2()                    { return fTime2;        }
  void     SetTime1(Double_t val)        { fTime1 = val;         }
  void     SetTime2(Double_t val)        { fTime2 = val;         }
  Double_t GetTime1NoT0()                { return fTime1NoT0;    }
  Double_t GetTime2NoT0()                { return fTime2NoT0;    }
  void     SetTime1NoT0(Double_t val)    { fTime1NoT0 = val;     }
  void     SetTime2NoT0(Double_t val)    { fTime2NoT0 = val;     }
  Double_t GetTimeNoTileT0()             { return fTimeNoTileT0; }
  void     SetTimeNoTileT0(Double_t val) { fTimeNoTileT0 = val;  }
  Double_t GetTimeNoT0()                 { return fTimeNoT0;     }
  void     SetTimeNoT0(Double_t val)     { fTimeNoT0 = val;      }
  Int_t    GetType()                     { return fType;         }
  void     SetType(Int_t val)            { fType  = val;         }

  Bool_t   IsTight()                     { return (fType==kTightCandidate);       }
  Bool_t   IsLoose()                     { return (fType==kLooseCandidate);       }
  Bool_t   IsLooseMasked()               { return (fType==kLooseMaskedCandidate); }
  Bool_t   IsInner()                     { return (fTileID>=144);                 }
  Bool_t   IsOuter()                     { return (fTileID< 144);                 }

  Double_t GetDeltaTime();
  Double_t GetDeltaTimeNoT0();
  Double_t GetAverageTime();

private:

  Double_t fX, fY, fZ;
  Int_t    fTileID;   ///< Valid tile IDs: 0-151 except 65,66,77,78
  Int_t    fChannel1; ///< Valid channel1 IDs: 0-151 except 65,66,77,78; 200-351 except 265,266,277,278
  Int_t    fChannel2; ///< Valid channel2 IDs: -1 (for loose candidates); 0-151 except 65,66,77,78; 200-351 except 265,266,277,278
  Int_t    fROChannel1, fROChannel2;
  Double_t fTime1, fTime2, fTime1NoT0, fTime2NoT0, fTimeNoTileT0, fTimeNoT0;
  Int_t    fType; ///< kTightCandidate, kLooseCandidate, kLooseMaskedCandidate, kUndefinedCandidate

  ClassDef(TRecoMUV3Candidate,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoMUV3Event_H
#define TRecoMUV3Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV3Candidate.hh"
#include "TRecoMUV3Hit.hh"

class TRecoMUV3Event : public TRecoVEvent {

public:
  TRecoMUV3Event();
  ~TRecoMUV3Event();

  void Clear(Option_t* = "");

private:
  ClassDef(TRecoMUV3Event,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------

#ifndef TRecoMUV3Hit_H
#define TRecoMUV3Hit_H

#include "TRecoVHit.hh"
#include "MUV3ChannelID.hh"

class TRecoMUV3Hit : public TRecoVHit, public MUV3ChannelID {

public:

  TRecoMUV3Hit();
  ~TRecoMUV3Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t    GetDetectedEdge()                 { return fDetectedEdge;     }
  void     SetDetectedEdge(Int_t val)        { fDetectedEdge = val;      }
  Double_t GetLeadingTime()                  { return fLeadingTime;      }
  void     SetLeadingTime(Double_t val)      { fLeadingTime = val;       }
  Double_t GetTrailingTime()                 { return fTrailingTime;     }
  void     SetTrailingTime(Double_t val)     { fTrailingTime = val;      }
  Double_t GetLeadingTimeNoT0()              { return fLeadingTimeNoT0;  }
  void     SetLeadingTimeNoT0(Double_t val)  { fLeadingTimeNoT0 = val;   }
  Double_t GetTrailingTimeNoT0()             { return fTrailingTimeNoT0; }
  void     SetTrailingTimeNoT0(Double_t val) { fTrailingTimeNoT0 = val;  }
  Double_t GetTimeNoT0()                     { return fTimeNoT0;         }
  void     SetTimeNoT0(Double_t val)         { fTimeNoT0 = val;          }
  int      GetROChannelID()                  { return fROChannelID;      }
  void     SetROChannelID(Int_t val)         { fROChannelID = val;       }

private:
  Int_t    fDetectedEdge;
  Double_t fLeadingTime, fTrailingTime;
  Double_t fLeadingTimeNoT0, fTrailingTimeNoT0, fTimeNoT0;
  Int_t    fROChannelID;

  ClassDef(TRecoMUV3Hit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"MUV3ChannelID", payloadCode, "@",
"TMUV3Digi", payloadCode, "@",
"TMUV3Event", payloadCode, "@",
"TMUV3Hit", payloadCode, "@",
"TRecoMUV3Candidate", payloadCode, "@",
"TRecoMUV3Event", payloadCode, "@",
"TRecoMUV3Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV3Persistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV3Persistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV3Persistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV3Persistency() {
  TriggerDictionaryInitialization_libMUV3Persistency_Impl();
}
