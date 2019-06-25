// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV0PersistencyDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include/MUV0ChannelID.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include/TMUV0Digi.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include/TMUV0Event.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include/TMUV0Hit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include/TRecoMUV0Candidate.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include/TRecoMUV0Event.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include/TRecoMUV0Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_MUV0ChannelID(void *p = 0);
   static void *newArray_MUV0ChannelID(Long_t size, void *p);
   static void delete_MUV0ChannelID(void *p);
   static void deleteArray_MUV0ChannelID(void *p);
   static void destruct_MUV0ChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MUV0ChannelID*)
   {
      ::MUV0ChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::MUV0ChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("MUV0ChannelID", ::MUV0ChannelID::Class_Version(), "", 29,
                  typeid(::MUV0ChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::MUV0ChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::MUV0ChannelID) );
      instance.SetNew(&new_MUV0ChannelID);
      instance.SetNewArray(&newArray_MUV0ChannelID);
      instance.SetDelete(&delete_MUV0ChannelID);
      instance.SetDeleteArray(&deleteArray_MUV0ChannelID);
      instance.SetDestructor(&destruct_MUV0ChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MUV0ChannelID*)
   {
      return GenerateInitInstanceLocal((::MUV0ChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::MUV0ChannelID*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV0Digi(void *p = 0);
   static void *newArray_TMUV0Digi(Long_t size, void *p);
   static void delete_TMUV0Digi(void *p);
   static void deleteArray_TMUV0Digi(void *p);
   static void destruct_TMUV0Digi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV0Digi*)
   {
      ::TMUV0Digi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV0Digi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV0Digi", ::TMUV0Digi::Class_Version(), "", 65,
                  typeid(::TMUV0Digi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV0Digi::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV0Digi) );
      instance.SetNew(&new_TMUV0Digi);
      instance.SetNewArray(&newArray_TMUV0Digi);
      instance.SetDelete(&delete_TMUV0Digi);
      instance.SetDeleteArray(&deleteArray_TMUV0Digi);
      instance.SetDestructor(&destruct_TMUV0Digi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV0Digi*)
   {
      return GenerateInitInstanceLocal((::TMUV0Digi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TMUV0Digi*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV0Event(void *p = 0);
   static void *newArray_TMUV0Event(Long_t size, void *p);
   static void delete_TMUV0Event(void *p);
   static void deleteArray_TMUV0Event(void *p);
   static void destruct_TMUV0Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV0Event*)
   {
      ::TMUV0Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV0Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV0Event", ::TMUV0Event::Class_Version(), "", 93,
                  typeid(::TMUV0Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV0Event::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV0Event) );
      instance.SetNew(&new_TMUV0Event);
      instance.SetNewArray(&newArray_TMUV0Event);
      instance.SetDelete(&delete_TMUV0Event);
      instance.SetDeleteArray(&deleteArray_TMUV0Event);
      instance.SetDestructor(&destruct_TMUV0Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV0Event*)
   {
      return GenerateInitInstanceLocal((::TMUV0Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TMUV0Event*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV0Hit(void *p = 0);
   static void *newArray_TMUV0Hit(Long_t size, void *p);
   static void delete_TMUV0Hit(void *p);
   static void deleteArray_TMUV0Hit(void *p);
   static void destruct_TMUV0Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV0Hit*)
   {
      ::TMUV0Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV0Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV0Hit", ::TMUV0Hit::Class_Version(), "", 119,
                  typeid(::TMUV0Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV0Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV0Hit) );
      instance.SetNew(&new_TMUV0Hit);
      instance.SetNewArray(&newArray_TMUV0Hit);
      instance.SetDelete(&delete_TMUV0Hit);
      instance.SetDeleteArray(&deleteArray_TMUV0Hit);
      instance.SetDestructor(&destruct_TMUV0Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV0Hit*)
   {
      return GenerateInitInstanceLocal((::TMUV0Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TMUV0Hit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV0Candidate(void *p = 0);
   static void *newArray_TRecoMUV0Candidate(Long_t size, void *p);
   static void delete_TRecoMUV0Candidate(void *p);
   static void deleteArray_TRecoMUV0Candidate(void *p);
   static void destruct_TRecoMUV0Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV0Candidate*)
   {
      ::TRecoMUV0Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV0Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV0Candidate", ::TRecoMUV0Candidate::Class_Version(), "", 142,
                  typeid(::TRecoMUV0Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV0Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV0Candidate) );
      instance.SetNew(&new_TRecoMUV0Candidate);
      instance.SetNewArray(&newArray_TRecoMUV0Candidate);
      instance.SetDelete(&delete_TRecoMUV0Candidate);
      instance.SetDeleteArray(&deleteArray_TRecoMUV0Candidate);
      instance.SetDestructor(&destruct_TRecoMUV0Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV0Candidate*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV0Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV0Candidate*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV0Hit(void *p = 0);
   static void *newArray_TRecoMUV0Hit(Long_t size, void *p);
   static void delete_TRecoMUV0Hit(void *p);
   static void deleteArray_TRecoMUV0Hit(void *p);
   static void destruct_TRecoMUV0Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV0Hit*)
   {
      ::TRecoMUV0Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV0Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV0Hit", ::TRecoMUV0Hit::Class_Version(), "TRecoMUV0Hit.hh", 14,
                  typeid(::TRecoMUV0Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV0Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV0Hit) );
      instance.SetNew(&new_TRecoMUV0Hit);
      instance.SetNewArray(&newArray_TRecoMUV0Hit);
      instance.SetDelete(&delete_TRecoMUV0Hit);
      instance.SetDeleteArray(&deleteArray_TRecoMUV0Hit);
      instance.SetDestructor(&destruct_TRecoMUV0Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV0Hit*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV0Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV0Hit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV0Event(void *p = 0);
   static void *newArray_TRecoMUV0Event(Long_t size, void *p);
   static void delete_TRecoMUV0Event(void *p);
   static void deleteArray_TRecoMUV0Event(void *p);
   static void destruct_TRecoMUV0Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV0Event*)
   {
      ::TRecoMUV0Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV0Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV0Event", ::TRecoMUV0Event::Class_Version(), "", 163,
                  typeid(::TRecoMUV0Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV0Event::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV0Event) );
      instance.SetNew(&new_TRecoMUV0Event);
      instance.SetNewArray(&newArray_TRecoMUV0Event);
      instance.SetDelete(&delete_TRecoMUV0Event);
      instance.SetDeleteArray(&deleteArray_TRecoMUV0Event);
      instance.SetDestructor(&destruct_TRecoMUV0Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV0Event*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV0Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV0Event*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr MUV0ChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *MUV0ChannelID::Class_Name()
{
   return "MUV0ChannelID";
}

//______________________________________________________________________________
const char *MUV0ChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV0ChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int MUV0ChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV0ChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *MUV0ChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV0ChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *MUV0ChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV0ChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV0Digi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV0Digi::Class_Name()
{
   return "TMUV0Digi";
}

//______________________________________________________________________________
const char *TMUV0Digi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Digi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV0Digi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Digi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV0Digi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Digi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV0Digi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Digi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV0Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV0Event::Class_Name()
{
   return "TMUV0Event";
}

//______________________________________________________________________________
const char *TMUV0Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV0Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV0Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV0Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV0Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV0Hit::Class_Name()
{
   return "TMUV0Hit";
}

//______________________________________________________________________________
const char *TMUV0Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV0Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV0Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV0Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV0Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV0Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV0Candidate::Class_Name()
{
   return "TRecoMUV0Candidate";
}

//______________________________________________________________________________
const char *TRecoMUV0Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV0Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV0Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV0Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV0Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV0Hit::Class_Name()
{
   return "TRecoMUV0Hit";
}

//______________________________________________________________________________
const char *TRecoMUV0Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV0Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV0Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV0Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV0Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV0Event::Class_Name()
{
   return "TRecoMUV0Event";
}

//______________________________________________________________________________
const char *TRecoMUV0Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV0Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV0Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV0Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV0Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void MUV0ChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class MUV0ChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(MUV0ChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(MUV0ChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MUV0ChannelID(void *p) {
      return  p ? new(p) ::MUV0ChannelID : new ::MUV0ChannelID;
   }
   static void *newArray_MUV0ChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::MUV0ChannelID[nElements] : new ::MUV0ChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_MUV0ChannelID(void *p) {
      delete ((::MUV0ChannelID*)p);
   }
   static void deleteArray_MUV0ChannelID(void *p) {
      delete [] ((::MUV0ChannelID*)p);
   }
   static void destruct_MUV0ChannelID(void *p) {
      typedef ::MUV0ChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MUV0ChannelID

//______________________________________________________________________________
void TMUV0Digi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV0Digi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV0Digi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV0Digi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV0Digi(void *p) {
      return  p ? new(p) ::TMUV0Digi : new ::TMUV0Digi;
   }
   static void *newArray_TMUV0Digi(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV0Digi[nElements] : new ::TMUV0Digi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV0Digi(void *p) {
      delete ((::TMUV0Digi*)p);
   }
   static void deleteArray_TMUV0Digi(void *p) {
      delete [] ((::TMUV0Digi*)p);
   }
   static void destruct_TMUV0Digi(void *p) {
      typedef ::TMUV0Digi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV0Digi

//______________________________________________________________________________
void TMUV0Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV0Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV0Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV0Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV0Event(void *p) {
      return  p ? new(p) ::TMUV0Event : new ::TMUV0Event;
   }
   static void *newArray_TMUV0Event(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV0Event[nElements] : new ::TMUV0Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV0Event(void *p) {
      delete ((::TMUV0Event*)p);
   }
   static void deleteArray_TMUV0Event(void *p) {
      delete [] ((::TMUV0Event*)p);
   }
   static void destruct_TMUV0Event(void *p) {
      typedef ::TMUV0Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV0Event

//______________________________________________________________________________
void TMUV0Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV0Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV0Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV0Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV0Hit(void *p) {
      return  p ? new(p) ::TMUV0Hit : new ::TMUV0Hit;
   }
   static void *newArray_TMUV0Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV0Hit[nElements] : new ::TMUV0Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV0Hit(void *p) {
      delete ((::TMUV0Hit*)p);
   }
   static void deleteArray_TMUV0Hit(void *p) {
      delete [] ((::TMUV0Hit*)p);
   }
   static void destruct_TMUV0Hit(void *p) {
      typedef ::TMUV0Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV0Hit

//______________________________________________________________________________
void TRecoMUV0Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV0Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV0Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV0Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV0Candidate(void *p) {
      return  p ? new(p) ::TRecoMUV0Candidate : new ::TRecoMUV0Candidate;
   }
   static void *newArray_TRecoMUV0Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV0Candidate[nElements] : new ::TRecoMUV0Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV0Candidate(void *p) {
      delete ((::TRecoMUV0Candidate*)p);
   }
   static void deleteArray_TRecoMUV0Candidate(void *p) {
      delete [] ((::TRecoMUV0Candidate*)p);
   }
   static void destruct_TRecoMUV0Candidate(void *p) {
      typedef ::TRecoMUV0Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV0Candidate

//______________________________________________________________________________
void TRecoMUV0Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV0Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV0Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV0Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV0Hit(void *p) {
      return  p ? new(p) ::TRecoMUV0Hit : new ::TRecoMUV0Hit;
   }
   static void *newArray_TRecoMUV0Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV0Hit[nElements] : new ::TRecoMUV0Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV0Hit(void *p) {
      delete ((::TRecoMUV0Hit*)p);
   }
   static void deleteArray_TRecoMUV0Hit(void *p) {
      delete [] ((::TRecoMUV0Hit*)p);
   }
   static void destruct_TRecoMUV0Hit(void *p) {
      typedef ::TRecoMUV0Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV0Hit

//______________________________________________________________________________
void TRecoMUV0Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV0Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV0Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV0Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV0Event(void *p) {
      return  p ? new(p) ::TRecoMUV0Event : new ::TRecoMUV0Event;
   }
   static void *newArray_TRecoMUV0Event(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV0Event[nElements] : new ::TRecoMUV0Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV0Event(void *p) {
      delete ((::TRecoMUV0Event*)p);
   }
   static void deleteArray_TRecoMUV0Event(void *p) {
      delete [] ((::TRecoMUV0Event*)p);
   }
   static void destruct_TRecoMUV0Event(void *p) {
      typedef ::TRecoMUV0Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV0Event

namespace {
  void TriggerDictionaryInitialization_libMUV0Persistency_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV0/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV0Persistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class MUV0ChannelID;
class TMUV0Digi;
class TMUV0Event;
class TMUV0Hit;
class TRecoMUV0Candidate;
class __attribute__((annotate("$clingAutoload$TRecoMUV0Hit.hh")))  TRecoMUV0Hit;
class TRecoMUV0Event;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV0Persistency dictionary payload"

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
// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-22
// Updated by E Goudzovski (eg@hep.ph.bham.ac.uk) 2016-05-11
//
// ---------------------------------------------------------

#ifndef MUV0CHANNELID_HH_
#define MUV0CHANNELID_HH_

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV0ChannelID {

public:

  MUV0ChannelID();
  virtual ~MUV0ChannelID() {}

  void Clear(Option_t* = "");

  Int_t  EncodeChannelID(); // returns position ID
  void   DecodeChannelID(Int_t);
  Int_t  GetTileID() { return fTileID; }
  
private:

  Int_t  fTileID;

 ClassDef(MUV0ChannelID,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-07-16
// Updated by E Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-05
//
// --------------------------------------------------------------

#ifndef TMUV0Digi_H
#define TMUV0Digi_H

#include "TDCVHit.hh"
#include "MUV0ChannelID.hh"

class TMUV0Digi : public TDCVHit, public MUV0ChannelID {

public:

  TMUV0Digi();
  ~TMUV0Digi() {}

  Int_t  EncodeChannelID();
  void   DecodeChannelID();

  void Clear(Option_t* = "");

  Int_t  GetStationID()     { return 0;                    }
  Bool_t HasLeadingEdge()   { return  GetDetectedEdge()&1; }
  Bool_t HasTrailingEdge()  { return  GetDetectedEdge()&2; }

  Int_t GetThresholdType() const { return fChannelID/10; } //Low Threshold: 0, High Threshold: 1
private:

  ClassDef(TMUV0Digi,1);
};
#endif
#ifndef TMUV0Event_H
#define TMUV0Event_H

#include "TDetectorVEvent.hh"

class TMUV0Event : public TDetectorVEvent {
public:
  TMUV0Event();
  ~TMUV0Event();

  void Clear(Option_t* = "");

private:

  ClassDef(TMUV0Event,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)     2016-05-11
//
// --------------------------------------------------------------

#ifndef TMUV0HIT_H
#define TMUV0HIT_H

#include "TDetectorVHit.hh"
#include "MUV0ChannelID.hh"

class TMUV0Hit : public TDetectorVHit, public MUV0ChannelID {

public:

  TMUV0Hit();
  ~TMUV0Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

protected:
    ClassDef(TMUV0Hit,1);
};
#endif
#ifndef TRecoMUV0Candidate_H
#define TRecoMUV0Candidate_H

#include "TRecoVCandidate.hh"

class TRecoMUV0Candidate : public TRecoVCandidate {

public:
  TRecoMUV0Candidate();
  ~TRecoMUV0Candidate(){};

  void Clear(Option_t* = "");

private:

  ClassDef(TRecoMUV0Candidate,1);
};
#endif
#ifndef TRecoMUV0Event_H
#define TRecoMUV0Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV0Candidate.hh"
#include "TRecoMUV0Hit.hh"

class TRecoMUV0Event : public TRecoVEvent {

    public:

        TRecoMUV0Event();
        ~TRecoMUV0Event();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoMUV0Event,1);
};
#endif
// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-23
//
// ---------------------------------------------------------

#ifndef TRecoMUV0Hit_H
#define TRecoMUV0Hit_H

#include "TRecoVHit.hh"
#include "MUV0ChannelID.hh"

class TRecoMUV0Hit : public TRecoVHit, public MUV0ChannelID {

public:

  TRecoMUV0Hit();
  ~TRecoMUV0Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

public:

  void SetLeadingEdgeLow  (Double_t edgeTime) { fLeadingEdgeLow   = edgeTime; fEdgeMask |= 1; }
  void SetLeadingEdgeHigh (Double_t edgeTime) { fLeadingEdgeHigh  = edgeTime; fEdgeMask |= 2; }
  void SetTrailingEdgeHigh(Double_t edgeTime) { fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4; }
  void SetTrailingEdgeLow (Double_t edgeTime) { fTrailingEdgeLow  = edgeTime; fEdgeMask |= 8; }
  void SetTimeNoT0        (Double_t val)      { fTimeNoT0 = val; }

  Double_t GetLeadingEdgeLow()  { if (fEdgeMask & 1) return fLeadingEdgeLow;   else return 0; }
  Double_t GetLeadingEdgeHigh() { if (fEdgeMask & 2) return fLeadingEdgeHigh;  else return 0; }
  Double_t GetTrailingEdgeHigh(){ if (fEdgeMask & 4) return fTrailingEdgeHigh; else return 0; }
  Double_t GetTrailingEdgeLow() { if (fEdgeMask & 8) return fTrailingEdgeLow;  else return 0; }

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

  Double_t fLeadingEdgeLow;   ///< Time of leading low, subtracted of the trigger time only
  Double_t fTrailingEdgeLow;  ///< Time of leading high, subtracted of the trigger time only
  Double_t fLeadingEdgeHigh;  ///< Time of trailing high, subtracted of the trigger time only
  Double_t fTrailingEdgeHigh; ///< Time of trailing low, subtracted of the trigger time only
  Int_t fLowThresholdROChannelID;
  Int_t fHighThresholdROChannelID;

  ClassDef(TRecoMUV0Hit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"MUV0ChannelID", payloadCode, "@",
"TMUV0Digi", payloadCode, "@",
"TMUV0Event", payloadCode, "@",
"TMUV0Hit", payloadCode, "@",
"TRecoMUV0Candidate", payloadCode, "@",
"TRecoMUV0Event", payloadCode, "@",
"TRecoMUV0Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV0Persistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV0Persistency_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV0Persistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV0Persistency() {
  TriggerDictionaryInitialization_libMUV0Persistency_Impl();
}
