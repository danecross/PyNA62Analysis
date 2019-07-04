// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV1PersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include/MUV1ChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include/TMUV1Digi.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include/TMUV1Event.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include/TMUV1Hit.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include/TRecoMUV1Candidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include/TRecoMUV1Event.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include/TRecoMUV1Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static TClass *MUV1ChannelID_Dictionary();
   static void MUV1ChannelID_TClassManip(TClass*);
   static void *new_MUV1ChannelID(void *p = 0);
   static void *newArray_MUV1ChannelID(Long_t size, void *p);
   static void delete_MUV1ChannelID(void *p);
   static void deleteArray_MUV1ChannelID(void *p);
   static void destruct_MUV1ChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MUV1ChannelID*)
   {
      ::MUV1ChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(::MUV1ChannelID));
      static ::ROOT::TGenericClassInfo 
         instance("MUV1ChannelID", "", 20,
                  typeid(::MUV1ChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &MUV1ChannelID_Dictionary, isa_proxy, 4,
                  sizeof(::MUV1ChannelID) );
      instance.SetNew(&new_MUV1ChannelID);
      instance.SetNewArray(&newArray_MUV1ChannelID);
      instance.SetDelete(&delete_MUV1ChannelID);
      instance.SetDeleteArray(&deleteArray_MUV1ChannelID);
      instance.SetDestructor(&destruct_MUV1ChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MUV1ChannelID*)
   {
      return GenerateInitInstanceLocal((::MUV1ChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::MUV1ChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *MUV1ChannelID_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const ::MUV1ChannelID*)0x0)->GetClass();
      MUV1ChannelID_TClassManip(theClass);
   return theClass;
   }

   static void MUV1ChannelID_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV1Digi(void *p = 0);
   static void *newArray_TMUV1Digi(Long_t size, void *p);
   static void delete_TMUV1Digi(void *p);
   static void deleteArray_TMUV1Digi(void *p);
   static void destruct_TMUV1Digi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV1Digi*)
   {
      ::TMUV1Digi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV1Digi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV1Digi", ::TMUV1Digi::Class_Version(), "", 75,
                  typeid(::TMUV1Digi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV1Digi::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV1Digi) );
      instance.SetNew(&new_TMUV1Digi);
      instance.SetNewArray(&newArray_TMUV1Digi);
      instance.SetDelete(&delete_TMUV1Digi);
      instance.SetDeleteArray(&deleteArray_TMUV1Digi);
      instance.SetDestructor(&destruct_TMUV1Digi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV1Digi*)
   {
      return GenerateInitInstanceLocal((::TMUV1Digi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV1Digi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV1Event(void *p = 0);
   static void *newArray_TMUV1Event(Long_t size, void *p);
   static void delete_TMUV1Event(void *p);
   static void deleteArray_TMUV1Event(void *p);
   static void destruct_TMUV1Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV1Event*)
   {
      ::TMUV1Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV1Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV1Event", ::TMUV1Event::Class_Version(), "", 113,
                  typeid(::TMUV1Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV1Event::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV1Event) );
      instance.SetNew(&new_TMUV1Event);
      instance.SetNewArray(&newArray_TMUV1Event);
      instance.SetDelete(&delete_TMUV1Event);
      instance.SetDeleteArray(&deleteArray_TMUV1Event);
      instance.SetDestructor(&destruct_TMUV1Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV1Event*)
   {
      return GenerateInitInstanceLocal((::TMUV1Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV1Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV1Hit(void *p = 0);
   static void *newArray_TMUV1Hit(Long_t size, void *p);
   static void delete_TMUV1Hit(void *p);
   static void deleteArray_TMUV1Hit(void *p);
   static void destruct_TMUV1Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV1Hit*)
   {
      ::TMUV1Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV1Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV1Hit", ::TMUV1Hit::Class_Version(), "", 142,
                  typeid(::TMUV1Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV1Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV1Hit) );
      instance.SetNew(&new_TMUV1Hit);
      instance.SetNewArray(&newArray_TMUV1Hit);
      instance.SetDelete(&delete_TMUV1Hit);
      instance.SetDeleteArray(&deleteArray_TMUV1Hit);
      instance.SetDestructor(&destruct_TMUV1Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV1Hit*)
   {
      return GenerateInitInstanceLocal((::TMUV1Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV1Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV1Candidate(void *p = 0);
   static void *newArray_TRecoMUV1Candidate(Long_t size, void *p);
   static void delete_TRecoMUV1Candidate(void *p);
   static void deleteArray_TRecoMUV1Candidate(void *p);
   static void destruct_TRecoMUV1Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV1Candidate*)
   {
      ::TRecoMUV1Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV1Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV1Candidate", ::TRecoMUV1Candidate::Class_Version(), "", 215,
                  typeid(::TRecoMUV1Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV1Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV1Candidate) );
      instance.SetNew(&new_TRecoMUV1Candidate);
      instance.SetNewArray(&newArray_TRecoMUV1Candidate);
      instance.SetDelete(&delete_TRecoMUV1Candidate);
      instance.SetDeleteArray(&deleteArray_TRecoMUV1Candidate);
      instance.SetDestructor(&destruct_TRecoMUV1Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV1Candidate*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV1Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV1Candidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV1Hit(void *p = 0);
   static void *newArray_TRecoMUV1Hit(Long_t size, void *p);
   static void delete_TRecoMUV1Hit(void *p);
   static void deleteArray_TRecoMUV1Hit(void *p);
   static void destruct_TRecoMUV1Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV1Hit*)
   {
      ::TRecoMUV1Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV1Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV1Hit", ::TRecoMUV1Hit::Class_Version(), "TRecoMUV1Hit.hh", 15,
                  typeid(::TRecoMUV1Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV1Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV1Hit) );
      instance.SetNew(&new_TRecoMUV1Hit);
      instance.SetNewArray(&newArray_TRecoMUV1Hit);
      instance.SetDelete(&delete_TRecoMUV1Hit);
      instance.SetDeleteArray(&deleteArray_TRecoMUV1Hit);
      instance.SetDestructor(&destruct_TRecoMUV1Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV1Hit*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV1Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV1Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV1Event(void *p = 0);
   static void *newArray_TRecoMUV1Event(Long_t size, void *p);
   static void delete_TRecoMUV1Event(void *p);
   static void deleteArray_TRecoMUV1Event(void *p);
   static void destruct_TRecoMUV1Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV1Event*)
   {
      ::TRecoMUV1Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV1Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV1Event", ::TRecoMUV1Event::Class_Version(), "", 347,
                  typeid(::TRecoMUV1Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV1Event::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV1Event) );
      instance.SetNew(&new_TRecoMUV1Event);
      instance.SetNewArray(&newArray_TRecoMUV1Event);
      instance.SetDelete(&delete_TRecoMUV1Event);
      instance.SetDeleteArray(&deleteArray_TRecoMUV1Event);
      instance.SetDestructor(&destruct_TRecoMUV1Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV1Event*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV1Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV1Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TMUV1Digi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV1Digi::Class_Name()
{
   return "TMUV1Digi";
}

//______________________________________________________________________________
const char *TMUV1Digi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Digi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV1Digi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Digi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV1Digi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Digi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV1Digi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Digi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV1Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV1Event::Class_Name()
{
   return "TMUV1Event";
}

//______________________________________________________________________________
const char *TMUV1Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV1Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV1Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV1Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV1Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV1Hit::Class_Name()
{
   return "TMUV1Hit";
}

//______________________________________________________________________________
const char *TMUV1Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV1Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV1Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV1Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV1Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV1Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV1Candidate::Class_Name()
{
   return "TRecoMUV1Candidate";
}

//______________________________________________________________________________
const char *TRecoMUV1Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV1Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV1Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV1Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV1Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV1Hit::Class_Name()
{
   return "TRecoMUV1Hit";
}

//______________________________________________________________________________
const char *TRecoMUV1Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV1Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV1Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV1Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV1Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV1Event::Class_Name()
{
   return "TRecoMUV1Event";
}

//______________________________________________________________________________
const char *TRecoMUV1Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV1Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV1Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV1Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV1Event*)0x0)->GetClass(); }
   return fgIsA;
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MUV1ChannelID(void *p) {
      return  p ? new(p) ::MUV1ChannelID : new ::MUV1ChannelID;
   }
   static void *newArray_MUV1ChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::MUV1ChannelID[nElements] : new ::MUV1ChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_MUV1ChannelID(void *p) {
      delete ((::MUV1ChannelID*)p);
   }
   static void deleteArray_MUV1ChannelID(void *p) {
      delete [] ((::MUV1ChannelID*)p);
   }
   static void destruct_MUV1ChannelID(void *p) {
      typedef ::MUV1ChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MUV1ChannelID

//______________________________________________________________________________
void TMUV1Digi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV1Digi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV1Digi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV1Digi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV1Digi(void *p) {
      return  p ? new(p) ::TMUV1Digi : new ::TMUV1Digi;
   }
   static void *newArray_TMUV1Digi(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV1Digi[nElements] : new ::TMUV1Digi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV1Digi(void *p) {
      delete ((::TMUV1Digi*)p);
   }
   static void deleteArray_TMUV1Digi(void *p) {
      delete [] ((::TMUV1Digi*)p);
   }
   static void destruct_TMUV1Digi(void *p) {
      typedef ::TMUV1Digi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV1Digi

//______________________________________________________________________________
void TMUV1Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV1Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV1Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV1Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV1Event(void *p) {
      return  p ? new(p) ::TMUV1Event : new ::TMUV1Event;
   }
   static void *newArray_TMUV1Event(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV1Event[nElements] : new ::TMUV1Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV1Event(void *p) {
      delete ((::TMUV1Event*)p);
   }
   static void deleteArray_TMUV1Event(void *p) {
      delete [] ((::TMUV1Event*)p);
   }
   static void destruct_TMUV1Event(void *p) {
      typedef ::TMUV1Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV1Event

//______________________________________________________________________________
void TMUV1Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV1Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV1Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV1Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV1Hit(void *p) {
      return  p ? new(p) ::TMUV1Hit : new ::TMUV1Hit;
   }
   static void *newArray_TMUV1Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV1Hit[nElements] : new ::TMUV1Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV1Hit(void *p) {
      delete ((::TMUV1Hit*)p);
   }
   static void deleteArray_TMUV1Hit(void *p) {
      delete [] ((::TMUV1Hit*)p);
   }
   static void destruct_TMUV1Hit(void *p) {
      typedef ::TMUV1Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV1Hit

//______________________________________________________________________________
void TRecoMUV1Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV1Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV1Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV1Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV1Candidate(void *p) {
      return  p ? new(p) ::TRecoMUV1Candidate : new ::TRecoMUV1Candidate;
   }
   static void *newArray_TRecoMUV1Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV1Candidate[nElements] : new ::TRecoMUV1Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV1Candidate(void *p) {
      delete ((::TRecoMUV1Candidate*)p);
   }
   static void deleteArray_TRecoMUV1Candidate(void *p) {
      delete [] ((::TRecoMUV1Candidate*)p);
   }
   static void destruct_TRecoMUV1Candidate(void *p) {
      typedef ::TRecoMUV1Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV1Candidate

//______________________________________________________________________________
void TRecoMUV1Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV1Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV1Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV1Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV1Hit(void *p) {
      return  p ? new(p) ::TRecoMUV1Hit : new ::TRecoMUV1Hit;
   }
   static void *newArray_TRecoMUV1Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV1Hit[nElements] : new ::TRecoMUV1Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV1Hit(void *p) {
      delete ((::TRecoMUV1Hit*)p);
   }
   static void deleteArray_TRecoMUV1Hit(void *p) {
      delete [] ((::TRecoMUV1Hit*)p);
   }
   static void destruct_TRecoMUV1Hit(void *p) {
      typedef ::TRecoMUV1Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV1Hit

//______________________________________________________________________________
void TRecoMUV1Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV1Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV1Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV1Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV1Event(void *p) {
      return  p ? new(p) ::TRecoMUV1Event : new ::TRecoMUV1Event;
   }
   static void *newArray_TRecoMUV1Event(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV1Event[nElements] : new ::TRecoMUV1Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV1Event(void *p) {
      delete ((::TRecoMUV1Event*)p);
   }
   static void deleteArray_TRecoMUV1Event(void *p) {
      delete [] ((::TRecoMUV1Event*)p);
   }
   static void destruct_TRecoMUV1Event(void *p) {
      typedef ::TRecoMUV1Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV1Event

namespace {
  void TriggerDictionaryInitialization_libMUV1Persistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV1Persistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class MUV1ChannelID;
class TMUV1Digi;
class TMUV1Event;
class TMUV1Hit;
class TRecoMUV1Candidate;
class __attribute__((annotate("$clingAutoload$TRecoMUV1Hit.hh")))  TRecoMUV1Hit;
class TRecoMUV1Event;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV1Persistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
//
//  MUV1ChannelID.hh
//
//
//  Created by riccardo aliberti on 10/10/14.
//
//

#ifndef _MUV1ChannelID_hh
#define _MUV1ChannelID_hh

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV1ChannelID {

public:
  struct chIDDecoded { Int_t fScintillatorNumber, fSide; };
  MUV1ChannelID();
  explicit MUV1ChannelID(Int_t ChannelID);
  virtual ~MUV1ChannelID();

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  static chIDDecoded DecodeChannelID_Static(Int_t ChannelID);
  void DecodeChannelID(Int_t ChannelID);

  
  Double_t GetScintillatorPosition ();

  Int_t GetScintillatorOrientation (){ return (fSide%2); }

  Int_t GetPlane ();

  Int_t GetQuadrant ();

  void  SetSide (Int_t side) { fSide = side; }
  Int_t GetSide () { return fSide; }

  void  SetScintillatorNumber (Int_t num) { fScintillatorNumber = num; }
  Int_t GetScintillatorNumber (){ return fScintillatorNumber; }

  Bool_t IsLongScintillator ();


private:
  Int_t fScintillatorNumber;
  Int_t fSide;

};


#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
// Copied and modified from GTK by Mario Vormstein (mario.vormstein@cern.ch) 2012-03-07
//
// --------------------------------------------------------------
#ifndef TMUV1Digi_H
#define TMUV1Digi_H

#include "FADCVHit.hh"
#include "MUV1ChannelID.hh"
#include "TVector3.h"

class TMUV1Digi : public FADCVHit , public MUV1ChannelID {

  public:

    TMUV1Digi();
    ~TMUV1Digi(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  public:


  private:


    ClassDef(TMUV1Digi,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TMUV1Event_H
#define TMUV1Event_H

#include "TDetectorVEvent.hh"

class TMUV1Event : public TDetectorVEvent {

    public:

      TMUV1Event();
      ~TMUV1Event();

      void Clear(Option_t* = "");

    private:

      ClassDef(TMUV1Event,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-11
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TMUV1Hit_H
#define TMUV1Hit_H

#include "TDetectorVHit.hh"

class TMUV1Hit: public TDetectorVHit {

  public:

    TMUV1Hit();
    ~TMUV1Hit() {};

    void Clear(Option_t* = "");

    Int_t GetStationID() { return 0; }

    void  SetPlane (Int_t plane) { fPlane = plane; }
    Int_t GetPlane ()	{ return fPlane; }

    void SetChannelID(Int_t ChID);


  public:

    Int_t GetScintillatorID() {
      return fScintillatorID;
    };
    void SetScintillatorID(Int_t value) {
      fScintillatorID = value;
    };

    Int_t GetPhotons() {
      return fPhotons;
    };

    void SetPhotons(Int_t value) {
      fPhotons = value;
    };
    double GetStepLength() {
      return fStepLength;
    };
    void SetStepLength(double value) {
      fStepLength = value;
    };

    double GetPositionInScintillator() {
      return fPositionInScintillator;
    };
    void SetPositionInScintillator(double value) {
      fPositionInScintillator = value;
    };

  protected:

    Int_t fScintillatorID;
    Int_t fPhotons;
	Int_t fPlane;

    double fPositionInScintillator;
    double fStepLength;

    ClassDef(TMUV1Hit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TRecoMUV1Candidate_H
#define TRecoMUV1Candidate_H

#include "TRecoVCandidate.hh"

class TRecoMUV1Candidate : public TRecoVCandidate {

  public:

    TRecoMUV1Candidate();
    ~TRecoMUV1Candidate(){};

    void Clear(Option_t* = "");

    void SetTimeHorizontal(Double_t value){ fTimeX=value; }
    void SetTimeVertical(Double_t value){ fTimeY=value; }

    void SetTimeSigmaHorizontal (Double_t value){ fTimeSigmaX = value; }
    void SetTimeSigmaVertical (Double_t value){ fTimeSigmaY = value; }

    void SetShowerWidthHorizontal(Double_t value ){	fShowerWidthHorizontal=value;	}
    void SetShowerWidthVertical(Double_t value ){	fShowerWidthVertical=value;	}
    void SetShowerWidth(Double_t value ){	fShowerWidth=value;	}

    void SetCharge (Double_t value ){	fCharge=value;	}
    void SetChargeHorizontal (Double_t value){ fChargeX=value; }
    void SetChargeVertical (Double_t value){ fChargeY=value; }

    void SetEnergyHorizontal (Double_t value){ fEnergyX=value; }
    void SetEnergyVertical (Double_t value){ fEnergyY=value; }
    void SetEnergy (Double_t value){ fEnergy=value; }

    void SetSeedEnergyHorizontal(Double_t value){ fSeedEnergyX=value; }
    void SetSeedIndexHorizontal(Int_t value) { fSeedIndexX=value; }

    void SetSeedEnergyVertical(Double_t value){ fSeedEnergyY=value; }
    void SetSeedIndexVertical(Int_t value) { fSeedIndexY=value; }

    void SetInnerEnergyHorizontal(Double_t value){ fInnerEnergyX=value; }
    void SetInnerEnergyVertical(Double_t value){ fInnerEnergyY=value; }

    void SetPosition (Double_t x, Double_t y ){	fPosition.Set(x,y);	}
    void SetPosition (TVector2 value ){ fPosition = value; }

    void SetQuadrant(Int_t value ){	fQuadrant=value;	}
    void SetVerticalIndex(Int_t value){ fVerticalIndex=value;}
    void SetHorizontalIndex(Int_t value){ fHorizontalIndex=value;}
    void SetVerticalChannel(Int_t value){ fVerticalChannel=value;}
    void SetHorizontalChannel(Int_t value){ fHorizontalChannel=value;}


    Double_t GetTimeHorizontal(){ return fTimeX; }
    Double_t GetTimeSigmaHorizontal(){ return fTimeSigmaX; }
    Double_t GetTimeVertical(){ return fTimeY; }
    Double_t GetTimeSigmaVertical(){ return fTimeSigmaY; }

    Double_t GetPlaneTimeDiff(){ return (fTimeX - fTimeY);}

    Double_t GetShowerWidthHorizontal(){	return fShowerWidthHorizontal;	}
    Double_t GetShowerWidthVertical(){	return fShowerWidthVertical;	}
    Double_t GetShowerWidth(){	return fShowerWidth;	}

    Double_t GetX(){	return fPosition.X();	}
    Double_t GetY(){	return fPosition.Y();	}
    TVector2 GetPosition(){ return fPosition; }

    Double_t GetCharge () { return fCharge; }
    Double_t GetChargeHorizontal(){ return fChargeX; }
    Double_t GetChargeVertical (){ return fChargeY;}

    Double_t GetEnergyHorizontal () { return fEnergyX; }
    Double_t GetEnergyVertical () { return fEnergyY; }
    Double_t GetEnergy () { return fEnergy; }

    Double_t GetSeedEnergyHorizontal(){ return fSeedEnergyX; }
    Int_t GetSeedIndexHorizontal() { return fSeedIndexX; }

    Double_t GetSeedEnergyVertical(){ return fSeedEnergyY; }
    Int_t GetSeedIndexVertical() { return fSeedIndexY; }

    Double_t GetSeedEnergy() { return (fSeedEnergyX + fSeedEnergyY); }

    Double_t GetInnerEnergyHorizontal(){ return fInnerEnergyX; }
    Double_t GetInnerEnergyVertical(){ return fInnerEnergyY; }
    Double_t GetInnerEnergy(){ return (fInnerEnergyX + fInnerEnergyY); }

    Int_t GetQuadrant(){ return fQuadrant;};
    Int_t GetVerticalIndex(){ return fVerticalIndex;}
    Int_t GetHorizontalIndex(){ return fHorizontalIndex;}
    Int_t GetVerticalChannel(){ return fVerticalChannel;}
    Int_t GetHorizontalChannel(){ return fHorizontalChannel;}

  private:

    Double_t fTimeX, fTimeY;            ///< Time from Horizontal and Vertical planes
    Double_t fTimeSigmaX, fTimeSigmaY;  ///< Standard Deviation of the hit time in Horizontal and Vertical planes

    Double_t fShowerWidth;              ///< Standard deviation of the shower assuming a gaussian shape
    Double_t fShowerWidthHorizontal, fShowerWidthVertical;    ///< As before for Horizontal and Vertical planes

    TVector2 fPosition; ///< Center of the Cluster

    Double_t fCharge, fChargeX, fChargeY; ///< Charge collected: Total, on Horizontal and Vertical planes
    Double_t fEnergy, fEnergyX, fEnergyY; ///< Energy collected: Total, on Horizontal and Vertical planes

    Double_t fSeedEnergyX, fSeedEnergyY;  ///< Energy in the most energetic channel in Horizontal and Vertical planes
    Int_t fSeedIndexX, fSeedIndexY;       ///< Index of the hit giving the seed for Horizontal and Vertical planes

    Double_t fInnerEnergyX, fInnerEnergyY; ///< Energy collected in ±1 channel from the seed

    Int_t fQuadrant;            ///< Quadrant containing the cluster
    Int_t fVerticalIndex;       ///< Vertical Readout side 1 = Saleve, 3 = Jura
    Int_t fHorizontalIndex;     ///< Horizontal Readout side 0 = Bottom, 2 = Top
    Int_t fHorizontalChannel;   ///< Central channel on Horizontal (1 - 22, ChannelID = 100 + 50 * HorizontalIndex + HorizontalChannel)
    Int_t fVerticalChannel;     ///< Central channel on Vertical (1 - 22, ChannelID = 100 + 50 * VerticalIndex + VerticalChannel)



    ClassDef(TRecoMUV1Candidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TRecoMUV1Event_H
#define TRecoMUV1Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV1Candidate.hh"
#include "TRecoMUV1Hit.hh"

class TRecoMUV1Event : public TRecoVEvent {

    public:

        TRecoMUV1Event();
        ~TRecoMUV1Event();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoMUV1Event,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TRecoMUV1Hit_H
#define TRecoMUV1Hit_H

#include "TRecoVHit.hh"
#include "MUV1ChannelID.hh"

class TRecoMUV1Hit : public TRecoVHit , public MUV1ChannelID {

  public:

    TRecoMUV1Hit();
    ~TRecoMUV1Hit(){};

    void Clear(Option_t* = "");

    void DecodeChannelID(Int_t ChannelID){  fChannelID = ChannelID; MUV1ChannelID::DecodeChannelID(fChannelID); }

  public:
    void SetPeakAmplitude (Double_t Amp) { fPeakAmplitude = Amp; }
    Double_t GetPeakAmplitude () { return fPeakAmplitude; }

    void SetCharge (Double_t Q) { fCharge = Q; }
    Double_t GetCharge () { return fCharge; }

    void SetSigma (Double_t sig){ fSigma = sig; }
    Double_t GetSigma (){ return fSigma; }

    void SetTimeError(Double_t Terr) { fTimeError = Terr; }
    void SetSigmaError(Double_t Serr) { fSigmaError = Serr; }
    void SetChargeError(Double_t Qerr) { fChargeError = Qerr; }
    void SetAmplitudeError(Double_t Aerr){ fPeakError = Aerr; }

    Double_t GetTimeError () { return fTimeError; }
    Double_t GetSigmaError (){ return fSigmaError; }
    Double_t GetChargeError () {return fChargeError; }
    Double_t GetAmplitudeError () {return fPeakError; }


  private:
    Double_t fTimeError;    ///< Error on the time in ns
    Double_t fPeakAmplitude;///< Amplitude of the peak in mV
    Double_t fPeakError;    ///< Error on the amplitude
    Double_t fCharge;       ///< Charge collected in pC
    Double_t fChargeError;  ///< Error on the charge
    Double_t fSigma;        ///< Sigma of the gaussian peak in ns
    Double_t fSigmaError;   ///< Error on the sigma

    ClassDef(TRecoMUV1Hit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"MUV1ChannelID", payloadCode, "@",
"TMUV1Digi", payloadCode, "@",
"TMUV1Event", payloadCode, "@",
"TMUV1Hit", payloadCode, "@",
"TRecoMUV1Candidate", payloadCode, "@",
"TRecoMUV1Event", payloadCode, "@",
"TRecoMUV1Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV1Persistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV1Persistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV1Persistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV1Persistency() {
  TriggerDictionaryInitialization_libMUV1Persistency_Impl();
}
