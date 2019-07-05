// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV2PersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include/MUV2ChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include/TMUV2Digi.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include/TMUV2Event.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include/TMUV2Hit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include/TRecoMUV2Candidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include/TRecoMUV2Event.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include/TRecoMUV2Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static TClass *MUV2ChannelID_Dictionary();
   static void MUV2ChannelID_TClassManip(TClass*);
   static void *new_MUV2ChannelID(void *p = 0);
   static void *newArray_MUV2ChannelID(Long_t size, void *p);
   static void delete_MUV2ChannelID(void *p);
   static void deleteArray_MUV2ChannelID(void *p);
   static void destruct_MUV2ChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MUV2ChannelID*)
   {
      ::MUV2ChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(::MUV2ChannelID));
      static ::ROOT::TGenericClassInfo 
         instance("MUV2ChannelID", "", 20,
                  typeid(::MUV2ChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &MUV2ChannelID_Dictionary, isa_proxy, 4,
                  sizeof(::MUV2ChannelID) );
      instance.SetNew(&new_MUV2ChannelID);
      instance.SetNewArray(&newArray_MUV2ChannelID);
      instance.SetDelete(&delete_MUV2ChannelID);
      instance.SetDeleteArray(&deleteArray_MUV2ChannelID);
      instance.SetDestructor(&destruct_MUV2ChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MUV2ChannelID*)
   {
      return GenerateInitInstanceLocal((::MUV2ChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::MUV2ChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *MUV2ChannelID_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const ::MUV2ChannelID*)0x0)->GetClass();
      MUV2ChannelID_TClassManip(theClass);
   return theClass;
   }

   static void MUV2ChannelID_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV2Digi(void *p = 0);
   static void *newArray_TMUV2Digi(Long_t size, void *p);
   static void delete_TMUV2Digi(void *p);
   static void deleteArray_TMUV2Digi(void *p);
   static void destruct_TMUV2Digi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV2Digi*)
   {
      ::TMUV2Digi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV2Digi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV2Digi", ::TMUV2Digi::Class_Version(), "", 75,
                  typeid(::TMUV2Digi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV2Digi::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV2Digi) );
      instance.SetNew(&new_TMUV2Digi);
      instance.SetNewArray(&newArray_TMUV2Digi);
      instance.SetDelete(&delete_TMUV2Digi);
      instance.SetDeleteArray(&deleteArray_TMUV2Digi);
      instance.SetDestructor(&destruct_TMUV2Digi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV2Digi*)
   {
      return GenerateInitInstanceLocal((::TMUV2Digi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV2Digi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV2Event(void *p = 0);
   static void *newArray_TMUV2Event(Long_t size, void *p);
   static void delete_TMUV2Event(void *p);
   static void deleteArray_TMUV2Event(void *p);
   static void destruct_TMUV2Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV2Event*)
   {
      ::TMUV2Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV2Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV2Event", ::TMUV2Event::Class_Version(), "", 111,
                  typeid(::TMUV2Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV2Event::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV2Event) );
      instance.SetNew(&new_TMUV2Event);
      instance.SetNewArray(&newArray_TMUV2Event);
      instance.SetDelete(&delete_TMUV2Event);
      instance.SetDeleteArray(&deleteArray_TMUV2Event);
      instance.SetDestructor(&destruct_TMUV2Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV2Event*)
   {
      return GenerateInitInstanceLocal((::TMUV2Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV2Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TMUV2Hit(void *p = 0);
   static void *newArray_TMUV2Hit(Long_t size, void *p);
   static void delete_TMUV2Hit(void *p);
   static void deleteArray_TMUV2Hit(void *p);
   static void destruct_TMUV2Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TMUV2Hit*)
   {
      ::TMUV2Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TMUV2Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TMUV2Hit", ::TMUV2Hit::Class_Version(), "", 140,
                  typeid(::TMUV2Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TMUV2Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TMUV2Hit) );
      instance.SetNew(&new_TMUV2Hit);
      instance.SetNewArray(&newArray_TMUV2Hit);
      instance.SetDelete(&delete_TMUV2Hit);
      instance.SetDeleteArray(&deleteArray_TMUV2Hit);
      instance.SetDestructor(&destruct_TMUV2Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TMUV2Hit*)
   {
      return GenerateInitInstanceLocal((::TMUV2Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TMUV2Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV2Candidate(void *p = 0);
   static void *newArray_TRecoMUV2Candidate(Long_t size, void *p);
   static void delete_TRecoMUV2Candidate(void *p);
   static void deleteArray_TRecoMUV2Candidate(void *p);
   static void destruct_TRecoMUV2Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV2Candidate*)
   {
      ::TRecoMUV2Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV2Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV2Candidate", ::TRecoMUV2Candidate::Class_Version(), "", 193,
                  typeid(::TRecoMUV2Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV2Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV2Candidate) );
      instance.SetNew(&new_TRecoMUV2Candidate);
      instance.SetNewArray(&newArray_TRecoMUV2Candidate);
      instance.SetDelete(&delete_TRecoMUV2Candidate);
      instance.SetDeleteArray(&deleteArray_TRecoMUV2Candidate);
      instance.SetDestructor(&destruct_TRecoMUV2Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV2Candidate*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV2Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV2Candidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV2Hit(void *p = 0);
   static void *newArray_TRecoMUV2Hit(Long_t size, void *p);
   static void delete_TRecoMUV2Hit(void *p);
   static void deleteArray_TRecoMUV2Hit(void *p);
   static void destruct_TRecoMUV2Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV2Hit*)
   {
      ::TRecoMUV2Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV2Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV2Hit", ::TRecoMUV2Hit::Class_Version(), "TRecoMUV2Hit.hh", 16,
                  typeid(::TRecoMUV2Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV2Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV2Hit) );
      instance.SetNew(&new_TRecoMUV2Hit);
      instance.SetNewArray(&newArray_TRecoMUV2Hit);
      instance.SetDelete(&delete_TRecoMUV2Hit);
      instance.SetDeleteArray(&deleteArray_TRecoMUV2Hit);
      instance.SetDestructor(&destruct_TRecoMUV2Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV2Hit*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV2Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV2Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoMUV2Event(void *p = 0);
   static void *newArray_TRecoMUV2Event(Long_t size, void *p);
   static void delete_TRecoMUV2Event(void *p);
   static void deleteArray_TRecoMUV2Event(void *p);
   static void destruct_TRecoMUV2Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoMUV2Event*)
   {
      ::TRecoMUV2Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoMUV2Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoMUV2Event", ::TRecoMUV2Event::Class_Version(), "", 327,
                  typeid(::TRecoMUV2Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoMUV2Event::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoMUV2Event) );
      instance.SetNew(&new_TRecoMUV2Event);
      instance.SetNewArray(&newArray_TRecoMUV2Event);
      instance.SetDelete(&delete_TRecoMUV2Event);
      instance.SetDeleteArray(&deleteArray_TRecoMUV2Event);
      instance.SetDestructor(&destruct_TRecoMUV2Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoMUV2Event*)
   {
      return GenerateInitInstanceLocal((::TRecoMUV2Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoMUV2Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TMUV2Digi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV2Digi::Class_Name()
{
   return "TMUV2Digi";
}

//______________________________________________________________________________
const char *TMUV2Digi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Digi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV2Digi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Digi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV2Digi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Digi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV2Digi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Digi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV2Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV2Event::Class_Name()
{
   return "TMUV2Event";
}

//______________________________________________________________________________
const char *TMUV2Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV2Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV2Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV2Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TMUV2Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TMUV2Hit::Class_Name()
{
   return "TMUV2Hit";
}

//______________________________________________________________________________
const char *TMUV2Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TMUV2Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TMUV2Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TMUV2Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TMUV2Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV2Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV2Candidate::Class_Name()
{
   return "TRecoMUV2Candidate";
}

//______________________________________________________________________________
const char *TRecoMUV2Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV2Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV2Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV2Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV2Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV2Hit::Class_Name()
{
   return "TRecoMUV2Hit";
}

//______________________________________________________________________________
const char *TRecoMUV2Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV2Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV2Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV2Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoMUV2Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoMUV2Event::Class_Name()
{
   return "TRecoMUV2Event";
}

//______________________________________________________________________________
const char *TRecoMUV2Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoMUV2Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoMUV2Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoMUV2Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoMUV2Event*)0x0)->GetClass(); }
   return fgIsA;
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MUV2ChannelID(void *p) {
      return  p ? new(p) ::MUV2ChannelID : new ::MUV2ChannelID;
   }
   static void *newArray_MUV2ChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::MUV2ChannelID[nElements] : new ::MUV2ChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_MUV2ChannelID(void *p) {
      delete ((::MUV2ChannelID*)p);
   }
   static void deleteArray_MUV2ChannelID(void *p) {
      delete [] ((::MUV2ChannelID*)p);
   }
   static void destruct_MUV2ChannelID(void *p) {
      typedef ::MUV2ChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MUV2ChannelID

//______________________________________________________________________________
void TMUV2Digi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV2Digi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV2Digi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV2Digi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV2Digi(void *p) {
      return  p ? new(p) ::TMUV2Digi : new ::TMUV2Digi;
   }
   static void *newArray_TMUV2Digi(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV2Digi[nElements] : new ::TMUV2Digi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV2Digi(void *p) {
      delete ((::TMUV2Digi*)p);
   }
   static void deleteArray_TMUV2Digi(void *p) {
      delete [] ((::TMUV2Digi*)p);
   }
   static void destruct_TMUV2Digi(void *p) {
      typedef ::TMUV2Digi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV2Digi

//______________________________________________________________________________
void TMUV2Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV2Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV2Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV2Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV2Event(void *p) {
      return  p ? new(p) ::TMUV2Event : new ::TMUV2Event;
   }
   static void *newArray_TMUV2Event(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV2Event[nElements] : new ::TMUV2Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV2Event(void *p) {
      delete ((::TMUV2Event*)p);
   }
   static void deleteArray_TMUV2Event(void *p) {
      delete [] ((::TMUV2Event*)p);
   }
   static void destruct_TMUV2Event(void *p) {
      typedef ::TMUV2Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV2Event

//______________________________________________________________________________
void TMUV2Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TMUV2Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TMUV2Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TMUV2Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TMUV2Hit(void *p) {
      return  p ? new(p) ::TMUV2Hit : new ::TMUV2Hit;
   }
   static void *newArray_TMUV2Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TMUV2Hit[nElements] : new ::TMUV2Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TMUV2Hit(void *p) {
      delete ((::TMUV2Hit*)p);
   }
   static void deleteArray_TMUV2Hit(void *p) {
      delete [] ((::TMUV2Hit*)p);
   }
   static void destruct_TMUV2Hit(void *p) {
      typedef ::TMUV2Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TMUV2Hit

//______________________________________________________________________________
void TRecoMUV2Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV2Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV2Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV2Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV2Candidate(void *p) {
      return  p ? new(p) ::TRecoMUV2Candidate : new ::TRecoMUV2Candidate;
   }
   static void *newArray_TRecoMUV2Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV2Candidate[nElements] : new ::TRecoMUV2Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV2Candidate(void *p) {
      delete ((::TRecoMUV2Candidate*)p);
   }
   static void deleteArray_TRecoMUV2Candidate(void *p) {
      delete [] ((::TRecoMUV2Candidate*)p);
   }
   static void destruct_TRecoMUV2Candidate(void *p) {
      typedef ::TRecoMUV2Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV2Candidate

//______________________________________________________________________________
void TRecoMUV2Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV2Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV2Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV2Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV2Hit(void *p) {
      return  p ? new(p) ::TRecoMUV2Hit : new ::TRecoMUV2Hit;
   }
   static void *newArray_TRecoMUV2Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV2Hit[nElements] : new ::TRecoMUV2Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV2Hit(void *p) {
      delete ((::TRecoMUV2Hit*)p);
   }
   static void deleteArray_TRecoMUV2Hit(void *p) {
      delete [] ((::TRecoMUV2Hit*)p);
   }
   static void destruct_TRecoMUV2Hit(void *p) {
      typedef ::TRecoMUV2Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV2Hit

//______________________________________________________________________________
void TRecoMUV2Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoMUV2Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoMUV2Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoMUV2Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoMUV2Event(void *p) {
      return  p ? new(p) ::TRecoMUV2Event : new ::TRecoMUV2Event;
   }
   static void *newArray_TRecoMUV2Event(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoMUV2Event[nElements] : new ::TRecoMUV2Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoMUV2Event(void *p) {
      delete ((::TRecoMUV2Event*)p);
   }
   static void deleteArray_TRecoMUV2Event(void *p) {
      delete [] ((::TRecoMUV2Event*)p);
   }
   static void destruct_TRecoMUV2Event(void *p) {
      typedef ::TRecoMUV2Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoMUV2Event

namespace {
  void TriggerDictionaryInitialization_libMUV2Persistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV2/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV2Persistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class MUV2ChannelID;
class TMUV2Digi;
class TMUV2Event;
class TMUV2Hit;
class TRecoMUV2Candidate;
class __attribute__((annotate("$clingAutoload$TRecoMUV2Hit.hh")))  TRecoMUV2Hit;
class TRecoMUV2Event;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV2Persistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
//
//  MUV2ChannelID.hh
//
//
//  Created by riccardo aliberti on 10/10/14.
//
//

#ifndef _MUV2ChannelID_hh
#define _MUV2ChannelID_hh

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV2ChannelID {

  public:
    struct chIDDecoded { Int_t fScintillatorNumber, fSide; };
    MUV2ChannelID();
    explicit MUV2ChannelID(Int_t ChannelID);
    virtual ~MUV2ChannelID();

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    static chIDDecoded DecodeChannelID_Static(Int_t ChannelID);
    void DecodeChannelID(Int_t ChannelID);


  Double_t GetScintillatorPosition ();

  Int_t GetScintillatorOrientation (){ return std::max(fSide%2,0); }

  Int_t GetPlane ();

  Int_t GetQuadrant ();

  void  SetSide (Int_t side) { fSide = side; }
  Int_t GetSide () { return fSide; }

  void  SetScintillatorNumber (Int_t num) { fScintillatorNumber = num; }
  Int_t GetScintillatorNumber (){ return fScintillatorNumber; }

  
  private:
    Int_t fScintillatorNumber;
    Int_t fSide;
    Double_t fScintillatorPosition;


};


#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
// Copied and modified from GTK by Mario Vormstein (mario.vormstein@cern.ch) 2012-03-07
//
// --------------------------------------------------------------
#ifndef TMUV2Digi_H
#define TMUV2Digi_H

#include "FADCVHit.hh"
#include "MUV2ChannelID.hh"
#include "TVector3.h"

class TMUV2Digi : public FADCVHit , public MUV2ChannelID {

  public:

    TMUV2Digi();

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID () { return 0; }



  private:


    ClassDef(TMUV2Digi,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#ifndef TMUV2Event_H
#define TMUV2Event_H

#include "TDetectorVEvent.hh"

class TMUV2Event : public TDetectorVEvent {

    public:

      TMUV2Event();
      ~TMUV2Event();

      void Clear(Option_t* = "");

    private:

        ClassDef(TMUV2Event,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-11
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#ifndef TMUV2Hit_H
#define TMUV2Hit_H

#include "TDetectorVHit.hh"

class TMUV2Hit : public TDetectorVHit {

  public:

    TMUV2Hit();
    ~TMUV2Hit(){};

    void Clear(Option_t* = "");

    Int_t GetStationID () { return 0; }

    void  SetPlane (Int_t plane) { fPlane = plane; }
    Int_t GetPlane ()	{ return fPlane; }
	

    void SetChannelID(Int_t ChID);


  public:

    Int_t	GetScintillatorID()             {  return fScintillatorID; };
    void	SetScintillatorID(Int_t value)  {  fScintillatorID = value;};
    double	GetStepLength()                 {  return fStepLength;     };
    void	SetStepLength(double value)     {  fStepLength = value;    };
    double GetPositionInScintillator()      {   return fPositionInScintillator;};
    void SetPositionInScintillator(double value) {fPositionInScintillator = value;};

  protected:

    Int_t	fScintillatorID;
	Int_t	fPlane;
	
    double	fStepLength;
    double fPositionInScintillator;

    ClassDef(TMUV2Hit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
// Modified by Riccardo Aliberti (riccardo.aliberti@cern.ch) 2015-03-12
//
// --------------------------------------------------------------
#ifndef TRecoMUV2Candidate_H
#define TRecoMUV2Candidate_H

#include "TRecoVCandidate.hh"

class TRecoMUV2Candidate : public TRecoVCandidate {

public:

    TRecoMUV2Candidate();
    ~TRecoMUV2Candidate(){};

    void Clear(Option_t* = "");

public:

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



    ClassDef(TRecoMUV2Candidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#ifndef TRecoMUV2Event_H
#define TRecoMUV2Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV2Candidate.hh"
#include "TRecoMUV2Hit.hh"

class TRecoMUV2Event : public TRecoVEvent {

    public:

      TRecoMUV2Event();
      ~TRecoMUV2Event();

      void Clear(Option_t* = "");

    private:
      ClassDef(TRecoMUV2Event,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
// Modified by Riccardo Aliberti (riccardo.aliberti@cern.ch) 2015-03-12
//
// --------------------------------------------------------------
#ifndef TRecoMUV2Hit_H
#define TRecoMUV2Hit_H

#include "TRecoVHit.hh"
#include "MUV2ChannelID.hh"

class TRecoMUV2Hit : public TRecoVHit , public MUV2ChannelID {

  public:

    TRecoMUV2Hit();
    ~TRecoMUV2Hit(){};

    void Clear(Option_t* = "");

    void DecodeChannelID (Int_t ChannelID){ fChannelID = ChannelID; MUV2ChannelID::DecodeChannelID(fChannelID); }

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
    Double_t fCharge;       ///< Charge collected in fC
    Double_t fChargeError;  ///< Error on the charge
    Double_t fSigma;        ///< Sigma of the gaussian peak in ns
    Double_t fSigmaError;   ///< Error on the sigma

    ClassDef(TRecoMUV2Hit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"MUV2ChannelID", payloadCode, "@",
"TMUV2Digi", payloadCode, "@",
"TMUV2Event", payloadCode, "@",
"TMUV2Hit", payloadCode, "@",
"TRecoMUV2Candidate", payloadCode, "@",
"TRecoMUV2Event", payloadCode, "@",
"TRecoMUV2Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV2Persistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV2Persistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV2Persistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV2Persistency() {
  TriggerDictionaryInitialization_libMUV2Persistency_Impl();
}
