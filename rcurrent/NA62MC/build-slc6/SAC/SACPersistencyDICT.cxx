// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME SACPersistencyDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/SACChannelID.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/SAVChannelID.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TRecoSACCandidate.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TRecoSACEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TRecoSACHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TRecoSAVCandidate.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TRecoSAVEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TRecoSAVHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TSACDigi.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TSACEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TSACHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include/TSAVDigi.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_SACChannelID(void *p = 0);
   static void *newArray_SACChannelID(Long_t size, void *p);
   static void delete_SACChannelID(void *p);
   static void deleteArray_SACChannelID(void *p);
   static void destruct_SACChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::SACChannelID*)
   {
      ::SACChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::SACChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("SACChannelID", ::SACChannelID::Class_Version(), "", 27,
                  typeid(::SACChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::SACChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::SACChannelID) );
      instance.SetNew(&new_SACChannelID);
      instance.SetNewArray(&newArray_SACChannelID);
      instance.SetDelete(&delete_SACChannelID);
      instance.SetDeleteArray(&deleteArray_SACChannelID);
      instance.SetDestructor(&destruct_SACChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::SACChannelID*)
   {
      return GenerateInitInstanceLocal((::SACChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::SACChannelID*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static TClass *SAVChannelID_Dictionary();
   static void SAVChannelID_TClassManip(TClass*);
   static void *new_SAVChannelID(void *p = 0);
   static void *newArray_SAVChannelID(Long_t size, void *p);
   static void delete_SAVChannelID(void *p);
   static void deleteArray_SAVChannelID(void *p);
   static void destruct_SAVChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::SAVChannelID*)
   {
      ::SAVChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(::SAVChannelID));
      static ::ROOT::TGenericClassInfo 
         instance("SAVChannelID", "", 61,
                  typeid(::SAVChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &SAVChannelID_Dictionary, isa_proxy, 4,
                  sizeof(::SAVChannelID) );
      instance.SetNew(&new_SAVChannelID);
      instance.SetNewArray(&newArray_SAVChannelID);
      instance.SetDelete(&delete_SAVChannelID);
      instance.SetDeleteArray(&deleteArray_SAVChannelID);
      instance.SetDestructor(&destruct_SAVChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::SAVChannelID*)
   {
      return GenerateInitInstanceLocal((::SAVChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::SAVChannelID*)0x0); R__UseDummy(_R__UNIQUE_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *SAVChannelID_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const ::SAVChannelID*)0x0)->GetClass();
      SAVChannelID_TClassManip(theClass);
   return theClass;
   }

   static void SAVChannelID_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSACCandidate(void *p = 0);
   static void *newArray_TRecoSACCandidate(Long_t size, void *p);
   static void delete_TRecoSACCandidate(void *p);
   static void deleteArray_TRecoSACCandidate(void *p);
   static void destruct_TRecoSACCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSACCandidate*)
   {
      ::TRecoSACCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSACCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSACCandidate", ::TRecoSACCandidate::Class_Version(), "", 100,
                  typeid(::TRecoSACCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSACCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSACCandidate) );
      instance.SetNew(&new_TRecoSACCandidate);
      instance.SetNewArray(&newArray_TRecoSACCandidate);
      instance.SetDelete(&delete_TRecoSACCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoSACCandidate);
      instance.SetDestructor(&destruct_TRecoSACCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSACCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoSACCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSACCandidate*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSACHit(void *p = 0);
   static void *newArray_TRecoSACHit(Long_t size, void *p);
   static void delete_TRecoSACHit(void *p);
   static void deleteArray_TRecoSACHit(void *p);
   static void destruct_TRecoSACHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSACHit*)
   {
      ::TRecoSACHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSACHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSACHit", ::TRecoSACHit::Class_Version(), "TRecoSACHit.hh", 13,
                  typeid(::TRecoSACHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSACHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSACHit) );
      instance.SetNew(&new_TRecoSACHit);
      instance.SetNewArray(&newArray_TRecoSACHit);
      instance.SetDelete(&delete_TRecoSACHit);
      instance.SetDeleteArray(&deleteArray_TRecoSACHit);
      instance.SetDestructor(&destruct_TRecoSACHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSACHit*)
   {
      return GenerateInitInstanceLocal((::TRecoSACHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSACHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSACEvent(void *p = 0);
   static void *newArray_TRecoSACEvent(Long_t size, void *p);
   static void delete_TRecoSACEvent(void *p);
   static void deleteArray_TRecoSACEvent(void *p);
   static void destruct_TRecoSACEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSACEvent*)
   {
      ::TRecoSACEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSACEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSACEvent", ::TRecoSACEvent::Class_Version(), "", 128,
                  typeid(::TRecoSACEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSACEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSACEvent) );
      instance.SetNew(&new_TRecoSACEvent);
      instance.SetNewArray(&newArray_TRecoSACEvent);
      instance.SetDelete(&delete_TRecoSACEvent);
      instance.SetDeleteArray(&deleteArray_TRecoSACEvent);
      instance.SetDestructor(&destruct_TRecoSACEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSACEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoSACEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSACEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSAVCandidate(void *p = 0);
   static void *newArray_TRecoSAVCandidate(Long_t size, void *p);
   static void delete_TRecoSAVCandidate(void *p);
   static void deleteArray_TRecoSAVCandidate(void *p);
   static void destruct_TRecoSAVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSAVCandidate*)
   {
      ::TRecoSAVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSAVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSAVCandidate", ::TRecoSAVCandidate::Class_Version(), "", 243,
                  typeid(::TRecoSAVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSAVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSAVCandidate) );
      instance.SetNew(&new_TRecoSAVCandidate);
      instance.SetNewArray(&newArray_TRecoSAVCandidate);
      instance.SetDelete(&delete_TRecoSAVCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoSAVCandidate);
      instance.SetDestructor(&destruct_TRecoSAVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSAVCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoSAVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSAVCandidate*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSAVHit(void *p = 0);
   static void *newArray_TRecoSAVHit(Long_t size, void *p);
   static void delete_TRecoSAVHit(void *p);
   static void deleteArray_TRecoSAVHit(void *p);
   static void destruct_TRecoSAVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSAVHit*)
   {
      ::TRecoSAVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSAVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSAVHit", ::TRecoSAVHit::Class_Version(), "TRecoSAVHit.hh", 13,
                  typeid(::TRecoSAVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSAVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSAVHit) );
      instance.SetNew(&new_TRecoSAVHit);
      instance.SetNewArray(&newArray_TRecoSAVHit);
      instance.SetDelete(&delete_TRecoSAVHit);
      instance.SetDeleteArray(&deleteArray_TRecoSAVHit);
      instance.SetDestructor(&destruct_TRecoSAVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSAVHit*)
   {
      return GenerateInitInstanceLocal((::TRecoSAVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSAVHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSAVEvent(void *p = 0);
   static void *newArray_TRecoSAVEvent(Long_t size, void *p);
   static void delete_TRecoSAVEvent(void *p);
   static void deleteArray_TRecoSAVEvent(void *p);
   static void destruct_TRecoSAVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSAVEvent*)
   {
      ::TRecoSAVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSAVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSAVEvent", ::TRecoSAVEvent::Class_Version(), "", 293,
                  typeid(::TRecoSAVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSAVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSAVEvent) );
      instance.SetNew(&new_TRecoSAVEvent);
      instance.SetNewArray(&newArray_TRecoSAVEvent);
      instance.SetDelete(&delete_TRecoSAVEvent);
      instance.SetDeleteArray(&deleteArray_TRecoSAVEvent);
      instance.SetDestructor(&destruct_TRecoSAVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSAVEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoSAVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSAVEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSACDigi(void *p = 0);
   static void *newArray_TSACDigi(Long_t size, void *p);
   static void delete_TSACDigi(void *p);
   static void deleteArray_TSACDigi(void *p);
   static void destruct_TSACDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSACDigi*)
   {
      ::TSACDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSACDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSACDigi", ::TSACDigi::Class_Version(), "", 366,
                  typeid(::TSACDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSACDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TSACDigi) );
      instance.SetNew(&new_TSACDigi);
      instance.SetNewArray(&newArray_TSACDigi);
      instance.SetDelete(&delete_TSACDigi);
      instance.SetDeleteArray(&deleteArray_TSACDigi);
      instance.SetDestructor(&destruct_TSACDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSACDigi*)
   {
      return GenerateInitInstanceLocal((::TSACDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TSACDigi*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSACEvent(void *p = 0);
   static void *newArray_TSACEvent(Long_t size, void *p);
   static void delete_TSACEvent(void *p);
   static void deleteArray_TSACEvent(void *p);
   static void destruct_TSACEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSACEvent*)
   {
      ::TSACEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSACEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSACEvent", ::TSACEvent::Class_Version(), "", 412,
                  typeid(::TSACEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSACEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSACEvent) );
      instance.SetNew(&new_TSACEvent);
      instance.SetNewArray(&newArray_TSACEvent);
      instance.SetDelete(&delete_TSACEvent);
      instance.SetDeleteArray(&deleteArray_TSACEvent);
      instance.SetDestructor(&destruct_TSACEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSACEvent*)
   {
      return GenerateInitInstanceLocal((::TSACEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TSACEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSACHit(void *p = 0);
   static void *newArray_TSACHit(Long_t size, void *p);
   static void delete_TSACHit(void *p);
   static void deleteArray_TSACHit(void *p);
   static void destruct_TSACHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSACHit*)
   {
      ::TSACHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSACHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSACHit", ::TSACHit::Class_Version(), "", 440,
                  typeid(::TSACHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSACHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSACHit) );
      instance.SetNew(&new_TSACHit);
      instance.SetNewArray(&newArray_TSACHit);
      instance.SetDelete(&delete_TSACHit);
      instance.SetDeleteArray(&deleteArray_TSACHit);
      instance.SetDestructor(&destruct_TSACHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSACHit*)
   {
      return GenerateInitInstanceLocal((::TSACHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TSACHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSAVDigi(void *p = 0);
   static void *newArray_TSAVDigi(Long_t size, void *p);
   static void delete_TSAVDigi(void *p);
   static void deleteArray_TSAVDigi(void *p);
   static void destruct_TSAVDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSAVDigi*)
   {
      ::TSAVDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSAVDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSAVDigi", ::TSAVDigi::Class_Version(), "", 471,
                  typeid(::TSAVDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSAVDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TSAVDigi) );
      instance.SetNew(&new_TSAVDigi);
      instance.SetNewArray(&newArray_TSAVDigi);
      instance.SetDelete(&delete_TSAVDigi);
      instance.SetDeleteArray(&deleteArray_TSAVDigi);
      instance.SetDestructor(&destruct_TSAVDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSAVDigi*)
   {
      return GenerateInitInstanceLocal((::TSAVDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TSAVDigi*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr SACChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *SACChannelID::Class_Name()
{
   return "SACChannelID";
}

//______________________________________________________________________________
const char *SACChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SACChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int SACChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SACChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *SACChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SACChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *SACChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SACChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSACCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSACCandidate::Class_Name()
{
   return "TRecoSACCandidate";
}

//______________________________________________________________________________
const char *TRecoSACCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSACCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSACCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSACCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSACHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSACHit::Class_Name()
{
   return "TRecoSACHit";
}

//______________________________________________________________________________
const char *TRecoSACHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSACHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSACHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSACHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSACEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSACEvent::Class_Name()
{
   return "TRecoSACEvent";
}

//______________________________________________________________________________
const char *TRecoSACEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSACEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSACEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSACEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSACEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSAVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSAVCandidate::Class_Name()
{
   return "TRecoSAVCandidate";
}

//______________________________________________________________________________
const char *TRecoSAVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSAVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSAVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSAVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSAVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSAVHit::Class_Name()
{
   return "TRecoSAVHit";
}

//______________________________________________________________________________
const char *TRecoSAVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSAVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSAVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSAVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSAVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSAVEvent::Class_Name()
{
   return "TRecoSAVEvent";
}

//______________________________________________________________________________
const char *TRecoSAVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSAVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSAVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSAVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSACDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSACDigi::Class_Name()
{
   return "TSACDigi";
}

//______________________________________________________________________________
const char *TSACDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSACDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSACDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSACDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSACDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSACDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSACDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSACDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSACEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSACEvent::Class_Name()
{
   return "TSACEvent";
}

//______________________________________________________________________________
const char *TSACEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSACEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSACEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSACEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSACEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSACEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSACEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSACEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSACHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSACHit::Class_Name()
{
   return "TSACHit";
}

//______________________________________________________________________________
const char *TSACHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSACHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSACHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSACHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSACHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSACHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSACHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSACHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSAVDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSAVDigi::Class_Name()
{
   return "TSAVDigi";
}

//______________________________________________________________________________
const char *TSAVDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSAVDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSAVDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSAVDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSAVDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSAVDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSAVDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSAVDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void SACChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class SACChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(SACChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(SACChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_SACChannelID(void *p) {
      return  p ? new(p) ::SACChannelID : new ::SACChannelID;
   }
   static void *newArray_SACChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::SACChannelID[nElements] : new ::SACChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_SACChannelID(void *p) {
      delete ((::SACChannelID*)p);
   }
   static void deleteArray_SACChannelID(void *p) {
      delete [] ((::SACChannelID*)p);
   }
   static void destruct_SACChannelID(void *p) {
      typedef ::SACChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::SACChannelID

namespace ROOT {
   // Wrappers around operator new
   static void *new_SAVChannelID(void *p) {
      return  p ? new(p) ::SAVChannelID : new ::SAVChannelID;
   }
   static void *newArray_SAVChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::SAVChannelID[nElements] : new ::SAVChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_SAVChannelID(void *p) {
      delete ((::SAVChannelID*)p);
   }
   static void deleteArray_SAVChannelID(void *p) {
      delete [] ((::SAVChannelID*)p);
   }
   static void destruct_SAVChannelID(void *p) {
      typedef ::SAVChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::SAVChannelID

//______________________________________________________________________________
void TRecoSACCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSACCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSACCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSACCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSACCandidate(void *p) {
      return  p ? new(p) ::TRecoSACCandidate : new ::TRecoSACCandidate;
   }
   static void *newArray_TRecoSACCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSACCandidate[nElements] : new ::TRecoSACCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSACCandidate(void *p) {
      delete ((::TRecoSACCandidate*)p);
   }
   static void deleteArray_TRecoSACCandidate(void *p) {
      delete [] ((::TRecoSACCandidate*)p);
   }
   static void destruct_TRecoSACCandidate(void *p) {
      typedef ::TRecoSACCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSACCandidate

//______________________________________________________________________________
void TRecoSACHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSACHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSACHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSACHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSACHit(void *p) {
      return  p ? new(p) ::TRecoSACHit : new ::TRecoSACHit;
   }
   static void *newArray_TRecoSACHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSACHit[nElements] : new ::TRecoSACHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSACHit(void *p) {
      delete ((::TRecoSACHit*)p);
   }
   static void deleteArray_TRecoSACHit(void *p) {
      delete [] ((::TRecoSACHit*)p);
   }
   static void destruct_TRecoSACHit(void *p) {
      typedef ::TRecoSACHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSACHit

//______________________________________________________________________________
void TRecoSACEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSACEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSACEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSACEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSACEvent(void *p) {
      return  p ? new(p) ::TRecoSACEvent : new ::TRecoSACEvent;
   }
   static void *newArray_TRecoSACEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSACEvent[nElements] : new ::TRecoSACEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSACEvent(void *p) {
      delete ((::TRecoSACEvent*)p);
   }
   static void deleteArray_TRecoSACEvent(void *p) {
      delete [] ((::TRecoSACEvent*)p);
   }
   static void destruct_TRecoSACEvent(void *p) {
      typedef ::TRecoSACEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSACEvent

//______________________________________________________________________________
void TRecoSAVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSAVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSAVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSAVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSAVCandidate(void *p) {
      return  p ? new(p) ::TRecoSAVCandidate : new ::TRecoSAVCandidate;
   }
   static void *newArray_TRecoSAVCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSAVCandidate[nElements] : new ::TRecoSAVCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSAVCandidate(void *p) {
      delete ((::TRecoSAVCandidate*)p);
   }
   static void deleteArray_TRecoSAVCandidate(void *p) {
      delete [] ((::TRecoSAVCandidate*)p);
   }
   static void destruct_TRecoSAVCandidate(void *p) {
      typedef ::TRecoSAVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSAVCandidate

//______________________________________________________________________________
void TRecoSAVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSAVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSAVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSAVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSAVHit(void *p) {
      return  p ? new(p) ::TRecoSAVHit : new ::TRecoSAVHit;
   }
   static void *newArray_TRecoSAVHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSAVHit[nElements] : new ::TRecoSAVHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSAVHit(void *p) {
      delete ((::TRecoSAVHit*)p);
   }
   static void deleteArray_TRecoSAVHit(void *p) {
      delete [] ((::TRecoSAVHit*)p);
   }
   static void destruct_TRecoSAVHit(void *p) {
      typedef ::TRecoSAVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSAVHit

//______________________________________________________________________________
void TRecoSAVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSAVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSAVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSAVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSAVEvent(void *p) {
      return  p ? new(p) ::TRecoSAVEvent : new ::TRecoSAVEvent;
   }
   static void *newArray_TRecoSAVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSAVEvent[nElements] : new ::TRecoSAVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSAVEvent(void *p) {
      delete ((::TRecoSAVEvent*)p);
   }
   static void deleteArray_TRecoSAVEvent(void *p) {
      delete [] ((::TRecoSAVEvent*)p);
   }
   static void destruct_TRecoSAVEvent(void *p) {
      typedef ::TRecoSAVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSAVEvent

//______________________________________________________________________________
void TSACDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSACDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSACDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSACDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSACDigi(void *p) {
      return  p ? new(p) ::TSACDigi : new ::TSACDigi;
   }
   static void *newArray_TSACDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TSACDigi[nElements] : new ::TSACDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSACDigi(void *p) {
      delete ((::TSACDigi*)p);
   }
   static void deleteArray_TSACDigi(void *p) {
      delete [] ((::TSACDigi*)p);
   }
   static void destruct_TSACDigi(void *p) {
      typedef ::TSACDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSACDigi

//______________________________________________________________________________
void TSACEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSACEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSACEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSACEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSACEvent(void *p) {
      return  p ? new(p) ::TSACEvent : new ::TSACEvent;
   }
   static void *newArray_TSACEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSACEvent[nElements] : new ::TSACEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSACEvent(void *p) {
      delete ((::TSACEvent*)p);
   }
   static void deleteArray_TSACEvent(void *p) {
      delete [] ((::TSACEvent*)p);
   }
   static void destruct_TSACEvent(void *p) {
      typedef ::TSACEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSACEvent

//______________________________________________________________________________
void TSACHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSACHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSACHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSACHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSACHit(void *p) {
      return  p ? new(p) ::TSACHit : new ::TSACHit;
   }
   static void *newArray_TSACHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSACHit[nElements] : new ::TSACHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSACHit(void *p) {
      delete ((::TSACHit*)p);
   }
   static void deleteArray_TSACHit(void *p) {
      delete [] ((::TSACHit*)p);
   }
   static void destruct_TSACHit(void *p) {
      typedef ::TSACHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSACHit

//______________________________________________________________________________
void TSAVDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSAVDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSAVDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSAVDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSAVDigi(void *p) {
      return  p ? new(p) ::TSAVDigi : new ::TSAVDigi;
   }
   static void *newArray_TSAVDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TSAVDigi[nElements] : new ::TSAVDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSAVDigi(void *p) {
      delete ((::TSAVDigi*)p);
   }
   static void deleteArray_TSAVDigi(void *p) {
      delete [] ((::TSAVDigi*)p);
   }
   static void destruct_TSAVDigi(void *p) {
      typedef ::TSAVDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSAVDigi

namespace {
  void TriggerDictionaryInitialization_libSACPersistency_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libSACPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class SACChannelID;
class SAVChannelID;
class TRecoSACCandidate;
class __attribute__((annotate("$clingAutoload$TRecoSACHit.hh")))  TRecoSACHit;
class TRecoSACEvent;
class TRecoSAVCandidate;
class __attribute__((annotate("$clingAutoload$TRecoSAVHit.hh")))  TRecoSAVHit;
class TRecoSAVEvent;
class TSACDigi;
class TSACEvent;
class TSACHit;
class TSAVDigi;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libSACPersistency dictionary payload"

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
/*
 * SACChannelID.hh
 *
 *  Created on: Sep 25, 2015
 *      Author: veni
 */

#ifndef SAC_PERSISTENCY_INCLUDE_SACCHANNELID_HH_
#define SAC_PERSISTENCY_INCLUDE_SACCHANNELID_HH_

#include "Rtypes.h"

class SACChannelID {
public:
  SACChannelID();
  explicit SACChannelID(Int_t);
  virtual ~SACChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  void  DecodeChannelID(Int_t); // converts position ID into PMTID, IsHighThreshold
  Int_t GetPMTID()           const { return fPMTID;              };

private:
  Int_t fPMTID;

 ClassDef(SACChannelID,1);
};

#endif /* SAC_PERSISTENCY_INCLUDE_SACCHANNELID_HH_ */
//
//  SAVChannelID.hh
//
//
//  Created by letizia peruzzo on 02/06/2016.
//
//

#ifndef _SAVChannelID_hh
#define _SAVChannelID_hh

#include "Rtypes.h"
#include "TVector2.h"

class SAVChannelID {

  public:
    SAVChannelID();
    explicit SAVChannelID(Int_t ChannelID);
    virtual ~SAVChannelID();

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void DecodeChannelID(Int_t ChannelID);

    Int_t GetDetector() {return fDetectorID;}
    Int_t GetChannel() {return fDetectorID*10 + fDetectorChannel;}
    Int_t GetChannelDetector() {return fDetectorChannel;}
  
    TVector2 GetChannelPosition();
  
  protected:

    Int_t fDetectorID;
    Int_t fDetectorChannel;

};


#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSACCandidate_H
#define TRecoSACCandidate_H

#include "TRecoVCandidate.hh"

class TRecoSACCandidate : public TRecoVCandidate {

    public:

        TRecoSACCandidate();
        ~TRecoSACCandidate(){};

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoSACCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSACEvent_H
#define TRecoSACEvent_H

#include "TRecoVEvent.hh"
#include "TRecoSACCandidate.hh"
#include "TRecoSACHit.hh"

class TRecoSACEvent : public TRecoVEvent {

    public:

        TRecoSACEvent();
        ~TRecoSACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoSACEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSACHit_H
#define TRecoSACHit_H 1

#include "TRecoVHit.hh"
#include "SACChannelID.hh"

class TRecoSACHit : public TRecoVHit, public SACChannelID {

  public:
    TRecoSACHit();
    ~TRecoSACHit(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

  public:
    void SetLeadingEdgeLow(Double_t edgeTime){fLeadingEdgeLow = edgeTime; fEdgeMask |= 1;}
    void SetLeadingEdgeHigh(Double_t edgeTime){fLeadingEdgeHigh = edgeTime; fEdgeMask |= 2;}
    void SetTrailingEdgeHigh(Double_t edgeTime){fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4;}
    void SetTrailingEdgeLow(Double_t edgeTime){fTrailingEdgeLow = edgeTime; fEdgeMask |= 8;}
    void SetTimeNoT0(Double_t val)         { fTimeNoT0 = val;          }
    //void SetTimeOverThreshold(Double_t val){fTimeOvThr=val;}
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
    Double_t GetTimeNoT0                ()const{return fTimeNoT0;}
    //Double_t GetTimeOverThreshold       ()const{return fTimeOvThr;}
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


    Int_t GetEdgeMask()const{return fEdgeMask;}



  private:

    Double_t fTimeNoT0;


    Int_t fEdgeMask; ///< Mask for the edges present: bit 0-LeadingLow, 1-LeadingHigh, 2-TrailingHigh, 3-TrailingLow
    //Double_t fTimeOvThr;
    Double_t fTimeOvThrLow;
    Double_t fTimeOvThrHigh;
    Double_t fLeadingESlewingSlope;
    Double_t fTrailingESlewingSlope;

    Double_t fLeadingEdgeLow; ///< Time of leading low, subtracted of the trigger time only
    Double_t fTrailingEdgeLow;///< Time of leading high, subtracted of the trigger time only
    Double_t fLeadingEdgeHigh;///< Time of trailing high, subtracted of the trigger time only
    Double_t fTrailingEdgeHigh;///< Time of trailing low, subtracted of the trigger time only
    Int_t fLowThresholdROChannelID;
    Int_t fHighThresholdROChannelID;

    ClassDef(TRecoSACHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
//
// --------------------------------------------------------------
#ifndef TRecoSAVCandidate_H
#define TRecoSAVCandidate_H

#include "TRecoVCandidate.hh"

class TRecoSAVCandidate : public TRecoVCandidate {
    
public:
    
    TRecoSAVCandidate();
    ~TRecoSAVCandidate(){};
    
    void Clear(Option_t* = "");

public:
    
    void SetTime(Double_t value){ fTime=value; }
    Double_t GetTime() { return fTime; }

    void SetEnergy (Double_t value){ fEnergy = value; }
    Double_t GetEnergy() { return fEnergy; }
  
    void SetPosition (Double_t x, Double_t y ){	fPosition.Set(x,y);	}
    void SetPosition (TVector2 value ){ fPosition = value; }
  
    Double_t GetX() {	return fPosition.X();	}
    Double_t GetY() {	return fPosition.Y();	}
    TVector2 GetPosition() { return fPosition; }
    
private:
    
    Double_t fTime;            ///< Candidate time as a mean of hit time.
  
    TVector2 fPosition;       ///< Candidate position everage mean of hit position.

    Double_t fEnergy;        ///< Candidate energy.
    
    
    ClassDef(TRecoSAVCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016_06_02
//
// --------------------------------------------------------------
#ifndef TRecoSAVEvent_H
#define TRecoSAVEvent_H

#include "TRecoVEvent.hh"
#include "TRecoSAVCandidate.hh"
#include "TRecoSAVHit.hh"

class TRecoSAVEvent : public TRecoVEvent {

    public:

        TRecoSAVEvent();
        ~TRecoSAVEvent();

        void Clear(Option_t* = "");

    private:
        ClassDef(TRecoSAVEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// --------------------------------------------------------------
#ifndef TRecoSAVHit_H
#define TRecoSAVHit_H

#include "TRecoVHit.hh"
#include "SAVChannelID.hh"

class TRecoSAVHit : public TRecoVHit , public SAVChannelID {

  public:

    TRecoSAVHit();
    ~TRecoSAVHit(){};

    void Clear(Option_t* = "");

    void DecodeChannelID (Int_t ChannelID);
  
  public:
  
    void SetTime(Double_t time) { fTime = time; }
    Double_t GetTime() { return fTime; }
    
    void SetAmplitude(Double_t amplitude) { fAmplitude = amplitude; }
    Double_t GetAmplitude() { return fAmplitude; }
    
    void SetBaseline(Double_t baseline) { fBaseline = baseline; }
    Double_t GetBaseline() { return fBaseline; }

    void SetEnergy(Double_t energy) { fEnergy = energy; }
    Double_t GetEnergy() { return fEnergy; }

  private:
    Double_t fTime;       ///< Hit time.
    Double_t fAmplitude;  ///< Hit amplitude.
    Double_t fBaseline;   ///< Hit baseline.
    Double_t fEnergy;     ///< Hit energy.
  
    ClassDef(TRecoSAVHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by T Spadaro (tommaso.spadaro@cern.ch) 2015-05-14
//
// --------------------------------------------------------------
#ifndef TSACDigi_H
#define TSACDigi_H 1

#include "TDCVHit.hh"
#include "SACChannelID.hh"

class TSACDigi : public TDCVHit, public SACChannelID {

  public:

    TSACDigi();// : TDCVHit(){}
    explicit TSACDigi(Int_t iCh) : TDCVHit(iCh), SACChannelID(iCh%1000){}
    explicit TSACDigi(TVHit*);
    ~TSACDigi() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }
    //Int_t      GetPMTID                ()  {return  SACChannelID::EncodeChannelID();}
    //Int_t      GetChannelID            ()const  {return  fChannelID;}
    Int_t      GetCorrespondingLowHighChannelId()const;

    Int_t GetThresholdType() const { return fChannelID/1000; } //Low Threshold: 0, High Threshold: 1

    Bool_t     HasLeadingEdge          ()       {return  GetDetectedEdge()&1;}
    Bool_t     HasTrailingEdge         ()       {return  GetDetectedEdge()&2;}

  private:
    //    Int_t fPMTID;
    //   Bool_t fIsLowThresholdChannel;

    ClassDef(TSACDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TSACEvent_H
#define TSACEvent_H

#include "TDetectorVEvent.hh"

class TSACEvent : public TDetectorVEvent {

    public:

        TSACEvent();
        ~TSACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TSACEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------
#ifndef TSACHit_H
#define TSACHit_H

#include "TDetectorVHit.hh"
#include "SACChannelID.hh"

class TSACHit : public TDetectorVHit, public SACChannelID {

    public:

      TSACHit();
      ~TSACHit(){};

      void Clear(Option_t* = "");

      Int_t EncodeChannelID();
      void  DecodeChannelID();

    protected:

        ClassDef(TSACHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Create by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// --------------------------------------------------------------
#ifndef TSAVDigi_H
#define TSAVDigi_H

#include "FADCVHit.hh"
#include "SAVChannelID.hh"
#include "TVector3.h"

class TSAVDigi : public FADCVHit , public SAVChannelID {

  public:

    TSAVDigi();
    ~TSAVDigi(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID () { return 0; }
    Int_t GetModuleID () { return fChannelID/10; }



  private:


    ClassDef(TSAVDigi,1);

};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"SACChannelID", payloadCode, "@",
"SAVChannelID", payloadCode, "@",
"TRecoSACCandidate", payloadCode, "@",
"TRecoSACEvent", payloadCode, "@",
"TRecoSACHit", payloadCode, "@",
"TRecoSAVCandidate", payloadCode, "@",
"TRecoSAVEvent", payloadCode, "@",
"TRecoSAVHit", payloadCode, "@",
"TSACDigi", payloadCode, "@",
"TSACEvent", payloadCode, "@",
"TSACHit", payloadCode, "@",
"TSAVDigi", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libSACPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libSACPersistency_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libSACPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libSACPersistency() {
  TriggerDictionaryInitialization_libSACPersistency_Impl();
}
