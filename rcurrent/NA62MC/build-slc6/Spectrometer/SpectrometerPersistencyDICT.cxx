// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME SpectrometerPersistencyDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/SRBError.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/SRBEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/SRBVHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/SpectrometerChannelID.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/TRecoSpectrometerCandidate.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/TRecoSpectrometerEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/TRecoSpectrometerHit.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/TSpectrometerDigi.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/TSpectrometerEvent.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include/TSpectrometerHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_SRBError(void *p = 0);
   static void *newArray_SRBError(Long_t size, void *p);
   static void delete_SRBError(void *p);
   static void deleteArray_SRBError(void *p);
   static void destruct_SRBError(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::SRBError*)
   {
      ::SRBError *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::SRBError >(0);
      static ::ROOT::TGenericClassInfo 
         instance("SRBError", ::SRBError::Class_Version(), "", 19,
                  typeid(::SRBError), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::SRBError::Dictionary, isa_proxy, 4,
                  sizeof(::SRBError) );
      instance.SetNew(&new_SRBError);
      instance.SetNewArray(&newArray_SRBError);
      instance.SetDelete(&delete_SRBError);
      instance.SetDeleteArray(&deleteArray_SRBError);
      instance.SetDestructor(&destruct_SRBError);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::SRBError*)
   {
      return GenerateInitInstanceLocal((::SRBError*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::SRBError*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_SRBVHit(void *p);
   static void deleteArray_SRBVHit(void *p);
   static void destruct_SRBVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::SRBVHit*)
   {
      ::SRBVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::SRBVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("SRBVHit", ::SRBVHit::Class_Version(), "SRBVHit.hh", 11,
                  typeid(::SRBVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::SRBVHit::Dictionary, isa_proxy, 4,
                  sizeof(::SRBVHit) );
      instance.SetDelete(&delete_SRBVHit);
      instance.SetDeleteArray(&deleteArray_SRBVHit);
      instance.SetDestructor(&destruct_SRBVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::SRBVHit*)
   {
      return GenerateInitInstanceLocal((::SRBVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::SRBVHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_SRBEvent(void *p = 0);
   static void *newArray_SRBEvent(Long_t size, void *p);
   static void delete_SRBEvent(void *p);
   static void deleteArray_SRBEvent(void *p);
   static void destruct_SRBEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::SRBEvent*)
   {
      ::SRBEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::SRBEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("SRBEvent", ::SRBEvent::Class_Version(), "", 52,
                  typeid(::SRBEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::SRBEvent::Dictionary, isa_proxy, 4,
                  sizeof(::SRBEvent) );
      instance.SetNew(&new_SRBEvent);
      instance.SetNewArray(&newArray_SRBEvent);
      instance.SetDelete(&delete_SRBEvent);
      instance.SetDeleteArray(&deleteArray_SRBEvent);
      instance.SetDestructor(&destruct_SRBEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::SRBEvent*)
   {
      return GenerateInitInstanceLocal((::SRBEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::SRBEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_SpectrometerChannelID(void *p = 0);
   static void *newArray_SpectrometerChannelID(Long_t size, void *p);
   static void delete_SpectrometerChannelID(void *p);
   static void deleteArray_SpectrometerChannelID(void *p);
   static void destruct_SpectrometerChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::SpectrometerChannelID*)
   {
      ::SpectrometerChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::SpectrometerChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("SpectrometerChannelID", ::SpectrometerChannelID::Class_Version(), "", 118,
                  typeid(::SpectrometerChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::SpectrometerChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::SpectrometerChannelID) );
      instance.SetNew(&new_SpectrometerChannelID);
      instance.SetNewArray(&newArray_SpectrometerChannelID);
      instance.SetDelete(&delete_SpectrometerChannelID);
      instance.SetDeleteArray(&deleteArray_SpectrometerChannelID);
      instance.SetDestructor(&destruct_SpectrometerChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::SpectrometerChannelID*)
   {
      return GenerateInitInstanceLocal((::SpectrometerChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::SpectrometerChannelID*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSpectrometerCandidate(void *p = 0);
   static void *newArray_TRecoSpectrometerCandidate(Long_t size, void *p);
   static void delete_TRecoSpectrometerCandidate(void *p);
   static void deleteArray_TRecoSpectrometerCandidate(void *p);
   static void destruct_TRecoSpectrometerCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSpectrometerCandidate*)
   {
      ::TRecoSpectrometerCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSpectrometerCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSpectrometerCandidate", ::TRecoSpectrometerCandidate::Class_Version(), "", 167,
                  typeid(::TRecoSpectrometerCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSpectrometerCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSpectrometerCandidate) );
      instance.SetNew(&new_TRecoSpectrometerCandidate);
      instance.SetNewArray(&newArray_TRecoSpectrometerCandidate);
      instance.SetDelete(&delete_TRecoSpectrometerCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoSpectrometerCandidate);
      instance.SetDestructor(&destruct_TRecoSpectrometerCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSpectrometerCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoSpectrometerCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSpectrometerCandidate*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSpectrometerHit(void *p = 0);
   static void *newArray_TRecoSpectrometerHit(Long_t size, void *p);
   static void delete_TRecoSpectrometerHit(void *p);
   static void deleteArray_TRecoSpectrometerHit(void *p);
   static void destruct_TRecoSpectrometerHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSpectrometerHit*)
   {
      ::TRecoSpectrometerHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSpectrometerHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSpectrometerHit", ::TRecoSpectrometerHit::Class_Version(), "TRecoSpectrometerHit.hh", 12,
                  typeid(::TRecoSpectrometerHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSpectrometerHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSpectrometerHit) );
      instance.SetNew(&new_TRecoSpectrometerHit);
      instance.SetNewArray(&newArray_TRecoSpectrometerHit);
      instance.SetDelete(&delete_TRecoSpectrometerHit);
      instance.SetDeleteArray(&deleteArray_TRecoSpectrometerHit);
      instance.SetDestructor(&destruct_TRecoSpectrometerHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSpectrometerHit*)
   {
      return GenerateInitInstanceLocal((::TRecoSpectrometerHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSpectrometerHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoSpectrometerEvent(void *p = 0);
   static void *newArray_TRecoSpectrometerEvent(Long_t size, void *p);
   static void delete_TRecoSpectrometerEvent(void *p);
   static void deleteArray_TRecoSpectrometerEvent(void *p);
   static void destruct_TRecoSpectrometerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoSpectrometerEvent*)
   {
      ::TRecoSpectrometerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoSpectrometerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoSpectrometerEvent", ::TRecoSpectrometerEvent::Class_Version(), "", 280,
                  typeid(::TRecoSpectrometerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoSpectrometerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoSpectrometerEvent) );
      instance.SetNew(&new_TRecoSpectrometerEvent);
      instance.SetNewArray(&newArray_TRecoSpectrometerEvent);
      instance.SetDelete(&delete_TRecoSpectrometerEvent);
      instance.SetDeleteArray(&deleteArray_TRecoSpectrometerEvent);
      instance.SetDestructor(&destruct_TRecoSpectrometerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoSpectrometerEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoSpectrometerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TRecoSpectrometerEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSpectrometerDigi(void *p = 0);
   static void *newArray_TSpectrometerDigi(Long_t size, void *p);
   static void delete_TSpectrometerDigi(void *p);
   static void deleteArray_TSpectrometerDigi(void *p);
   static void destruct_TSpectrometerDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSpectrometerDigi*)
   {
      ::TSpectrometerDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSpectrometerDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSpectrometerDigi", ::TSpectrometerDigi::Class_Version(), "", 516,
                  typeid(::TSpectrometerDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSpectrometerDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TSpectrometerDigi) );
      instance.SetNew(&new_TSpectrometerDigi);
      instance.SetNewArray(&newArray_TSpectrometerDigi);
      instance.SetDelete(&delete_TSpectrometerDigi);
      instance.SetDeleteArray(&deleteArray_TSpectrometerDigi);
      instance.SetDestructor(&destruct_TSpectrometerDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSpectrometerDigi*)
   {
      return GenerateInitInstanceLocal((::TSpectrometerDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TSpectrometerDigi*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSpectrometerEvent(void *p = 0);
   static void *newArray_TSpectrometerEvent(Long_t size, void *p);
   static void delete_TSpectrometerEvent(void *p);
   static void deleteArray_TSpectrometerEvent(void *p);
   static void destruct_TSpectrometerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSpectrometerEvent*)
   {
      ::TSpectrometerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSpectrometerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSpectrometerEvent", ::TSpectrometerEvent::Class_Version(), "", 557,
                  typeid(::TSpectrometerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSpectrometerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSpectrometerEvent) );
      instance.SetNew(&new_TSpectrometerEvent);
      instance.SetNewArray(&newArray_TSpectrometerEvent);
      instance.SetDelete(&delete_TSpectrometerEvent);
      instance.SetDeleteArray(&deleteArray_TSpectrometerEvent);
      instance.SetDestructor(&destruct_TSpectrometerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSpectrometerEvent*)
   {
      return GenerateInitInstanceLocal((::TSpectrometerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TSpectrometerEvent*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSpectrometerHit(void *p = 0);
   static void *newArray_TSpectrometerHit(Long_t size, void *p);
   static void delete_TSpectrometerHit(void *p);
   static void deleteArray_TSpectrometerHit(void *p);
   static void destruct_TSpectrometerHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSpectrometerHit*)
   {
      ::TSpectrometerHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSpectrometerHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSpectrometerHit", ::TSpectrometerHit::Class_Version(), "", 583,
                  typeid(::TSpectrometerHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSpectrometerHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSpectrometerHit) );
      instance.SetNew(&new_TSpectrometerHit);
      instance.SetNewArray(&newArray_TSpectrometerHit);
      instance.SetDelete(&delete_TSpectrometerHit);
      instance.SetDeleteArray(&deleteArray_TSpectrometerHit);
      instance.SetDestructor(&destruct_TSpectrometerHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSpectrometerHit*)
   {
      return GenerateInitInstanceLocal((::TSpectrometerHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TSpectrometerHit*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr SRBError::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *SRBError::Class_Name()
{
   return "SRBError";
}

//______________________________________________________________________________
const char *SRBError::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SRBError*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int SRBError::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SRBError*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *SRBError::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SRBError*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *SRBError::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SRBError*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr SRBVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *SRBVHit::Class_Name()
{
   return "SRBVHit";
}

//______________________________________________________________________________
const char *SRBVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SRBVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int SRBVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SRBVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *SRBVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SRBVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *SRBVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SRBVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr SRBEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *SRBEvent::Class_Name()
{
   return "SRBEvent";
}

//______________________________________________________________________________
const char *SRBEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SRBEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int SRBEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SRBEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *SRBEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SRBEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *SRBEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SRBEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr SpectrometerChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *SpectrometerChannelID::Class_Name()
{
   return "SpectrometerChannelID";
}

//______________________________________________________________________________
const char *SpectrometerChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SpectrometerChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int SpectrometerChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::SpectrometerChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *SpectrometerChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SpectrometerChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *SpectrometerChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::SpectrometerChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSpectrometerCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSpectrometerCandidate::Class_Name()
{
   return "TRecoSpectrometerCandidate";
}

//______________________________________________________________________________
const char *TRecoSpectrometerCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSpectrometerCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSpectrometerCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSpectrometerCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSpectrometerHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSpectrometerHit::Class_Name()
{
   return "TRecoSpectrometerHit";
}

//______________________________________________________________________________
const char *TRecoSpectrometerHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSpectrometerHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSpectrometerHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSpectrometerHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoSpectrometerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoSpectrometerEvent::Class_Name()
{
   return "TRecoSpectrometerEvent";
}

//______________________________________________________________________________
const char *TRecoSpectrometerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoSpectrometerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoSpectrometerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoSpectrometerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSpectrometerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSpectrometerDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSpectrometerDigi::Class_Name()
{
   return "TSpectrometerDigi";
}

//______________________________________________________________________________
const char *TSpectrometerDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSpectrometerDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSpectrometerDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSpectrometerDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSpectrometerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSpectrometerEvent::Class_Name()
{
   return "TSpectrometerEvent";
}

//______________________________________________________________________________
const char *TSpectrometerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSpectrometerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSpectrometerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSpectrometerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSpectrometerHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSpectrometerHit::Class_Name()
{
   return "TSpectrometerHit";
}

//______________________________________________________________________________
const char *TSpectrometerHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSpectrometerHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSpectrometerHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSpectrometerHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpectrometerHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void SRBError::Streamer(TBuffer &R__b)
{
   // Stream an object of class SRBError.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(SRBError::Class(),this);
   } else {
      R__b.WriteClassBuffer(SRBError::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_SRBError(void *p) {
      return  p ? new(p) ::SRBError : new ::SRBError;
   }
   static void *newArray_SRBError(Long_t nElements, void *p) {
      return p ? new(p) ::SRBError[nElements] : new ::SRBError[nElements];
   }
   // Wrapper around operator delete
   static void delete_SRBError(void *p) {
      delete ((::SRBError*)p);
   }
   static void deleteArray_SRBError(void *p) {
      delete [] ((::SRBError*)p);
   }
   static void destruct_SRBError(void *p) {
      typedef ::SRBError current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::SRBError

//______________________________________________________________________________
void SRBVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class SRBVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(SRBVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(SRBVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_SRBVHit(void *p) {
      delete ((::SRBVHit*)p);
   }
   static void deleteArray_SRBVHit(void *p) {
      delete [] ((::SRBVHit*)p);
   }
   static void destruct_SRBVHit(void *p) {
      typedef ::SRBVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::SRBVHit

//______________________________________________________________________________
void SRBEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class SRBEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(SRBEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(SRBEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_SRBEvent(void *p) {
      return  p ? new(p) ::SRBEvent : new ::SRBEvent;
   }
   static void *newArray_SRBEvent(Long_t nElements, void *p) {
      return p ? new(p) ::SRBEvent[nElements] : new ::SRBEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_SRBEvent(void *p) {
      delete ((::SRBEvent*)p);
   }
   static void deleteArray_SRBEvent(void *p) {
      delete [] ((::SRBEvent*)p);
   }
   static void destruct_SRBEvent(void *p) {
      typedef ::SRBEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::SRBEvent

//______________________________________________________________________________
void SpectrometerChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class SpectrometerChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(SpectrometerChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(SpectrometerChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_SpectrometerChannelID(void *p) {
      return  p ? new(p) ::SpectrometerChannelID : new ::SpectrometerChannelID;
   }
   static void *newArray_SpectrometerChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::SpectrometerChannelID[nElements] : new ::SpectrometerChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_SpectrometerChannelID(void *p) {
      delete ((::SpectrometerChannelID*)p);
   }
   static void deleteArray_SpectrometerChannelID(void *p) {
      delete [] ((::SpectrometerChannelID*)p);
   }
   static void destruct_SpectrometerChannelID(void *p) {
      typedef ::SpectrometerChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::SpectrometerChannelID

//______________________________________________________________________________
void TRecoSpectrometerCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSpectrometerCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSpectrometerCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSpectrometerCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSpectrometerCandidate(void *p) {
      return  p ? new(p) ::TRecoSpectrometerCandidate : new ::TRecoSpectrometerCandidate;
   }
   static void *newArray_TRecoSpectrometerCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSpectrometerCandidate[nElements] : new ::TRecoSpectrometerCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSpectrometerCandidate(void *p) {
      delete ((::TRecoSpectrometerCandidate*)p);
   }
   static void deleteArray_TRecoSpectrometerCandidate(void *p) {
      delete [] ((::TRecoSpectrometerCandidate*)p);
   }
   static void destruct_TRecoSpectrometerCandidate(void *p) {
      typedef ::TRecoSpectrometerCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSpectrometerCandidate

//______________________________________________________________________________
void TRecoSpectrometerHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSpectrometerHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSpectrometerHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSpectrometerHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSpectrometerHit(void *p) {
      return  p ? new(p) ::TRecoSpectrometerHit : new ::TRecoSpectrometerHit;
   }
   static void *newArray_TRecoSpectrometerHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSpectrometerHit[nElements] : new ::TRecoSpectrometerHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSpectrometerHit(void *p) {
      delete ((::TRecoSpectrometerHit*)p);
   }
   static void deleteArray_TRecoSpectrometerHit(void *p) {
      delete [] ((::TRecoSpectrometerHit*)p);
   }
   static void destruct_TRecoSpectrometerHit(void *p) {
      typedef ::TRecoSpectrometerHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSpectrometerHit

//______________________________________________________________________________
void TRecoSpectrometerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoSpectrometerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoSpectrometerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoSpectrometerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoSpectrometerEvent(void *p) {
      return  p ? new(p) ::TRecoSpectrometerEvent : new ::TRecoSpectrometerEvent;
   }
   static void *newArray_TRecoSpectrometerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoSpectrometerEvent[nElements] : new ::TRecoSpectrometerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoSpectrometerEvent(void *p) {
      delete ((::TRecoSpectrometerEvent*)p);
   }
   static void deleteArray_TRecoSpectrometerEvent(void *p) {
      delete [] ((::TRecoSpectrometerEvent*)p);
   }
   static void destruct_TRecoSpectrometerEvent(void *p) {
      typedef ::TRecoSpectrometerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoSpectrometerEvent

//______________________________________________________________________________
void TSpectrometerDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSpectrometerDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSpectrometerDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSpectrometerDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSpectrometerDigi(void *p) {
      return  p ? new(p) ::TSpectrometerDigi : new ::TSpectrometerDigi;
   }
   static void *newArray_TSpectrometerDigi(Long_t nElements, void *p) {
      return p ? new(p) ::TSpectrometerDigi[nElements] : new ::TSpectrometerDigi[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSpectrometerDigi(void *p) {
      delete ((::TSpectrometerDigi*)p);
   }
   static void deleteArray_TSpectrometerDigi(void *p) {
      delete [] ((::TSpectrometerDigi*)p);
   }
   static void destruct_TSpectrometerDigi(void *p) {
      typedef ::TSpectrometerDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSpectrometerDigi

//______________________________________________________________________________
void TSpectrometerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSpectrometerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSpectrometerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSpectrometerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSpectrometerEvent(void *p) {
      return  p ? new(p) ::TSpectrometerEvent : new ::TSpectrometerEvent;
   }
   static void *newArray_TSpectrometerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSpectrometerEvent[nElements] : new ::TSpectrometerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSpectrometerEvent(void *p) {
      delete ((::TSpectrometerEvent*)p);
   }
   static void deleteArray_TSpectrometerEvent(void *p) {
      delete [] ((::TSpectrometerEvent*)p);
   }
   static void destruct_TSpectrometerEvent(void *p) {
      typedef ::TSpectrometerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSpectrometerEvent

//______________________________________________________________________________
void TSpectrometerHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSpectrometerHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSpectrometerHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSpectrometerHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSpectrometerHit(void *p) {
      return  p ? new(p) ::TSpectrometerHit : new ::TSpectrometerHit;
   }
   static void *newArray_TSpectrometerHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSpectrometerHit[nElements] : new ::TSpectrometerHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSpectrometerHit(void *p) {
      delete ((::TSpectrometerHit*)p);
   }
   static void deleteArray_TSpectrometerHit(void *p) {
      delete [] ((::TSpectrometerHit*)p);
   }
   static void destruct_TSpectrometerHit(void *p) {
      typedef ::TSpectrometerHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSpectrometerHit

namespace {
  void TriggerDictionaryInitialization_libSpectrometerPersistency_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Spectrometer/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libSpectrometerPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class SRBError;
class __attribute__((annotate("$clingAutoload$SRBVHit.hh")))  SRBVHit;
class SRBEvent;
class SpectrometerChannelID;
class TRecoSpectrometerCandidate;
class __attribute__((annotate("$clingAutoload$TRecoSpectrometerHit.hh")))  TRecoSpectrometerHit;
class TRecoSpectrometerEvent;
class TSpectrometerDigi;
class TSpectrometerEvent;
class TSpectrometerHit;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libSpectrometerPersistency dictionary payload"

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
#ifndef SRBError_H
#define SRBError_H
#include "TDigiVError.hh"

class SRBError : public TDigiVError {

  public:

    SRBError();
    ~SRBError(){};

    void Clear(Option_t* = "");

  public:

    Int_t  GetStrawAddr() {return fStrawAddr;};
    void   SetStrawAddr(Int_t value) {fStrawAddr = value;};
    Int_t  GetSRBAddr() { return GetROBoardID();};
    void   SetSRBAddr(Int_t value){SetROBoardID(value);};
    Int_t  GetFlag() {return fFlag;};
    void   SetFlag(Int_t value) {fFlag=value;};

  private:

    Int_t fStrawAddr;
    Int_t fFlag;

    ClassDef(SRBError,1);
};
#endif
#ifndef SRBEvent_H
#define SRBEvent_H
#include "TClass.h"
#include "TDigiVEvent.hh"
#include "SRBVHit.hh"

class SRBEvent : public TDigiVEvent {

  public:

    SRBEvent();
    explicit SRBEvent(TClass *);
    SRBVHit* GetHit(Int_t);
    void Clear(Option_t* = "");

  private:

    ClassDef(SRBEvent,1);
};
#endif
#ifndef SRBVHit_H
#define SRBVHit_H
#include "TVDigi.hh"
#include "TDCVHit.hh"
#include "Riostream.h"

// Convention for leading/trailing detected edge bits
static const Int_t SRB_HIT_EDGE_LEADING  = 1;
static const Int_t SRB_HIT_EDGE_TRAILING = 2;

class SRBVHit : public TDCVHit {

    public:

        SRBVHit();
        explicit SRBVHit(Int_t);
        explicit SRBVHit(TVHit* MCHit);
        void Clear(Option_t* = "");

    public:

      Int_t GetSRBAddr() {return fSRBAddr;};
      void SetSRBAddr(Int_t val) {fSRBAddr=val;};
      Int_t GetStrawAddr() {return fStrawAddr;};
      void SetStrawAddr(Int_t val) {fStrawAddr=val;};
      Bool_t GetMultiHit() {return fMultiHit;};
      void SetMultiHit(Bool_t val) {fMultiHit=val;};
       

    private:

      Int_t   fSRBAddr       ;
      Int_t   fStrawAddr      ;
      Bool_t  fMultiHit       ;

    private:

        ClassDef(SRBVHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-01-31
//
// --------------------------------------------------------------
#ifndef SpectrometerChannelID_H
#define SpectrometerChannelID_H
#include "Rtypes.h"

class SpectrometerChannelID {

    public:

      SpectrometerChannelID();
      virtual ~SpectrometerChannelID() {}

      void Clear(Option_t* = "");

      Int_t EncodeChannelID();
      void DecodeChannelID(Int_t);

    public:

        Int_t                GetStrawID()                                       { return fStrawID;                      };
        void                 SetStrawID(Int_t value)                            { fStrawID = value;                     };
        Int_t                GetPlaneID()                                       { return fPlaneID;                      };
        void                 SetPlaneID(Int_t value)                            { fPlaneID = value;                     };
        Int_t                GetHalfViewID()                                    { return fHalfViewID;                   };
        void                 SetHalfViewID(Int_t value)                         { fHalfViewID = value;                  };
        Int_t                GetViewID()                                        { return fViewID;                       };
        void                 SetViewID(Int_t value)                             { fViewID = value;                      };
        Int_t                GetChamberID()                                     { return fChamberID;                    };
        void                 SetChamberID(Int_t value)                          { fChamberID = value;                   };

    private:

        Int_t      fStrawID;
        Int_t      fPlaneID;
        Int_t      fHalfViewID;
        Int_t      fViewID;
        Int_t      fChamberID;

        ClassDef(SpectrometerChannelID,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSpectrometerCandidate_H
#define TRecoSpectrometerCandidate_H

#include "TRecoVCandidate.hh"
#include "TLorentzVector.h"

class TRecoSpectrometerCandidate : public TRecoVCandidate {

public:
  TRecoSpectrometerCandidate();
  ~TRecoSpectrometerCandidate() {}
  void Clear(Option_t* = "");

  void SetSlopeXBeforeFit(Double_t val)                { fSlopeXBeforeFit=val;             }
  void SetSlopeYBeforeFit(Double_t val)                { fSlopeYBeforeFit=val;             }
  void SetMomentumBeforeFit(Double_t val)              { fMomentumBeforeFit=val;           }
  void SetCombinationTotalQuality(Double_t val)        { fCombinationTotalQuality=val;     }
  void SetCombinationHoughQuality(Double_t val)        { fCombinationHoughQuality=val;     }
  void SetSlopeXBeforeMagnet(Double_t val)             { fSlopeXBeforeMagnet=val;          }
  void SetSlopeYBeforeMagnet(Double_t val)             { fSlopeYBeforeMagnet=val;          }
  void SetPositionBeforeMagnet(TVector3 val)           { fPositionBeforeMagnet=val;        }
  void SetSlopeXAfterMagnet(Double_t val)              { fSlopeXAfterMagnet=val;           }
  void SetSlopeYAfterMagnet(Double_t val)              { fSlopeYAfterMagnet=val;           }
  void SetPositionAfterMagnet(TVector3 val)            { fPositionAfterMagnet=val;         }
  void SetMomentum(Double_t val)                       { fMomentum=val;                    }
  void SetCharge(Int_t val)                            { fCharge=val;                      }
  void SetChi2(Double_t val)                           { fChi2=val;                        }
  void SetNChambers(Int_t val)                         { fNChambers=val;                   }
  void SetChamberId(Int_t ich, Int_t val)              { fChamberId[ich]=val;              }
  void SetNTotalHitPerChamber(Int_t ich, Int_t val)    { fNTotalHitPerChamber[ich]=val;    }
  void SetNViewsPerChamber(Int_t ich, Int_t val)       { fNViewsPerChamber[ich]=val;       }
  void SetN2HitClusterPerChamber(Int_t ich, Int_t val) { fN2HitClusterPerChamber[ich]=val; }
  void SetN3HitClusterPerChamber(Int_t ich, Int_t val) { fN3HitClusterPerChamber[ich]=val; }
  void SetCovariance(Int_t i, Int_t j, Double_t val)   { fCovariance[i][j]=val;            }
  void SetPositionBeforeFit(TVector3 val)              { fPositionBeforeFit = val;         }
  void SetLeadingTime(Double_t val)                    { fLeadingTime = val;               }
  Double_t GetLeadingTime()                    { return fLeadingTime;                 }
  TVector3 GetPositionBeforeFit()              { return fPositionBeforeFit;           }
  Double_t GetSlopeXBeforeFit()                { return fSlopeXBeforeFit;             }
  Double_t GetSlopeYBeforeFit()                { return fSlopeYBeforeFit;             }
  Double_t GetChargeTimesMomentumBeforeFit()   { return fMomentumBeforeFit;           } // signed
  Double_t GetMomentumBeforeFit()              { return fabs(fMomentumBeforeFit);     } // unsigned
  Double_t GetCombinationTotalQuality()        { return fCombinationTotalQuality;     }
  Double_t GetCombinationHoughQuality()        { return fCombinationHoughQuality;     }
  Double_t GetSlopeXBeforeMagnet()             { return fSlopeXBeforeMagnet;          }
  Double_t GetSlopeYBeforeMagnet()             { return fSlopeYBeforeMagnet;          }
  TVector3 GetPositionBeforeMagnet()           { return fPositionBeforeMagnet;        }
  Double_t GetSlopeXAfterMagnet()              { return fSlopeXAfterMagnet;           }
  Double_t GetSlopeYAfterMagnet()              { return fSlopeYAfterMagnet;           }
  TVector3 GetPositionAfterMagnet()            { return fPositionAfterMagnet;         }
  Double_t GetMomentum()                       { return fMomentum;                    }
  Int_t    GetCharge()                         { return fCharge;                      }
  Double_t GetChi2()                           { return fChi2;                        }
  Int_t    GetNChambers()                      { return fNChambers;                   }
  Int_t    GetChamberId(Int_t ich)             { return fChamberId[ich];              }
  Int_t    GetNTotalHitPerChamber(Int_t ich)   { return fNTotalHitPerChamber[ich];    }
  Int_t    GetNViewsPerChamber(Int_t ich)      { return fNViewsPerChamber[ich];       }
  Int_t    GetN2HitClusterPerChamber(Int_t ich){ return fN2HitClusterPerChamber[ich]; }
  Int_t    GetN3HitClusterPerChamber(Int_t ich){ return fN3HitClusterPerChamber[ich]; }
  Double_t GetCovariance(Int_t i, Int_t j)     { return fCovariance[i][j];            }

  Double_t xAtBeforeMagnet(Double_t z); ///< X position of track before magnet in a certain Z plane
  Double_t yAtBeforeMagnet(Double_t z); ///< Y position of track before magnet in a certain Z plane
  Double_t xAtAfterMagnet (Double_t z); ///< X position of track after magnet in a certain Z plane
  Double_t yAtAfterMagnet (Double_t z); ///< Y position of track after magnet in a certain Z plane
  Double_t xAt(Double_t z); ///< Either xAtBeforeMagnet() or xAtAfterMagnet(), depending on the Z plane
  Double_t yAt(Double_t z); ///< Either yAtBeforeMagnet() or yAtAfterMagnet(), depending on the Z plane
  TVector3 GetThreeMomentumBeforeMagnet();
  TVector3 GetThreeMomentumAfterMagnet();
  TVector3 GetBlueFieldCorrectedPositionAtZ(Double_t);
  TVector3 GetBlueFieldCorrectedMomentumAtZ(Double_t);

private:
  Double_t fSlopeXBeforeFit;           ///< Track candidate slope in XZ plane from pattern recognition 
  Double_t fSlopeYBeforeFit;           ///< Track candidate slope in YZ plane from pattern recognition 
  TVector3 fPositionBeforeFit;         ///< X,Y position of the track candidate from pattern recognition (reference plane at Z = 0)
  Double_t fMomentumBeforeFit;         ///< Momentum*Charge of the track candidate from pattern recognition
  Double_t fCombinationTotalQuality;   ///< Quality of the track candidate from pattern recognition (to be used for track candidate built with 4 chambers)
  Double_t fCombinationHoughQuality;   ///< Quality of the Hough transformed in the YZ plane used in pattern recognition (usable for candidates with either 3 or 4 chambers)
  Double_t fSlopeXBeforeMagnet;        ///< Track slope in XZ before the MNP33 after fit
  Double_t fSlopeYBeforeMagnet;        ///< Track slope in YZ before the MNP33 after fit
  TVector3 fPositionBeforeMagnet;      ///< X,Y position of the track before MNP33 after fit (reference plane Z = 180000)
  Double_t fSlopeXAfterMagnet;         ///< Track slope in XZ after the MNP33 after fit
  Double_t fSlopeYAfterMagnet;         ///< Track slope in YZ after the MNP33 after fit
  TVector3 fPositionAfterMagnet;       ///< X,Y position of the track after MNP33 after fit (reference plane Z = 220000)
  Double_t fMomentum;                  ///< Absolute value of track momentum after fit
  Int_t    fCharge;                    ///< Charge of the track
  Double_t fChi2;                      ///< Chi2 of the fit
  Double_t fLeadingTime;               ///< Leading time of the track
  Int_t    fNChambers;                 ///< Number of chambers used to make the track candidate
  Int_t    fChamberId[4];              ///< Id of the chambers used to make the track candidate
  Int_t    fNTotalHitPerChamber[4];    ///< Number of straws per chambers used to make the track candidate
  Int_t    fNViewsPerChamber[4];       ///< Number of view per chambers used to make the track candidate
  Int_t    fN2HitClusterPerChamber[4]; ///< Number of 2-straw clusters per chamber used to make the track candidate
  Int_t    fN3HitClusterPerChamber[4]; ///< Number of 3-straw clusters per chamber used to make the track candidate
  Double_t fCovariance[5][5];          ///< Covariance matrix provided by the fit (slopes, positions, momentum -> thetaXY, XY, P) 
                                       ///< s2(thetaX)  s(thetaX)s(thetaY)  s(thetaX)s(X)  s(thetaX)s(Y)  s(thetaX)s(1/P) 
                                       ///<                s2(thetaY)       s(thetaY)s(X)  s(thetaY)s(Y)  s(thetaY)s(1/P)
                                       ///<                                     s2(X)        s(X)s(Y)       s(X)s(1/P)
                                       ///<                                                   s2(Y)         s(Y)s(1/P)
                                       ///<                                                                   s2(1/P)

     ClassDef(TRecoSpectrometerCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSpectrometerEvent_H
#define TRecoSpectrometerEvent_H

#include "TRecoVEvent.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TRecoSpectrometerHit.hh"

class TRecoSpectrometerEvent : public TRecoVEvent {

    public:

        TRecoSpectrometerEvent();
        ~TRecoSpectrometerEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoSpectrometerEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSpectrometerHit_H
#define TRecoSpectrometerHit_H

#include "TRecoVHit.hh"

class TRecoSpectrometerHit : public TRecoVHit {

    public:

        TRecoSpectrometerHit();
        ~TRecoSpectrometerHit(){};
        TRecoSpectrometerHit(const TRecoSpectrometerHit&);
        void Clear(Option_t* = "");
        void                 AddUsedDigi()                                      { fNUsedDigis++;                        };
        Double_t             GetDriftTime()                                     { return GetTime();                     };
        void                 SetDriftTime(Double_t value)                       { SetTime(value);                       };

    public:

        Int_t                GetHalfViewID()                                    { return fHalfViewID;                   };
        void                 SetHalfViewID(Int_t value)                         { fHalfViewID = value;                  };
        Int_t                GetViewID()                                        { return fViewID;                       };
        void                 SetViewID(Int_t value)                             { fViewID = value;                      };
        Int_t                GetChamberID()                                     { return fChamberID;                    };
        void                 SetChamberID(Int_t value)                          { fChamberID = value;                   };
        Double_t             GetWireAverageDistance()                           { return fWireAverageDistance;          };
        void                 SetWireAverageDistance(Double_t value)             { fWireAverageDistance = value;         };
        TVector3             GetDirection()                                     { return fDirection;                    };
        void                 SetDirection(TVector3 value)                       { fDirection = value;                   };

        Double_t             GetTimeWidth()                                     { return fTimeWidth;                    };
        void                 SetTimeWidth(Double_t value)                       { fTimeWidth = value;                   };
        Double_t             GetWireDistance()                                  { return fWireDistance;                 };
        void                 SetWireDistance(Double_t value)                    { fWireDistance = value;                };
        Int_t                GetNUsedDigis()                                    { return fNUsedDigis;                   };
        void                 SetNUsedDigis(Int_t value)                         { fNUsedDigis = value;                  };

        Int_t GetTDCID() { return fTDCID; };
        void SetTDCID(Int_t val) { fTDCID = val; };

        TVector3 GetLocalPosition()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fLocalPosition.
            ///
            /// The x coordinate is by default the measured one: indeed only one coordinate is measured per tube-hit.
            /// The not measured coordinates are -9999. (e.g. LR algorithm failed).
            /// \EndMemberDescr

            return fLocalPosition;
        };
        TVector3 GetPosition()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fPosition.
            /// \EndMemberDescr

            return fPosition;
        };
        TVector3 GetOriginalPosition()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fOriginalPosition.
            /// \EndMemberDescr

            return fOriginalPosition;
        };
        Double_t GetTime()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fTime.
            /// \EndMemberDescr

            return fTime;
        };
        Double_t GetEnergy()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fEnergy.
            /// \EndMemberDescr

            return fEnergy;
        };
        Int_t GetStrawID() 
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fStrawID.
            ///
            /// The id of the straw is a number nx, set in HalfView::CreateGeometry(), which runs over the straws in the x view-plane.
            /// The numeration is, according to the plane:
            /// -# plane 0: n0.
            /// -# plane 1: 1000+n1.
            /// -# plane 2: 10000+n2.
            /// -# plane 3: 11000+n3.
            /// \EndMemberDescr

            return fStrawID;
        };
        Int_t GetPlaneID() 
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fPlaneID.
            ///
            /// The view-planes are 0123.
            /// \EndMemberDescr

            return fPlaneID;
        };
        Double_t GetRadius()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fRadius.
            /// \EndMemberDescr

            return fRadius;
        };
        Int_t GetID()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fID.
            /// \EndMemberDescr

            return fID;
        };
        Int_t GetRecoID()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fRecoID.
            /// \EndMemberDescr

            return fRecoID;
        };
        Int_t GetMCID()
        {
            return fMCID;
        };
        Int_t GetSingle()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fSingle.
            /// \EndMemberDescr

            return fSingle;
        }   
        Int_t GetEdgeStatus()
        {
            /// \MemberDescr
            /// \return TRecoSpectrometerHit::fEdgeStatus.
            /// \EndMemberDescr

          return fEdgeStatus;
        }

        void SetLocalPosition(TVector3 val){fLocalPosition=val;};
        void SetPosition(TVector3 val){fPosition=val;};
        void SetOriginalPosition(TVector3 val){fOriginalPosition=val;};
        void SetTime(Double_t val){fTime=val;};
        void SetEnergy(Double_t val){fEnergy=val;};
        void SetStrawID(Int_t val){fStrawID=val;};
        void SetPlaneID(Int_t val){fPlaneID=val;};
        void SetID(Int_t val){fID=val;};
        void SetRecoID(Int_t val){fRecoID=val;};
        void SetRadius(Double_t val){fRadius=val;};
        void SetMCID(Int_t val){fMCID=val;};
        void SetSingle(Int_t val){fSingle=val;};
        void SetEdgeStatus(Int_t val){fEdgeStatus=val;};
        // overriding methods of TVChannelID, needed due to fChannelID redefinition below
        Int_t GetChannelID()            { return fChannelID;         }
        Int_t SetChannelID(Int_t value) { return fChannelID = TVChannelID::SetChannelID(value); }

    private:
        TVector3 fLocalPosition; ///< Reconstructed tube-hit position in the chamber reference frame.
        TVector3 fPosition; ///< Reconstructed tube-hit position in the laboratory reference frame.
        TVector3 fOriginalPosition; ///< Original hit position as given by NA62MC, but after the delta-ray merging algorithm (see SpectrometerReconstruction)
        Double_t fTime; ///< Time of the hit as given by NA62MC.
        Double_t fEnergy; ///< Energy deposition as given by NA62MC, but after the delta-ray merging algorithm.
        Int_t fStrawID; ///< ID of the hitted straw tube.
        Int_t fPlaneID; ///< ID of the view-plane of the hitted straw tube.
        Double_t fRadius; ///< Measured radius, after digitization.
        Int_t fRecoID; ///< ID of the tube-hit according to the TRecoSpectrometerHit sequence.
        Int_t fID; ///< ID of the tube-hit according to the TSpectrometerHit sequence
        Int_t fTDCID;
        Int_t fMCID;
        Int_t fChannelID;
        Int_t fSingle; ///< Flag the hit as single (no LR ambiguity solution) or not.
        Int_t fEdgeStatus; ///< Status of the edge: 0 only leading, 1 leading and trailing. 

        TVector3   fDirection;
        Int_t      fHalfViewID;
        Int_t      fViewID;
        Int_t      fChamberID;
        Double_t   fWireAverageDistance;
        Double_t fTimeWidth;
        Double_t fWireDistance;

        Int_t fNUsedDigis;

        ClassDef(TRecoSpectrometerHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TSpectrometerDigi_H
#define TSpectrometerDigi_H

//mod//#include "TDCVHit.hh"
#include "SRBVHit.hh"
#include "SpectrometerChannelID.hh"

//class TSpectrometerDigi : public TDCVHit, public SpectrometerChannelID {
class TSpectrometerDigi : public SRBVHit, public SpectrometerChannelID {

    public:

//        TSpectrometerDigi() : TDCVHit(){}
//        TSpectrometerDigi(Int_t iCh) : TDCVHit(iCh){}
        TSpectrometerDigi() : SRBVHit(), SpectrometerChannelID(), fHitID(0) {}
        explicit TSpectrometerDigi(Int_t iCh) : SRBVHit(iCh), SpectrometerChannelID(), fHitID(0) {}
        ~TSpectrometerDigi(){}
        void Clear(Option_t* = "");
        Bool_t IsSortable() const { return kTRUE; }
        Int_t Compare(const TObject *obj) const {Int_t res = TVHit::Compare(obj); if(res == 0){ return TDCVHit::Compare(obj);
                                                                                              } else {return res;}}
        Int_t EncodeChannelID();
        void  DecodeChannelID();

        //Int_t GetStationID() { return GetChamberID(); }
        Int_t GetStationID() { return 0; }

    public:
        Int_t GetHitID() {return fHitID;};
        void SetHitID(Int_t val) {fHitID=val;};

    private:
        Int_t fHitID;

        ClassDef(TSpectrometerDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TSpectrometerEvent_H
#define TSpectrometerEvent_H

#include "TDetectorVEvent.hh"

class TSpectrometerEvent : public TDetectorVEvent {

    public:

        TSpectrometerEvent();
        ~TSpectrometerEvent();
        void Clear(Option_t* = "");

    private:

        ClassDef(TSpectrometerEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TSpectrometerHit_H
#define TSpectrometerHit_H

#include "TDetectorVHit.hh"
#include "SpectrometerChannelID.hh"

class TSpectrometerHit : public TDetectorVHit, public SpectrometerChannelID {

    public:

        TSpectrometerHit();
        virtual ~TSpectrometerHit(){};
        void Clear(Option_t* = "");
        Int_t EncodeChannelID();
        void DecodeChannelID();

        //Int_t GetStationID() { return GetChamberID(); }
        Int_t GetStationID() { return 0; }

    public:

        TVector3             GetDirection()                                     { return fDirection;                    };
        void                 SetDirection(TVector3 value)                       { fDirection = value;                   };
        TVector3             GetLocalPosition()                                 { return fLocalPosition;                };
        void                 SetLocalPosition(TVector3 value)                   { fLocalPosition = value;               };

        Double_t             GetWireDistance()                                  { return fWireDistance;                 };
        void                 SetWireDistance(Double_t value)                    { fWireDistance = value;                };

    protected:

        TVector3   fDirection;
        TVector3   fLocalPosition;

        Double_t   fWireDistance;

        ClassDef(TSpectrometerHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"SRBError", payloadCode, "@",
"SRBEvent", payloadCode, "@",
"SRBVHit", payloadCode, "@",
"SpectrometerChannelID", payloadCode, "@",
"TRecoSpectrometerCandidate", payloadCode, "@",
"TRecoSpectrometerEvent", payloadCode, "@",
"TRecoSpectrometerHit", payloadCode, "@",
"TSpectrometerDigi", payloadCode, "@",
"TSpectrometerEvent", payloadCode, "@",
"TSpectrometerHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libSpectrometerPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libSpectrometerPersistency_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libSpectrometerPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libSpectrometerPersistency() {
  TriggerDictionaryInitialization_libSpectrometerPersistency_Impl();
}
