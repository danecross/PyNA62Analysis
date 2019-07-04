// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME SAVPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAV/include/SAVChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAV/include/TRecoSAVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAV/include/TRecoSAVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAV/include/TRecoSAVHit.hh"
#include "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAV/include/TSAVDigi.hh"

// Header files passed via #pragma extra_include

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
         instance("SAVChannelID", "", 20,
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
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::SAVChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

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
         instance("TRecoSAVCandidate", ::TRecoSAVCandidate::Class_Version(), "", 61,
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
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoSAVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
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
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoSAVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
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
         instance("TRecoSAVEvent", ::TRecoSAVEvent::Class_Version(), "", 111,
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
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoSAVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
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
         instance("TSAVDigi", ::TSAVDigi::Class_Version(), "", 185,
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
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSAVDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

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
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVCandidate*)0x0)->GetClass(); }
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
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVHit*)0x0)->GetClass(); }
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
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoSAVEvent*)0x0)->GetClass(); }
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
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSAVDigi*)0x0)->GetClass(); }
   return fgIsA;
}

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
  void TriggerDictionaryInitialization_libSAVPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAV/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAV/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libSAVPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class SAVChannelID;
class TRecoSAVCandidate;
class __attribute__((annotate("$clingAutoload$TRecoSAVHit.hh")))  TRecoSAVHit;
class TRecoSAVEvent;
class TSAVDigi;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libSAVPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
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
    struct chIDDecoded { Int_t fDetectorID, fDetectorChannel; };

    SAVChannelID();
    explicit SAVChannelID(Int_t ChannelID);
    virtual ~SAVChannelID();

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    static struct chIDDecoded DecodeChannelID_Static(Int_t ChannelID);
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
"SAVChannelID", payloadCode, "@",
"TRecoSAVCandidate", payloadCode, "@",
"TRecoSAVEvent", payloadCode, "@",
"TRecoSAVHit", payloadCode, "@",
"TSAVDigi", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libSAVPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libSAVPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libSAVPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libSAVPersistency() {
  TriggerDictionaryInitialization_libSAVPersistency_Impl();
}
