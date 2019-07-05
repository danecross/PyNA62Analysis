// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME IRCSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/IRC/include/TSlimRecoIRCCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/IRC/include/TSlimRecoIRCEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/IRC/include/TSlimRecoIRCHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoIRCCandidate(void *p = 0);
   static void *newArray_TSlimRecoIRCCandidate(Long_t size, void *p);
   static void delete_TSlimRecoIRCCandidate(void *p);
   static void deleteArray_TSlimRecoIRCCandidate(void *p);
   static void destruct_TSlimRecoIRCCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoIRCCandidate*)
   {
      ::TSlimRecoIRCCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoIRCCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoIRCCandidate", ::TSlimRecoIRCCandidate::Class_Version(), "", 17,
                  typeid(::TSlimRecoIRCCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoIRCCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoIRCCandidate) );
      instance.SetNew(&new_TSlimRecoIRCCandidate);
      instance.SetNewArray(&newArray_TSlimRecoIRCCandidate);
      instance.SetDelete(&delete_TSlimRecoIRCCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoIRCCandidate);
      instance.SetDestructor(&destruct_TSlimRecoIRCCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoIRCCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoIRCCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoIRCCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoIRCEvent(void *p = 0);
   static void *newArray_TSlimRecoIRCEvent(Long_t size, void *p);
   static void delete_TSlimRecoIRCEvent(void *p);
   static void deleteArray_TSlimRecoIRCEvent(void *p);
   static void destruct_TSlimRecoIRCEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoIRCEvent*)
   {
      ::TSlimRecoIRCEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoIRCEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoIRCEvent", ::TSlimRecoIRCEvent::Class_Version(), "", 96,
                  typeid(::TSlimRecoIRCEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoIRCEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoIRCEvent) );
      instance.SetNew(&new_TSlimRecoIRCEvent);
      instance.SetNewArray(&newArray_TSlimRecoIRCEvent);
      instance.SetDelete(&delete_TSlimRecoIRCEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoIRCEvent);
      instance.SetDestructor(&destruct_TSlimRecoIRCEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoIRCEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoIRCEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoIRCEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoIRCHit(void *p = 0);
   static void *newArray_TSlimRecoIRCHit(Long_t size, void *p);
   static void delete_TSlimRecoIRCHit(void *p);
   static void deleteArray_TSlimRecoIRCHit(void *p);
   static void destruct_TSlimRecoIRCHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoIRCHit*)
   {
      ::TSlimRecoIRCHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoIRCHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoIRCHit", ::TSlimRecoIRCHit::Class_Version(), "", 130,
                  typeid(::TSlimRecoIRCHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoIRCHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoIRCHit) );
      instance.SetNew(&new_TSlimRecoIRCHit);
      instance.SetNewArray(&newArray_TSlimRecoIRCHit);
      instance.SetDelete(&delete_TSlimRecoIRCHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoIRCHit);
      instance.SetDestructor(&destruct_TSlimRecoIRCHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoIRCHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoIRCHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoIRCHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoIRCCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoIRCCandidate::Class_Name()
{
   return "TSlimRecoIRCCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoIRCCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoIRCCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoIRCCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoIRCCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoIRCEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoIRCEvent::Class_Name()
{
   return "TSlimRecoIRCEvent";
}

//______________________________________________________________________________
const char *TSlimRecoIRCEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoIRCEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoIRCEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoIRCEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoIRCHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoIRCHit::Class_Name()
{
   return "TSlimRecoIRCHit";
}

//______________________________________________________________________________
const char *TSlimRecoIRCHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoIRCHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoIRCHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoIRCHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoIRCHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoIRCCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoIRCCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoIRCCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoIRCCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoIRCCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoIRCCandidate : new ::TSlimRecoIRCCandidate;
   }
   static void *newArray_TSlimRecoIRCCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoIRCCandidate[nElements] : new ::TSlimRecoIRCCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoIRCCandidate(void *p) {
      delete ((::TSlimRecoIRCCandidate*)p);
   }
   static void deleteArray_TSlimRecoIRCCandidate(void *p) {
      delete [] ((::TSlimRecoIRCCandidate*)p);
   }
   static void destruct_TSlimRecoIRCCandidate(void *p) {
      typedef ::TSlimRecoIRCCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoIRCCandidate

//______________________________________________________________________________
void TSlimRecoIRCEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoIRCEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoIRCEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoIRCEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoIRCEvent(void *p) {
      return  p ? new(p) ::TSlimRecoIRCEvent : new ::TSlimRecoIRCEvent;
   }
   static void *newArray_TSlimRecoIRCEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoIRCEvent[nElements] : new ::TSlimRecoIRCEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoIRCEvent(void *p) {
      delete ((::TSlimRecoIRCEvent*)p);
   }
   static void deleteArray_TSlimRecoIRCEvent(void *p) {
      delete [] ((::TSlimRecoIRCEvent*)p);
   }
   static void destruct_TSlimRecoIRCEvent(void *p) {
      typedef ::TSlimRecoIRCEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoIRCEvent

//______________________________________________________________________________
void TSlimRecoIRCHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoIRCHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoIRCHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoIRCHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoIRCHit(void *p) {
      return  p ? new(p) ::TSlimRecoIRCHit : new ::TSlimRecoIRCHit;
   }
   static void *newArray_TSlimRecoIRCHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoIRCHit[nElements] : new ::TSlimRecoIRCHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoIRCHit(void *p) {
      delete ((::TSlimRecoIRCHit*)p);
   }
   static void deleteArray_TSlimRecoIRCHit(void *p) {
      delete [] ((::TSlimRecoIRCHit*)p);
   }
   static void destruct_TSlimRecoIRCHit(void *p) {
      typedef ::TSlimRecoIRCHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoIRCHit

namespace ROOT {
   static TClass *vectorlETSlimRecoIRCCandidategR_Dictionary();
   static void vectorlETSlimRecoIRCCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoIRCCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoIRCCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoIRCCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoIRCCandidategR(void *p);
   static void destruct_vectorlETSlimRecoIRCCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoIRCCandidate>*)
   {
      vector<TSlimRecoIRCCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoIRCCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoIRCCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoIRCCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoIRCCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoIRCCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoIRCCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoIRCCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoIRCCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoIRCCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoIRCCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoIRCCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoIRCCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoIRCCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoIRCCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoIRCCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoIRCCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoIRCCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoIRCCandidate> : new vector<TSlimRecoIRCCandidate>;
   }
   static void *newArray_vectorlETSlimRecoIRCCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoIRCCandidate>[nElements] : new vector<TSlimRecoIRCCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoIRCCandidategR(void *p) {
      delete ((vector<TSlimRecoIRCCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoIRCCandidategR(void *p) {
      delete [] ((vector<TSlimRecoIRCCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoIRCCandidategR(void *p) {
      typedef vector<TSlimRecoIRCCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoIRCCandidate>

namespace {
  void TriggerDictionaryInitialization_libIRCSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/IRC/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/IRC/../../FullReco/IRC/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/IRC/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libIRCSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class TSlimRecoIRCCandidate;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoIRCEvent;
class TSlimRecoIRCHit;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libIRCSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOIRCCANDIDATE_H
#define TSLIMRECOIRCCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVCandidate.hh"
#include <TVector3.h>

class TRecoIRCHit;
class TRecoVHit;

class TSlimRecoIRCCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoIRCCandidate() = default;
    explicit TSlimRecoIRCCandidate(TRecoIRCHit *);
    virtual ~TSlimRecoIRCCandidate() = default;

    // setters for members
    void SetChannelID       (Short_t value)    { fChannelID = value;          }
    void SetTime            (Float_t value)    { fTime = value;               }
    void SetLeadingEdgeLow  (Float_t edgeTime) { fLeadingEdgeLow = edgeTime;  }
    void SetLeadingEdgeHigh (Float_t edgeTime) { fLeadingEdgeHigh = edgeTime; }

    void SetTimeOverThresholdLowThr (Float_t val) { fTimeOvThrLow=val;          }
    void SetTimeOverThresholdHighThr(Float_t val) { fTimeOvThrHigh=val;         }
    void SetLeadingESlewingSlope    (Float_t val) { fLeadingESlewingSlope =val; }
    void SetTrailingESlewingSlope   (Float_t val) { fTrailingESlewingSlope=val; }

    Float_t GetLeadingEdgeLow  () const { return ((fEdgeMask & 0x1)? fLeadingEdgeLow  :0); }
    Float_t GetLeadingEdgeHigh () const { return ((fEdgeMask & 0x2)? fLeadingEdgeHigh :0); }
    Float_t GetTrailingEdgeHigh() const { return ((fEdgeMask & 0x4)? ComputeTrailingEdgeHigh():0); }
    Float_t GetTrailingEdgeLow () const { return ((fEdgeMask & 0x8)? ComputeTrailingEdgeLow() :0); }

    Float_t GetTimeOverThresholdLowThr () const { return fTimeOvThrLow;          }
    Float_t GetTimeOverThresholdHighThr() const { return fTimeOvThrHigh;         }
    Float_t GetLeadingESlewingSlope    () const { return fLeadingESlewingSlope ; }
    Float_t GetTrailingESlewingSlope   () const { return fTrailingESlewingSlope; }
    Short_t GetChannelID               () const { return fChannelID;             }

    Bool_t HasLeadingEdgeLow   () const { return fEdgeMask & 0x1; }
    Bool_t HasLeadingEdgeHigh  () const { return fEdgeMask & 0x2; }
    Bool_t HasTrailingEdgeHigh () const { return fEdgeMask & 0x4; }
    Bool_t HasTrailingEdgeLow  () const { return fEdgeMask & 0x8; }
    Bool_t HasAll4EdgesDetected() const { return fEdgeMask==0xF;  }
    Bool_t HasAllTimesInOrder  () const;

    Short_t GetPMTID()    const;
    Short_t GetEdgeMask() const { return fEdgeMask;       }
    Float_t GetTime()     const { return fTime;           }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *) {}
    virtual void ToReco(TRecoVCandidate *)   {}

    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Float_t ComputeTrailingEdgeLow()  const;
    Float_t ComputeTrailingEdgeHigh() const;

    Short_t fChannelID = -1;
    Short_t fEdgeMask = 0; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow

    Float_t fTime      = 0.;

    Float_t fTimeOvThrLow          = -999.;
    Float_t fTimeOvThrHigh         = -999.;
    Float_t fLeadingESlewingSlope  = -999.;
    Float_t fTrailingESlewingSlope = -999.;

    Float_t fLeadingEdgeLow   = -999.; ///< Time of leading low, subtracted of the trigger time only
    Float_t fLeadingEdgeHigh  = -999.; ///< Time of trailing high, subtracted of the trigger time only

    ClassDef(TSlimRecoIRCCandidate, 1)
};

#endif /* TSLIMRECOIRCCANDIDATE_H */
#ifndef TRECOIRCEVENTSLIM_H
#define TRECOIRCEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoIRCCandidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoIRCEvent;

class TSlimRecoIRCEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoIRCEvent() = default;
    explicit TSlimRecoIRCEvent(TRecoIRCEvent *evReco);
    virtual ~TSlimRecoIRCEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearCandidates();

    void AddCandidate(TSlimRecoIRCCandidate h) { fCandidates.emplace_back(std::move(h));  }

    Int_t GetNCandidates()                              const { return fCandidates.size(); }
    std::vector<TSlimRecoIRCCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoIRCCandidate> fCandidates;

    ClassDef(TSlimRecoIRCEvent, 1)
};

#endif /* TRECOIRCEVENTSLIM_H */
#ifndef TSLIMRECOIRCHIT_H
#define TSLIMRECOIRCHIT_H

#include "TSlimRecoVHit.hh"

class TRecoIRCHit;

class TSlimRecoIRCHit : public TSlimRecoVHit
{
public:
    TSlimRecoIRCHit()              {}
    explicit TSlimRecoIRCHit(TRecoIRCHit *) {}
    virtual ~TSlimRecoIRCHit()     {}

    // conversion functions
    virtual void FromReco(TRecoVHit *) {}
    virtual void ToReco(TRecoVHit *)   {}
private:

    ClassDef(TSlimRecoIRCHit, 1)
};

#endif /* TSLIMRECOIRCHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoIRCCandidate", payloadCode, "@",
"TSlimRecoIRCEvent", payloadCode, "@",
"TSlimRecoIRCHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libIRCSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libIRCSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libIRCSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libIRCSlimPersistency() {
  TriggerDictionaryInitialization_libIRCSlimPersistency_Impl();
}
