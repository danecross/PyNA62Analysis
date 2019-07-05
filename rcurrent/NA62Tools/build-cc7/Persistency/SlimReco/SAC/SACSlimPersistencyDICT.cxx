// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME SACSlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAC/include/TSlimRecoSACCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAC/include/TSlimRecoSACEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAC/include/TSlimRecoSACHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoSACCandidate(void *p = 0);
   static void *newArray_TSlimRecoSACCandidate(Long_t size, void *p);
   static void delete_TSlimRecoSACCandidate(void *p);
   static void deleteArray_TSlimRecoSACCandidate(void *p);
   static void destruct_TSlimRecoSACCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSACCandidate*)
   {
      ::TSlimRecoSACCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSACCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSACCandidate", ::TSlimRecoSACCandidate::Class_Version(), "", 17,
                  typeid(::TSlimRecoSACCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSACCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSACCandidate) );
      instance.SetNew(&new_TSlimRecoSACCandidate);
      instance.SetNewArray(&newArray_TSlimRecoSACCandidate);
      instance.SetDelete(&delete_TSlimRecoSACCandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSACCandidate);
      instance.SetDestructor(&destruct_TSlimRecoSACCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSACCandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSACCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSACCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoSACEvent(void *p = 0);
   static void *newArray_TSlimRecoSACEvent(Long_t size, void *p);
   static void delete_TSlimRecoSACEvent(void *p);
   static void deleteArray_TSlimRecoSACEvent(void *p);
   static void destruct_TSlimRecoSACEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSACEvent*)
   {
      ::TSlimRecoSACEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSACEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSACEvent", ::TSlimRecoSACEvent::Class_Version(), "", 95,
                  typeid(::TSlimRecoSACEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSACEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSACEvent) );
      instance.SetNew(&new_TSlimRecoSACEvent);
      instance.SetNewArray(&newArray_TSlimRecoSACEvent);
      instance.SetDelete(&delete_TSlimRecoSACEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSACEvent);
      instance.SetDestructor(&destruct_TSlimRecoSACEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSACEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSACEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSACEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoSACHit(void *p = 0);
   static void *newArray_TSlimRecoSACHit(Long_t size, void *p);
   static void delete_TSlimRecoSACHit(void *p);
   static void deleteArray_TSlimRecoSACHit(void *p);
   static void destruct_TSlimRecoSACHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoSACHit*)
   {
      ::TSlimRecoSACHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoSACHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoSACHit", ::TSlimRecoSACHit::Class_Version(), "", 129,
                  typeid(::TSlimRecoSACHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoSACHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoSACHit) );
      instance.SetNew(&new_TSlimRecoSACHit);
      instance.SetNewArray(&newArray_TSlimRecoSACHit);
      instance.SetDelete(&delete_TSlimRecoSACHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoSACHit);
      instance.SetDestructor(&destruct_TSlimRecoSACHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoSACHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoSACHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoSACHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSACCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSACCandidate::Class_Name()
{
   return "TSlimRecoSACCandidate";
}

//______________________________________________________________________________
const char *TSlimRecoSACCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSACCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSACCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSACCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSACEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSACEvent::Class_Name()
{
   return "TSlimRecoSACEvent";
}

//______________________________________________________________________________
const char *TSlimRecoSACEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSACEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSACEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSACEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoSACHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoSACHit::Class_Name()
{
   return "TSlimRecoSACHit";
}

//______________________________________________________________________________
const char *TSlimRecoSACHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoSACHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoSACHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoSACHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoSACHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoSACCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSACCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSACCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSACCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSACCandidate(void *p) {
      return  p ? new(p) ::TSlimRecoSACCandidate : new ::TSlimRecoSACCandidate;
   }
   static void *newArray_TSlimRecoSACCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSACCandidate[nElements] : new ::TSlimRecoSACCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSACCandidate(void *p) {
      delete ((::TSlimRecoSACCandidate*)p);
   }
   static void deleteArray_TSlimRecoSACCandidate(void *p) {
      delete [] ((::TSlimRecoSACCandidate*)p);
   }
   static void destruct_TSlimRecoSACCandidate(void *p) {
      typedef ::TSlimRecoSACCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSACCandidate

//______________________________________________________________________________
void TSlimRecoSACEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSACEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSACEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSACEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSACEvent(void *p) {
      return  p ? new(p) ::TSlimRecoSACEvent : new ::TSlimRecoSACEvent;
   }
   static void *newArray_TSlimRecoSACEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSACEvent[nElements] : new ::TSlimRecoSACEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSACEvent(void *p) {
      delete ((::TSlimRecoSACEvent*)p);
   }
   static void deleteArray_TSlimRecoSACEvent(void *p) {
      delete [] ((::TSlimRecoSACEvent*)p);
   }
   static void destruct_TSlimRecoSACEvent(void *p) {
      typedef ::TSlimRecoSACEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSACEvent

//______________________________________________________________________________
void TSlimRecoSACHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoSACHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoSACHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoSACHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoSACHit(void *p) {
      return  p ? new(p) ::TSlimRecoSACHit : new ::TSlimRecoSACHit;
   }
   static void *newArray_TSlimRecoSACHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoSACHit[nElements] : new ::TSlimRecoSACHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoSACHit(void *p) {
      delete ((::TSlimRecoSACHit*)p);
   }
   static void deleteArray_TSlimRecoSACHit(void *p) {
      delete [] ((::TSlimRecoSACHit*)p);
   }
   static void destruct_TSlimRecoSACHit(void *p) {
      typedef ::TSlimRecoSACHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoSACHit

namespace ROOT {
   static TClass *vectorlETSlimRecoSACCandidategR_Dictionary();
   static void vectorlETSlimRecoSACCandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoSACCandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoSACCandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoSACCandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoSACCandidategR(void *p);
   static void destruct_vectorlETSlimRecoSACCandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoSACCandidate>*)
   {
      vector<TSlimRecoSACCandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoSACCandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoSACCandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoSACCandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoSACCandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoSACCandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoSACCandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoSACCandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoSACCandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoSACCandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoSACCandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoSACCandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoSACCandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoSACCandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoSACCandidate>*)0x0)->GetClass();
      vectorlETSlimRecoSACCandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoSACCandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoSACCandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSACCandidate> : new vector<TSlimRecoSACCandidate>;
   }
   static void *newArray_vectorlETSlimRecoSACCandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoSACCandidate>[nElements] : new vector<TSlimRecoSACCandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoSACCandidategR(void *p) {
      delete ((vector<TSlimRecoSACCandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoSACCandidategR(void *p) {
      delete [] ((vector<TSlimRecoSACCandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoSACCandidategR(void *p) {
      typedef vector<TSlimRecoSACCandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoSACCandidate>

namespace {
  void TriggerDictionaryInitialization_libSACSlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAC/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAC/../../FullReco/SAC/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAC/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libSACSlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class TSlimRecoSACCandidate;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoSACEvent;
class TSlimRecoSACHit;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libSACSlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOSACCANDIDATE_H
#define TSLIMRECOSACCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVCandidate.hh"
#include <TVector3.h>

class TRecoSACHit;
class TRecoVHit;

class TSlimRecoSACCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoSACCandidate() = default;
    explicit TSlimRecoSACCandidate(TRecoSACHit *);
    virtual ~TSlimRecoSACCandidate() = default;

    // setters for members
    void SetChannelID       (Short_t value)    { fChannelID = value;          }
    void SetTime            (Float_t value)    { fTime = value;               }
    void SetLeadingEdgeLow  (Float_t edgeTime) { fLeadingEdgeLow = edgeTime;  }
    void SetLeadingEdgeHigh (Float_t edgeTime) { fLeadingEdgeHigh = edgeTime; }

    void SetTimeOverThresholdLowThr (Float_t val) { fTimeOvThrLow=val;          }
    void SetLeadingESlewingSlope    (Float_t val) { fLeadingESlewingSlope =val; }
    void SetTrailingESlewingSlope   (Float_t val) { fTrailingESlewingSlope=val; }

    Float_t GetLeadingEdgeLow  () const { return ((fEdgeMask & 0x1)? fLeadingEdgeLow  :0);         }
    Float_t GetLeadingEdgeHigh () const { return ((fEdgeMask & 0x2)? fLeadingEdgeHigh :0);         }
    Float_t GetTrailingEdgeHigh() const { return ((fEdgeMask & 0x4)? ComputeTrailingEdgeHigh():0); }
    Float_t GetTrailingEdgeLow () const { return ((fEdgeMask & 0x8)? ComputeTrailingEdgeLow() :0); }

    Float_t GetTimeOverThresholdLowThr () const { return fTimeOvThrLow;          }
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
    Short_t fEdgeMask = 0; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow -> Not needed if we keep the 4 times. If the following 4 are not needed, this one replaces them.

    Float_t fTime      = 0.;

    Float_t fTimeOvThrLow     = -999.;
    Float_t fTimeOvThrHigh    = -999.;

    Float_t fLeadingESlewingSlope  = -999.;
    Float_t fTrailingESlewingSlope = -999.;

    Float_t fLeadingEdgeLow   = -999.; ///< Time of leading low, subtracted of the trigger time only
    Float_t fLeadingEdgeHigh  = -999.;///< Time of trailing high, subtracted of the trigger time only

    ClassDef(TSlimRecoSACCandidate, 1)
};

#endif /* TSLIMRECOSACCANDIDATE_H */
#ifndef TRECOSACEVENTSLIM_H
#define TRECOSACEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoSACCandidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoSACEvent;

class TSlimRecoSACEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoSACEvent() = default;
    explicit TSlimRecoSACEvent(TRecoSACEvent *evReco);
    virtual ~TSlimRecoSACEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearCandidates();

    void AddCandidate(TSlimRecoSACCandidate h) { fCandidates.emplace_back(std::move(h));  }

    Int_t GetNCandidates()                              const { return fCandidates.size(); }
    std::vector<TSlimRecoSACCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoSACCandidate> fCandidates;

    ClassDef(TSlimRecoSACEvent, 1)
};

#endif /* TRECOSACEVENTSLIM_H */
#ifndef TSLIMRECOSACHIT_H
#define TSLIMRECOSACHIT_H

#include "TSlimRecoVHit.hh"

class TRecoSACHit;

class TSlimRecoSACHit : public TSlimRecoVHit
{
public:
    TSlimRecoSACHit()              {}
    explicit TSlimRecoSACHit(TRecoSACHit *) {}
    virtual ~TSlimRecoSACHit()     {}

    // conversion functions
    virtual void FromReco(TRecoVHit *) {}
    virtual void ToReco(TRecoVHit *)   {}
private:

    ClassDef(TSlimRecoSACHit, 1)
};

#endif /* TSLIMRECOSACHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoSACCandidate", payloadCode, "@",
"TSlimRecoSACEvent", payloadCode, "@",
"TSlimRecoSACHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libSACSlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libSACSlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libSACSlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libSACSlimPersistency() {
  TriggerDictionaryInitialization_libSACSlimPersistency_Impl();
}
