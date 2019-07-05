// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV0SlimPersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV0/include/TSlimRecoMUV0Candidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV0/include/TSlimRecoMUV0Event.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV0/include/TSlimRecoMUV0Hit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoMUV0Candidate(void *p = 0);
   static void *newArray_TSlimRecoMUV0Candidate(Long_t size, void *p);
   static void delete_TSlimRecoMUV0Candidate(void *p);
   static void deleteArray_TSlimRecoMUV0Candidate(void *p);
   static void destruct_TSlimRecoMUV0Candidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV0Candidate*)
   {
      ::TSlimRecoMUV0Candidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV0Candidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV0Candidate", ::TSlimRecoMUV0Candidate::Class_Version(), "", 17,
                  typeid(::TSlimRecoMUV0Candidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV0Candidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV0Candidate) );
      instance.SetNew(&new_TSlimRecoMUV0Candidate);
      instance.SetNewArray(&newArray_TSlimRecoMUV0Candidate);
      instance.SetDelete(&delete_TSlimRecoMUV0Candidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV0Candidate);
      instance.SetDestructor(&destruct_TSlimRecoMUV0Candidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV0Candidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV0Candidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV0Candidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV0Event(void *p = 0);
   static void *newArray_TSlimRecoMUV0Event(Long_t size, void *p);
   static void delete_TSlimRecoMUV0Event(void *p);
   static void deleteArray_TSlimRecoMUV0Event(void *p);
   static void destruct_TSlimRecoMUV0Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV0Event*)
   {
      ::TSlimRecoMUV0Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV0Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV0Event", ::TSlimRecoMUV0Event::Class_Version(), "", 96,
                  typeid(::TSlimRecoMUV0Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV0Event::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV0Event) );
      instance.SetNew(&new_TSlimRecoMUV0Event);
      instance.SetNewArray(&newArray_TSlimRecoMUV0Event);
      instance.SetDelete(&delete_TSlimRecoMUV0Event);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV0Event);
      instance.SetDestructor(&destruct_TSlimRecoMUV0Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV0Event*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV0Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV0Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoMUV0Hit(void *p = 0);
   static void *newArray_TSlimRecoMUV0Hit(Long_t size, void *p);
   static void delete_TSlimRecoMUV0Hit(void *p);
   static void deleteArray_TSlimRecoMUV0Hit(void *p);
   static void destruct_TSlimRecoMUV0Hit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoMUV0Hit*)
   {
      ::TSlimRecoMUV0Hit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoMUV0Hit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoMUV0Hit", ::TSlimRecoMUV0Hit::Class_Version(), "", 130,
                  typeid(::TSlimRecoMUV0Hit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoMUV0Hit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoMUV0Hit) );
      instance.SetNew(&new_TSlimRecoMUV0Hit);
      instance.SetNewArray(&newArray_TSlimRecoMUV0Hit);
      instance.SetDelete(&delete_TSlimRecoMUV0Hit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoMUV0Hit);
      instance.SetDestructor(&destruct_TSlimRecoMUV0Hit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoMUV0Hit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoMUV0Hit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoMUV0Hit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV0Candidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV0Candidate::Class_Name()
{
   return "TSlimRecoMUV0Candidate";
}

//______________________________________________________________________________
const char *TSlimRecoMUV0Candidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Candidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV0Candidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Candidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV0Candidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Candidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV0Candidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Candidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV0Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV0Event::Class_Name()
{
   return "TSlimRecoMUV0Event";
}

//______________________________________________________________________________
const char *TSlimRecoMUV0Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV0Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV0Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV0Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoMUV0Hit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoMUV0Hit::Class_Name()
{
   return "TSlimRecoMUV0Hit";
}

//______________________________________________________________________________
const char *TSlimRecoMUV0Hit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Hit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoMUV0Hit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Hit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoMUV0Hit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Hit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoMUV0Hit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoMUV0Hit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoMUV0Candidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV0Candidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV0Candidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV0Candidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV0Candidate(void *p) {
      return  p ? new(p) ::TSlimRecoMUV0Candidate : new ::TSlimRecoMUV0Candidate;
   }
   static void *newArray_TSlimRecoMUV0Candidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV0Candidate[nElements] : new ::TSlimRecoMUV0Candidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV0Candidate(void *p) {
      delete ((::TSlimRecoMUV0Candidate*)p);
   }
   static void deleteArray_TSlimRecoMUV0Candidate(void *p) {
      delete [] ((::TSlimRecoMUV0Candidate*)p);
   }
   static void destruct_TSlimRecoMUV0Candidate(void *p) {
      typedef ::TSlimRecoMUV0Candidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV0Candidate

//______________________________________________________________________________
void TSlimRecoMUV0Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV0Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV0Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV0Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV0Event(void *p) {
      return  p ? new(p) ::TSlimRecoMUV0Event : new ::TSlimRecoMUV0Event;
   }
   static void *newArray_TSlimRecoMUV0Event(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV0Event[nElements] : new ::TSlimRecoMUV0Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV0Event(void *p) {
      delete ((::TSlimRecoMUV0Event*)p);
   }
   static void deleteArray_TSlimRecoMUV0Event(void *p) {
      delete [] ((::TSlimRecoMUV0Event*)p);
   }
   static void destruct_TSlimRecoMUV0Event(void *p) {
      typedef ::TSlimRecoMUV0Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV0Event

//______________________________________________________________________________
void TSlimRecoMUV0Hit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoMUV0Hit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoMUV0Hit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoMUV0Hit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoMUV0Hit(void *p) {
      return  p ? new(p) ::TSlimRecoMUV0Hit : new ::TSlimRecoMUV0Hit;
   }
   static void *newArray_TSlimRecoMUV0Hit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoMUV0Hit[nElements] : new ::TSlimRecoMUV0Hit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoMUV0Hit(void *p) {
      delete ((::TSlimRecoMUV0Hit*)p);
   }
   static void deleteArray_TSlimRecoMUV0Hit(void *p) {
      delete [] ((::TSlimRecoMUV0Hit*)p);
   }
   static void destruct_TSlimRecoMUV0Hit(void *p) {
      typedef ::TSlimRecoMUV0Hit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoMUV0Hit

namespace ROOT {
   static TClass *vectorlETSlimRecoMUV0CandidategR_Dictionary();
   static void vectorlETSlimRecoMUV0CandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoMUV0CandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoMUV0CandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoMUV0CandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoMUV0CandidategR(void *p);
   static void destruct_vectorlETSlimRecoMUV0CandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoMUV0Candidate>*)
   {
      vector<TSlimRecoMUV0Candidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoMUV0Candidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoMUV0Candidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoMUV0Candidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoMUV0CandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoMUV0Candidate>) );
      instance.SetNew(&new_vectorlETSlimRecoMUV0CandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoMUV0CandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoMUV0CandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoMUV0CandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoMUV0CandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoMUV0Candidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoMUV0Candidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoMUV0CandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoMUV0Candidate>*)0x0)->GetClass();
      vectorlETSlimRecoMUV0CandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoMUV0CandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoMUV0CandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV0Candidate> : new vector<TSlimRecoMUV0Candidate>;
   }
   static void *newArray_vectorlETSlimRecoMUV0CandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoMUV0Candidate>[nElements] : new vector<TSlimRecoMUV0Candidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoMUV0CandidategR(void *p) {
      delete ((vector<TSlimRecoMUV0Candidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoMUV0CandidategR(void *p) {
      delete [] ((vector<TSlimRecoMUV0Candidate>*)p);
   }
   static void destruct_vectorlETSlimRecoMUV0CandidategR(void *p) {
      typedef vector<TSlimRecoMUV0Candidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoMUV0Candidate>

namespace {
  void TriggerDictionaryInitialization_libMUV0SlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV0/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV0/../../FullReco/MUV0/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV0/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV0SlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class TSlimRecoMUV0Candidate;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoMUV0Event;
class TSlimRecoMUV0Hit;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV0SlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOMUV0CANDIDATE_H
#define TSLIMRECOMUV0CANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVCandidate.hh"
#include <TVector3.h>

class TRecoMUV0Hit;
class TRecoVHit;

class TSlimRecoMUV0Candidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoMUV0Candidate() = default;
    explicit TSlimRecoMUV0Candidate(TRecoMUV0Hit *);
    virtual ~TSlimRecoMUV0Candidate() = default;

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

    Short_t GetTileID()   const;
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

    ClassDef(TSlimRecoMUV0Candidate, 1)
};

#endif /* TSLIMRECOMUV0CANDIDATE_H */
#ifndef TRECOMUV0EVENTSLIM_H
#define TRECOMUV0EVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoMUV0Candidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoMUV0Event;

class TSlimRecoMUV0Event : public TSlimRecoVEvent
{
public:
    TSlimRecoMUV0Event() = default;
    explicit TSlimRecoMUV0Event(TRecoMUV0Event *evReco);
    virtual ~TSlimRecoMUV0Event() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearCandidates();

    void AddCandidate(TSlimRecoMUV0Candidate h) { fCandidates.emplace_back(std::move(h));  }

    Int_t GetNCandidates()                               const { return fCandidates.size(); }
    std::vector<TSlimRecoMUV0Candidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)            { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoMUV0Candidate> fCandidates;

    ClassDef(TSlimRecoMUV0Event, 1)
};

#endif /* TRECOMUV0EVENTSLIM_H */
#ifndef TSLIMRECOMUV0HIT_H
#define TSLIMRECOMUV0HIT_H

#include "TSlimRecoVHit.hh"

class TRecoMUV0Hit;

class TSlimRecoMUV0Hit : public TSlimRecoVHit
{
public:
    TSlimRecoMUV0Hit()               {}
    explicit TSlimRecoMUV0Hit(TRecoMUV0Hit *) {}
    virtual ~TSlimRecoMUV0Hit()      {}

    // conversion functions
    virtual void FromReco(TRecoVHit *) {}
    virtual void ToReco(TRecoVHit *)   {}
private:

    ClassDef(TSlimRecoMUV0Hit, 1)
};

#endif /* TSLIMRECOMUV0HIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoMUV0Candidate", payloadCode, "@",
"TSlimRecoMUV0Event", payloadCode, "@",
"TSlimRecoMUV0Hit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV0SlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV0SlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV0SlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV0SlimPersistency() {
  TriggerDictionaryInitialization_libMUV0SlimPersistency_Impl();
}
