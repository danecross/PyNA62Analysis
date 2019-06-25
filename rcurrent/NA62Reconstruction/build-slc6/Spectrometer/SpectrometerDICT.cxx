// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME SpectrometerDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Spectrometer/include/Cluster.hh"
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Spectrometer/include/Combination.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_Cluster(void *p = 0);
   static void *newArray_Cluster(Long_t size, void *p);
   static void delete_Cluster(void *p);
   static void deleteArray_Cluster(void *p);
   static void destruct_Cluster(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::Cluster*)
   {
      ::Cluster *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::Cluster >(0);
      static ::ROOT::TGenericClassInfo 
         instance("Cluster", ::Cluster::Class_Version(), "", 27,
                  typeid(::Cluster), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::Cluster::Dictionary, isa_proxy, 4,
                  sizeof(::Cluster) );
      instance.SetNew(&new_Cluster);
      instance.SetNewArray(&newArray_Cluster);
      instance.SetDelete(&delete_Cluster);
      instance.SetDeleteArray(&deleteArray_Cluster);
      instance.SetDestructor(&destruct_Cluster);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::Cluster*)
   {
      return GenerateInitInstanceLocal((::Cluster*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::Cluster*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_Combination(void *p = 0);
   static void *newArray_Combination(Long_t size, void *p);
   static void delete_Combination(void *p);
   static void deleteArray_Combination(void *p);
   static void destruct_Combination(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::Combination*)
   {
      ::Combination *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::Combination >(0);
      static ::ROOT::TGenericClassInfo 
         instance("Combination", ::Combination::Class_Version(), "", 150,
                  typeid(::Combination), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::Combination::Dictionary, isa_proxy, 4,
                  sizeof(::Combination) );
      instance.SetNew(&new_Combination);
      instance.SetNewArray(&newArray_Combination);
      instance.SetDelete(&delete_Combination);
      instance.SetDeleteArray(&deleteArray_Combination);
      instance.SetDestructor(&destruct_Combination);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::Combination*)
   {
      return GenerateInitInstanceLocal((::Combination*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::Combination*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr Cluster::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *Cluster::Class_Name()
{
   return "Cluster";
}

//______________________________________________________________________________
const char *Cluster::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Cluster*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int Cluster::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Cluster*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *Cluster::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Cluster*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *Cluster::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Cluster*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr Combination::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *Combination::Class_Name()
{
   return "Combination";
}

//______________________________________________________________________________
const char *Combination::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Combination*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int Combination::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Combination*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *Combination::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Combination*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *Combination::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Combination*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void Cluster::Streamer(TBuffer &R__b)
{
   // Stream an object of class Cluster.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(Cluster::Class(),this);
   } else {
      R__b.WriteClassBuffer(Cluster::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_Cluster(void *p) {
      return  p ? new(p) ::Cluster : new ::Cluster;
   }
   static void *newArray_Cluster(Long_t nElements, void *p) {
      return p ? new(p) ::Cluster[nElements] : new ::Cluster[nElements];
   }
   // Wrapper around operator delete
   static void delete_Cluster(void *p) {
      delete ((::Cluster*)p);
   }
   static void deleteArray_Cluster(void *p) {
      delete [] ((::Cluster*)p);
   }
   static void destruct_Cluster(void *p) {
      typedef ::Cluster current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::Cluster

//______________________________________________________________________________
void Combination::Streamer(TBuffer &R__b)
{
   // Stream an object of class Combination.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(Combination::Class(),this);
   } else {
      R__b.WriteClassBuffer(Combination::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_Combination(void *p) {
      return  p ? new(p) ::Combination : new ::Combination;
   }
   static void *newArray_Combination(Long_t nElements, void *p) {
      return p ? new(p) ::Combination[nElements] : new ::Combination[nElements];
   }
   // Wrapper around operator delete
   static void delete_Combination(void *p) {
      delete ((::Combination*)p);
   }
   static void deleteArray_Combination(void *p) {
      delete [] ((::Combination*)p);
   }
   static void destruct_Combination(void *p) {
      typedef ::Combination current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::Combination

namespace ROOT {
   static TClass *vectorlEintgR_Dictionary();
   static void vectorlEintgR_TClassManip(TClass*);
   static void *new_vectorlEintgR(void *p = 0);
   static void *newArray_vectorlEintgR(Long_t size, void *p);
   static void delete_vectorlEintgR(void *p);
   static void deleteArray_vectorlEintgR(void *p);
   static void destruct_vectorlEintgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<int>*)
   {
      vector<int> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<int>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<int>", -2, "vector", 214,
                  typeid(vector<int>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEintgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<int>) );
      instance.SetNew(&new_vectorlEintgR);
      instance.SetNewArray(&newArray_vectorlEintgR);
      instance.SetDelete(&delete_vectorlEintgR);
      instance.SetDeleteArray(&deleteArray_vectorlEintgR);
      instance.SetDestructor(&destruct_vectorlEintgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<int> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const vector<int>*)0x0); R__UseDummy(_R__UNIQUE_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEintgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<int>*)0x0)->GetClass();
      vectorlEintgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEintgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEintgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<int> : new vector<int>;
   }
   static void *newArray_vectorlEintgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<int>[nElements] : new vector<int>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEintgR(void *p) {
      delete ((vector<int>*)p);
   }
   static void deleteArray_vectorlEintgR(void *p) {
      delete [] ((vector<int>*)p);
   }
   static void destruct_vectorlEintgR(void *p) {
      typedef vector<int> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<int>

namespace {
  void TriggerDictionaryInitialization_libSpectrometer_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/Cedar",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/CHANTI",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/CHOD",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/NewCHOD",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/GigaTracker",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/HAC",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/IRC",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/LAV",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/LKr",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV0",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV1",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV2",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV3",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/RICH",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/SAC",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/Spectrometer",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4",
"/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui",
"/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL",
"/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Service/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Cedar/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/CHANTI/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/CHOD/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/NewCHOD/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/GigaTracker/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/HAC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/IRC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/LAV/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/LKr/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV0/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV1/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV2/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV3/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/RICH/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/SAC/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/SAV/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Spectrometer/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/RecoBase/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Spectrometer/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libSpectrometer dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class Cluster;
class Combination;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libSpectrometer dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif
#ifndef G4_STORE_TRAJECTORY
  #define G4_STORE_TRAJECTORY 1
#endif
#ifndef G4VERBOSE
  #define G4VERBOSE 1
#endif
#ifndef G4UI_USE
  #define G4UI_USE 1
#endif
#ifndef G4VIS_USE
  #define G4VIS_USE 1
#endif
#ifndef G4MULTITHREADED
  #define G4MULTITHREADED 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef Cluster_H
#define Cluster_H 1

#include "TObject.h"
#include "TVector3.h"

class Cluster : public TObject
{

public:
  Cluster();
  ~Cluster();
  Cluster(const Cluster&);

  size_t GetNHit()
  {
    /// \MemberDescr
    /// \return Total number of tube-hits forming the cluster. 
    /// \EndMemberDescr

    return fHitId.size();
  };
  Int_t GetHitId(Int_t jHit)
  {
    /// \MemberDescr
    /// \param jHit Current tube-hit.
    /// \return The element jHit of the vector Cluster::fHitId. 
    ///
    /// \EndMemberDescr

    return fHitId[jHit];
  }; 
  TVector3 GetLocalPosition()
  {
    /// \MemberDescr
    /// \return Cluster::fLocalPosition. 
    /// \EndMemberDescr

    return fLocalPosition;
  };
  TVector3 GetPosition()
  {
    /// \MemberDescr
    /// \return Cluster::fPosition. 
    /// \EndMemberDescr

    return fPosition;
  };
  Double_t GetLocalSlope()
  {
    /// \MemberDescr
    /// \return Cluster::fLocalSlope. 
    /// \EndMemberDescr

    return fLocalSlope;
  };
  Double_t GetError()
  {
    /// \MemberDescr
    /// \return Cluster::fError. 
    /// \EndMemberDescr

    return fError;
  };
  Double_t GetUsedForHit()
  {
    /// \MemberDescr
    /// \return Cluster::fUsedForHit. 
    /// \EndMemberDescr

    return fUsedForHit;
  };
  Double_t GetFlagUsed()
  {
    /// \MemberDescr
    /// \return Cluster::fFlagUsed. 
    /// \EndMemberDescr

    return fFlagUsed;
  };
  Double_t GetTrailingTime()
  {
    return fTrailingTime;
  }
  Double_t GetQuality()
  {
    return fQuality;
  }
  Int_t GetEdge() {
    return fEdge;
  }
  void SetHitId(Int_t val){fHitId.push_back(val);}; 
  void SetLocalPosition(TVector3 val){fLocalPosition=val;};
  void SetPosition(TVector3 val){fPosition=val;};
  void SetLocalSlope(Double_t val){fLocalSlope=val;};
  void SetError(Double_t val){fError=val;};
  void SetUsedForHit(Int_t val) {fUsedForHit=val;};
  void SetFlagUsed(Int_t val){fFlagUsed=val;};
  void SetTrailingTime(Double_t val){fTrailingTime=val;};
  void SetQuality(Double_t val){fQuality=val;};
  void SetEdge(Int_t val){fEdge=val;};
 
  void AddHit(Int_t);   

  void Reset();

private:
  std::vector<Int_t> fHitId; ///< Vector of the id of the tube-hits forming the cluster. 
  TVector3 fLocalPosition; ///< Position of the cluster in the view reference frame.
  TVector3 fPosition; ///< Position of the cluster in the laboratory reference frame.
  Double_t fLocalSlope; ///< Slope associated to a hit as extracted from the LR algorithm.
  Double_t fError; ///< Uncertainty on the straw position.
  Int_t fUsedForHit; ///< Flag which identifies the clusters used for chamber hit reconstruction (see ChamberHitCollector).
  Int_t fFlagUsed; ///< Flag which identifies the clusters used for track reconstruction (see TrackCollector).
  Double_t fTrailingTime;
  Double_t fQuality;
  Int_t fEdge;

  ClassDef(Cluster,1);
};
#endif
#ifndef Combination_H
#define Combination_H 1

#include "TObject.h"
////#include "gmatrix.h"
#include <cmath>

class Combination : public TObject
{

public:
  Combination();
  ~Combination();
  Combination(const Combination&);

  Int_t GetNClusters() 
  {
    /// \MemberDescr
    /// \return Number of clusters forming this combination 
    /// \EndMemberDescr

    return (Int_t)fClusterId.size();
  };
  Int_t GetClusterId(Int_t val) 
  {
    /// \MemberDescr
    /// \param val hit id. 
    /// \return Combination::fClusterId. 
    /// \EndMemberDescr

    return fClusterId[val];
  };
  Int_t GetViewId(Int_t val) 
  {
    /// \MemberDescr
    /// \param val hit id. 
    /// \return Combination::fViewId. 
    /// \EndMemberDescr

    return fViewId[val];
  };
  Int_t GetChamberId(Int_t val) 
  {
    /// \MemberDescr
    /// \param val hit id. 
    /// \return Combination::fChamberId. 
    /// \EndMemberDescr

    return fChamberId[val];
  };
  Double_t GetX0() 
  {
    /// \MemberDescr
    /// \return Combination::fX0. 
    /// \EndMemberDescr

    return fX0;
  };
  Double_t GetY0() 
  {
    /// \MemberDescr
    /// \return Combination::fY0. 
    /// \EndMemberDescr

    return fY0;
  };
  Double_t GetZ0() 
  {
    /// \MemberDescr
    /// \return Combination::fZ0. 
    /// \EndMemberDescr

    return fZ0;
  };
  Double_t GetThetaX() 
  {
    /// \MemberDescr
    /// \return Combination::fThetaX. 
    /// \EndMemberDescr

    return fThetaX;
  };
  Double_t GetThetaY() 
  {
    /// \MemberDescr
    /// \return Combination::fThetaY. 
    /// \EndMemberDescr

    return fThetaY;
  };
  Double_t GetP() 
  {
    /// \MemberDescr
    /// \return Combination::fP. 
    /// \EndMemberDescr

    return fP;
  };
  Int_t GetNChambers();
  Int_t GetNViewAfterMagnet() 
  {
    /// \MemberDescr
    /// \return Combination::fNViewAfterMagnet. 
    /// \EndMemberDescr

    return fNViewAfterMagnet;
  };
  Double_t GetChi2() 
  {
    /// \MemberDescr
    /// \return Combination::fChi2. 
    /// \EndMemberDescr

    return fChi2;
  };
  Double_t GetSigmaX0() 
  {
    /// \MemberDescr
    /// \return Combination::fSigmaX0. 
    /// \EndMemberDescr

    return fSigmaX0;
  };
  Double_t GetSigmaY0() 
  {
    /// \MemberDescr
    /// \return Combination::fSigmaY0. 
    /// \EndMemberDescr

    return fSigmaY0;
  };
  Double_t GetSigmaThetaX() 
  {
    /// \MemberDescr
    /// \return Combination::fSigmaThetaX. 
    /// \EndMemberDescr

    return fSigmaThetaX;
  };
  Double_t GetSigmaThetaY() 
  {
    /// \MemberDescr
    /// \return Combination::fSigmaThetaY. 
    /// \EndMemberDescr

    return fSigmaThetaY;
  };
  Double_t GetSigmaP() 
  {
    /// \MemberDescr
    /// \return Combination::fSigmaP. 
    /// \EndMemberDescr

    return fSigmaP;
  };
  Int_t GetNTotalHits() 
  {
    /// \MemberDescr
    /// \return Combination::fNTotalHits. 
    /// \EndMemberDescr

    return fNTotalHits;
  };

  Int_t GetType() { return fType; };
  Double_t GetHDelta() { return fHDelta; };
  Double_t GetDeltaX() { return fDeltaX; };
  Double_t GetTrailingTime() { return fTrailingTime; };
  Int_t GetChamberHitId(Int_t val) { return fChamberHitId[val]; };
  Double_t GetQuality() { return fQuality; };
  Double_t GetSubHDelta(Int_t val) { return fSubHDelta[val]; };
  Double_t GetSubThetaY(Int_t val) { return fSubThetaY[val]; };
  Int_t GetSubType(Int_t val) { return fSubType[val]; };
  Int_t GetNCommon() { return fNCommon; };
 

  void SetClusterId(Int_t val){fClusterId.push_back(val);}; 
  void SetViewId(Int_t val){fViewId.push_back(val);}; 
  void SetChamberId(Int_t val){fChamberId.push_back(val);}; 
  void SetChamberHitId(Int_t val){fChamberHitId.push_back(val);}; 
  void SetX0(Double_t val) {fX0=val;};
  void SetY0(Double_t val) {fY0=val;};
  void SetZ0(Double_t val) {fZ0=val;};
  void SetThetaX(Double_t val) {fThetaX=val;};
  void SetThetaY(Double_t val) {fThetaY=val;};
  void SetP(Double_t val) {fP=val;};
  void SetNChambers(Int_t val) {fNChambers=val;};
  void SetNViewAfterMagnet(Int_t val) {fNViewAfterMagnet=val;};
  void SetChi2(Double_t val) {fChi2=val;};
  void SetSigmaX0(Double_t val) {fSigmaX0=val;};
  void SetSigmaY0(Double_t val) {fSigmaY0=val;};
  void SetSigmaThetaX(Double_t val) {fSigmaThetaX=val;};
  void SetSigmaThetaY(Double_t val) {fSigmaThetaY=val;};
  void SetSigmaP(Double_t val) {fSigmaP=val;};
  void SetNTotalHits(Int_t val) {fNTotalHits=val;};

  void SetType(Int_t val) {fType=val;};  
  void SetHDelta(Double_t val) {fHDelta=val;};
  void SetDeltaX(Double_t val) {fDeltaX=val;};
  void SetTrailingTime(Double_t val) {fTrailingTime=val;};
  void SetQuality(Double_t, Double_t);
  void SetSubHDelta(Int_t j, Double_t val) {fSubHDelta[j]=val;};  
  void SetSubThetaY(Int_t j, Double_t val) {fSubThetaY[j]=val;};  
  void SetSubType(Int_t j, Int_t val) {fSubType[j]=val;};  
  void SetNCommon(Int_t val) {fNCommon=val;};

  void AddCluster(Int_t,Int_t,Int_t);
  void Sort();
  void Reset();
  Double_t Project(Double_t, Double_t);

private:
  std::vector<Int_t> fClusterId; ///< Vector of the id of the clusters forming the combination. 
  std::vector<Int_t> fViewId; ///< Vector of the id  of the view of the clusters forming the combination.
  std::vector<Int_t> fChamberId; ///< Vector of the id of the chamber of the clusters forming the combianation.
  Double_t fX0; ///< X0 at Z0 of the track fitted in TrackCollector associated to the combination.
  Double_t fY0; ///< Y0 at Z0 of the track fitted in TrackCollector associated to the combination.
  Double_t fZ0; ///< Starting position of the track fitted in TrackCollector associated to the combination.
  Double_t fThetaX; ///< Slope in X of the track fitted in TrackCollector associated to the combination.
  Double_t fThetaY; ///< Slope in Y of the track fitted in TrackCollector associated to the combination.
  Double_t fP; ///< Momentum of the track fitted in TrackCollector associated to the combination.
  Int_t fNChambers; ///< Number of chambers used to form the candidate combination.
  Int_t fNViewAfterMagnet; ///< Number of chambers after the magnet used to form the candidate combination.
  Double_t fChi2; ///< Chi2 of the track fitted in TrackCollector associated to the combination.
  Double_t fSigmaX0; ///< Estimated uncertainty on fX0 (given by the correlation matrix of the fit).
  Double_t fSigmaY0; ///< Estimated uncertainty on fY0 (given by the correlation matrix of the fit).
  Double_t fSigmaThetaX; ///< Estimated uncertainty on fThetaX (given by the correlation matrix of the fit).
  Double_t fSigmaThetaY; ///< Estimated uncertainty on fThetaY (given by the correlation matrix of the fit).
  Double_t fSigmaP; ///< Estimated uncertainty on fP (given by the correlation matrix of the fit).
  Int_t fNTotalHits; ///< Total number of straw hits forming the combination

  // Constant
  Double_t fDMag; ///< Thickness of the magnet.
  Double_t fZMag; ///< Position of the front face of the magnet.
  Double_t fBMag; ///< Intensity of the magnetic field.
  Double_t fEC; ///< e*c

  void SortChamber();
  void SortView(Int_t);

  // Added
  Int_t fType;
  Double_t fHDelta;
  Double_t fDeltaX;
  Double_t fQuality;
  Double_t fTrailingTime;
  std::vector<Int_t> fChamberHitId;
  Double_t fSubHDelta[4];
  Double_t fSubThetaY[4];
  Int_t fSubType[4];
  Int_t fNCommon;

  ClassDef(Combination,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"Cluster", payloadCode, "@",
"Combination", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libSpectrometer",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libSpectrometer_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libSpectrometer_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libSpectrometer() {
  TriggerDictionaryInitialization_libSpectrometer_Impl();
}
