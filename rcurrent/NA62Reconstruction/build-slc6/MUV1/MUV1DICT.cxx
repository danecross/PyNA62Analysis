// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV1DICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV1/include/MUV1HitsCluster.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_MUV1HitsCluster(void *p = 0);
   static void *newArray_MUV1HitsCluster(Long_t size, void *p);
   static void delete_MUV1HitsCluster(void *p);
   static void deleteArray_MUV1HitsCluster(void *p);
   static void destruct_MUV1HitsCluster(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MUV1HitsCluster*)
   {
      ::MUV1HitsCluster *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::MUV1HitsCluster >(0);
      static ::ROOT::TGenericClassInfo 
         instance("MUV1HitsCluster", ::MUV1HitsCluster::Class_Version(), "", 34,
                  typeid(::MUV1HitsCluster), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::MUV1HitsCluster::Dictionary, isa_proxy, 4,
                  sizeof(::MUV1HitsCluster) );
      instance.SetNew(&new_MUV1HitsCluster);
      instance.SetNewArray(&newArray_MUV1HitsCluster);
      instance.SetDelete(&delete_MUV1HitsCluster);
      instance.SetDeleteArray(&deleteArray_MUV1HitsCluster);
      instance.SetDestructor(&destruct_MUV1HitsCluster);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MUV1HitsCluster*)
   {
      return GenerateInitInstanceLocal((::MUV1HitsCluster*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::MUV1HitsCluster*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr MUV1HitsCluster::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *MUV1HitsCluster::Class_Name()
{
   return "MUV1HitsCluster";
}

//______________________________________________________________________________
const char *MUV1HitsCluster::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV1HitsCluster*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int MUV1HitsCluster::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV1HitsCluster*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *MUV1HitsCluster::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV1HitsCluster*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *MUV1HitsCluster::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV1HitsCluster*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void MUV1HitsCluster::Streamer(TBuffer &R__b)
{
   // Stream an object of class MUV1HitsCluster.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(MUV1HitsCluster::Class(),this);
   } else {
      R__b.WriteClassBuffer(MUV1HitsCluster::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MUV1HitsCluster(void *p) {
      return  p ? new(p) ::MUV1HitsCluster : new ::MUV1HitsCluster;
   }
   static void *newArray_MUV1HitsCluster(Long_t nElements, void *p) {
      return p ? new(p) ::MUV1HitsCluster[nElements] : new ::MUV1HitsCluster[nElements];
   }
   // Wrapper around operator delete
   static void delete_MUV1HitsCluster(void *p) {
      delete ((::MUV1HitsCluster*)p);
   }
   static void deleteArray_MUV1HitsCluster(void *p) {
      delete [] ((::MUV1HitsCluster*)p);
   }
   static void destruct_MUV1HitsCluster(void *p) {
      typedef ::MUV1HitsCluster current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MUV1HitsCluster

namespace ROOT {
   static TClass *vectorlETRecoMUV1HitmUgR_Dictionary();
   static void vectorlETRecoMUV1HitmUgR_TClassManip(TClass*);
   static void *new_vectorlETRecoMUV1HitmUgR(void *p = 0);
   static void *newArray_vectorlETRecoMUV1HitmUgR(Long_t size, void *p);
   static void delete_vectorlETRecoMUV1HitmUgR(void *p);
   static void deleteArray_vectorlETRecoMUV1HitmUgR(void *p);
   static void destruct_vectorlETRecoMUV1HitmUgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TRecoMUV1Hit*>*)
   {
      vector<TRecoMUV1Hit*> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TRecoMUV1Hit*>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TRecoMUV1Hit*>", -2, "vector", 214,
                  typeid(vector<TRecoMUV1Hit*>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETRecoMUV1HitmUgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<TRecoMUV1Hit*>) );
      instance.SetNew(&new_vectorlETRecoMUV1HitmUgR);
      instance.SetNewArray(&newArray_vectorlETRecoMUV1HitmUgR);
      instance.SetDelete(&delete_vectorlETRecoMUV1HitmUgR);
      instance.SetDeleteArray(&deleteArray_vectorlETRecoMUV1HitmUgR);
      instance.SetDestructor(&destruct_vectorlETRecoMUV1HitmUgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TRecoMUV1Hit*> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const vector<TRecoMUV1Hit*>*)0x0); R__UseDummy(_R__UNIQUE_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETRecoMUV1HitmUgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TRecoMUV1Hit*>*)0x0)->GetClass();
      vectorlETRecoMUV1HitmUgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETRecoMUV1HitmUgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETRecoMUV1HitmUgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TRecoMUV1Hit*> : new vector<TRecoMUV1Hit*>;
   }
   static void *newArray_vectorlETRecoMUV1HitmUgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TRecoMUV1Hit*>[nElements] : new vector<TRecoMUV1Hit*>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETRecoMUV1HitmUgR(void *p) {
      delete ((vector<TRecoMUV1Hit*>*)p);
   }
   static void deleteArray_vectorlETRecoMUV1HitmUgR(void *p) {
      delete [] ((vector<TRecoMUV1Hit*>*)p);
   }
   static void destruct_vectorlETRecoMUV1HitmUgR(void *p) {
      typedef vector<TRecoMUV1Hit*> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TRecoMUV1Hit*>

namespace {
  void TriggerDictionaryInitialization_libMUV1_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/RecoBase/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV1/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV1 dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class MUV1HitsCluster;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV1 dictionary payload"

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
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#ifndef MUV1HitsCluster_H
#define MUV1HitsCluster_H

#include "TObject.h"
#include "TClonesArray.h"
#include "TRecoMUV1Hit.hh"

class MUV1HitsCluster : public TObject {

  public:

    MUV1HitsCluster();
    void AddHit(TRecoMUV1Hit*);
    void Clear(Option_t* option ="");

  public:

    Int_t                GetNHits()                                         { return fNHits;                        };
    void                 SetNHits(Int_t value)                              { fNHits = value;                       };

    TRecoMUV1Hit **      GetHits()                                          { return fHits.data();                  };

    TVector3             GetPosition()                                      { return fPosition;                     };
    void                 SetPosition(TVector3 value)                        { fPosition = value;                    };

    Double_t             GetTime()                                          { return fTime; }
    void                 SetTime(Double_t time)                             { fTime = time; }

    Double_t             GetCharge()                                        { return fCharge; }
    void                 SetCharge(Double_t charge)                         { fCharge = charge; }

    Double_t             GetAmplitude()                                     { return fAmplitude; }
    void                 SetAmplitude(Double_t amplitude)                   { fAmplitude = amplitude; }

    Double_t             GetX()                                             { return fCoordX;       }
    void                 SetX(Double_t x)                                   { fCoordX = x; fPosition[0] = x;}

    Double_t             GetY()                                             { return fCoordY;       }
    void                 SetY(Double_t y)                                   { fCoordY = y; fPosition[1] = y;}

    Double_t             GetSigmaT()                                        { return fSigmaT; }
    Double_t             GetSigmaA()                                        { return fSigmaA; }
    Double_t             GetSigmaQ()                                        { return fSigmaQ; }
    Double_t             GetSigmaX()                                        { return fSigmaX; }
    Double_t             GetSigmaY()                                        { return fSigmaY; }

    void                 UpdateValue();


  private:

    Int_t      fNHits;
    std::vector <TRecoMUV1Hit *> fHits;

    TVector3   fPosition;
    Int_t fNHitsSide[2];

    Double_t fTime, fSigmaT;
    Double_t fCharge, fSigmaQ;
    Double_t fAmplitude, fSigmaA;
    Double_t fCoordX, fSigmaX;
    Double_t fCoordY, fSigmaY;

    ClassDef(MUV1HitsCluster,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"MUV1HitsCluster", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV1",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV1_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV1_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV1() {
  TriggerDictionaryInitialization_libMUV1_Impl();
}
