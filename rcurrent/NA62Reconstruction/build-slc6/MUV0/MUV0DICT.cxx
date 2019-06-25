// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME MUV0DICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV0/include/MUV0HitsCluster.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_MUV0HitsCluster(void *p = 0);
   static void *newArray_MUV0HitsCluster(Long_t size, void *p);
   static void delete_MUV0HitsCluster(void *p);
   static void deleteArray_MUV0HitsCluster(void *p);
   static void destruct_MUV0HitsCluster(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MUV0HitsCluster*)
   {
      ::MUV0HitsCluster *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::MUV0HitsCluster >(0);
      static ::ROOT::TGenericClassInfo 
         instance("MUV0HitsCluster", ::MUV0HitsCluster::Class_Version(), "", 34,
                  typeid(::MUV0HitsCluster), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::MUV0HitsCluster::Dictionary, isa_proxy, 4,
                  sizeof(::MUV0HitsCluster) );
      instance.SetNew(&new_MUV0HitsCluster);
      instance.SetNewArray(&newArray_MUV0HitsCluster);
      instance.SetDelete(&delete_MUV0HitsCluster);
      instance.SetDeleteArray(&deleteArray_MUV0HitsCluster);
      instance.SetDestructor(&destruct_MUV0HitsCluster);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MUV0HitsCluster*)
   {
      return GenerateInitInstanceLocal((::MUV0HitsCluster*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::MUV0HitsCluster*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr MUV0HitsCluster::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *MUV0HitsCluster::Class_Name()
{
   return "MUV0HitsCluster";
}

//______________________________________________________________________________
const char *MUV0HitsCluster::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV0HitsCluster*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int MUV0HitsCluster::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MUV0HitsCluster*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *MUV0HitsCluster::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV0HitsCluster*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *MUV0HitsCluster::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MUV0HitsCluster*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void MUV0HitsCluster::Streamer(TBuffer &R__b)
{
   // Stream an object of class MUV0HitsCluster.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(MUV0HitsCluster::Class(),this);
   } else {
      R__b.WriteClassBuffer(MUV0HitsCluster::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MUV0HitsCluster(void *p) {
      return  p ? new(p) ::MUV0HitsCluster : new ::MUV0HitsCluster;
   }
   static void *newArray_MUV0HitsCluster(Long_t nElements, void *p) {
      return p ? new(p) ::MUV0HitsCluster[nElements] : new ::MUV0HitsCluster[nElements];
   }
   // Wrapper around operator delete
   static void delete_MUV0HitsCluster(void *p) {
      delete ((::MUV0HitsCluster*)p);
   }
   static void deleteArray_MUV0HitsCluster(void *p) {
      delete [] ((::MUV0HitsCluster*)p);
   }
   static void destruct_MUV0HitsCluster(void *p) {
      typedef ::MUV0HitsCluster current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MUV0HitsCluster

namespace {
  void TriggerDictionaryInitialization_libMUV0_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/RecoBase/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV0/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libMUV0 dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class MUV0HitsCluster;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libMUV0 dictionary payload"

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
#ifndef MUV0HitsCluster_H
#define MUV0HitsCluster_H

#include "TObject.h"
#include "TClonesArray.h"
#include "TMUV0Hit.hh"

class MUV0HitsCluster : public TObject {

public:

  MUV0HitsCluster();
  TMUV0Hit * AddHit(TMUV0Hit);
  void Clear(Option_t* option ="");

public:

  Int_t                GetNHits()                                         { return fNHits;                        };
  void                 SetNHits(Int_t value)                              { fNHits = value;                       };

  TClonesArray *       GetHits()                                          { return fHits;                         };
  void                 SetHits(TClonesArray * value)                      { fHits = value;                        };

  TVector3             GetPosition()                                      { return fPosition;                     };
  void                 SetPosition(TVector3 value)                        { fPosition = value;                    };

private:

  Int_t      fNHits;
//   TVectorT<Int_t> fMCTrackIDs; // For MCTruth Association

  TClonesArray * fHits;

  TVector3   fPosition;

  ClassDef(MUV0HitsCluster,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"MUV0HitsCluster", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libMUV0",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libMUV0_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libMUV0_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libMUV0() {
  TriggerDictionaryInitialization_libMUV0_Impl();
}
