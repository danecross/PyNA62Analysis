// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME CHANTIDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/CHANTI/include/CHANTIHitsCluster.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_CHANTIHitsCluster(void *p = 0);
   static void *newArray_CHANTIHitsCluster(Long_t size, void *p);
   static void delete_CHANTIHitsCluster(void *p);
   static void deleteArray_CHANTIHitsCluster(void *p);
   static void destruct_CHANTIHitsCluster(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::CHANTIHitsCluster*)
   {
      ::CHANTIHitsCluster *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::CHANTIHitsCluster >(0);
      static ::ROOT::TGenericClassInfo 
         instance("CHANTIHitsCluster", ::CHANTIHitsCluster::Class_Version(), "", 34,
                  typeid(::CHANTIHitsCluster), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::CHANTIHitsCluster::Dictionary, isa_proxy, 4,
                  sizeof(::CHANTIHitsCluster) );
      instance.SetNew(&new_CHANTIHitsCluster);
      instance.SetNewArray(&newArray_CHANTIHitsCluster);
      instance.SetDelete(&delete_CHANTIHitsCluster);
      instance.SetDeleteArray(&deleteArray_CHANTIHitsCluster);
      instance.SetDestructor(&destruct_CHANTIHitsCluster);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::CHANTIHitsCluster*)
   {
      return GenerateInitInstanceLocal((::CHANTIHitsCluster*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::CHANTIHitsCluster*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr CHANTIHitsCluster::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *CHANTIHitsCluster::Class_Name()
{
   return "CHANTIHitsCluster";
}

//______________________________________________________________________________
const char *CHANTIHitsCluster::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CHANTIHitsCluster*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int CHANTIHitsCluster::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::CHANTIHitsCluster*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *CHANTIHitsCluster::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CHANTIHitsCluster*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *CHANTIHitsCluster::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::CHANTIHitsCluster*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void CHANTIHitsCluster::Streamer(TBuffer &R__b)
{
   // Stream an object of class CHANTIHitsCluster.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(CHANTIHitsCluster::Class(),this);
   } else {
      R__b.WriteClassBuffer(CHANTIHitsCluster::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_CHANTIHitsCluster(void *p) {
      return  p ? new(p) ::CHANTIHitsCluster : new ::CHANTIHitsCluster;
   }
   static void *newArray_CHANTIHitsCluster(Long_t nElements, void *p) {
      return p ? new(p) ::CHANTIHitsCluster[nElements] : new ::CHANTIHitsCluster[nElements];
   }
   // Wrapper around operator delete
   static void delete_CHANTIHitsCluster(void *p) {
      delete ((::CHANTIHitsCluster*)p);
   }
   static void deleteArray_CHANTIHitsCluster(void *p) {
      delete [] ((::CHANTIHitsCluster*)p);
   }
   static void destruct_CHANTIHitsCluster(void *p) {
      typedef ::CHANTIHitsCluster current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::CHANTIHitsCluster

namespace {
  void TriggerDictionaryInitialization_libCHANTI_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/RecoBase/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CHANTI/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libCHANTI dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class CHANTIHitsCluster;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libCHANTI dictionary payload"

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
#ifndef CHANTIHitsCluster_H
#define CHANTIHitsCluster_H

#include "TObject.h"
#include "TClonesArray.h"
#include "TCHANTIHit.hh"

class CHANTIHitsCluster : public TObject {

public:

  CHANTIHitsCluster();
  TCHANTIHit * AddHit(TCHANTIHit);
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

  ClassDef(CHANTIHitsCluster,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"CHANTIHitsCluster", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libCHANTI",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libCHANTI_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libCHANTI_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libCHANTI() {
  TriggerDictionaryInitialization_libCHANTI_Impl();
}
