// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME EventDisplayDICT

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
#include "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/EventDisplay/include/TNA62MagField.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void delete_TNA62MagField(void *p);
   static void deleteArray_TNA62MagField(void *p);
   static void destruct_TNA62MagField(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TNA62MagField*)
   {
      ::TNA62MagField *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TNA62MagField >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TNA62MagField", ::TNA62MagField::Class_Version(), "", 35,
                  typeid(::TNA62MagField), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TNA62MagField::Dictionary, isa_proxy, 4,
                  sizeof(::TNA62MagField) );
      instance.SetDelete(&delete_TNA62MagField);
      instance.SetDeleteArray(&deleteArray_TNA62MagField);
      instance.SetDestructor(&destruct_TNA62MagField);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TNA62MagField*)
   {
      return GenerateInitInstanceLocal((::TNA62MagField*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_(Init) = GenerateInitInstanceLocal((const ::TNA62MagField*)0x0); R__UseDummy(_R__UNIQUE_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TNA62MagField::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TNA62MagField::Class_Name()
{
   return "TNA62MagField";
}

//______________________________________________________________________________
const char *TNA62MagField::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNA62MagField*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TNA62MagField::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TNA62MagField*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TNA62MagField::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNA62MagField*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TNA62MagField::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD2(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TNA62MagField*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TNA62MagField::Streamer(TBuffer &R__b)
{
   // Stream an object of class TNA62MagField.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TNA62MagField::Class(),this);
   } else {
      R__b.WriteClassBuffer(TNA62MagField::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TNA62MagField(void *p) {
      delete ((::TNA62MagField*)p);
   }
   static void deleteArray_TNA62MagField(void *p) {
      delete [] ((::TNA62MagField*)p);
   }
   static void destruct_TNA62MagField(void *p) {
      typedef ::TNA62MagField current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TNA62MagField

namespace {
  void TriggerDictionaryInitialization_libEventDisplay_Impl() {
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
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/EventDisplay/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/RecoBase/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.08.00-7de1c/x86_64-slc6-gcc49-opt/include",
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/EventDisplay/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libEventDisplay dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class TNA62MagField;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libEventDisplay dictionary payload"

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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-04
// 
// --------------------------------------------------------------
#ifndef TNA62MagField_H
#define TNA62MagField_H

#include "TEveTrackPropagator.h"
#include "TEveVSDStructs.h"
#include "TGeoManager.h"
#include "TObjArray.h"

class TNA62MagField : public TEveMagField
{
    public:
        explicit TNA62MagField(TGeoManager * GeoManager);
        virtual ~TNA62MagField() {}

        using   TEveMagField::GetField;
        virtual TEveVector GetField(Float_t /*x*/, Float_t /*y*/, Float_t /*z*/) const; 

        virtual Float_t    GetMaxFieldMag() const { return 1.7; }

    private:
	TObjArray *fMagnets, *fTransformations;

        ClassDef(TNA62MagField, 0); 
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TNA62MagField", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libEventDisplay",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libEventDisplay_Impl, {}, classesHeaders);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libEventDisplay_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libEventDisplay() {
  TriggerDictionaryInitialization_libEventDisplay_Impl();
}
