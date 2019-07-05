// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME NA62PersistencyDICT

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
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/AnalysisInfo.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/AnalyzerIdentifier.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/BeamData.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/BeamSpecialTrigger.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/DetectorParameter.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/Event.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/EventBoundary.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/EventHeader.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/FADCEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/FADCVHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/GenePart.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/HLTEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/KinePart.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/L0TPData.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/L0TPSpecialTrigger.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/L1TPData.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/L1TPSpecialTrigger.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/L2EBData.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/L2EBSpecialTrigger.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/MCInfo.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/RecoInfo.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/Rndm.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/Stream.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDCError.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDCEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDCVHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDetectorVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDetectorVHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDigiVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDigiVError.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TDigiVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TEventInfo.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TPrimSpecialTrigger.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TPrimitive.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TRecoVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TRecoVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TRecoVHit.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TSpecialTrigger.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TSpecialTriggerEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TTDCBSpecialTrigger.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TTimeCluster.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TVCandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TVChannelID.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TVDigi.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TVEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/TVHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p = 0);
   static void *newArray_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(Long_t size, void *p);
   static void delete_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p);
   static void deleteArray_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p);
   static void destruct_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::NA62Analysis::Core::AnalyzerIdentifier*)
   {
      ::NA62Analysis::Core::AnalyzerIdentifier *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::NA62Analysis::Core::AnalyzerIdentifier >(0);
      static ::ROOT::TGenericClassInfo 
         instance("NA62Analysis::Core::AnalyzerIdentifier", ::NA62Analysis::Core::AnalyzerIdentifier::Class_Version(), "AnalyzerIdentifier.hh", 26,
                  typeid(::NA62Analysis::Core::AnalyzerIdentifier), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::NA62Analysis::Core::AnalyzerIdentifier::Dictionary, isa_proxy, 4,
                  sizeof(::NA62Analysis::Core::AnalyzerIdentifier) );
      instance.SetNew(&new_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier);
      instance.SetNewArray(&newArray_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier);
      instance.SetDelete(&delete_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier);
      instance.SetDeleteArray(&deleteArray_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier);
      instance.SetDestructor(&destruct_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::NA62Analysis::Core::AnalyzerIdentifier*)
   {
      return GenerateInitInstanceLocal((::NA62Analysis::Core::AnalyzerIdentifier*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::NA62Analysis::Core::AnalyzerIdentifier*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_AnalysisInfo(void *p = 0);
   static void *newArray_AnalysisInfo(Long_t size, void *p);
   static void delete_AnalysisInfo(void *p);
   static void deleteArray_AnalysisInfo(void *p);
   static void destruct_AnalysisInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::AnalysisInfo*)
   {
      ::AnalysisInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::AnalysisInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("AnalysisInfo", ::AnalysisInfo::Class_Version(), "", 20,
                  typeid(::AnalysisInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::AnalysisInfo::Dictionary, isa_proxy, 4,
                  sizeof(::AnalysisInfo) );
      instance.SetNew(&new_AnalysisInfo);
      instance.SetNewArray(&newArray_AnalysisInfo);
      instance.SetDelete(&delete_AnalysisInfo);
      instance.SetDeleteArray(&deleteArray_AnalysisInfo);
      instance.SetDestructor(&destruct_AnalysisInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::AnalysisInfo*)
   {
      return GenerateInitInstanceLocal((::AnalysisInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::AnalysisInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_BeamData(void *p = 0);
   static void *newArray_BeamData(Long_t size, void *p);
   static void delete_BeamData(void *p);
   static void deleteArray_BeamData(void *p);
   static void destruct_BeamData(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::BeamData*)
   {
      ::BeamData *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::BeamData >(0);
      static ::ROOT::TGenericClassInfo 
         instance("BeamData", ::BeamData::Class_Version(), "", 118,
                  typeid(::BeamData), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::BeamData::Dictionary, isa_proxy, 4,
                  sizeof(::BeamData) );
      instance.SetNew(&new_BeamData);
      instance.SetNewArray(&newArray_BeamData);
      instance.SetDelete(&delete_BeamData);
      instance.SetDeleteArray(&deleteArray_BeamData);
      instance.SetDestructor(&destruct_BeamData);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::BeamData*)
   {
      return GenerateInitInstanceLocal((::BeamData*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::BeamData*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TargetInfo(void *p = 0);
   static void *newArray_TargetInfo(Long_t size, void *p);
   static void delete_TargetInfo(void *p);
   static void deleteArray_TargetInfo(void *p);
   static void destruct_TargetInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TargetInfo*)
   {
      ::TargetInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TargetInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TargetInfo", ::TargetInfo::Class_Version(), "", 164,
                  typeid(::TargetInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TargetInfo::Dictionary, isa_proxy, 4,
                  sizeof(::TargetInfo) );
      instance.SetNew(&new_TargetInfo);
      instance.SetNewArray(&newArray_TargetInfo);
      instance.SetDelete(&delete_TargetInfo);
      instance.SetDeleteArray(&deleteArray_TargetInfo);
      instance.SetDestructor(&destruct_TargetInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TargetInfo*)
   {
      return GenerateInitInstanceLocal((::TargetInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TargetInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_MagnetInfo(void *p = 0);
   static void *newArray_MagnetInfo(Long_t size, void *p);
   static void delete_MagnetInfo(void *p);
   static void deleteArray_MagnetInfo(void *p);
   static void destruct_MagnetInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MagnetInfo*)
   {
      ::MagnetInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::MagnetInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("MagnetInfo", ::MagnetInfo::Class_Version(), "", 191,
                  typeid(::MagnetInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::MagnetInfo::Dictionary, isa_proxy, 4,
                  sizeof(::MagnetInfo) );
      instance.SetNew(&new_MagnetInfo);
      instance.SetNewArray(&newArray_MagnetInfo);
      instance.SetDelete(&delete_MagnetInfo);
      instance.SetDeleteArray(&deleteArray_MagnetInfo);
      instance.SetDestructor(&destruct_MagnetInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MagnetInfo*)
   {
      return GenerateInitInstanceLocal((::MagnetInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::MagnetInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_ScalerInfo(void *p = 0);
   static void *newArray_ScalerInfo(Long_t size, void *p);
   static void delete_ScalerInfo(void *p);
   static void deleteArray_ScalerInfo(void *p);
   static void destruct_ScalerInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::ScalerInfo*)
   {
      ::ScalerInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::ScalerInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("ScalerInfo", ::ScalerInfo::Class_Version(), "", 209,
                  typeid(::ScalerInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::ScalerInfo::Dictionary, isa_proxy, 4,
                  sizeof(::ScalerInfo) );
      instance.SetNew(&new_ScalerInfo);
      instance.SetNewArray(&newArray_ScalerInfo);
      instance.SetDelete(&delete_ScalerInfo);
      instance.SetDeleteArray(&deleteArray_ScalerInfo);
      instance.SetDestructor(&destruct_ScalerInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::ScalerInfo*)
   {
      return GenerateInitInstanceLocal((::ScalerInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::ScalerInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_PrimitiveInfo(void *p = 0);
   static void *newArray_PrimitiveInfo(Long_t size, void *p);
   static void delete_PrimitiveInfo(void *p);
   static void deleteArray_PrimitiveInfo(void *p);
   static void destruct_PrimitiveInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::PrimitiveInfo*)
   {
      ::PrimitiveInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::PrimitiveInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("PrimitiveInfo", ::PrimitiveInfo::Class_Version(), "", 227,
                  typeid(::PrimitiveInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::PrimitiveInfo::Dictionary, isa_proxy, 4,
                  sizeof(::PrimitiveInfo) );
      instance.SetNew(&new_PrimitiveInfo);
      instance.SetNewArray(&newArray_PrimitiveInfo);
      instance.SetDelete(&delete_PrimitiveInfo);
      instance.SetDeleteArray(&deleteArray_PrimitiveInfo);
      instance.SetDestructor(&destruct_PrimitiveInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::PrimitiveInfo*)
   {
      return GenerateInitInstanceLocal((::PrimitiveInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::PrimitiveInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_BeamSpecialTrigger(void *p = 0);
   static void *newArray_BeamSpecialTrigger(Long_t size, void *p);
   static void delete_BeamSpecialTrigger(void *p);
   static void deleteArray_BeamSpecialTrigger(void *p);
   static void destruct_BeamSpecialTrigger(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::BeamSpecialTrigger*)
   {
      ::BeamSpecialTrigger *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::BeamSpecialTrigger >(0);
      static ::ROOT::TGenericClassInfo 
         instance("BeamSpecialTrigger", ::BeamSpecialTrigger::Class_Version(), "", 269,
                  typeid(::BeamSpecialTrigger), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::BeamSpecialTrigger::Dictionary, isa_proxy, 4,
                  sizeof(::BeamSpecialTrigger) );
      instance.SetNew(&new_BeamSpecialTrigger);
      instance.SetNewArray(&newArray_BeamSpecialTrigger);
      instance.SetDelete(&delete_BeamSpecialTrigger);
      instance.SetDeleteArray(&deleteArray_BeamSpecialTrigger);
      instance.SetDestructor(&destruct_BeamSpecialTrigger);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::BeamSpecialTrigger*)
   {
      return GenerateInitInstanceLocal((::BeamSpecialTrigger*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::BeamSpecialTrigger*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_DetectorParameter(void *p = 0);
   static void *newArray_DetectorParameter(Long_t size, void *p);
   static void delete_DetectorParameter(void *p);
   static void deleteArray_DetectorParameter(void *p);
   static void destruct_DetectorParameter(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::DetectorParameter*)
   {
      ::DetectorParameter *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::DetectorParameter >(0);
      static ::ROOT::TGenericClassInfo 
         instance("DetectorParameter", ::DetectorParameter::Class_Version(), "", 425,
                  typeid(::DetectorParameter), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::DetectorParameter::Dictionary, isa_proxy, 4,
                  sizeof(::DetectorParameter) );
      instance.SetNew(&new_DetectorParameter);
      instance.SetNewArray(&newArray_DetectorParameter);
      instance.SetDelete(&delete_DetectorParameter);
      instance.SetDeleteArray(&deleteArray_DetectorParameter);
      instance.SetDestructor(&destruct_DetectorParameter);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::DetectorParameter*)
   {
      return GenerateInitInstanceLocal((::DetectorParameter*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::DetectorParameter*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_EventBoundary(void *p = 0);
   static void *newArray_EventBoundary(Long_t size, void *p);
   static void delete_EventBoundary(void *p);
   static void deleteArray_EventBoundary(void *p);
   static void destruct_EventBoundary(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::EventBoundary*)
   {
      ::EventBoundary *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::EventBoundary >(0);
      static ::ROOT::TGenericClassInfo 
         instance("EventBoundary", ::EventBoundary::Class_Version(), "EventBoundary.hh", 12,
                  typeid(::EventBoundary), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::EventBoundary::Dictionary, isa_proxy, 4,
                  sizeof(::EventBoundary) );
      instance.SetNew(&new_EventBoundary);
      instance.SetNewArray(&newArray_EventBoundary);
      instance.SetDelete(&delete_EventBoundary);
      instance.SetDeleteArray(&deleteArray_EventBoundary);
      instance.SetDestructor(&destruct_EventBoundary);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::EventBoundary*)
   {
      return GenerateInitInstanceLocal((::EventBoundary*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::EventBoundary*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_GenePart(void *p = 0);
   static void *newArray_GenePart(Long_t size, void *p);
   static void delete_GenePart(void *p);
   static void deleteArray_GenePart(void *p);
   static void destruct_GenePart(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::GenePart*)
   {
      ::GenePart *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::GenePart >(0);
      static ::ROOT::TGenericClassInfo 
         instance("GenePart", ::GenePart::Class_Version(), "GenePart.hh", 14,
                  typeid(::GenePart), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::GenePart::Dictionary, isa_proxy, 4,
                  sizeof(::GenePart) );
      instance.SetNew(&new_GenePart);
      instance.SetNewArray(&newArray_GenePart);
      instance.SetDelete(&delete_GenePart);
      instance.SetDeleteArray(&deleteArray_GenePart);
      instance.SetDestructor(&destruct_GenePart);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::GenePart*)
   {
      return GenerateInitInstanceLocal((::GenePart*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::GenePart*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_KinePart(void *p = 0);
   static void *newArray_KinePart(Long_t size, void *p);
   static void delete_KinePart(void *p);
   static void deleteArray_KinePart(void *p);
   static void destruct_KinePart(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::KinePart*)
   {
      ::KinePart *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::KinePart >(0);
      static ::ROOT::TGenericClassInfo 
         instance("KinePart", ::KinePart::Class_Version(), "KinePart.hh", 20,
                  typeid(::KinePart), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::KinePart::Dictionary, isa_proxy, 4,
                  sizeof(::KinePart) );
      instance.SetNew(&new_KinePart);
      instance.SetNewArray(&newArray_KinePart);
      instance.SetDelete(&delete_KinePart);
      instance.SetDeleteArray(&deleteArray_KinePart);
      instance.SetDestructor(&destruct_KinePart);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::KinePart*)
   {
      return GenerateInitInstanceLocal((::KinePart*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::KinePart*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TEventInfo(void *p = 0);
   static void *newArray_TEventInfo(Long_t size, void *p);
   static void delete_TEventInfo(void *p);
   static void deleteArray_TEventInfo(void *p);
   static void destruct_TEventInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TEventInfo*)
   {
      ::TEventInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TEventInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TEventInfo", ::TEventInfo::Class_Version(), "TEventInfo.hh", 14,
                  typeid(::TEventInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TEventInfo::Dictionary, isa_proxy, 4,
                  sizeof(::TEventInfo) );
      instance.SetNew(&new_TEventInfo);
      instance.SetNewArray(&newArray_TEventInfo);
      instance.SetDelete(&delete_TEventInfo);
      instance.SetDeleteArray(&deleteArray_TEventInfo);
      instance.SetDestructor(&destruct_TEventInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TEventInfo*)
   {
      return GenerateInitInstanceLocal((::TEventInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TEventInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_Event(void *p = 0);
   static void *newArray_Event(Long_t size, void *p);
   static void delete_Event(void *p);
   static void deleteArray_Event(void *p);
   static void destruct_Event(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::Event*)
   {
      ::Event *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::Event >(0);
      static ::ROOT::TGenericClassInfo 
         instance("Event", ::Event::Class_Version(), "", 478,
                  typeid(::Event), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::Event::Dictionary, isa_proxy, 4,
                  sizeof(::Event) );
      instance.SetNew(&new_Event);
      instance.SetNewArray(&newArray_Event);
      instance.SetDelete(&delete_Event);
      instance.SetDeleteArray(&deleteArray_Event);
      instance.SetDestructor(&destruct_Event);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::Event*)
   {
      return GenerateInitInstanceLocal((::Event*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::Event*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_HLTTrack(void *p = 0);
   static void *newArray_HLTTrack(Long_t size, void *p);
   static void delete_HLTTrack(void *p);
   static void deleteArray_HLTTrack(void *p);
   static void destruct_HLTTrack(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::HLTTrack*)
   {
      ::HLTTrack *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::HLTTrack >(0);
      static ::ROOT::TGenericClassInfo 
         instance("HLTTrack", ::HLTTrack::Class_Version(), "HLTEvent.hh", 15,
                  typeid(::HLTTrack), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::HLTTrack::Dictionary, isa_proxy, 4,
                  sizeof(::HLTTrack) );
      instance.SetNew(&new_HLTTrack);
      instance.SetNewArray(&newArray_HLTTrack);
      instance.SetDelete(&delete_HLTTrack);
      instance.SetDeleteArray(&deleteArray_HLTTrack);
      instance.SetDestructor(&destruct_HLTTrack);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::HLTTrack*)
   {
      return GenerateInitInstanceLocal((::HLTTrack*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::HLTTrack*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_HLTEvent(void *p = 0);
   static void *newArray_HLTEvent(Long_t size, void *p);
   static void delete_HLTEvent(void *p);
   static void deleteArray_HLTEvent(void *p);
   static void destruct_HLTEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::HLTEvent*)
   {
      ::HLTEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::HLTEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("HLTEvent", ::HLTEvent::Class_Version(), "HLTEvent.hh", 84,
                  typeid(::HLTEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::HLTEvent::Dictionary, isa_proxy, 4,
                  sizeof(::HLTEvent) );
      instance.SetNew(&new_HLTEvent);
      instance.SetNewArray(&newArray_HLTEvent);
      instance.SetDelete(&delete_HLTEvent);
      instance.SetDeleteArray(&deleteArray_HLTEvent);
      instance.SetDestructor(&destruct_HLTEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::HLTEvent*)
   {
      return GenerateInitInstanceLocal((::HLTEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::HLTEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L0TPData(void *p = 0);
   static void *newArray_L0TPData(Long_t size, void *p);
   static void delete_L0TPData(void *p);
   static void deleteArray_L0TPData(void *p);
   static void destruct_L0TPData(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L0TPData*)
   {
      ::L0TPData *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L0TPData >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L0TPData", ::L0TPData::Class_Version(), "L0TPData.hh", 73,
                  typeid(::L0TPData), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L0TPData::Dictionary, isa_proxy, 4,
                  sizeof(::L0TPData) );
      instance.SetNew(&new_L0TPData);
      instance.SetNewArray(&newArray_L0TPData);
      instance.SetDelete(&delete_L0TPData);
      instance.SetDeleteArray(&deleteArray_L0TPData);
      instance.SetDestructor(&destruct_L0TPData);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L0TPData*)
   {
      return GenerateInitInstanceLocal((::L0TPData*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L0TPData*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L0Primitive(void *p = 0);
   static void *newArray_L0Primitive(Long_t size, void *p);
   static void delete_L0Primitive(void *p);
   static void deleteArray_L0Primitive(void *p);
   static void destruct_L0Primitive(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L0Primitive*)
   {
      ::L0Primitive *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L0Primitive >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L0Primitive", ::L0Primitive::Class_Version(), "L0TPData.hh", 125,
                  typeid(::L0Primitive), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L0Primitive::Dictionary, isa_proxy, 4,
                  sizeof(::L0Primitive) );
      instance.SetNew(&new_L0Primitive);
      instance.SetNewArray(&newArray_L0Primitive);
      instance.SetDelete(&delete_L0Primitive);
      instance.SetDeleteArray(&deleteArray_L0Primitive);
      instance.SetDestructor(&destruct_L0Primitive);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L0Primitive*)
   {
      return GenerateInitInstanceLocal((::L0Primitive*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L0Primitive*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L0TPSpecialTrigger(void *p = 0);
   static void *newArray_L0TPSpecialTrigger(Long_t size, void *p);
   static void delete_L0TPSpecialTrigger(void *p);
   static void deleteArray_L0TPSpecialTrigger(void *p);
   static void destruct_L0TPSpecialTrigger(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L0TPSpecialTrigger*)
   {
      ::L0TPSpecialTrigger *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L0TPSpecialTrigger >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L0TPSpecialTrigger", ::L0TPSpecialTrigger::Class_Version(), "L0TPSpecialTrigger.hh", 111,
                  typeid(::L0TPSpecialTrigger), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L0TPSpecialTrigger::Dictionary, isa_proxy, 4,
                  sizeof(::L0TPSpecialTrigger) );
      instance.SetNew(&new_L0TPSpecialTrigger);
      instance.SetNewArray(&newArray_L0TPSpecialTrigger);
      instance.SetDelete(&delete_L0TPSpecialTrigger);
      instance.SetDeleteArray(&deleteArray_L0TPSpecialTrigger);
      instance.SetDestructor(&destruct_L0TPSpecialTrigger);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L0TPSpecialTrigger*)
   {
      return GenerateInitInstanceLocal((::L0TPSpecialTrigger*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L0TPSpecialTrigger*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L0Mask(void *p = 0);
   static void *newArray_L0Mask(Long_t size, void *p);
   static void delete_L0Mask(void *p);
   static void deleteArray_L0Mask(void *p);
   static void destruct_L0Mask(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L0Mask*)
   {
      ::L0Mask *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L0Mask >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L0Mask", ::L0Mask::Class_Version(), "L0TPSpecialTrigger.hh", 177,
                  typeid(::L0Mask), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L0Mask::Dictionary, isa_proxy, 4,
                  sizeof(::L0Mask) );
      instance.SetNew(&new_L0Mask);
      instance.SetNewArray(&newArray_L0Mask);
      instance.SetDelete(&delete_L0Mask);
      instance.SetDeleteArray(&deleteArray_L0Mask);
      instance.SetDestructor(&destruct_L0Mask);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L0Mask*)
   {
      return GenerateInitInstanceLocal((::L0Mask*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L0Mask*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L1TPData(void *p = 0);
   static void *newArray_L1TPData(Long_t size, void *p);
   static void delete_L1TPData(void *p);
   static void deleteArray_L1TPData(void *p);
   static void destruct_L1TPData(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L1TPData*)
   {
      ::L1TPData *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L1TPData >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L1TPData", ::L1TPData::Class_Version(), "L1TPData.hh", 116,
                  typeid(::L1TPData), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L1TPData::Dictionary, isa_proxy, 4,
                  sizeof(::L1TPData) );
      instance.SetNew(&new_L1TPData);
      instance.SetNewArray(&newArray_L1TPData);
      instance.SetDelete(&delete_L1TPData);
      instance.SetDeleteArray(&deleteArray_L1TPData);
      instance.SetDestructor(&destruct_L1TPData);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L1TPData*)
   {
      return GenerateInitInstanceLocal((::L1TPData*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L1TPData*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L1MaskBlock(void *p = 0);
   static void *newArray_L1MaskBlock(Long_t size, void *p);
   static void delete_L1MaskBlock(void *p);
   static void deleteArray_L1MaskBlock(void *p);
   static void destruct_L1MaskBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L1MaskBlock*)
   {
      ::L1MaskBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L1MaskBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L1MaskBlock", ::L1MaskBlock::Class_Version(), "L1TPData.hh", 165,
                  typeid(::L1MaskBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L1MaskBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L1MaskBlock) );
      instance.SetNew(&new_L1MaskBlock);
      instance.SetNewArray(&newArray_L1MaskBlock);
      instance.SetDelete(&delete_L1MaskBlock);
      instance.SetDeleteArray(&deleteArray_L1MaskBlock);
      instance.SetDestructor(&destruct_L1MaskBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L1MaskBlock*)
   {
      return GenerateInitInstanceLocal((::L1MaskBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L1MaskBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L1AlgoBlock(void *p = 0);
   static void *newArray_L1AlgoBlock(Long_t size, void *p);
   static void delete_L1AlgoBlock(void *p);
   static void deleteArray_L1AlgoBlock(void *p);
   static void destruct_L1AlgoBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L1AlgoBlock*)
   {
      ::L1AlgoBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L1AlgoBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L1AlgoBlock", ::L1AlgoBlock::Class_Version(), "L1TPData.hh", 205,
                  typeid(::L1AlgoBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L1AlgoBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L1AlgoBlock) );
      instance.SetNew(&new_L1AlgoBlock);
      instance.SetNewArray(&newArray_L1AlgoBlock);
      instance.SetDelete(&delete_L1AlgoBlock);
      instance.SetDeleteArray(&deleteArray_L1AlgoBlock);
      instance.SetDestructor(&destruct_L1AlgoBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L1AlgoBlock*)
   {
      return GenerateInitInstanceLocal((::L1AlgoBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L1AlgoBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L1TPSpecialTrigger(void *p = 0);
   static void *newArray_L1TPSpecialTrigger(Long_t size, void *p);
   static void delete_L1TPSpecialTrigger(void *p);
   static void deleteArray_L1TPSpecialTrigger(void *p);
   static void destruct_L1TPSpecialTrigger(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L1TPSpecialTrigger*)
   {
      ::L1TPSpecialTrigger *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L1TPSpecialTrigger >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L1TPSpecialTrigger", ::L1TPSpecialTrigger::Class_Version(), "L1TPSpecialTrigger.hh", 58,
                  typeid(::L1TPSpecialTrigger), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L1TPSpecialTrigger::Dictionary, isa_proxy, 4,
                  sizeof(::L1TPSpecialTrigger) );
      instance.SetNew(&new_L1TPSpecialTrigger);
      instance.SetNewArray(&newArray_L1TPSpecialTrigger);
      instance.SetDelete(&delete_L1TPSpecialTrigger);
      instance.SetDeleteArray(&deleteArray_L1TPSpecialTrigger);
      instance.SetDestructor(&destruct_L1TPSpecialTrigger);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L1TPSpecialTrigger*)
   {
      return GenerateInitInstanceLocal((::L1TPSpecialTrigger*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L1TPSpecialTrigger*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L1PCSpecialBlock(void *p = 0);
   static void *newArray_L1PCSpecialBlock(Long_t size, void *p);
   static void delete_L1PCSpecialBlock(void *p);
   static void deleteArray_L1PCSpecialBlock(void *p);
   static void destruct_L1PCSpecialBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L1PCSpecialBlock*)
   {
      ::L1PCSpecialBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L1PCSpecialBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L1PCSpecialBlock", ::L1PCSpecialBlock::Class_Version(), "L1TPSpecialTrigger.hh", 76,
                  typeid(::L1PCSpecialBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L1PCSpecialBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L1PCSpecialBlock) );
      instance.SetNew(&new_L1PCSpecialBlock);
      instance.SetNewArray(&newArray_L1PCSpecialBlock);
      instance.SetDelete(&delete_L1PCSpecialBlock);
      instance.SetDeleteArray(&deleteArray_L1PCSpecialBlock);
      instance.SetDestructor(&destruct_L1PCSpecialBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L1PCSpecialBlock*)
   {
      return GenerateInitInstanceLocal((::L1PCSpecialBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L1PCSpecialBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L1MaskSpecialBlock(void *p = 0);
   static void *newArray_L1MaskSpecialBlock(Long_t size, void *p);
   static void delete_L1MaskSpecialBlock(void *p);
   static void deleteArray_L1MaskSpecialBlock(void *p);
   static void destruct_L1MaskSpecialBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L1MaskSpecialBlock*)
   {
      ::L1MaskSpecialBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L1MaskSpecialBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L1MaskSpecialBlock", ::L1MaskSpecialBlock::Class_Version(), "L1TPSpecialTrigger.hh", 154,
                  typeid(::L1MaskSpecialBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L1MaskSpecialBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L1MaskSpecialBlock) );
      instance.SetNew(&new_L1MaskSpecialBlock);
      instance.SetNewArray(&newArray_L1MaskSpecialBlock);
      instance.SetDelete(&delete_L1MaskSpecialBlock);
      instance.SetDeleteArray(&deleteArray_L1MaskSpecialBlock);
      instance.SetDestructor(&destruct_L1MaskSpecialBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L1MaskSpecialBlock*)
   {
      return GenerateInitInstanceLocal((::L1MaskSpecialBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L1MaskSpecialBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L2EBData(void *p = 0);
   static void *newArray_L2EBData(Long_t size, void *p);
   static void delete_L2EBData(void *p);
   static void deleteArray_L2EBData(void *p);
   static void destruct_L2EBData(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L2EBData*)
   {
      ::L2EBData *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L2EBData >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L2EBData", ::L2EBData::Class_Version(), "L2EBData.hh", 116,
                  typeid(::L2EBData), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L2EBData::Dictionary, isa_proxy, 4,
                  sizeof(::L2EBData) );
      instance.SetNew(&new_L2EBData);
      instance.SetNewArray(&newArray_L2EBData);
      instance.SetDelete(&delete_L2EBData);
      instance.SetDeleteArray(&deleteArray_L2EBData);
      instance.SetDestructor(&destruct_L2EBData);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L2EBData*)
   {
      return GenerateInitInstanceLocal((::L2EBData*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L2EBData*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L2MaskBlock(void *p = 0);
   static void *newArray_L2MaskBlock(Long_t size, void *p);
   static void delete_L2MaskBlock(void *p);
   static void deleteArray_L2MaskBlock(void *p);
   static void destruct_L2MaskBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L2MaskBlock*)
   {
      ::L2MaskBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L2MaskBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L2MaskBlock", ::L2MaskBlock::Class_Version(), "L2EBData.hh", 165,
                  typeid(::L2MaskBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L2MaskBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L2MaskBlock) );
      instance.SetNew(&new_L2MaskBlock);
      instance.SetNewArray(&newArray_L2MaskBlock);
      instance.SetDelete(&delete_L2MaskBlock);
      instance.SetDeleteArray(&deleteArray_L2MaskBlock);
      instance.SetDestructor(&destruct_L2MaskBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L2MaskBlock*)
   {
      return GenerateInitInstanceLocal((::L2MaskBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L2MaskBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L2AlgoBlock(void *p = 0);
   static void *newArray_L2AlgoBlock(Long_t size, void *p);
   static void delete_L2AlgoBlock(void *p);
   static void deleteArray_L2AlgoBlock(void *p);
   static void destruct_L2AlgoBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L2AlgoBlock*)
   {
      ::L2AlgoBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L2AlgoBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L2AlgoBlock", ::L2AlgoBlock::Class_Version(), "L2EBData.hh", 205,
                  typeid(::L2AlgoBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L2AlgoBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L2AlgoBlock) );
      instance.SetNew(&new_L2AlgoBlock);
      instance.SetNewArray(&newArray_L2AlgoBlock);
      instance.SetDelete(&delete_L2AlgoBlock);
      instance.SetDeleteArray(&deleteArray_L2AlgoBlock);
      instance.SetDestructor(&destruct_L2AlgoBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L2AlgoBlock*)
   {
      return GenerateInitInstanceLocal((::L2AlgoBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L2AlgoBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L2EBSpecialTrigger(void *p = 0);
   static void *newArray_L2EBSpecialTrigger(Long_t size, void *p);
   static void delete_L2EBSpecialTrigger(void *p);
   static void deleteArray_L2EBSpecialTrigger(void *p);
   static void destruct_L2EBSpecialTrigger(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L2EBSpecialTrigger*)
   {
      ::L2EBSpecialTrigger *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L2EBSpecialTrigger >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L2EBSpecialTrigger", ::L2EBSpecialTrigger::Class_Version(), "L2EBSpecialTrigger.hh", 57,
                  typeid(::L2EBSpecialTrigger), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L2EBSpecialTrigger::Dictionary, isa_proxy, 4,
                  sizeof(::L2EBSpecialTrigger) );
      instance.SetNew(&new_L2EBSpecialTrigger);
      instance.SetNewArray(&newArray_L2EBSpecialTrigger);
      instance.SetDelete(&delete_L2EBSpecialTrigger);
      instance.SetDeleteArray(&deleteArray_L2EBSpecialTrigger);
      instance.SetDestructor(&destruct_L2EBSpecialTrigger);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L2EBSpecialTrigger*)
   {
      return GenerateInitInstanceLocal((::L2EBSpecialTrigger*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L2EBSpecialTrigger*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L2PCSpecialBlock(void *p = 0);
   static void *newArray_L2PCSpecialBlock(Long_t size, void *p);
   static void delete_L2PCSpecialBlock(void *p);
   static void deleteArray_L2PCSpecialBlock(void *p);
   static void destruct_L2PCSpecialBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L2PCSpecialBlock*)
   {
      ::L2PCSpecialBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L2PCSpecialBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L2PCSpecialBlock", ::L2PCSpecialBlock::Class_Version(), "L2EBSpecialTrigger.hh", 75,
                  typeid(::L2PCSpecialBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L2PCSpecialBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L2PCSpecialBlock) );
      instance.SetNew(&new_L2PCSpecialBlock);
      instance.SetNewArray(&newArray_L2PCSpecialBlock);
      instance.SetDelete(&delete_L2PCSpecialBlock);
      instance.SetDeleteArray(&deleteArray_L2PCSpecialBlock);
      instance.SetDestructor(&destruct_L2PCSpecialBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L2PCSpecialBlock*)
   {
      return GenerateInitInstanceLocal((::L2PCSpecialBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L2PCSpecialBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_L2MaskSpecialBlock(void *p = 0);
   static void *newArray_L2MaskSpecialBlock(Long_t size, void *p);
   static void delete_L2MaskSpecialBlock(void *p);
   static void deleteArray_L2MaskSpecialBlock(void *p);
   static void destruct_L2MaskSpecialBlock(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::L2MaskSpecialBlock*)
   {
      ::L2MaskSpecialBlock *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::L2MaskSpecialBlock >(0);
      static ::ROOT::TGenericClassInfo 
         instance("L2MaskSpecialBlock", ::L2MaskSpecialBlock::Class_Version(), "L2EBSpecialTrigger.hh", 150,
                  typeid(::L2MaskSpecialBlock), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::L2MaskSpecialBlock::Dictionary, isa_proxy, 4,
                  sizeof(::L2MaskSpecialBlock) );
      instance.SetNew(&new_L2MaskSpecialBlock);
      instance.SetNewArray(&newArray_L2MaskSpecialBlock);
      instance.SetDelete(&delete_L2MaskSpecialBlock);
      instance.SetDeleteArray(&deleteArray_L2MaskSpecialBlock);
      instance.SetDestructor(&destruct_L2MaskSpecialBlock);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::L2MaskSpecialBlock*)
   {
      return GenerateInitInstanceLocal((::L2MaskSpecialBlock*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::L2MaskSpecialBlock*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_EventHeader(void *p = 0);
   static void *newArray_EventHeader(Long_t size, void *p);
   static void delete_EventHeader(void *p);
   static void deleteArray_EventHeader(void *p);
   static void destruct_EventHeader(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::EventHeader*)
   {
      ::EventHeader *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::EventHeader >(0);
      static ::ROOT::TGenericClassInfo 
         instance("EventHeader", ::EventHeader::Class_Version(), "", 634,
                  typeid(::EventHeader), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::EventHeader::Dictionary, isa_proxy, 4,
                  sizeof(::EventHeader) );
      instance.SetNew(&new_EventHeader);
      instance.SetNewArray(&newArray_EventHeader);
      instance.SetDelete(&delete_EventHeader);
      instance.SetDeleteArray(&deleteArray_EventHeader);
      instance.SetDestructor(&destruct_EventHeader);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::EventHeader*)
   {
      return GenerateInitInstanceLocal((::EventHeader*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::EventHeader*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TVChannelID(void *p = 0);
   static void *newArray_TVChannelID(Long_t size, void *p);
   static void delete_TVChannelID(void *p);
   static void deleteArray_TVChannelID(void *p);
   static void destruct_TVChannelID(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TVChannelID*)
   {
      ::TVChannelID *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TVChannelID >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TVChannelID", ::TVChannelID::Class_Version(), "TVChannelID.hh", 14,
                  typeid(::TVChannelID), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TVChannelID::Dictionary, isa_proxy, 4,
                  sizeof(::TVChannelID) );
      instance.SetNew(&new_TVChannelID);
      instance.SetNewArray(&newArray_TVChannelID);
      instance.SetDelete(&delete_TVChannelID);
      instance.SetDeleteArray(&deleteArray_TVChannelID);
      instance.SetDestructor(&destruct_TVChannelID);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TVChannelID*)
   {
      return GenerateInitInstanceLocal((::TVChannelID*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TVChannelID*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_TVHit(void *p);
   static void deleteArray_TVHit(void *p);
   static void destruct_TVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TVHit*)
   {
      ::TVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TVHit", ::TVHit::Class_Version(), "TVHit.hh", 14,
                  typeid(::TVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TVHit) );
      instance.SetDelete(&delete_TVHit);
      instance.SetDeleteArray(&deleteArray_TVHit);
      instance.SetDestructor(&destruct_TVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TVHit*)
   {
      return GenerateInitInstanceLocal((::TVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_TVDigi(void *p);
   static void deleteArray_TVDigi(void *p);
   static void destruct_TVDigi(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TVDigi*)
   {
      ::TVDigi *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TVDigi >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TVDigi", ::TVDigi::Class_Version(), "TVDigi.hh", 12,
                  typeid(::TVDigi), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TVDigi::Dictionary, isa_proxy, 4,
                  sizeof(::TVDigi) );
      instance.SetDelete(&delete_TVDigi);
      instance.SetDeleteArray(&deleteArray_TVDigi);
      instance.SetDestructor(&destruct_TVDigi);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TVDigi*)
   {
      return GenerateInitInstanceLocal((::TVDigi*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TVDigi*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_FADCVHit(void *p);
   static void deleteArray_FADCVHit(void *p);
   static void destruct_FADCVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::FADCVHit*)
   {
      ::FADCVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::FADCVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("FADCVHit", ::FADCVHit::Class_Version(), "FADCVHit.hh", 14,
                  typeid(::FADCVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::FADCVHit::Dictionary, isa_proxy, 4,
                  sizeof(::FADCVHit) );
      instance.SetDelete(&delete_FADCVHit);
      instance.SetDeleteArray(&deleteArray_FADCVHit);
      instance.SetDestructor(&destruct_FADCVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::FADCVHit*)
   {
      return GenerateInitInstanceLocal((::FADCVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::FADCVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDetectorVHit(void *p = 0);
   static void *newArray_TDetectorVHit(Long_t size, void *p);
   static void delete_TDetectorVHit(void *p);
   static void deleteArray_TDetectorVHit(void *p);
   static void destruct_TDetectorVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDetectorVHit*)
   {
      ::TDetectorVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDetectorVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDetectorVHit", ::TDetectorVHit::Class_Version(), "TDetectorVHit.hh", 13,
                  typeid(::TDetectorVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDetectorVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TDetectorVHit) );
      instance.SetNew(&new_TDetectorVHit);
      instance.SetNewArray(&newArray_TDetectorVHit);
      instance.SetDelete(&delete_TDetectorVHit);
      instance.SetDeleteArray(&deleteArray_TDetectorVHit);
      instance.SetDestructor(&destruct_TDetectorVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDetectorVHit*)
   {
      return GenerateInitInstanceLocal((::TDetectorVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDetectorVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TVEvent(void *p = 0);
   static void *newArray_TVEvent(Long_t size, void *p);
   static void delete_TVEvent(void *p);
   static void deleteArray_TVEvent(void *p);
   static void destruct_TVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TVEvent*)
   {
      ::TVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TVEvent", ::TVEvent::Class_Version(), "TVEvent.hh", 13,
                  typeid(::TVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TVEvent) );
      instance.SetNew(&new_TVEvent);
      instance.SetNewArray(&newArray_TVEvent);
      instance.SetDelete(&delete_TVEvent);
      instance.SetDeleteArray(&deleteArray_TVEvent);
      instance.SetDestructor(&destruct_TVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TVEvent*)
   {
      return GenerateInitInstanceLocal((::TVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDetectorVEvent(void *p = 0);
   static void *newArray_TDetectorVEvent(Long_t size, void *p);
   static void delete_TDetectorVEvent(void *p);
   static void deleteArray_TDetectorVEvent(void *p);
   static void destruct_TDetectorVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDetectorVEvent*)
   {
      ::TDetectorVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDetectorVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDetectorVEvent", ::TDetectorVEvent::Class_Version(), "TDetectorVEvent.hh", 18,
                  typeid(::TDetectorVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDetectorVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TDetectorVEvent) );
      instance.SetNew(&new_TDetectorVEvent);
      instance.SetNewArray(&newArray_TDetectorVEvent);
      instance.SetDelete(&delete_TDetectorVEvent);
      instance.SetDeleteArray(&deleteArray_TDetectorVEvent);
      instance.SetDestructor(&destruct_TDetectorVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDetectorVEvent*)
   {
      return GenerateInitInstanceLocal((::TDetectorVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDetectorVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TVCandidate(void *p = 0);
   static void *newArray_TVCandidate(Long_t size, void *p);
   static void delete_TVCandidate(void *p);
   static void deleteArray_TVCandidate(void *p);
   static void destruct_TVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TVCandidate*)
   {
      ::TVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TVCandidate", ::TVCandidate::Class_Version(), "TVCandidate.hh", 19,
                  typeid(::TVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TVCandidate) );
      instance.SetNew(&new_TVCandidate);
      instance.SetNewArray(&newArray_TVCandidate);
      instance.SetDelete(&delete_TVCandidate);
      instance.SetDeleteArray(&deleteArray_TVCandidate);
      instance.SetDestructor(&destruct_TVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TVCandidate*)
   {
      return GenerateInitInstanceLocal((::TVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDigiVCandidate(void *p = 0);
   static void *newArray_TDigiVCandidate(Long_t size, void *p);
   static void delete_TDigiVCandidate(void *p);
   static void deleteArray_TDigiVCandidate(void *p);
   static void destruct_TDigiVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDigiVCandidate*)
   {
      ::TDigiVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDigiVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDigiVCandidate", ::TDigiVCandidate::Class_Version(), "TDigiVCandidate.hh", 13,
                  typeid(::TDigiVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDigiVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TDigiVCandidate) );
      instance.SetNew(&new_TDigiVCandidate);
      instance.SetNewArray(&newArray_TDigiVCandidate);
      instance.SetDelete(&delete_TDigiVCandidate);
      instance.SetDeleteArray(&deleteArray_TDigiVCandidate);
      instance.SetDestructor(&destruct_TDigiVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDigiVCandidate*)
   {
      return GenerateInitInstanceLocal((::TDigiVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDigiVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDigiVError(void *p = 0);
   static void *newArray_TDigiVError(Long_t size, void *p);
   static void delete_TDigiVError(void *p);
   static void deleteArray_TDigiVError(void *p);
   static void destruct_TDigiVError(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDigiVError*)
   {
      ::TDigiVError *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDigiVError >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDigiVError", ::TDigiVError::Class_Version(), "TDigiVError.hh", 11,
                  typeid(::TDigiVError), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDigiVError::Dictionary, isa_proxy, 4,
                  sizeof(::TDigiVError) );
      instance.SetNew(&new_TDigiVError);
      instance.SetNewArray(&newArray_TDigiVError);
      instance.SetDelete(&delete_TDigiVError);
      instance.SetDeleteArray(&deleteArray_TDigiVError);
      instance.SetDestructor(&destruct_TDigiVError);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDigiVError*)
   {
      return GenerateInitInstanceLocal((::TDigiVError*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDigiVError*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDigiVEvent(void *p = 0);
   static void *newArray_TDigiVEvent(Long_t size, void *p);
   static void delete_TDigiVEvent(void *p);
   static void deleteArray_TDigiVEvent(void *p);
   static void destruct_TDigiVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDigiVEvent*)
   {
      ::TDigiVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDigiVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDigiVEvent", ::TDigiVEvent::Class_Version(), "TDigiVEvent.hh", 18,
                  typeid(::TDigiVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDigiVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TDigiVEvent) );
      instance.SetNew(&new_TDigiVEvent);
      instance.SetNewArray(&newArray_TDigiVEvent);
      instance.SetDelete(&delete_TDigiVEvent);
      instance.SetDeleteArray(&deleteArray_TDigiVEvent);
      instance.SetDestructor(&destruct_TDigiVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDigiVEvent*)
   {
      return GenerateInitInstanceLocal((::TDigiVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDigiVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_FADCEvent(void *p = 0);
   static void *newArray_FADCEvent(Long_t size, void *p);
   static void delete_FADCEvent(void *p);
   static void deleteArray_FADCEvent(void *p);
   static void destruct_FADCEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::FADCEvent*)
   {
      ::FADCEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::FADCEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("FADCEvent", ::FADCEvent::Class_Version(), "", 735,
                  typeid(::FADCEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::FADCEvent::Dictionary, isa_proxy, 4,
                  sizeof(::FADCEvent) );
      instance.SetNew(&new_FADCEvent);
      instance.SetNewArray(&newArray_FADCEvent);
      instance.SetDelete(&delete_FADCEvent);
      instance.SetDeleteArray(&deleteArray_FADCEvent);
      instance.SetDestructor(&destruct_FADCEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::FADCEvent*)
   {
      return GenerateInitInstanceLocal((::FADCEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::FADCEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_MCInfo(void *p = 0);
   static void *newArray_MCInfo(Long_t size, void *p);
   static void delete_MCInfo(void *p);
   static void deleteArray_MCInfo(void *p);
   static void destruct_MCInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::MCInfo*)
   {
      ::MCInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::MCInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("MCInfo", ::MCInfo::Class_Version(), "", 2493,
                  typeid(::MCInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::MCInfo::Dictionary, isa_proxy, 4,
                  sizeof(::MCInfo) );
      instance.SetNew(&new_MCInfo);
      instance.SetNewArray(&newArray_MCInfo);
      instance.SetDelete(&delete_MCInfo);
      instance.SetDeleteArray(&deleteArray_MCInfo);
      instance.SetDestructor(&destruct_MCInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::MCInfo*)
   {
      return GenerateInitInstanceLocal((::MCInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::MCInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_RecoInfo(void *p = 0);
   static void *newArray_RecoInfo(Long_t size, void *p);
   static void delete_RecoInfo(void *p);
   static void deleteArray_RecoInfo(void *p);
   static void destruct_RecoInfo(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::RecoInfo*)
   {
      ::RecoInfo *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::RecoInfo >(0);
      static ::ROOT::TGenericClassInfo 
         instance("RecoInfo", ::RecoInfo::Class_Version(), "", 2593,
                  typeid(::RecoInfo), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::RecoInfo::Dictionary, isa_proxy, 4,
                  sizeof(::RecoInfo) );
      instance.SetNew(&new_RecoInfo);
      instance.SetNewArray(&newArray_RecoInfo);
      instance.SetDelete(&delete_RecoInfo);
      instance.SetDeleteArray(&deleteArray_RecoInfo);
      instance.SetDestructor(&destruct_RecoInfo);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::RecoInfo*)
   {
      return GenerateInitInstanceLocal((::RecoInfo*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::RecoInfo*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_Rndm(void *p = 0);
   static void *newArray_Rndm(Long_t size, void *p);
   static void delete_Rndm(void *p);
   static void deleteArray_Rndm(void *p);
   static void destruct_Rndm(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::Rndm*)
   {
      ::Rndm *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::Rndm >(0);
      static ::ROOT::TGenericClassInfo 
         instance("Rndm", ::Rndm::Class_Version(), "", 2670,
                  typeid(::Rndm), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::Rndm::Dictionary, isa_proxy, 4,
                  sizeof(::Rndm) );
      instance.SetNew(&new_Rndm);
      instance.SetNewArray(&newArray_Rndm);
      instance.SetDelete(&delete_Rndm);
      instance.SetDeleteArray(&deleteArray_Rndm);
      instance.SetDestructor(&destruct_Rndm);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::Rndm*)
   {
      return GenerateInitInstanceLocal((::Rndm*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::Rndm*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_Stream(void *p = 0);
   static void *newArray_Stream(Long_t size, void *p);
   static void delete_Stream(void *p);
   static void deleteArray_Stream(void *p);
   static void destruct_Stream(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::Stream*)
   {
      ::Stream *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::Stream >(0);
      static ::ROOT::TGenericClassInfo 
         instance("Stream", ::Stream::Class_Version(), "", 2705,
                  typeid(::Stream), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::Stream::Dictionary, isa_proxy, 4,
                  sizeof(::Stream) );
      instance.SetNew(&new_Stream);
      instance.SetNewArray(&newArray_Stream);
      instance.SetDelete(&delete_Stream);
      instance.SetDeleteArray(&deleteArray_Stream);
      instance.SetDestructor(&destruct_Stream);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::Stream*)
   {
      return GenerateInitInstanceLocal((::Stream*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::Stream*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDCError(void *p = 0);
   static void *newArray_TDCError(Long_t size, void *p);
   static void delete_TDCError(void *p);
   static void deleteArray_TDCError(void *p);
   static void destruct_TDCError(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDCError*)
   {
      ::TDCError *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDCError >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDCError", ::TDCError::Class_Version(), "", 2744,
                  typeid(::TDCError), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDCError::Dictionary, isa_proxy, 4,
                  sizeof(::TDCError) );
      instance.SetNew(&new_TDCError);
      instance.SetNewArray(&newArray_TDCError);
      instance.SetDelete(&delete_TDCError);
      instance.SetDeleteArray(&deleteArray_TDCError);
      instance.SetDestructor(&destruct_TDCError);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDCError*)
   {
      return GenerateInitInstanceLocal((::TDCError*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDCError*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void delete_TDCVHit(void *p);
   static void deleteArray_TDCVHit(void *p);
   static void destruct_TDCVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDCVHit*)
   {
      ::TDCVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDCVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDCVHit", ::TDCVHit::Class_Version(), "TDCVHit.hh", 12,
                  typeid(::TDCVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDCVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TDCVHit) );
      instance.SetDelete(&delete_TDCVHit);
      instance.SetDeleteArray(&deleteArray_TDCVHit);
      instance.SetDestructor(&destruct_TDCVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDCVHit*)
   {
      return GenerateInitInstanceLocal((::TDCVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDCVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TDCEvent(void *p = 0);
   static void *newArray_TDCEvent(Long_t size, void *p);
   static void delete_TDCEvent(void *p);
   static void deleteArray_TDCEvent(void *p);
   static void destruct_TDCEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TDCEvent*)
   {
      ::TDCEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TDCEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TDCEvent", ::TDCEvent::Class_Version(), "", 2781,
                  typeid(::TDCEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TDCEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TDCEvent) );
      instance.SetNew(&new_TDCEvent);
      instance.SetNewArray(&newArray_TDCEvent);
      instance.SetDelete(&delete_TDCEvent);
      instance.SetDeleteArray(&deleteArray_TDCEvent);
      instance.SetDestructor(&destruct_TDCEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TDCEvent*)
   {
      return GenerateInitInstanceLocal((::TDCEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TDCEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSpecialTrigger(void *p = 0);
   static void *newArray_TSpecialTrigger(Long_t size, void *p);
   static void delete_TSpecialTrigger(void *p);
   static void deleteArray_TSpecialTrigger(void *p);
   static void destruct_TSpecialTrigger(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSpecialTrigger*)
   {
      ::TSpecialTrigger *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSpecialTrigger >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSpecialTrigger", ::TSpecialTrigger::Class_Version(), "TSpecialTrigger.hh", 11,
                  typeid(::TSpecialTrigger), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSpecialTrigger::Dictionary, isa_proxy, 4,
                  sizeof(::TSpecialTrigger) );
      instance.SetNew(&new_TSpecialTrigger);
      instance.SetNewArray(&newArray_TSpecialTrigger);
      instance.SetDelete(&delete_TSpecialTrigger);
      instance.SetDeleteArray(&deleteArray_TSpecialTrigger);
      instance.SetDestructor(&destruct_TSpecialTrigger);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSpecialTrigger*)
   {
      return GenerateInitInstanceLocal((::TSpecialTrigger*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSpecialTrigger*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_PrimRegister(void *p = 0);
   static void *newArray_PrimRegister(Long_t size, void *p);
   static void delete_PrimRegister(void *p);
   static void deleteArray_PrimRegister(void *p);
   static void destruct_PrimRegister(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::PrimRegister*)
   {
      ::PrimRegister *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::PrimRegister >(0);
      static ::ROOT::TGenericClassInfo 
         instance("PrimRegister", ::PrimRegister::Class_Version(), "", 3150,
                  typeid(::PrimRegister), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::PrimRegister::Dictionary, isa_proxy, 4,
                  sizeof(::PrimRegister) );
      instance.SetNew(&new_PrimRegister);
      instance.SetNewArray(&newArray_PrimRegister);
      instance.SetDelete(&delete_PrimRegister);
      instance.SetDeleteArray(&deleteArray_PrimRegister);
      instance.SetDestructor(&destruct_PrimRegister);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::PrimRegister*)
   {
      return GenerateInitInstanceLocal((::PrimRegister*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::PrimRegister*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_PrimCounter(void *p = 0);
   static void *newArray_PrimCounter(Long_t size, void *p);
   static void delete_PrimCounter(void *p);
   static void deleteArray_PrimCounter(void *p);
   static void destruct_PrimCounter(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::PrimCounter*)
   {
      ::PrimCounter *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::PrimCounter >(0);
      static ::ROOT::TGenericClassInfo 
         instance("PrimCounter", ::PrimCounter::Class_Version(), "", 3170,
                  typeid(::PrimCounter), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::PrimCounter::Dictionary, isa_proxy, 4,
                  sizeof(::PrimCounter) );
      instance.SetNew(&new_PrimCounter);
      instance.SetNewArray(&newArray_PrimCounter);
      instance.SetDelete(&delete_PrimCounter);
      instance.SetDeleteArray(&deleteArray_PrimCounter);
      instance.SetDestructor(&destruct_PrimCounter);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::PrimCounter*)
   {
      return GenerateInitInstanceLocal((::PrimCounter*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::PrimCounter*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TPrimSpecialTrigger(void *p = 0);
   static void *newArray_TPrimSpecialTrigger(Long_t size, void *p);
   static void delete_TPrimSpecialTrigger(void *p);
   static void deleteArray_TPrimSpecialTrigger(void *p);
   static void destruct_TPrimSpecialTrigger(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TPrimSpecialTrigger*)
   {
      ::TPrimSpecialTrigger *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TPrimSpecialTrigger >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TPrimSpecialTrigger", ::TPrimSpecialTrigger::Class_Version(), "", 3196,
                  typeid(::TPrimSpecialTrigger), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TPrimSpecialTrigger::Dictionary, isa_proxy, 4,
                  sizeof(::TPrimSpecialTrigger) );
      instance.SetNew(&new_TPrimSpecialTrigger);
      instance.SetNewArray(&newArray_TPrimSpecialTrigger);
      instance.SetDelete(&delete_TPrimSpecialTrigger);
      instance.SetDeleteArray(&deleteArray_TPrimSpecialTrigger);
      instance.SetDestructor(&destruct_TPrimSpecialTrigger);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TPrimSpecialTrigger*)
   {
      return GenerateInitInstanceLocal((::TPrimSpecialTrigger*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TPrimSpecialTrigger*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TPrimitive(void *p = 0);
   static void *newArray_TPrimitive(Long_t size, void *p);
   static void delete_TPrimitive(void *p);
   static void deleteArray_TPrimitive(void *p);
   static void destruct_TPrimitive(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TPrimitive*)
   {
      ::TPrimitive *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TPrimitive >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TPrimitive", ::TPrimitive::Class_Version(), "", 3236,
                  typeid(::TPrimitive), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TPrimitive::Dictionary, isa_proxy, 4,
                  sizeof(::TPrimitive) );
      instance.SetNew(&new_TPrimitive);
      instance.SetNewArray(&newArray_TPrimitive);
      instance.SetDelete(&delete_TPrimitive);
      instance.SetDeleteArray(&deleteArray_TPrimitive);
      instance.SetDestructor(&destruct_TPrimitive);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TPrimitive*)
   {
      return GenerateInitInstanceLocal((::TPrimitive*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TPrimitive*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoVHit(void *p = 0);
   static void *newArray_TRecoVHit(Long_t size, void *p);
   static void delete_TRecoVHit(void *p);
   static void deleteArray_TRecoVHit(void *p);
   static void destruct_TRecoVHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoVHit*)
   {
      ::TRecoVHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoVHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoVHit", ::TRecoVHit::Class_Version(), "TRecoVHit.hh", 13,
                  typeid(::TRecoVHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoVHit::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoVHit) );
      instance.SetNew(&new_TRecoVHit);
      instance.SetNewArray(&newArray_TRecoVHit);
      instance.SetDelete(&delete_TRecoVHit);
      instance.SetDeleteArray(&deleteArray_TRecoVHit);
      instance.SetDestructor(&destruct_TRecoVHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoVHit*)
   {
      return GenerateInitInstanceLocal((::TRecoVHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoVHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoVCandidate(void *p = 0);
   static void *newArray_TRecoVCandidate(Long_t size, void *p);
   static void delete_TRecoVCandidate(void *p);
   static void deleteArray_TRecoVCandidate(void *p);
   static void destruct_TRecoVCandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoVCandidate*)
   {
      ::TRecoVCandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoVCandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoVCandidate", ::TRecoVCandidate::Class_Version(), "", 3299,
                  typeid(::TRecoVCandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoVCandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoVCandidate) );
      instance.SetNew(&new_TRecoVCandidate);
      instance.SetNewArray(&newArray_TRecoVCandidate);
      instance.SetDelete(&delete_TRecoVCandidate);
      instance.SetDeleteArray(&deleteArray_TRecoVCandidate);
      instance.SetDestructor(&destruct_TRecoVCandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoVCandidate*)
   {
      return GenerateInitInstanceLocal((::TRecoVCandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoVCandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TRecoVEvent(void *p = 0);
   static void *newArray_TRecoVEvent(Long_t size, void *p);
   static void delete_TRecoVEvent(void *p);
   static void deleteArray_TRecoVEvent(void *p);
   static void destruct_TRecoVEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TRecoVEvent*)
   {
      ::TRecoVEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TRecoVEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TRecoVEvent", ::TRecoVEvent::Class_Version(), "", 3335,
                  typeid(::TRecoVEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TRecoVEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TRecoVEvent) );
      instance.SetNew(&new_TRecoVEvent);
      instance.SetNewArray(&newArray_TRecoVEvent);
      instance.SetDelete(&delete_TRecoVEvent);
      instance.SetDeleteArray(&deleteArray_TRecoVEvent);
      instance.SetDestructor(&destruct_TRecoVEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TRecoVEvent*)
   {
      return GenerateInitInstanceLocal((::TRecoVEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TRecoVEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSpecialTriggerEvent(void *p = 0);
   static void *newArray_TSpecialTriggerEvent(Long_t size, void *p);
   static void delete_TSpecialTriggerEvent(void *p);
   static void deleteArray_TSpecialTriggerEvent(void *p);
   static void destruct_TSpecialTriggerEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSpecialTriggerEvent*)
   {
      ::TSpecialTriggerEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSpecialTriggerEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSpecialTriggerEvent", ::TSpecialTriggerEvent::Class_Version(), "", 3476,
                  typeid(::TSpecialTriggerEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSpecialTriggerEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSpecialTriggerEvent) );
      instance.SetNew(&new_TSpecialTriggerEvent);
      instance.SetNewArray(&newArray_TSpecialTriggerEvent);
      instance.SetDelete(&delete_TSpecialTriggerEvent);
      instance.SetDeleteArray(&deleteArray_TSpecialTriggerEvent);
      instance.SetDestructor(&destruct_TSpecialTriggerEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSpecialTriggerEvent*)
   {
      return GenerateInitInstanceLocal((::TSpecialTriggerEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSpecialTriggerEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TTDCBSpecialTrigger(void *p = 0);
   static void *newArray_TTDCBSpecialTrigger(Long_t size, void *p);
   static void delete_TTDCBSpecialTrigger(void *p);
   static void deleteArray_TTDCBSpecialTrigger(void *p);
   static void destruct_TTDCBSpecialTrigger(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TTDCBSpecialTrigger*)
   {
      ::TTDCBSpecialTrigger *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TTDCBSpecialTrigger >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TTDCBSpecialTrigger", ::TTDCBSpecialTrigger::Class_Version(), "", 3512,
                  typeid(::TTDCBSpecialTrigger), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TTDCBSpecialTrigger::Dictionary, isa_proxy, 4,
                  sizeof(::TTDCBSpecialTrigger) );
      instance.SetNew(&new_TTDCBSpecialTrigger);
      instance.SetNewArray(&newArray_TTDCBSpecialTrigger);
      instance.SetDelete(&delete_TTDCBSpecialTrigger);
      instance.SetDeleteArray(&deleteArray_TTDCBSpecialTrigger);
      instance.SetDestructor(&destruct_TTDCBSpecialTrigger);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TTDCBSpecialTrigger*)
   {
      return GenerateInitInstanceLocal((::TTDCBSpecialTrigger*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TTDCBSpecialTrigger*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TTimeCluster(void *p = 0);
   static void *newArray_TTimeCluster(Long_t size, void *p);
   static void delete_TTimeCluster(void *p);
   static void deleteArray_TTimeCluster(void *p);
   static void destruct_TTimeCluster(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TTimeCluster*)
   {
      ::TTimeCluster *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TTimeCluster >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TTimeCluster", ::TTimeCluster::Class_Version(), "", 3544,
                  typeid(::TTimeCluster), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TTimeCluster::Dictionary, isa_proxy, 4,
                  sizeof(::TTimeCluster) );
      instance.SetNew(&new_TTimeCluster);
      instance.SetNewArray(&newArray_TTimeCluster);
      instance.SetDelete(&delete_TTimeCluster);
      instance.SetDeleteArray(&deleteArray_TTimeCluster);
      instance.SetDestructor(&destruct_TTimeCluster);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TTimeCluster*)
   {
      return GenerateInitInstanceLocal((::TTimeCluster*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TTimeCluster*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace NA62Analysis {
   namespace Core {
//______________________________________________________________________________
atomic_TClass_ptr AnalyzerIdentifier::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *AnalyzerIdentifier::Class_Name()
{
   return "NA62Analysis::Core::AnalyzerIdentifier";
}

//______________________________________________________________________________
const char *AnalyzerIdentifier::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::NA62Analysis::Core::AnalyzerIdentifier*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int AnalyzerIdentifier::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::NA62Analysis::Core::AnalyzerIdentifier*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *AnalyzerIdentifier::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::NA62Analysis::Core::AnalyzerIdentifier*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *AnalyzerIdentifier::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::NA62Analysis::Core::AnalyzerIdentifier*)0x0)->GetClass(); }
   return fgIsA;
}

} // namespace NA62Analysis::Core
} // namespace NA62Analysis::Core
//______________________________________________________________________________
atomic_TClass_ptr AnalysisInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *AnalysisInfo::Class_Name()
{
   return "AnalysisInfo";
}

//______________________________________________________________________________
const char *AnalysisInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::AnalysisInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int AnalysisInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::AnalysisInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *AnalysisInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::AnalysisInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *AnalysisInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::AnalysisInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr BeamData::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *BeamData::Class_Name()
{
   return "BeamData";
}

//______________________________________________________________________________
const char *BeamData::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::BeamData*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int BeamData::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::BeamData*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *BeamData::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::BeamData*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *BeamData::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::BeamData*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TargetInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TargetInfo::Class_Name()
{
   return "TargetInfo";
}

//______________________________________________________________________________
const char *TargetInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TargetInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TargetInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TargetInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TargetInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TargetInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TargetInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TargetInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr MagnetInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *MagnetInfo::Class_Name()
{
   return "MagnetInfo";
}

//______________________________________________________________________________
const char *MagnetInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MagnetInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int MagnetInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MagnetInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *MagnetInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MagnetInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *MagnetInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MagnetInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr ScalerInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *ScalerInfo::Class_Name()
{
   return "ScalerInfo";
}

//______________________________________________________________________________
const char *ScalerInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::ScalerInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int ScalerInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::ScalerInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *ScalerInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::ScalerInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *ScalerInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::ScalerInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr PrimitiveInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *PrimitiveInfo::Class_Name()
{
   return "PrimitiveInfo";
}

//______________________________________________________________________________
const char *PrimitiveInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::PrimitiveInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int PrimitiveInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::PrimitiveInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *PrimitiveInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::PrimitiveInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *PrimitiveInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::PrimitiveInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr BeamSpecialTrigger::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *BeamSpecialTrigger::Class_Name()
{
   return "BeamSpecialTrigger";
}

//______________________________________________________________________________
const char *BeamSpecialTrigger::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::BeamSpecialTrigger*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int BeamSpecialTrigger::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::BeamSpecialTrigger*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *BeamSpecialTrigger::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::BeamSpecialTrigger*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *BeamSpecialTrigger::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::BeamSpecialTrigger*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr DetectorParameter::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *DetectorParameter::Class_Name()
{
   return "DetectorParameter";
}

//______________________________________________________________________________
const char *DetectorParameter::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::DetectorParameter*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int DetectorParameter::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::DetectorParameter*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *DetectorParameter::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::DetectorParameter*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *DetectorParameter::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::DetectorParameter*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr EventBoundary::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *EventBoundary::Class_Name()
{
   return "EventBoundary";
}

//______________________________________________________________________________
const char *EventBoundary::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::EventBoundary*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int EventBoundary::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::EventBoundary*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *EventBoundary::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::EventBoundary*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *EventBoundary::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::EventBoundary*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr GenePart::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *GenePart::Class_Name()
{
   return "GenePart";
}

//______________________________________________________________________________
const char *GenePart::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::GenePart*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int GenePart::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::GenePart*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *GenePart::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::GenePart*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *GenePart::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::GenePart*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr KinePart::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *KinePart::Class_Name()
{
   return "KinePart";
}

//______________________________________________________________________________
const char *KinePart::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::KinePart*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int KinePart::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::KinePart*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *KinePart::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::KinePart*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *KinePart::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::KinePart*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TEventInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TEventInfo::Class_Name()
{
   return "TEventInfo";
}

//______________________________________________________________________________
const char *TEventInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TEventInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TEventInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TEventInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TEventInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TEventInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TEventInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TEventInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr Event::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *Event::Class_Name()
{
   return "Event";
}

//______________________________________________________________________________
const char *Event::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Event*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int Event::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Event*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *Event::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Event*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *Event::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Event*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr HLTTrack::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *HLTTrack::Class_Name()
{
   return "HLTTrack";
}

//______________________________________________________________________________
const char *HLTTrack::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::HLTTrack*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int HLTTrack::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::HLTTrack*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *HLTTrack::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::HLTTrack*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *HLTTrack::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::HLTTrack*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr HLTEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *HLTEvent::Class_Name()
{
   return "HLTEvent";
}

//______________________________________________________________________________
const char *HLTEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::HLTEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int HLTEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::HLTEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *HLTEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::HLTEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *HLTEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::HLTEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L0TPData::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L0TPData::Class_Name()
{
   return "L0TPData";
}

//______________________________________________________________________________
const char *L0TPData::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0TPData*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L0TPData::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0TPData*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L0TPData::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0TPData*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L0TPData::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0TPData*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L0Primitive::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L0Primitive::Class_Name()
{
   return "L0Primitive";
}

//______________________________________________________________________________
const char *L0Primitive::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0Primitive*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L0Primitive::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0Primitive*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L0Primitive::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0Primitive*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L0Primitive::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0Primitive*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L0TPSpecialTrigger::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L0TPSpecialTrigger::Class_Name()
{
   return "L0TPSpecialTrigger";
}

//______________________________________________________________________________
const char *L0TPSpecialTrigger::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0TPSpecialTrigger*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L0TPSpecialTrigger::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0TPSpecialTrigger*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L0TPSpecialTrigger::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0TPSpecialTrigger*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L0TPSpecialTrigger::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0TPSpecialTrigger*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L0Mask::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L0Mask::Class_Name()
{
   return "L0Mask";
}

//______________________________________________________________________________
const char *L0Mask::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0Mask*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L0Mask::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L0Mask*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L0Mask::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0Mask*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L0Mask::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L0Mask*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L1TPData::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L1TPData::Class_Name()
{
   return "L1TPData";
}

//______________________________________________________________________________
const char *L1TPData::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1TPData*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L1TPData::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1TPData*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L1TPData::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1TPData*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L1TPData::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1TPData*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L1MaskBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L1MaskBlock::Class_Name()
{
   return "L1MaskBlock";
}

//______________________________________________________________________________
const char *L1MaskBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1MaskBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L1MaskBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1MaskBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L1MaskBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1MaskBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L1MaskBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1MaskBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L1AlgoBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L1AlgoBlock::Class_Name()
{
   return "L1AlgoBlock";
}

//______________________________________________________________________________
const char *L1AlgoBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1AlgoBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L1AlgoBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1AlgoBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L1AlgoBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1AlgoBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L1AlgoBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1AlgoBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L1TPSpecialTrigger::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L1TPSpecialTrigger::Class_Name()
{
   return "L1TPSpecialTrigger";
}

//______________________________________________________________________________
const char *L1TPSpecialTrigger::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1TPSpecialTrigger*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L1TPSpecialTrigger::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1TPSpecialTrigger*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L1TPSpecialTrigger::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1TPSpecialTrigger*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L1TPSpecialTrigger::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1TPSpecialTrigger*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L1PCSpecialBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L1PCSpecialBlock::Class_Name()
{
   return "L1PCSpecialBlock";
}

//______________________________________________________________________________
const char *L1PCSpecialBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1PCSpecialBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L1PCSpecialBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1PCSpecialBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L1PCSpecialBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1PCSpecialBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L1PCSpecialBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1PCSpecialBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L1MaskSpecialBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L1MaskSpecialBlock::Class_Name()
{
   return "L1MaskSpecialBlock";
}

//______________________________________________________________________________
const char *L1MaskSpecialBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1MaskSpecialBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L1MaskSpecialBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L1MaskSpecialBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L1MaskSpecialBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1MaskSpecialBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L1MaskSpecialBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L1MaskSpecialBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L2EBData::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L2EBData::Class_Name()
{
   return "L2EBData";
}

//______________________________________________________________________________
const char *L2EBData::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2EBData*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L2EBData::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2EBData*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L2EBData::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2EBData*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L2EBData::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2EBData*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L2MaskBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L2MaskBlock::Class_Name()
{
   return "L2MaskBlock";
}

//______________________________________________________________________________
const char *L2MaskBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2MaskBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L2MaskBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2MaskBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L2MaskBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2MaskBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L2MaskBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2MaskBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L2AlgoBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L2AlgoBlock::Class_Name()
{
   return "L2AlgoBlock";
}

//______________________________________________________________________________
const char *L2AlgoBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2AlgoBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L2AlgoBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2AlgoBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L2AlgoBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2AlgoBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L2AlgoBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2AlgoBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L2EBSpecialTrigger::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L2EBSpecialTrigger::Class_Name()
{
   return "L2EBSpecialTrigger";
}

//______________________________________________________________________________
const char *L2EBSpecialTrigger::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2EBSpecialTrigger*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L2EBSpecialTrigger::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2EBSpecialTrigger*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L2EBSpecialTrigger::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2EBSpecialTrigger*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L2EBSpecialTrigger::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2EBSpecialTrigger*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L2PCSpecialBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L2PCSpecialBlock::Class_Name()
{
   return "L2PCSpecialBlock";
}

//______________________________________________________________________________
const char *L2PCSpecialBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2PCSpecialBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L2PCSpecialBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2PCSpecialBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L2PCSpecialBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2PCSpecialBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L2PCSpecialBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2PCSpecialBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr L2MaskSpecialBlock::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *L2MaskSpecialBlock::Class_Name()
{
   return "L2MaskSpecialBlock";
}

//______________________________________________________________________________
const char *L2MaskSpecialBlock::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2MaskSpecialBlock*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int L2MaskSpecialBlock::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::L2MaskSpecialBlock*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *L2MaskSpecialBlock::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2MaskSpecialBlock*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *L2MaskSpecialBlock::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::L2MaskSpecialBlock*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr EventHeader::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *EventHeader::Class_Name()
{
   return "EventHeader";
}

//______________________________________________________________________________
const char *EventHeader::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::EventHeader*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int EventHeader::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::EventHeader*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *EventHeader::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::EventHeader*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *EventHeader::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::EventHeader*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TVChannelID::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TVChannelID::Class_Name()
{
   return "TVChannelID";
}

//______________________________________________________________________________
const char *TVChannelID::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVChannelID*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TVChannelID::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVChannelID*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TVChannelID::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVChannelID*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TVChannelID::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVChannelID*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TVHit::Class_Name()
{
   return "TVHit";
}

//______________________________________________________________________________
const char *TVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TVDigi::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TVDigi::Class_Name()
{
   return "TVDigi";
}

//______________________________________________________________________________
const char *TVDigi::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVDigi*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TVDigi::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVDigi*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TVDigi::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVDigi*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TVDigi::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVDigi*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr FADCVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *FADCVHit::Class_Name()
{
   return "FADCVHit";
}

//______________________________________________________________________________
const char *FADCVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::FADCVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int FADCVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::FADCVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *FADCVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::FADCVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *FADCVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::FADCVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDetectorVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDetectorVHit::Class_Name()
{
   return "TDetectorVHit";
}

//______________________________________________________________________________
const char *TDetectorVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDetectorVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDetectorVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDetectorVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TVEvent::Class_Name()
{
   return "TVEvent";
}

//______________________________________________________________________________
const char *TVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDetectorVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDetectorVEvent::Class_Name()
{
   return "TDetectorVEvent";
}

//______________________________________________________________________________
const char *TDetectorVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDetectorVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDetectorVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDetectorVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDetectorVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TVCandidate::Class_Name()
{
   return "TVCandidate";
}

//______________________________________________________________________________
const char *TVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDigiVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDigiVCandidate::Class_Name()
{
   return "TDigiVCandidate";
}

//______________________________________________________________________________
const char *TDigiVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDigiVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDigiVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDigiVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDigiVError::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDigiVError::Class_Name()
{
   return "TDigiVError";
}

//______________________________________________________________________________
const char *TDigiVError::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiVError*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDigiVError::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiVError*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDigiVError::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiVError*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDigiVError::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiVError*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDigiVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDigiVEvent::Class_Name()
{
   return "TDigiVEvent";
}

//______________________________________________________________________________
const char *TDigiVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDigiVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDigiVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDigiVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDigiVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDigiVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr FADCEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *FADCEvent::Class_Name()
{
   return "FADCEvent";
}

//______________________________________________________________________________
const char *FADCEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::FADCEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int FADCEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::FADCEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *FADCEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::FADCEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *FADCEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::FADCEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr MCInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *MCInfo::Class_Name()
{
   return "MCInfo";
}

//______________________________________________________________________________
const char *MCInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MCInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int MCInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::MCInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *MCInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MCInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *MCInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::MCInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr RecoInfo::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *RecoInfo::Class_Name()
{
   return "RecoInfo";
}

//______________________________________________________________________________
const char *RecoInfo::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::RecoInfo*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int RecoInfo::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::RecoInfo*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *RecoInfo::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::RecoInfo*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *RecoInfo::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::RecoInfo*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr Rndm::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *Rndm::Class_Name()
{
   return "Rndm";
}

//______________________________________________________________________________
const char *Rndm::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Rndm*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int Rndm::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Rndm*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *Rndm::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Rndm*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *Rndm::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Rndm*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr Stream::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *Stream::Class_Name()
{
   return "Stream";
}

//______________________________________________________________________________
const char *Stream::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Stream*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int Stream::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::Stream*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *Stream::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Stream*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *Stream::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::Stream*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDCError::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDCError::Class_Name()
{
   return "TDCError";
}

//______________________________________________________________________________
const char *TDCError::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDCError*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDCError::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDCError*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDCError::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDCError*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDCError::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDCError*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDCVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDCVHit::Class_Name()
{
   return "TDCVHit";
}

//______________________________________________________________________________
const char *TDCVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDCVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDCVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDCVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDCVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDCVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDCVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDCVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TDCEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TDCEvent::Class_Name()
{
   return "TDCEvent";
}

//______________________________________________________________________________
const char *TDCEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDCEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TDCEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TDCEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TDCEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDCEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TDCEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TDCEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSpecialTrigger::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSpecialTrigger::Class_Name()
{
   return "TSpecialTrigger";
}

//______________________________________________________________________________
const char *TSpecialTrigger::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTrigger*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSpecialTrigger::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTrigger*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSpecialTrigger::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTrigger*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSpecialTrigger::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTrigger*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr PrimRegister::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *PrimRegister::Class_Name()
{
   return "PrimRegister";
}

//______________________________________________________________________________
const char *PrimRegister::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::PrimRegister*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int PrimRegister::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::PrimRegister*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *PrimRegister::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::PrimRegister*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *PrimRegister::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::PrimRegister*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr PrimCounter::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *PrimCounter::Class_Name()
{
   return "PrimCounter";
}

//______________________________________________________________________________
const char *PrimCounter::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::PrimCounter*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int PrimCounter::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::PrimCounter*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *PrimCounter::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::PrimCounter*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *PrimCounter::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::PrimCounter*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TPrimSpecialTrigger::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TPrimSpecialTrigger::Class_Name()
{
   return "TPrimSpecialTrigger";
}

//______________________________________________________________________________
const char *TPrimSpecialTrigger::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TPrimSpecialTrigger*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TPrimSpecialTrigger::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TPrimSpecialTrigger*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TPrimSpecialTrigger::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TPrimSpecialTrigger*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TPrimSpecialTrigger::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TPrimSpecialTrigger*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TPrimitive::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TPrimitive::Class_Name()
{
   return "TPrimitive";
}

//______________________________________________________________________________
const char *TPrimitive::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TPrimitive*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TPrimitive::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TPrimitive*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TPrimitive::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TPrimitive*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TPrimitive::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TPrimitive*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoVHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoVHit::Class_Name()
{
   return "TRecoVHit";
}

//______________________________________________________________________________
const char *TRecoVHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoVHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoVHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoVHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoVHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoVHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoVHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoVHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoVCandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoVCandidate::Class_Name()
{
   return "TRecoVCandidate";
}

//______________________________________________________________________________
const char *TRecoVCandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoVCandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoVCandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoVCandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoVCandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoVCandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoVCandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoVCandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TRecoVEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TRecoVEvent::Class_Name()
{
   return "TRecoVEvent";
}

//______________________________________________________________________________
const char *TRecoVEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoVEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TRecoVEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TRecoVEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TRecoVEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoVEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TRecoVEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TRecoVEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSpecialTriggerEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSpecialTriggerEvent::Class_Name()
{
   return "TSpecialTriggerEvent";
}

//______________________________________________________________________________
const char *TSpecialTriggerEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTriggerEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSpecialTriggerEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTriggerEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSpecialTriggerEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTriggerEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSpecialTriggerEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSpecialTriggerEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TTDCBSpecialTrigger::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TTDCBSpecialTrigger::Class_Name()
{
   return "TTDCBSpecialTrigger";
}

//______________________________________________________________________________
const char *TTDCBSpecialTrigger::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TTDCBSpecialTrigger*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TTDCBSpecialTrigger::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TTDCBSpecialTrigger*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TTDCBSpecialTrigger::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TTDCBSpecialTrigger*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TTDCBSpecialTrigger::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TTDCBSpecialTrigger*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TTimeCluster::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TTimeCluster::Class_Name()
{
   return "TTimeCluster";
}

//______________________________________________________________________________
const char *TTimeCluster::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TTimeCluster*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TTimeCluster::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TTimeCluster*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TTimeCluster::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TTimeCluster*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TTimeCluster::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TTimeCluster*)0x0)->GetClass(); }
   return fgIsA;
}

namespace NA62Analysis {
   namespace Core {
//______________________________________________________________________________
void AnalyzerIdentifier::Streamer(TBuffer &R__b)
{
   // Stream an object of class NA62Analysis::Core::AnalyzerIdentifier.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(NA62Analysis::Core::AnalyzerIdentifier::Class(),this);
   } else {
      R__b.WriteClassBuffer(NA62Analysis::Core::AnalyzerIdentifier::Class(),this);
   }
}

} // namespace NA62Analysis::Core
} // namespace NA62Analysis::Core
namespace ROOT {
   // Wrappers around operator new
   static void *new_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p) {
      return  p ? new(p) ::NA62Analysis::Core::AnalyzerIdentifier : new ::NA62Analysis::Core::AnalyzerIdentifier;
   }
   static void *newArray_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(Long_t nElements, void *p) {
      return p ? new(p) ::NA62Analysis::Core::AnalyzerIdentifier[nElements] : new ::NA62Analysis::Core::AnalyzerIdentifier[nElements];
   }
   // Wrapper around operator delete
   static void delete_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p) {
      delete ((::NA62Analysis::Core::AnalyzerIdentifier*)p);
   }
   static void deleteArray_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p) {
      delete [] ((::NA62Analysis::Core::AnalyzerIdentifier*)p);
   }
   static void destruct_NA62AnalysiscLcLCorecLcLAnalyzerIdentifier(void *p) {
      typedef ::NA62Analysis::Core::AnalyzerIdentifier current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::NA62Analysis::Core::AnalyzerIdentifier

//______________________________________________________________________________
void AnalysisInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class AnalysisInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(AnalysisInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(AnalysisInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_AnalysisInfo(void *p) {
      return  p ? new(p) ::AnalysisInfo : new ::AnalysisInfo;
   }
   static void *newArray_AnalysisInfo(Long_t nElements, void *p) {
      return p ? new(p) ::AnalysisInfo[nElements] : new ::AnalysisInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_AnalysisInfo(void *p) {
      delete ((::AnalysisInfo*)p);
   }
   static void deleteArray_AnalysisInfo(void *p) {
      delete [] ((::AnalysisInfo*)p);
   }
   static void destruct_AnalysisInfo(void *p) {
      typedef ::AnalysisInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::AnalysisInfo

//______________________________________________________________________________
void BeamData::Streamer(TBuffer &R__b)
{
   // Stream an object of class BeamData.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(BeamData::Class(),this);
   } else {
      R__b.WriteClassBuffer(BeamData::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_BeamData(void *p) {
      return  p ? new(p) ::BeamData : new ::BeamData;
   }
   static void *newArray_BeamData(Long_t nElements, void *p) {
      return p ? new(p) ::BeamData[nElements] : new ::BeamData[nElements];
   }
   // Wrapper around operator delete
   static void delete_BeamData(void *p) {
      delete ((::BeamData*)p);
   }
   static void deleteArray_BeamData(void *p) {
      delete [] ((::BeamData*)p);
   }
   static void destruct_BeamData(void *p) {
      typedef ::BeamData current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::BeamData

//______________________________________________________________________________
void TargetInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class TargetInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TargetInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(TargetInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TargetInfo(void *p) {
      return  p ? new(p) ::TargetInfo : new ::TargetInfo;
   }
   static void *newArray_TargetInfo(Long_t nElements, void *p) {
      return p ? new(p) ::TargetInfo[nElements] : new ::TargetInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_TargetInfo(void *p) {
      delete ((::TargetInfo*)p);
   }
   static void deleteArray_TargetInfo(void *p) {
      delete [] ((::TargetInfo*)p);
   }
   static void destruct_TargetInfo(void *p) {
      typedef ::TargetInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TargetInfo

//______________________________________________________________________________
void MagnetInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class MagnetInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(MagnetInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(MagnetInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MagnetInfo(void *p) {
      return  p ? new(p) ::MagnetInfo : new ::MagnetInfo;
   }
   static void *newArray_MagnetInfo(Long_t nElements, void *p) {
      return p ? new(p) ::MagnetInfo[nElements] : new ::MagnetInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_MagnetInfo(void *p) {
      delete ((::MagnetInfo*)p);
   }
   static void deleteArray_MagnetInfo(void *p) {
      delete [] ((::MagnetInfo*)p);
   }
   static void destruct_MagnetInfo(void *p) {
      typedef ::MagnetInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MagnetInfo

//______________________________________________________________________________
void ScalerInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class ScalerInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(ScalerInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(ScalerInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_ScalerInfo(void *p) {
      return  p ? new(p) ::ScalerInfo : new ::ScalerInfo;
   }
   static void *newArray_ScalerInfo(Long_t nElements, void *p) {
      return p ? new(p) ::ScalerInfo[nElements] : new ::ScalerInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_ScalerInfo(void *p) {
      delete ((::ScalerInfo*)p);
   }
   static void deleteArray_ScalerInfo(void *p) {
      delete [] ((::ScalerInfo*)p);
   }
   static void destruct_ScalerInfo(void *p) {
      typedef ::ScalerInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::ScalerInfo

//______________________________________________________________________________
void PrimitiveInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class PrimitiveInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(PrimitiveInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(PrimitiveInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_PrimitiveInfo(void *p) {
      return  p ? new(p) ::PrimitiveInfo : new ::PrimitiveInfo;
   }
   static void *newArray_PrimitiveInfo(Long_t nElements, void *p) {
      return p ? new(p) ::PrimitiveInfo[nElements] : new ::PrimitiveInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_PrimitiveInfo(void *p) {
      delete ((::PrimitiveInfo*)p);
   }
   static void deleteArray_PrimitiveInfo(void *p) {
      delete [] ((::PrimitiveInfo*)p);
   }
   static void destruct_PrimitiveInfo(void *p) {
      typedef ::PrimitiveInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::PrimitiveInfo

//______________________________________________________________________________
void BeamSpecialTrigger::Streamer(TBuffer &R__b)
{
   // Stream an object of class BeamSpecialTrigger.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(BeamSpecialTrigger::Class(),this);
   } else {
      R__b.WriteClassBuffer(BeamSpecialTrigger::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_BeamSpecialTrigger(void *p) {
      return  p ? new(p) ::BeamSpecialTrigger : new ::BeamSpecialTrigger;
   }
   static void *newArray_BeamSpecialTrigger(Long_t nElements, void *p) {
      return p ? new(p) ::BeamSpecialTrigger[nElements] : new ::BeamSpecialTrigger[nElements];
   }
   // Wrapper around operator delete
   static void delete_BeamSpecialTrigger(void *p) {
      delete ((::BeamSpecialTrigger*)p);
   }
   static void deleteArray_BeamSpecialTrigger(void *p) {
      delete [] ((::BeamSpecialTrigger*)p);
   }
   static void destruct_BeamSpecialTrigger(void *p) {
      typedef ::BeamSpecialTrigger current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::BeamSpecialTrigger

//______________________________________________________________________________
void DetectorParameter::Streamer(TBuffer &R__b)
{
   // Stream an object of class DetectorParameter.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(DetectorParameter::Class(),this);
   } else {
      R__b.WriteClassBuffer(DetectorParameter::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_DetectorParameter(void *p) {
      return  p ? new(p) ::DetectorParameter : new ::DetectorParameter;
   }
   static void *newArray_DetectorParameter(Long_t nElements, void *p) {
      return p ? new(p) ::DetectorParameter[nElements] : new ::DetectorParameter[nElements];
   }
   // Wrapper around operator delete
   static void delete_DetectorParameter(void *p) {
      delete ((::DetectorParameter*)p);
   }
   static void deleteArray_DetectorParameter(void *p) {
      delete [] ((::DetectorParameter*)p);
   }
   static void destruct_DetectorParameter(void *p) {
      typedef ::DetectorParameter current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::DetectorParameter

//______________________________________________________________________________
void EventBoundary::Streamer(TBuffer &R__b)
{
   // Stream an object of class EventBoundary.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(EventBoundary::Class(),this);
   } else {
      R__b.WriteClassBuffer(EventBoundary::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_EventBoundary(void *p) {
      return  p ? new(p) ::EventBoundary : new ::EventBoundary;
   }
   static void *newArray_EventBoundary(Long_t nElements, void *p) {
      return p ? new(p) ::EventBoundary[nElements] : new ::EventBoundary[nElements];
   }
   // Wrapper around operator delete
   static void delete_EventBoundary(void *p) {
      delete ((::EventBoundary*)p);
   }
   static void deleteArray_EventBoundary(void *p) {
      delete [] ((::EventBoundary*)p);
   }
   static void destruct_EventBoundary(void *p) {
      typedef ::EventBoundary current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::EventBoundary

//______________________________________________________________________________
void GenePart::Streamer(TBuffer &R__b)
{
   // Stream an object of class GenePart.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(GenePart::Class(),this);
   } else {
      R__b.WriteClassBuffer(GenePart::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_GenePart(void *p) {
      return  p ? new(p) ::GenePart : new ::GenePart;
   }
   static void *newArray_GenePart(Long_t nElements, void *p) {
      return p ? new(p) ::GenePart[nElements] : new ::GenePart[nElements];
   }
   // Wrapper around operator delete
   static void delete_GenePart(void *p) {
      delete ((::GenePart*)p);
   }
   static void deleteArray_GenePart(void *p) {
      delete [] ((::GenePart*)p);
   }
   static void destruct_GenePart(void *p) {
      typedef ::GenePart current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::GenePart

//______________________________________________________________________________
void KinePart::Streamer(TBuffer &R__b)
{
   // Stream an object of class KinePart.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(KinePart::Class(),this);
   } else {
      R__b.WriteClassBuffer(KinePart::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_KinePart(void *p) {
      return  p ? new(p) ::KinePart : new ::KinePart;
   }
   static void *newArray_KinePart(Long_t nElements, void *p) {
      return p ? new(p) ::KinePart[nElements] : new ::KinePart[nElements];
   }
   // Wrapper around operator delete
   static void delete_KinePart(void *p) {
      delete ((::KinePart*)p);
   }
   static void deleteArray_KinePart(void *p) {
      delete [] ((::KinePart*)p);
   }
   static void destruct_KinePart(void *p) {
      typedef ::KinePart current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::KinePart

//______________________________________________________________________________
void TEventInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class TEventInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TEventInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(TEventInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TEventInfo(void *p) {
      return  p ? new(p) ::TEventInfo : new ::TEventInfo;
   }
   static void *newArray_TEventInfo(Long_t nElements, void *p) {
      return p ? new(p) ::TEventInfo[nElements] : new ::TEventInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_TEventInfo(void *p) {
      delete ((::TEventInfo*)p);
   }
   static void deleteArray_TEventInfo(void *p) {
      delete [] ((::TEventInfo*)p);
   }
   static void destruct_TEventInfo(void *p) {
      typedef ::TEventInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TEventInfo

//______________________________________________________________________________
void Event::Streamer(TBuffer &R__b)
{
   // Stream an object of class Event.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(Event::Class(),this);
   } else {
      R__b.WriteClassBuffer(Event::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_Event(void *p) {
      return  p ? new(p) ::Event : new ::Event;
   }
   static void *newArray_Event(Long_t nElements, void *p) {
      return p ? new(p) ::Event[nElements] : new ::Event[nElements];
   }
   // Wrapper around operator delete
   static void delete_Event(void *p) {
      delete ((::Event*)p);
   }
   static void deleteArray_Event(void *p) {
      delete [] ((::Event*)p);
   }
   static void destruct_Event(void *p) {
      typedef ::Event current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::Event

//______________________________________________________________________________
void HLTTrack::Streamer(TBuffer &R__b)
{
   // Stream an object of class HLTTrack.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(HLTTrack::Class(),this);
   } else {
      R__b.WriteClassBuffer(HLTTrack::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_HLTTrack(void *p) {
      return  p ? new(p) ::HLTTrack : new ::HLTTrack;
   }
   static void *newArray_HLTTrack(Long_t nElements, void *p) {
      return p ? new(p) ::HLTTrack[nElements] : new ::HLTTrack[nElements];
   }
   // Wrapper around operator delete
   static void delete_HLTTrack(void *p) {
      delete ((::HLTTrack*)p);
   }
   static void deleteArray_HLTTrack(void *p) {
      delete [] ((::HLTTrack*)p);
   }
   static void destruct_HLTTrack(void *p) {
      typedef ::HLTTrack current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::HLTTrack

//______________________________________________________________________________
void HLTEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class HLTEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(HLTEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(HLTEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_HLTEvent(void *p) {
      return  p ? new(p) ::HLTEvent : new ::HLTEvent;
   }
   static void *newArray_HLTEvent(Long_t nElements, void *p) {
      return p ? new(p) ::HLTEvent[nElements] : new ::HLTEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_HLTEvent(void *p) {
      delete ((::HLTEvent*)p);
   }
   static void deleteArray_HLTEvent(void *p) {
      delete [] ((::HLTEvent*)p);
   }
   static void destruct_HLTEvent(void *p) {
      typedef ::HLTEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::HLTEvent

//______________________________________________________________________________
void L0TPData::Streamer(TBuffer &R__b)
{
   // Stream an object of class L0TPData.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L0TPData::Class(),this);
   } else {
      R__b.WriteClassBuffer(L0TPData::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L0TPData(void *p) {
      return  p ? new(p) ::L0TPData : new ::L0TPData;
   }
   static void *newArray_L0TPData(Long_t nElements, void *p) {
      return p ? new(p) ::L0TPData[nElements] : new ::L0TPData[nElements];
   }
   // Wrapper around operator delete
   static void delete_L0TPData(void *p) {
      delete ((::L0TPData*)p);
   }
   static void deleteArray_L0TPData(void *p) {
      delete [] ((::L0TPData*)p);
   }
   static void destruct_L0TPData(void *p) {
      typedef ::L0TPData current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L0TPData

//______________________________________________________________________________
void L0Primitive::Streamer(TBuffer &R__b)
{
   // Stream an object of class L0Primitive.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L0Primitive::Class(),this);
   } else {
      R__b.WriteClassBuffer(L0Primitive::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L0Primitive(void *p) {
      return  p ? new(p) ::L0Primitive : new ::L0Primitive;
   }
   static void *newArray_L0Primitive(Long_t nElements, void *p) {
      return p ? new(p) ::L0Primitive[nElements] : new ::L0Primitive[nElements];
   }
   // Wrapper around operator delete
   static void delete_L0Primitive(void *p) {
      delete ((::L0Primitive*)p);
   }
   static void deleteArray_L0Primitive(void *p) {
      delete [] ((::L0Primitive*)p);
   }
   static void destruct_L0Primitive(void *p) {
      typedef ::L0Primitive current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L0Primitive

//______________________________________________________________________________
void L0TPSpecialTrigger::Streamer(TBuffer &R__b)
{
   // Stream an object of class L0TPSpecialTrigger.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L0TPSpecialTrigger::Class(),this);
   } else {
      R__b.WriteClassBuffer(L0TPSpecialTrigger::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L0TPSpecialTrigger(void *p) {
      return  p ? new(p) ::L0TPSpecialTrigger : new ::L0TPSpecialTrigger;
   }
   static void *newArray_L0TPSpecialTrigger(Long_t nElements, void *p) {
      return p ? new(p) ::L0TPSpecialTrigger[nElements] : new ::L0TPSpecialTrigger[nElements];
   }
   // Wrapper around operator delete
   static void delete_L0TPSpecialTrigger(void *p) {
      delete ((::L0TPSpecialTrigger*)p);
   }
   static void deleteArray_L0TPSpecialTrigger(void *p) {
      delete [] ((::L0TPSpecialTrigger*)p);
   }
   static void destruct_L0TPSpecialTrigger(void *p) {
      typedef ::L0TPSpecialTrigger current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L0TPSpecialTrigger

//______________________________________________________________________________
void L0Mask::Streamer(TBuffer &R__b)
{
   // Stream an object of class L0Mask.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L0Mask::Class(),this);
   } else {
      R__b.WriteClassBuffer(L0Mask::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L0Mask(void *p) {
      return  p ? new(p) ::L0Mask : new ::L0Mask;
   }
   static void *newArray_L0Mask(Long_t nElements, void *p) {
      return p ? new(p) ::L0Mask[nElements] : new ::L0Mask[nElements];
   }
   // Wrapper around operator delete
   static void delete_L0Mask(void *p) {
      delete ((::L0Mask*)p);
   }
   static void deleteArray_L0Mask(void *p) {
      delete [] ((::L0Mask*)p);
   }
   static void destruct_L0Mask(void *p) {
      typedef ::L0Mask current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L0Mask

//______________________________________________________________________________
void L1TPData::Streamer(TBuffer &R__b)
{
   // Stream an object of class L1TPData.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L1TPData::Class(),this);
   } else {
      R__b.WriteClassBuffer(L1TPData::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L1TPData(void *p) {
      return  p ? new(p) ::L1TPData : new ::L1TPData;
   }
   static void *newArray_L1TPData(Long_t nElements, void *p) {
      return p ? new(p) ::L1TPData[nElements] : new ::L1TPData[nElements];
   }
   // Wrapper around operator delete
   static void delete_L1TPData(void *p) {
      delete ((::L1TPData*)p);
   }
   static void deleteArray_L1TPData(void *p) {
      delete [] ((::L1TPData*)p);
   }
   static void destruct_L1TPData(void *p) {
      typedef ::L1TPData current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L1TPData

//______________________________________________________________________________
void L1MaskBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L1MaskBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L1MaskBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L1MaskBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L1MaskBlock(void *p) {
      return  p ? new(p) ::L1MaskBlock : new ::L1MaskBlock;
   }
   static void *newArray_L1MaskBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L1MaskBlock[nElements] : new ::L1MaskBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L1MaskBlock(void *p) {
      delete ((::L1MaskBlock*)p);
   }
   static void deleteArray_L1MaskBlock(void *p) {
      delete [] ((::L1MaskBlock*)p);
   }
   static void destruct_L1MaskBlock(void *p) {
      typedef ::L1MaskBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L1MaskBlock

//______________________________________________________________________________
void L1AlgoBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L1AlgoBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L1AlgoBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L1AlgoBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L1AlgoBlock(void *p) {
      return  p ? new(p) ::L1AlgoBlock : new ::L1AlgoBlock;
   }
   static void *newArray_L1AlgoBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L1AlgoBlock[nElements] : new ::L1AlgoBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L1AlgoBlock(void *p) {
      delete ((::L1AlgoBlock*)p);
   }
   static void deleteArray_L1AlgoBlock(void *p) {
      delete [] ((::L1AlgoBlock*)p);
   }
   static void destruct_L1AlgoBlock(void *p) {
      typedef ::L1AlgoBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L1AlgoBlock

//______________________________________________________________________________
void L1TPSpecialTrigger::Streamer(TBuffer &R__b)
{
   // Stream an object of class L1TPSpecialTrigger.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L1TPSpecialTrigger::Class(),this);
   } else {
      R__b.WriteClassBuffer(L1TPSpecialTrigger::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L1TPSpecialTrigger(void *p) {
      return  p ? new(p) ::L1TPSpecialTrigger : new ::L1TPSpecialTrigger;
   }
   static void *newArray_L1TPSpecialTrigger(Long_t nElements, void *p) {
      return p ? new(p) ::L1TPSpecialTrigger[nElements] : new ::L1TPSpecialTrigger[nElements];
   }
   // Wrapper around operator delete
   static void delete_L1TPSpecialTrigger(void *p) {
      delete ((::L1TPSpecialTrigger*)p);
   }
   static void deleteArray_L1TPSpecialTrigger(void *p) {
      delete [] ((::L1TPSpecialTrigger*)p);
   }
   static void destruct_L1TPSpecialTrigger(void *p) {
      typedef ::L1TPSpecialTrigger current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L1TPSpecialTrigger

//______________________________________________________________________________
void L1PCSpecialBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L1PCSpecialBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L1PCSpecialBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L1PCSpecialBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L1PCSpecialBlock(void *p) {
      return  p ? new(p) ::L1PCSpecialBlock : new ::L1PCSpecialBlock;
   }
   static void *newArray_L1PCSpecialBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L1PCSpecialBlock[nElements] : new ::L1PCSpecialBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L1PCSpecialBlock(void *p) {
      delete ((::L1PCSpecialBlock*)p);
   }
   static void deleteArray_L1PCSpecialBlock(void *p) {
      delete [] ((::L1PCSpecialBlock*)p);
   }
   static void destruct_L1PCSpecialBlock(void *p) {
      typedef ::L1PCSpecialBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L1PCSpecialBlock

//______________________________________________________________________________
void L1MaskSpecialBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L1MaskSpecialBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L1MaskSpecialBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L1MaskSpecialBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L1MaskSpecialBlock(void *p) {
      return  p ? new(p) ::L1MaskSpecialBlock : new ::L1MaskSpecialBlock;
   }
   static void *newArray_L1MaskSpecialBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L1MaskSpecialBlock[nElements] : new ::L1MaskSpecialBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L1MaskSpecialBlock(void *p) {
      delete ((::L1MaskSpecialBlock*)p);
   }
   static void deleteArray_L1MaskSpecialBlock(void *p) {
      delete [] ((::L1MaskSpecialBlock*)p);
   }
   static void destruct_L1MaskSpecialBlock(void *p) {
      typedef ::L1MaskSpecialBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L1MaskSpecialBlock

//______________________________________________________________________________
void L2EBData::Streamer(TBuffer &R__b)
{
   // Stream an object of class L2EBData.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L2EBData::Class(),this);
   } else {
      R__b.WriteClassBuffer(L2EBData::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L2EBData(void *p) {
      return  p ? new(p) ::L2EBData : new ::L2EBData;
   }
   static void *newArray_L2EBData(Long_t nElements, void *p) {
      return p ? new(p) ::L2EBData[nElements] : new ::L2EBData[nElements];
   }
   // Wrapper around operator delete
   static void delete_L2EBData(void *p) {
      delete ((::L2EBData*)p);
   }
   static void deleteArray_L2EBData(void *p) {
      delete [] ((::L2EBData*)p);
   }
   static void destruct_L2EBData(void *p) {
      typedef ::L2EBData current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L2EBData

//______________________________________________________________________________
void L2MaskBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L2MaskBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L2MaskBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L2MaskBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L2MaskBlock(void *p) {
      return  p ? new(p) ::L2MaskBlock : new ::L2MaskBlock;
   }
   static void *newArray_L2MaskBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L2MaskBlock[nElements] : new ::L2MaskBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L2MaskBlock(void *p) {
      delete ((::L2MaskBlock*)p);
   }
   static void deleteArray_L2MaskBlock(void *p) {
      delete [] ((::L2MaskBlock*)p);
   }
   static void destruct_L2MaskBlock(void *p) {
      typedef ::L2MaskBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L2MaskBlock

//______________________________________________________________________________
void L2AlgoBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L2AlgoBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L2AlgoBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L2AlgoBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L2AlgoBlock(void *p) {
      return  p ? new(p) ::L2AlgoBlock : new ::L2AlgoBlock;
   }
   static void *newArray_L2AlgoBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L2AlgoBlock[nElements] : new ::L2AlgoBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L2AlgoBlock(void *p) {
      delete ((::L2AlgoBlock*)p);
   }
   static void deleteArray_L2AlgoBlock(void *p) {
      delete [] ((::L2AlgoBlock*)p);
   }
   static void destruct_L2AlgoBlock(void *p) {
      typedef ::L2AlgoBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L2AlgoBlock

//______________________________________________________________________________
void L2EBSpecialTrigger::Streamer(TBuffer &R__b)
{
   // Stream an object of class L2EBSpecialTrigger.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L2EBSpecialTrigger::Class(),this);
   } else {
      R__b.WriteClassBuffer(L2EBSpecialTrigger::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L2EBSpecialTrigger(void *p) {
      return  p ? new(p) ::L2EBSpecialTrigger : new ::L2EBSpecialTrigger;
   }
   static void *newArray_L2EBSpecialTrigger(Long_t nElements, void *p) {
      return p ? new(p) ::L2EBSpecialTrigger[nElements] : new ::L2EBSpecialTrigger[nElements];
   }
   // Wrapper around operator delete
   static void delete_L2EBSpecialTrigger(void *p) {
      delete ((::L2EBSpecialTrigger*)p);
   }
   static void deleteArray_L2EBSpecialTrigger(void *p) {
      delete [] ((::L2EBSpecialTrigger*)p);
   }
   static void destruct_L2EBSpecialTrigger(void *p) {
      typedef ::L2EBSpecialTrigger current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L2EBSpecialTrigger

//______________________________________________________________________________
void L2PCSpecialBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L2PCSpecialBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L2PCSpecialBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L2PCSpecialBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L2PCSpecialBlock(void *p) {
      return  p ? new(p) ::L2PCSpecialBlock : new ::L2PCSpecialBlock;
   }
   static void *newArray_L2PCSpecialBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L2PCSpecialBlock[nElements] : new ::L2PCSpecialBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L2PCSpecialBlock(void *p) {
      delete ((::L2PCSpecialBlock*)p);
   }
   static void deleteArray_L2PCSpecialBlock(void *p) {
      delete [] ((::L2PCSpecialBlock*)p);
   }
   static void destruct_L2PCSpecialBlock(void *p) {
      typedef ::L2PCSpecialBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L2PCSpecialBlock

//______________________________________________________________________________
void L2MaskSpecialBlock::Streamer(TBuffer &R__b)
{
   // Stream an object of class L2MaskSpecialBlock.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(L2MaskSpecialBlock::Class(),this);
   } else {
      R__b.WriteClassBuffer(L2MaskSpecialBlock::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_L2MaskSpecialBlock(void *p) {
      return  p ? new(p) ::L2MaskSpecialBlock : new ::L2MaskSpecialBlock;
   }
   static void *newArray_L2MaskSpecialBlock(Long_t nElements, void *p) {
      return p ? new(p) ::L2MaskSpecialBlock[nElements] : new ::L2MaskSpecialBlock[nElements];
   }
   // Wrapper around operator delete
   static void delete_L2MaskSpecialBlock(void *p) {
      delete ((::L2MaskSpecialBlock*)p);
   }
   static void deleteArray_L2MaskSpecialBlock(void *p) {
      delete [] ((::L2MaskSpecialBlock*)p);
   }
   static void destruct_L2MaskSpecialBlock(void *p) {
      typedef ::L2MaskSpecialBlock current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::L2MaskSpecialBlock

//______________________________________________________________________________
void EventHeader::Streamer(TBuffer &R__b)
{
   // Stream an object of class EventHeader.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(EventHeader::Class(),this);
   } else {
      R__b.WriteClassBuffer(EventHeader::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_EventHeader(void *p) {
      return  p ? new(p) ::EventHeader : new ::EventHeader;
   }
   static void *newArray_EventHeader(Long_t nElements, void *p) {
      return p ? new(p) ::EventHeader[nElements] : new ::EventHeader[nElements];
   }
   // Wrapper around operator delete
   static void delete_EventHeader(void *p) {
      delete ((::EventHeader*)p);
   }
   static void deleteArray_EventHeader(void *p) {
      delete [] ((::EventHeader*)p);
   }
   static void destruct_EventHeader(void *p) {
      typedef ::EventHeader current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::EventHeader

//______________________________________________________________________________
void TVChannelID::Streamer(TBuffer &R__b)
{
   // Stream an object of class TVChannelID.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TVChannelID::Class(),this);
   } else {
      R__b.WriteClassBuffer(TVChannelID::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TVChannelID(void *p) {
      return  p ? new(p) ::TVChannelID : new ::TVChannelID;
   }
   static void *newArray_TVChannelID(Long_t nElements, void *p) {
      return p ? new(p) ::TVChannelID[nElements] : new ::TVChannelID[nElements];
   }
   // Wrapper around operator delete
   static void delete_TVChannelID(void *p) {
      delete ((::TVChannelID*)p);
   }
   static void deleteArray_TVChannelID(void *p) {
      delete [] ((::TVChannelID*)p);
   }
   static void destruct_TVChannelID(void *p) {
      typedef ::TVChannelID current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TVChannelID

//______________________________________________________________________________
void TVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TVHit(void *p) {
      delete ((::TVHit*)p);
   }
   static void deleteArray_TVHit(void *p) {
      delete [] ((::TVHit*)p);
   }
   static void destruct_TVHit(void *p) {
      typedef ::TVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TVHit

//______________________________________________________________________________
void TVDigi::Streamer(TBuffer &R__b)
{
   // Stream an object of class TVDigi.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TVDigi::Class(),this);
   } else {
      R__b.WriteClassBuffer(TVDigi::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TVDigi(void *p) {
      delete ((::TVDigi*)p);
   }
   static void deleteArray_TVDigi(void *p) {
      delete [] ((::TVDigi*)p);
   }
   static void destruct_TVDigi(void *p) {
      typedef ::TVDigi current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TVDigi

//______________________________________________________________________________
void FADCVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class FADCVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(FADCVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(FADCVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_FADCVHit(void *p) {
      delete ((::FADCVHit*)p);
   }
   static void deleteArray_FADCVHit(void *p) {
      delete [] ((::FADCVHit*)p);
   }
   static void destruct_FADCVHit(void *p) {
      typedef ::FADCVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::FADCVHit

//______________________________________________________________________________
void TDetectorVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDetectorVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDetectorVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDetectorVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDetectorVHit(void *p) {
      return  p ? new(p) ::TDetectorVHit : new ::TDetectorVHit;
   }
   static void *newArray_TDetectorVHit(Long_t nElements, void *p) {
      return p ? new(p) ::TDetectorVHit[nElements] : new ::TDetectorVHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDetectorVHit(void *p) {
      delete ((::TDetectorVHit*)p);
   }
   static void deleteArray_TDetectorVHit(void *p) {
      delete [] ((::TDetectorVHit*)p);
   }
   static void destruct_TDetectorVHit(void *p) {
      typedef ::TDetectorVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDetectorVHit

//______________________________________________________________________________
void TVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TVEvent(void *p) {
      return  p ? new(p) ::TVEvent : new ::TVEvent;
   }
   static void *newArray_TVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TVEvent[nElements] : new ::TVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TVEvent(void *p) {
      delete ((::TVEvent*)p);
   }
   static void deleteArray_TVEvent(void *p) {
      delete [] ((::TVEvent*)p);
   }
   static void destruct_TVEvent(void *p) {
      typedef ::TVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TVEvent

//______________________________________________________________________________
void TDetectorVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDetectorVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDetectorVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDetectorVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDetectorVEvent(void *p) {
      return  p ? new(p) ::TDetectorVEvent : new ::TDetectorVEvent;
   }
   static void *newArray_TDetectorVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TDetectorVEvent[nElements] : new ::TDetectorVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDetectorVEvent(void *p) {
      delete ((::TDetectorVEvent*)p);
   }
   static void deleteArray_TDetectorVEvent(void *p) {
      delete [] ((::TDetectorVEvent*)p);
   }
   static void destruct_TDetectorVEvent(void *p) {
      typedef ::TDetectorVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDetectorVEvent

//______________________________________________________________________________
void TVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TVCandidate(void *p) {
      return  p ? new(p) ::TVCandidate : new ::TVCandidate;
   }
   static void *newArray_TVCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TVCandidate[nElements] : new ::TVCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TVCandidate(void *p) {
      delete ((::TVCandidate*)p);
   }
   static void deleteArray_TVCandidate(void *p) {
      delete [] ((::TVCandidate*)p);
   }
   static void destruct_TVCandidate(void *p) {
      typedef ::TVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TVCandidate

//______________________________________________________________________________
void TDigiVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDigiVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDigiVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDigiVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDigiVCandidate(void *p) {
      return  p ? new(p) ::TDigiVCandidate : new ::TDigiVCandidate;
   }
   static void *newArray_TDigiVCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TDigiVCandidate[nElements] : new ::TDigiVCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDigiVCandidate(void *p) {
      delete ((::TDigiVCandidate*)p);
   }
   static void deleteArray_TDigiVCandidate(void *p) {
      delete [] ((::TDigiVCandidate*)p);
   }
   static void destruct_TDigiVCandidate(void *p) {
      typedef ::TDigiVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDigiVCandidate

//______________________________________________________________________________
void TDigiVError::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDigiVError.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDigiVError::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDigiVError::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDigiVError(void *p) {
      return  p ? new(p) ::TDigiVError : new ::TDigiVError;
   }
   static void *newArray_TDigiVError(Long_t nElements, void *p) {
      return p ? new(p) ::TDigiVError[nElements] : new ::TDigiVError[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDigiVError(void *p) {
      delete ((::TDigiVError*)p);
   }
   static void deleteArray_TDigiVError(void *p) {
      delete [] ((::TDigiVError*)p);
   }
   static void destruct_TDigiVError(void *p) {
      typedef ::TDigiVError current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDigiVError

//______________________________________________________________________________
void TDigiVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDigiVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDigiVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDigiVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDigiVEvent(void *p) {
      return  p ? new(p) ::TDigiVEvent : new ::TDigiVEvent;
   }
   static void *newArray_TDigiVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TDigiVEvent[nElements] : new ::TDigiVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDigiVEvent(void *p) {
      delete ((::TDigiVEvent*)p);
   }
   static void deleteArray_TDigiVEvent(void *p) {
      delete [] ((::TDigiVEvent*)p);
   }
   static void destruct_TDigiVEvent(void *p) {
      typedef ::TDigiVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDigiVEvent

//______________________________________________________________________________
void FADCEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class FADCEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(FADCEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(FADCEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_FADCEvent(void *p) {
      return  p ? new(p) ::FADCEvent : new ::FADCEvent;
   }
   static void *newArray_FADCEvent(Long_t nElements, void *p) {
      return p ? new(p) ::FADCEvent[nElements] : new ::FADCEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_FADCEvent(void *p) {
      delete ((::FADCEvent*)p);
   }
   static void deleteArray_FADCEvent(void *p) {
      delete [] ((::FADCEvent*)p);
   }
   static void destruct_FADCEvent(void *p) {
      typedef ::FADCEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::FADCEvent

//______________________________________________________________________________
void MCInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class MCInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(MCInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(MCInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_MCInfo(void *p) {
      return  p ? new(p) ::MCInfo : new ::MCInfo;
   }
   static void *newArray_MCInfo(Long_t nElements, void *p) {
      return p ? new(p) ::MCInfo[nElements] : new ::MCInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_MCInfo(void *p) {
      delete ((::MCInfo*)p);
   }
   static void deleteArray_MCInfo(void *p) {
      delete [] ((::MCInfo*)p);
   }
   static void destruct_MCInfo(void *p) {
      typedef ::MCInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::MCInfo

//______________________________________________________________________________
void RecoInfo::Streamer(TBuffer &R__b)
{
   // Stream an object of class RecoInfo.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(RecoInfo::Class(),this);
   } else {
      R__b.WriteClassBuffer(RecoInfo::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_RecoInfo(void *p) {
      return  p ? new(p) ::RecoInfo : new ::RecoInfo;
   }
   static void *newArray_RecoInfo(Long_t nElements, void *p) {
      return p ? new(p) ::RecoInfo[nElements] : new ::RecoInfo[nElements];
   }
   // Wrapper around operator delete
   static void delete_RecoInfo(void *p) {
      delete ((::RecoInfo*)p);
   }
   static void deleteArray_RecoInfo(void *p) {
      delete [] ((::RecoInfo*)p);
   }
   static void destruct_RecoInfo(void *p) {
      typedef ::RecoInfo current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::RecoInfo

//______________________________________________________________________________
void Rndm::Streamer(TBuffer &R__b)
{
   // Stream an object of class Rndm.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(Rndm::Class(),this);
   } else {
      R__b.WriteClassBuffer(Rndm::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_Rndm(void *p) {
      return  p ? new(p) ::Rndm : new ::Rndm;
   }
   static void *newArray_Rndm(Long_t nElements, void *p) {
      return p ? new(p) ::Rndm[nElements] : new ::Rndm[nElements];
   }
   // Wrapper around operator delete
   static void delete_Rndm(void *p) {
      delete ((::Rndm*)p);
   }
   static void deleteArray_Rndm(void *p) {
      delete [] ((::Rndm*)p);
   }
   static void destruct_Rndm(void *p) {
      typedef ::Rndm current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::Rndm

//______________________________________________________________________________
void Stream::Streamer(TBuffer &R__b)
{
   // Stream an object of class Stream.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(Stream::Class(),this);
   } else {
      R__b.WriteClassBuffer(Stream::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_Stream(void *p) {
      return  p ? new(p) ::Stream : new ::Stream;
   }
   static void *newArray_Stream(Long_t nElements, void *p) {
      return p ? new(p) ::Stream[nElements] : new ::Stream[nElements];
   }
   // Wrapper around operator delete
   static void delete_Stream(void *p) {
      delete ((::Stream*)p);
   }
   static void deleteArray_Stream(void *p) {
      delete [] ((::Stream*)p);
   }
   static void destruct_Stream(void *p) {
      typedef ::Stream current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::Stream

//______________________________________________________________________________
void TDCError::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDCError.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDCError::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDCError::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDCError(void *p) {
      return  p ? new(p) ::TDCError : new ::TDCError;
   }
   static void *newArray_TDCError(Long_t nElements, void *p) {
      return p ? new(p) ::TDCError[nElements] : new ::TDCError[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDCError(void *p) {
      delete ((::TDCError*)p);
   }
   static void deleteArray_TDCError(void *p) {
      delete [] ((::TDCError*)p);
   }
   static void destruct_TDCError(void *p) {
      typedef ::TDCError current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDCError

//______________________________________________________________________________
void TDCVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDCVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDCVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDCVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrapper around operator delete
   static void delete_TDCVHit(void *p) {
      delete ((::TDCVHit*)p);
   }
   static void deleteArray_TDCVHit(void *p) {
      delete [] ((::TDCVHit*)p);
   }
   static void destruct_TDCVHit(void *p) {
      typedef ::TDCVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDCVHit

//______________________________________________________________________________
void TDCEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TDCEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TDCEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TDCEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TDCEvent(void *p) {
      return  p ? new(p) ::TDCEvent : new ::TDCEvent;
   }
   static void *newArray_TDCEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TDCEvent[nElements] : new ::TDCEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TDCEvent(void *p) {
      delete ((::TDCEvent*)p);
   }
   static void deleteArray_TDCEvent(void *p) {
      delete [] ((::TDCEvent*)p);
   }
   static void destruct_TDCEvent(void *p) {
      typedef ::TDCEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TDCEvent

//______________________________________________________________________________
void TSpecialTrigger::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSpecialTrigger.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSpecialTrigger::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSpecialTrigger::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSpecialTrigger(void *p) {
      return  p ? new(p) ::TSpecialTrigger : new ::TSpecialTrigger;
   }
   static void *newArray_TSpecialTrigger(Long_t nElements, void *p) {
      return p ? new(p) ::TSpecialTrigger[nElements] : new ::TSpecialTrigger[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSpecialTrigger(void *p) {
      delete ((::TSpecialTrigger*)p);
   }
   static void deleteArray_TSpecialTrigger(void *p) {
      delete [] ((::TSpecialTrigger*)p);
   }
   static void destruct_TSpecialTrigger(void *p) {
      typedef ::TSpecialTrigger current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSpecialTrigger

//______________________________________________________________________________
void PrimRegister::Streamer(TBuffer &R__b)
{
   // Stream an object of class PrimRegister.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(PrimRegister::Class(),this);
   } else {
      R__b.WriteClassBuffer(PrimRegister::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_PrimRegister(void *p) {
      return  p ? new(p) ::PrimRegister : new ::PrimRegister;
   }
   static void *newArray_PrimRegister(Long_t nElements, void *p) {
      return p ? new(p) ::PrimRegister[nElements] : new ::PrimRegister[nElements];
   }
   // Wrapper around operator delete
   static void delete_PrimRegister(void *p) {
      delete ((::PrimRegister*)p);
   }
   static void deleteArray_PrimRegister(void *p) {
      delete [] ((::PrimRegister*)p);
   }
   static void destruct_PrimRegister(void *p) {
      typedef ::PrimRegister current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::PrimRegister

//______________________________________________________________________________
void PrimCounter::Streamer(TBuffer &R__b)
{
   // Stream an object of class PrimCounter.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(PrimCounter::Class(),this);
   } else {
      R__b.WriteClassBuffer(PrimCounter::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_PrimCounter(void *p) {
      return  p ? new(p) ::PrimCounter : new ::PrimCounter;
   }
   static void *newArray_PrimCounter(Long_t nElements, void *p) {
      return p ? new(p) ::PrimCounter[nElements] : new ::PrimCounter[nElements];
   }
   // Wrapper around operator delete
   static void delete_PrimCounter(void *p) {
      delete ((::PrimCounter*)p);
   }
   static void deleteArray_PrimCounter(void *p) {
      delete [] ((::PrimCounter*)p);
   }
   static void destruct_PrimCounter(void *p) {
      typedef ::PrimCounter current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::PrimCounter

//______________________________________________________________________________
void TPrimSpecialTrigger::Streamer(TBuffer &R__b)
{
   // Stream an object of class TPrimSpecialTrigger.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TPrimSpecialTrigger::Class(),this);
   } else {
      R__b.WriteClassBuffer(TPrimSpecialTrigger::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TPrimSpecialTrigger(void *p) {
      return  p ? new(p) ::TPrimSpecialTrigger : new ::TPrimSpecialTrigger;
   }
   static void *newArray_TPrimSpecialTrigger(Long_t nElements, void *p) {
      return p ? new(p) ::TPrimSpecialTrigger[nElements] : new ::TPrimSpecialTrigger[nElements];
   }
   // Wrapper around operator delete
   static void delete_TPrimSpecialTrigger(void *p) {
      delete ((::TPrimSpecialTrigger*)p);
   }
   static void deleteArray_TPrimSpecialTrigger(void *p) {
      delete [] ((::TPrimSpecialTrigger*)p);
   }
   static void destruct_TPrimSpecialTrigger(void *p) {
      typedef ::TPrimSpecialTrigger current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TPrimSpecialTrigger

//______________________________________________________________________________
void TPrimitive::Streamer(TBuffer &R__b)
{
   // Stream an object of class TPrimitive.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TPrimitive::Class(),this);
   } else {
      R__b.WriteClassBuffer(TPrimitive::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TPrimitive(void *p) {
      return  p ? new(p) ::TPrimitive : new ::TPrimitive;
   }
   static void *newArray_TPrimitive(Long_t nElements, void *p) {
      return p ? new(p) ::TPrimitive[nElements] : new ::TPrimitive[nElements];
   }
   // Wrapper around operator delete
   static void delete_TPrimitive(void *p) {
      delete ((::TPrimitive*)p);
   }
   static void deleteArray_TPrimitive(void *p) {
      delete [] ((::TPrimitive*)p);
   }
   static void destruct_TPrimitive(void *p) {
      typedef ::TPrimitive current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TPrimitive

//______________________________________________________________________________
void TRecoVHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoVHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoVHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoVHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoVHit(void *p) {
      return  p ? new(p) ::TRecoVHit : new ::TRecoVHit;
   }
   static void *newArray_TRecoVHit(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoVHit[nElements] : new ::TRecoVHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoVHit(void *p) {
      delete ((::TRecoVHit*)p);
   }
   static void deleteArray_TRecoVHit(void *p) {
      delete [] ((::TRecoVHit*)p);
   }
   static void destruct_TRecoVHit(void *p) {
      typedef ::TRecoVHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoVHit

//______________________________________________________________________________
void TRecoVCandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoVCandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoVCandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoVCandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoVCandidate(void *p) {
      return  p ? new(p) ::TRecoVCandidate : new ::TRecoVCandidate;
   }
   static void *newArray_TRecoVCandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoVCandidate[nElements] : new ::TRecoVCandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoVCandidate(void *p) {
      delete ((::TRecoVCandidate*)p);
   }
   static void deleteArray_TRecoVCandidate(void *p) {
      delete [] ((::TRecoVCandidate*)p);
   }
   static void destruct_TRecoVCandidate(void *p) {
      typedef ::TRecoVCandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoVCandidate

//______________________________________________________________________________
void TRecoVEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TRecoVEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TRecoVEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TRecoVEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TRecoVEvent(void *p) {
      return  p ? new(p) ::TRecoVEvent : new ::TRecoVEvent;
   }
   static void *newArray_TRecoVEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TRecoVEvent[nElements] : new ::TRecoVEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TRecoVEvent(void *p) {
      delete ((::TRecoVEvent*)p);
   }
   static void deleteArray_TRecoVEvent(void *p) {
      delete [] ((::TRecoVEvent*)p);
   }
   static void destruct_TRecoVEvent(void *p) {
      typedef ::TRecoVEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TRecoVEvent

//______________________________________________________________________________
void TSpecialTriggerEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSpecialTriggerEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSpecialTriggerEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSpecialTriggerEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSpecialTriggerEvent(void *p) {
      return  p ? new(p) ::TSpecialTriggerEvent : new ::TSpecialTriggerEvent;
   }
   static void *newArray_TSpecialTriggerEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSpecialTriggerEvent[nElements] : new ::TSpecialTriggerEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSpecialTriggerEvent(void *p) {
      delete ((::TSpecialTriggerEvent*)p);
   }
   static void deleteArray_TSpecialTriggerEvent(void *p) {
      delete [] ((::TSpecialTriggerEvent*)p);
   }
   static void destruct_TSpecialTriggerEvent(void *p) {
      typedef ::TSpecialTriggerEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSpecialTriggerEvent

//______________________________________________________________________________
void TTDCBSpecialTrigger::Streamer(TBuffer &R__b)
{
   // Stream an object of class TTDCBSpecialTrigger.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TTDCBSpecialTrigger::Class(),this);
   } else {
      R__b.WriteClassBuffer(TTDCBSpecialTrigger::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TTDCBSpecialTrigger(void *p) {
      return  p ? new(p) ::TTDCBSpecialTrigger : new ::TTDCBSpecialTrigger;
   }
   static void *newArray_TTDCBSpecialTrigger(Long_t nElements, void *p) {
      return p ? new(p) ::TTDCBSpecialTrigger[nElements] : new ::TTDCBSpecialTrigger[nElements];
   }
   // Wrapper around operator delete
   static void delete_TTDCBSpecialTrigger(void *p) {
      delete ((::TTDCBSpecialTrigger*)p);
   }
   static void deleteArray_TTDCBSpecialTrigger(void *p) {
      delete [] ((::TTDCBSpecialTrigger*)p);
   }
   static void destruct_TTDCBSpecialTrigger(void *p) {
      typedef ::TTDCBSpecialTrigger current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TTDCBSpecialTrigger

//______________________________________________________________________________
void TTimeCluster::Streamer(TBuffer &R__b)
{
   // Stream an object of class TTimeCluster.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TTimeCluster::Class(),this);
   } else {
      R__b.WriteClassBuffer(TTimeCluster::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TTimeCluster(void *p) {
      return  p ? new(p) ::TTimeCluster : new ::TTimeCluster;
   }
   static void *newArray_TTimeCluster(Long_t nElements, void *p) {
      return p ? new(p) ::TTimeCluster[nElements] : new ::TTimeCluster[nElements];
   }
   // Wrapper around operator delete
   static void delete_TTimeCluster(void *p) {
      delete ((::TTimeCluster*)p);
   }
   static void deleteArray_TTimeCluster(void *p) {
      delete [] ((::TTimeCluster*)p);
   }
   static void destruct_TTimeCluster(void *p) {
      typedef ::TTimeCluster current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TTimeCluster

namespace ROOT {
   static TClass *vectorlEunsignedsPintgR_Dictionary();
   static void vectorlEunsignedsPintgR_TClassManip(TClass*);
   static void *new_vectorlEunsignedsPintgR(void *p = 0);
   static void *newArray_vectorlEunsignedsPintgR(Long_t size, void *p);
   static void delete_vectorlEunsignedsPintgR(void *p);
   static void deleteArray_vectorlEunsignedsPintgR(void *p);
   static void destruct_vectorlEunsignedsPintgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<unsigned int>*)
   {
      vector<unsigned int> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<unsigned int>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<unsigned int>", -2, "vector", 216,
                  typeid(vector<unsigned int>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEunsignedsPintgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<unsigned int>) );
      instance.SetNew(&new_vectorlEunsignedsPintgR);
      instance.SetNewArray(&newArray_vectorlEunsignedsPintgR);
      instance.SetDelete(&delete_vectorlEunsignedsPintgR);
      instance.SetDeleteArray(&deleteArray_vectorlEunsignedsPintgR);
      instance.SetDestructor(&destruct_vectorlEunsignedsPintgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<unsigned int> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<unsigned int>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEunsignedsPintgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<unsigned int>*)0x0)->GetClass();
      vectorlEunsignedsPintgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEunsignedsPintgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEunsignedsPintgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<unsigned int> : new vector<unsigned int>;
   }
   static void *newArray_vectorlEunsignedsPintgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<unsigned int>[nElements] : new vector<unsigned int>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEunsignedsPintgR(void *p) {
      delete ((vector<unsigned int>*)p);
   }
   static void deleteArray_vectorlEunsignedsPintgR(void *p) {
      delete [] ((vector<unsigned int>*)p);
   }
   static void destruct_vectorlEunsignedsPintgR(void *p) {
      typedef vector<unsigned int> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<unsigned int>

namespace ROOT {
   static TClass *vectorlEunsignedsPchargR_Dictionary();
   static void vectorlEunsignedsPchargR_TClassManip(TClass*);
   static void *new_vectorlEunsignedsPchargR(void *p = 0);
   static void *newArray_vectorlEunsignedsPchargR(Long_t size, void *p);
   static void delete_vectorlEunsignedsPchargR(void *p);
   static void deleteArray_vectorlEunsignedsPchargR(void *p);
   static void destruct_vectorlEunsignedsPchargR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<unsigned char>*)
   {
      vector<unsigned char> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<unsigned char>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<unsigned char>", -2, "vector", 216,
                  typeid(vector<unsigned char>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEunsignedsPchargR_Dictionary, isa_proxy, 0,
                  sizeof(vector<unsigned char>) );
      instance.SetNew(&new_vectorlEunsignedsPchargR);
      instance.SetNewArray(&newArray_vectorlEunsignedsPchargR);
      instance.SetDelete(&delete_vectorlEunsignedsPchargR);
      instance.SetDeleteArray(&deleteArray_vectorlEunsignedsPchargR);
      instance.SetDestructor(&destruct_vectorlEunsignedsPchargR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<unsigned char> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<unsigned char>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEunsignedsPchargR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<unsigned char>*)0x0)->GetClass();
      vectorlEunsignedsPchargR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEunsignedsPchargR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEunsignedsPchargR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<unsigned char> : new vector<unsigned char>;
   }
   static void *newArray_vectorlEunsignedsPchargR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<unsigned char>[nElements] : new vector<unsigned char>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEunsignedsPchargR(void *p) {
      delete ((vector<unsigned char>*)p);
   }
   static void deleteArray_vectorlEunsignedsPchargR(void *p) {
      delete [] ((vector<unsigned char>*)p);
   }
   static void destruct_vectorlEunsignedsPchargR(void *p) {
      typedef vector<unsigned char> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<unsigned char>

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
         instance("vector<int>", -2, "vector", 216,
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
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<int>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

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

namespace ROOT {
   static TClass *vectorlEdoublegR_Dictionary();
   static void vectorlEdoublegR_TClassManip(TClass*);
   static void *new_vectorlEdoublegR(void *p = 0);
   static void *newArray_vectorlEdoublegR(Long_t size, void *p);
   static void delete_vectorlEdoublegR(void *p);
   static void deleteArray_vectorlEdoublegR(void *p);
   static void destruct_vectorlEdoublegR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<double>*)
   {
      vector<double> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<double>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<double>", -2, "vector", 216,
                  typeid(vector<double>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEdoublegR_Dictionary, isa_proxy, 0,
                  sizeof(vector<double>) );
      instance.SetNew(&new_vectorlEdoublegR);
      instance.SetNewArray(&newArray_vectorlEdoublegR);
      instance.SetDelete(&delete_vectorlEdoublegR);
      instance.SetDeleteArray(&deleteArray_vectorlEdoublegR);
      instance.SetDestructor(&destruct_vectorlEdoublegR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<double> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<double>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEdoublegR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<double>*)0x0)->GetClass();
      vectorlEdoublegR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEdoublegR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEdoublegR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<double> : new vector<double>;
   }
   static void *newArray_vectorlEdoublegR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<double>[nElements] : new vector<double>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEdoublegR(void *p) {
      delete ((vector<double>*)p);
   }
   static void deleteArray_vectorlEdoublegR(void *p) {
      delete [] ((vector<double>*)p);
   }
   static void destruct_vectorlEdoublegR(void *p) {
      typedef vector<double> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<double>

namespace ROOT {
   static TClass *vectorlETStringgR_Dictionary();
   static void vectorlETStringgR_TClassManip(TClass*);
   static void *new_vectorlETStringgR(void *p = 0);
   static void *newArray_vectorlETStringgR(Long_t size, void *p);
   static void delete_vectorlETStringgR(void *p);
   static void deleteArray_vectorlETStringgR(void *p);
   static void destruct_vectorlETStringgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TString>*)
   {
      vector<TString> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TString>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TString>", -2, "vector", 216,
                  typeid(vector<TString>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETStringgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<TString>) );
      instance.SetNew(&new_vectorlETStringgR);
      instance.SetNewArray(&newArray_vectorlETStringgR);
      instance.SetDelete(&delete_vectorlETStringgR);
      instance.SetDeleteArray(&deleteArray_vectorlETStringgR);
      instance.SetDestructor(&destruct_vectorlETStringgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TString> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TString>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETStringgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TString>*)0x0)->GetClass();
      vectorlETStringgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETStringgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETStringgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TString> : new vector<TString>;
   }
   static void *newArray_vectorlETStringgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TString>[nElements] : new vector<TString>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETStringgR(void *p) {
      delete ((vector<TString>*)p);
   }
   static void deleteArray_vectorlETStringgR(void *p) {
      delete [] ((vector<TString>*)p);
   }
   static void destruct_vectorlETStringgR(void *p) {
      typedef vector<TString> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TString>

namespace ROOT {
   static TClass *vectorlEPrimRegistergR_Dictionary();
   static void vectorlEPrimRegistergR_TClassManip(TClass*);
   static void *new_vectorlEPrimRegistergR(void *p = 0);
   static void *newArray_vectorlEPrimRegistergR(Long_t size, void *p);
   static void delete_vectorlEPrimRegistergR(void *p);
   static void deleteArray_vectorlEPrimRegistergR(void *p);
   static void destruct_vectorlEPrimRegistergR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<PrimRegister>*)
   {
      vector<PrimRegister> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<PrimRegister>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<PrimRegister>", -2, "vector", 216,
                  typeid(vector<PrimRegister>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEPrimRegistergR_Dictionary, isa_proxy, 0,
                  sizeof(vector<PrimRegister>) );
      instance.SetNew(&new_vectorlEPrimRegistergR);
      instance.SetNewArray(&newArray_vectorlEPrimRegistergR);
      instance.SetDelete(&delete_vectorlEPrimRegistergR);
      instance.SetDeleteArray(&deleteArray_vectorlEPrimRegistergR);
      instance.SetDestructor(&destruct_vectorlEPrimRegistergR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<PrimRegister> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<PrimRegister>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEPrimRegistergR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<PrimRegister>*)0x0)->GetClass();
      vectorlEPrimRegistergR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEPrimRegistergR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEPrimRegistergR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<PrimRegister> : new vector<PrimRegister>;
   }
   static void *newArray_vectorlEPrimRegistergR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<PrimRegister>[nElements] : new vector<PrimRegister>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEPrimRegistergR(void *p) {
      delete ((vector<PrimRegister>*)p);
   }
   static void deleteArray_vectorlEPrimRegistergR(void *p) {
      delete [] ((vector<PrimRegister>*)p);
   }
   static void destruct_vectorlEPrimRegistergR(void *p) {
      typedef vector<PrimRegister> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<PrimRegister>

namespace ROOT {
   static TClass *vectorlEPrimCountergR_Dictionary();
   static void vectorlEPrimCountergR_TClassManip(TClass*);
   static void *new_vectorlEPrimCountergR(void *p = 0);
   static void *newArray_vectorlEPrimCountergR(Long_t size, void *p);
   static void delete_vectorlEPrimCountergR(void *p);
   static void deleteArray_vectorlEPrimCountergR(void *p);
   static void destruct_vectorlEPrimCountergR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<PrimCounter>*)
   {
      vector<PrimCounter> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<PrimCounter>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<PrimCounter>", -2, "vector", 216,
                  typeid(vector<PrimCounter>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEPrimCountergR_Dictionary, isa_proxy, 0,
                  sizeof(vector<PrimCounter>) );
      instance.SetNew(&new_vectorlEPrimCountergR);
      instance.SetNewArray(&newArray_vectorlEPrimCountergR);
      instance.SetDelete(&delete_vectorlEPrimCountergR);
      instance.SetDeleteArray(&deleteArray_vectorlEPrimCountergR);
      instance.SetDestructor(&destruct_vectorlEPrimCountergR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<PrimCounter> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<PrimCounter>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEPrimCountergR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<PrimCounter>*)0x0)->GetClass();
      vectorlEPrimCountergR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEPrimCountergR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEPrimCountergR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<PrimCounter> : new vector<PrimCounter>;
   }
   static void *newArray_vectorlEPrimCountergR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<PrimCounter>[nElements] : new vector<PrimCounter>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEPrimCountergR(void *p) {
      delete ((vector<PrimCounter>*)p);
   }
   static void deleteArray_vectorlEPrimCountergR(void *p) {
      delete [] ((vector<PrimCounter>*)p);
   }
   static void destruct_vectorlEPrimCountergR(void *p) {
      typedef vector<PrimCounter> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<PrimCounter>

namespace ROOT {
   static TClass *vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR_Dictionary();
   static void vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR_TClassManip(TClass*);
   static void *new_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p = 0);
   static void *newArray_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(Long_t size, void *p);
   static void delete_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p);
   static void deleteArray_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p);
   static void destruct_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<NA62Analysis::Core::AnalyzerIdentifier>*)
   {
      vector<NA62Analysis::Core::AnalyzerIdentifier> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<NA62Analysis::Core::AnalyzerIdentifier>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<NA62Analysis::Core::AnalyzerIdentifier>", -2, "vector", 216,
                  typeid(vector<NA62Analysis::Core::AnalyzerIdentifier>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR_Dictionary, isa_proxy, 0,
                  sizeof(vector<NA62Analysis::Core::AnalyzerIdentifier>) );
      instance.SetNew(&new_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR);
      instance.SetNewArray(&newArray_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR);
      instance.SetDelete(&delete_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR);
      instance.SetDeleteArray(&deleteArray_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR);
      instance.SetDestructor(&destruct_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<NA62Analysis::Core::AnalyzerIdentifier> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<NA62Analysis::Core::AnalyzerIdentifier>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<NA62Analysis::Core::AnalyzerIdentifier>*)0x0)->GetClass();
      vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<NA62Analysis::Core::AnalyzerIdentifier> : new vector<NA62Analysis::Core::AnalyzerIdentifier>;
   }
   static void *newArray_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<NA62Analysis::Core::AnalyzerIdentifier>[nElements] : new vector<NA62Analysis::Core::AnalyzerIdentifier>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p) {
      delete ((vector<NA62Analysis::Core::AnalyzerIdentifier>*)p);
   }
   static void deleteArray_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p) {
      delete [] ((vector<NA62Analysis::Core::AnalyzerIdentifier>*)p);
   }
   static void destruct_vectorlENA62AnalysiscLcLCorecLcLAnalyzerIdentifiergR(void *p) {
      typedef vector<NA62Analysis::Core::AnalyzerIdentifier> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<NA62Analysis::Core::AnalyzerIdentifier>

namespace ROOT {
   static TClass *vectorlEL2PCSpecialBlockgR_Dictionary();
   static void vectorlEL2PCSpecialBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL2PCSpecialBlockgR(void *p = 0);
   static void *newArray_vectorlEL2PCSpecialBlockgR(Long_t size, void *p);
   static void delete_vectorlEL2PCSpecialBlockgR(void *p);
   static void deleteArray_vectorlEL2PCSpecialBlockgR(void *p);
   static void destruct_vectorlEL2PCSpecialBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L2PCSpecialBlock>*)
   {
      vector<L2PCSpecialBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L2PCSpecialBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L2PCSpecialBlock>", -2, "vector", 216,
                  typeid(vector<L2PCSpecialBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL2PCSpecialBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L2PCSpecialBlock>) );
      instance.SetNew(&new_vectorlEL2PCSpecialBlockgR);
      instance.SetNewArray(&newArray_vectorlEL2PCSpecialBlockgR);
      instance.SetDelete(&delete_vectorlEL2PCSpecialBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL2PCSpecialBlockgR);
      instance.SetDestructor(&destruct_vectorlEL2PCSpecialBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L2PCSpecialBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L2PCSpecialBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL2PCSpecialBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L2PCSpecialBlock>*)0x0)->GetClass();
      vectorlEL2PCSpecialBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL2PCSpecialBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL2PCSpecialBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2PCSpecialBlock> : new vector<L2PCSpecialBlock>;
   }
   static void *newArray_vectorlEL2PCSpecialBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2PCSpecialBlock>[nElements] : new vector<L2PCSpecialBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL2PCSpecialBlockgR(void *p) {
      delete ((vector<L2PCSpecialBlock>*)p);
   }
   static void deleteArray_vectorlEL2PCSpecialBlockgR(void *p) {
      delete [] ((vector<L2PCSpecialBlock>*)p);
   }
   static void destruct_vectorlEL2PCSpecialBlockgR(void *p) {
      typedef vector<L2PCSpecialBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L2PCSpecialBlock>

namespace ROOT {
   static TClass *vectorlEL2MaskSpecialBlockgR_Dictionary();
   static void vectorlEL2MaskSpecialBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL2MaskSpecialBlockgR(void *p = 0);
   static void *newArray_vectorlEL2MaskSpecialBlockgR(Long_t size, void *p);
   static void delete_vectorlEL2MaskSpecialBlockgR(void *p);
   static void deleteArray_vectorlEL2MaskSpecialBlockgR(void *p);
   static void destruct_vectorlEL2MaskSpecialBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L2MaskSpecialBlock>*)
   {
      vector<L2MaskSpecialBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L2MaskSpecialBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L2MaskSpecialBlock>", -2, "vector", 216,
                  typeid(vector<L2MaskSpecialBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL2MaskSpecialBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L2MaskSpecialBlock>) );
      instance.SetNew(&new_vectorlEL2MaskSpecialBlockgR);
      instance.SetNewArray(&newArray_vectorlEL2MaskSpecialBlockgR);
      instance.SetDelete(&delete_vectorlEL2MaskSpecialBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL2MaskSpecialBlockgR);
      instance.SetDestructor(&destruct_vectorlEL2MaskSpecialBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L2MaskSpecialBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L2MaskSpecialBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL2MaskSpecialBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L2MaskSpecialBlock>*)0x0)->GetClass();
      vectorlEL2MaskSpecialBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL2MaskSpecialBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL2MaskSpecialBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2MaskSpecialBlock> : new vector<L2MaskSpecialBlock>;
   }
   static void *newArray_vectorlEL2MaskSpecialBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2MaskSpecialBlock>[nElements] : new vector<L2MaskSpecialBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL2MaskSpecialBlockgR(void *p) {
      delete ((vector<L2MaskSpecialBlock>*)p);
   }
   static void deleteArray_vectorlEL2MaskSpecialBlockgR(void *p) {
      delete [] ((vector<L2MaskSpecialBlock>*)p);
   }
   static void destruct_vectorlEL2MaskSpecialBlockgR(void *p) {
      typedef vector<L2MaskSpecialBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L2MaskSpecialBlock>

namespace ROOT {
   static TClass *vectorlEL2MaskBlockgR_Dictionary();
   static void vectorlEL2MaskBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL2MaskBlockgR(void *p = 0);
   static void *newArray_vectorlEL2MaskBlockgR(Long_t size, void *p);
   static void delete_vectorlEL2MaskBlockgR(void *p);
   static void deleteArray_vectorlEL2MaskBlockgR(void *p);
   static void destruct_vectorlEL2MaskBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L2MaskBlock>*)
   {
      vector<L2MaskBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L2MaskBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L2MaskBlock>", -2, "vector", 216,
                  typeid(vector<L2MaskBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL2MaskBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L2MaskBlock>) );
      instance.SetNew(&new_vectorlEL2MaskBlockgR);
      instance.SetNewArray(&newArray_vectorlEL2MaskBlockgR);
      instance.SetDelete(&delete_vectorlEL2MaskBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL2MaskBlockgR);
      instance.SetDestructor(&destruct_vectorlEL2MaskBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L2MaskBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L2MaskBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL2MaskBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L2MaskBlock>*)0x0)->GetClass();
      vectorlEL2MaskBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL2MaskBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL2MaskBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2MaskBlock> : new vector<L2MaskBlock>;
   }
   static void *newArray_vectorlEL2MaskBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2MaskBlock>[nElements] : new vector<L2MaskBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL2MaskBlockgR(void *p) {
      delete ((vector<L2MaskBlock>*)p);
   }
   static void deleteArray_vectorlEL2MaskBlockgR(void *p) {
      delete [] ((vector<L2MaskBlock>*)p);
   }
   static void destruct_vectorlEL2MaskBlockgR(void *p) {
      typedef vector<L2MaskBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L2MaskBlock>

namespace ROOT {
   static TClass *vectorlEL2AlgoBlockgR_Dictionary();
   static void vectorlEL2AlgoBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL2AlgoBlockgR(void *p = 0);
   static void *newArray_vectorlEL2AlgoBlockgR(Long_t size, void *p);
   static void delete_vectorlEL2AlgoBlockgR(void *p);
   static void deleteArray_vectorlEL2AlgoBlockgR(void *p);
   static void destruct_vectorlEL2AlgoBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L2AlgoBlock>*)
   {
      vector<L2AlgoBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L2AlgoBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L2AlgoBlock>", -2, "vector", 216,
                  typeid(vector<L2AlgoBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL2AlgoBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L2AlgoBlock>) );
      instance.SetNew(&new_vectorlEL2AlgoBlockgR);
      instance.SetNewArray(&newArray_vectorlEL2AlgoBlockgR);
      instance.SetDelete(&delete_vectorlEL2AlgoBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL2AlgoBlockgR);
      instance.SetDestructor(&destruct_vectorlEL2AlgoBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L2AlgoBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L2AlgoBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL2AlgoBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L2AlgoBlock>*)0x0)->GetClass();
      vectorlEL2AlgoBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL2AlgoBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL2AlgoBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2AlgoBlock> : new vector<L2AlgoBlock>;
   }
   static void *newArray_vectorlEL2AlgoBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L2AlgoBlock>[nElements] : new vector<L2AlgoBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL2AlgoBlockgR(void *p) {
      delete ((vector<L2AlgoBlock>*)p);
   }
   static void deleteArray_vectorlEL2AlgoBlockgR(void *p) {
      delete [] ((vector<L2AlgoBlock>*)p);
   }
   static void destruct_vectorlEL2AlgoBlockgR(void *p) {
      typedef vector<L2AlgoBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L2AlgoBlock>

namespace ROOT {
   static TClass *vectorlEL1PCSpecialBlockgR_Dictionary();
   static void vectorlEL1PCSpecialBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL1PCSpecialBlockgR(void *p = 0);
   static void *newArray_vectorlEL1PCSpecialBlockgR(Long_t size, void *p);
   static void delete_vectorlEL1PCSpecialBlockgR(void *p);
   static void deleteArray_vectorlEL1PCSpecialBlockgR(void *p);
   static void destruct_vectorlEL1PCSpecialBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L1PCSpecialBlock>*)
   {
      vector<L1PCSpecialBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L1PCSpecialBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L1PCSpecialBlock>", -2, "vector", 216,
                  typeid(vector<L1PCSpecialBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL1PCSpecialBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L1PCSpecialBlock>) );
      instance.SetNew(&new_vectorlEL1PCSpecialBlockgR);
      instance.SetNewArray(&newArray_vectorlEL1PCSpecialBlockgR);
      instance.SetDelete(&delete_vectorlEL1PCSpecialBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL1PCSpecialBlockgR);
      instance.SetDestructor(&destruct_vectorlEL1PCSpecialBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L1PCSpecialBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L1PCSpecialBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL1PCSpecialBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L1PCSpecialBlock>*)0x0)->GetClass();
      vectorlEL1PCSpecialBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL1PCSpecialBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL1PCSpecialBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1PCSpecialBlock> : new vector<L1PCSpecialBlock>;
   }
   static void *newArray_vectorlEL1PCSpecialBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1PCSpecialBlock>[nElements] : new vector<L1PCSpecialBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL1PCSpecialBlockgR(void *p) {
      delete ((vector<L1PCSpecialBlock>*)p);
   }
   static void deleteArray_vectorlEL1PCSpecialBlockgR(void *p) {
      delete [] ((vector<L1PCSpecialBlock>*)p);
   }
   static void destruct_vectorlEL1PCSpecialBlockgR(void *p) {
      typedef vector<L1PCSpecialBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L1PCSpecialBlock>

namespace ROOT {
   static TClass *vectorlEL1MaskSpecialBlockgR_Dictionary();
   static void vectorlEL1MaskSpecialBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL1MaskSpecialBlockgR(void *p = 0);
   static void *newArray_vectorlEL1MaskSpecialBlockgR(Long_t size, void *p);
   static void delete_vectorlEL1MaskSpecialBlockgR(void *p);
   static void deleteArray_vectorlEL1MaskSpecialBlockgR(void *p);
   static void destruct_vectorlEL1MaskSpecialBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L1MaskSpecialBlock>*)
   {
      vector<L1MaskSpecialBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L1MaskSpecialBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L1MaskSpecialBlock>", -2, "vector", 216,
                  typeid(vector<L1MaskSpecialBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL1MaskSpecialBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L1MaskSpecialBlock>) );
      instance.SetNew(&new_vectorlEL1MaskSpecialBlockgR);
      instance.SetNewArray(&newArray_vectorlEL1MaskSpecialBlockgR);
      instance.SetDelete(&delete_vectorlEL1MaskSpecialBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL1MaskSpecialBlockgR);
      instance.SetDestructor(&destruct_vectorlEL1MaskSpecialBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L1MaskSpecialBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L1MaskSpecialBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL1MaskSpecialBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L1MaskSpecialBlock>*)0x0)->GetClass();
      vectorlEL1MaskSpecialBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL1MaskSpecialBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL1MaskSpecialBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1MaskSpecialBlock> : new vector<L1MaskSpecialBlock>;
   }
   static void *newArray_vectorlEL1MaskSpecialBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1MaskSpecialBlock>[nElements] : new vector<L1MaskSpecialBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL1MaskSpecialBlockgR(void *p) {
      delete ((vector<L1MaskSpecialBlock>*)p);
   }
   static void deleteArray_vectorlEL1MaskSpecialBlockgR(void *p) {
      delete [] ((vector<L1MaskSpecialBlock>*)p);
   }
   static void destruct_vectorlEL1MaskSpecialBlockgR(void *p) {
      typedef vector<L1MaskSpecialBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L1MaskSpecialBlock>

namespace ROOT {
   static TClass *vectorlEL1MaskBlockgR_Dictionary();
   static void vectorlEL1MaskBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL1MaskBlockgR(void *p = 0);
   static void *newArray_vectorlEL1MaskBlockgR(Long_t size, void *p);
   static void delete_vectorlEL1MaskBlockgR(void *p);
   static void deleteArray_vectorlEL1MaskBlockgR(void *p);
   static void destruct_vectorlEL1MaskBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L1MaskBlock>*)
   {
      vector<L1MaskBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L1MaskBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L1MaskBlock>", -2, "vector", 216,
                  typeid(vector<L1MaskBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL1MaskBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L1MaskBlock>) );
      instance.SetNew(&new_vectorlEL1MaskBlockgR);
      instance.SetNewArray(&newArray_vectorlEL1MaskBlockgR);
      instance.SetDelete(&delete_vectorlEL1MaskBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL1MaskBlockgR);
      instance.SetDestructor(&destruct_vectorlEL1MaskBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L1MaskBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L1MaskBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL1MaskBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L1MaskBlock>*)0x0)->GetClass();
      vectorlEL1MaskBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL1MaskBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL1MaskBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1MaskBlock> : new vector<L1MaskBlock>;
   }
   static void *newArray_vectorlEL1MaskBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1MaskBlock>[nElements] : new vector<L1MaskBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL1MaskBlockgR(void *p) {
      delete ((vector<L1MaskBlock>*)p);
   }
   static void deleteArray_vectorlEL1MaskBlockgR(void *p) {
      delete [] ((vector<L1MaskBlock>*)p);
   }
   static void destruct_vectorlEL1MaskBlockgR(void *p) {
      typedef vector<L1MaskBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L1MaskBlock>

namespace ROOT {
   static TClass *vectorlEL1AlgoBlockgR_Dictionary();
   static void vectorlEL1AlgoBlockgR_TClassManip(TClass*);
   static void *new_vectorlEL1AlgoBlockgR(void *p = 0);
   static void *newArray_vectorlEL1AlgoBlockgR(Long_t size, void *p);
   static void delete_vectorlEL1AlgoBlockgR(void *p);
   static void deleteArray_vectorlEL1AlgoBlockgR(void *p);
   static void destruct_vectorlEL1AlgoBlockgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L1AlgoBlock>*)
   {
      vector<L1AlgoBlock> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L1AlgoBlock>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L1AlgoBlock>", -2, "vector", 216,
                  typeid(vector<L1AlgoBlock>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL1AlgoBlockgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L1AlgoBlock>) );
      instance.SetNew(&new_vectorlEL1AlgoBlockgR);
      instance.SetNewArray(&newArray_vectorlEL1AlgoBlockgR);
      instance.SetDelete(&delete_vectorlEL1AlgoBlockgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL1AlgoBlockgR);
      instance.SetDestructor(&destruct_vectorlEL1AlgoBlockgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L1AlgoBlock> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L1AlgoBlock>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL1AlgoBlockgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L1AlgoBlock>*)0x0)->GetClass();
      vectorlEL1AlgoBlockgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL1AlgoBlockgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL1AlgoBlockgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1AlgoBlock> : new vector<L1AlgoBlock>;
   }
   static void *newArray_vectorlEL1AlgoBlockgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L1AlgoBlock>[nElements] : new vector<L1AlgoBlock>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL1AlgoBlockgR(void *p) {
      delete ((vector<L1AlgoBlock>*)p);
   }
   static void deleteArray_vectorlEL1AlgoBlockgR(void *p) {
      delete [] ((vector<L1AlgoBlock>*)p);
   }
   static void destruct_vectorlEL1AlgoBlockgR(void *p) {
      typedef vector<L1AlgoBlock> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L1AlgoBlock>

namespace ROOT {
   static TClass *vectorlEL0PrimitivegR_Dictionary();
   static void vectorlEL0PrimitivegR_TClassManip(TClass*);
   static void *new_vectorlEL0PrimitivegR(void *p = 0);
   static void *newArray_vectorlEL0PrimitivegR(Long_t size, void *p);
   static void delete_vectorlEL0PrimitivegR(void *p);
   static void deleteArray_vectorlEL0PrimitivegR(void *p);
   static void destruct_vectorlEL0PrimitivegR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L0Primitive>*)
   {
      vector<L0Primitive> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L0Primitive>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L0Primitive>", -2, "vector", 216,
                  typeid(vector<L0Primitive>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL0PrimitivegR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L0Primitive>) );
      instance.SetNew(&new_vectorlEL0PrimitivegR);
      instance.SetNewArray(&newArray_vectorlEL0PrimitivegR);
      instance.SetDelete(&delete_vectorlEL0PrimitivegR);
      instance.SetDeleteArray(&deleteArray_vectorlEL0PrimitivegR);
      instance.SetDestructor(&destruct_vectorlEL0PrimitivegR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L0Primitive> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L0Primitive>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL0PrimitivegR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L0Primitive>*)0x0)->GetClass();
      vectorlEL0PrimitivegR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL0PrimitivegR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL0PrimitivegR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L0Primitive> : new vector<L0Primitive>;
   }
   static void *newArray_vectorlEL0PrimitivegR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L0Primitive>[nElements] : new vector<L0Primitive>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL0PrimitivegR(void *p) {
      delete ((vector<L0Primitive>*)p);
   }
   static void deleteArray_vectorlEL0PrimitivegR(void *p) {
      delete [] ((vector<L0Primitive>*)p);
   }
   static void destruct_vectorlEL0PrimitivegR(void *p) {
      typedef vector<L0Primitive> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L0Primitive>

namespace ROOT {
   static TClass *vectorlEL0MaskgR_Dictionary();
   static void vectorlEL0MaskgR_TClassManip(TClass*);
   static void *new_vectorlEL0MaskgR(void *p = 0);
   static void *newArray_vectorlEL0MaskgR(Long_t size, void *p);
   static void delete_vectorlEL0MaskgR(void *p);
   static void deleteArray_vectorlEL0MaskgR(void *p);
   static void destruct_vectorlEL0MaskgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<L0Mask>*)
   {
      vector<L0Mask> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<L0Mask>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<L0Mask>", -2, "vector", 216,
                  typeid(vector<L0Mask>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEL0MaskgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<L0Mask>) );
      instance.SetNew(&new_vectorlEL0MaskgR);
      instance.SetNewArray(&newArray_vectorlEL0MaskgR);
      instance.SetDelete(&delete_vectorlEL0MaskgR);
      instance.SetDeleteArray(&deleteArray_vectorlEL0MaskgR);
      instance.SetDestructor(&destruct_vectorlEL0MaskgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<L0Mask> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<L0Mask>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEL0MaskgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<L0Mask>*)0x0)->GetClass();
      vectorlEL0MaskgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEL0MaskgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEL0MaskgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L0Mask> : new vector<L0Mask>;
   }
   static void *newArray_vectorlEL0MaskgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<L0Mask>[nElements] : new vector<L0Mask>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEL0MaskgR(void *p) {
      delete ((vector<L0Mask>*)p);
   }
   static void deleteArray_vectorlEL0MaskgR(void *p) {
      delete [] ((vector<L0Mask>*)p);
   }
   static void destruct_vectorlEL0MaskgR(void *p) {
      typedef vector<L0Mask> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<L0Mask>

namespace ROOT {
   static TClass *vectorlEHLTTrackgR_Dictionary();
   static void vectorlEHLTTrackgR_TClassManip(TClass*);
   static void *new_vectorlEHLTTrackgR(void *p = 0);
   static void *newArray_vectorlEHLTTrackgR(Long_t size, void *p);
   static void delete_vectorlEHLTTrackgR(void *p);
   static void deleteArray_vectorlEHLTTrackgR(void *p);
   static void destruct_vectorlEHLTTrackgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<HLTTrack>*)
   {
      vector<HLTTrack> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<HLTTrack>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<HLTTrack>", -2, "vector", 216,
                  typeid(vector<HLTTrack>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEHLTTrackgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<HLTTrack>) );
      instance.SetNew(&new_vectorlEHLTTrackgR);
      instance.SetNewArray(&newArray_vectorlEHLTTrackgR);
      instance.SetDelete(&delete_vectorlEHLTTrackgR);
      instance.SetDeleteArray(&deleteArray_vectorlEHLTTrackgR);
      instance.SetDestructor(&destruct_vectorlEHLTTrackgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<HLTTrack> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<HLTTrack>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEHLTTrackgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<HLTTrack>*)0x0)->GetClass();
      vectorlEHLTTrackgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEHLTTrackgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEHLTTrackgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<HLTTrack> : new vector<HLTTrack>;
   }
   static void *newArray_vectorlEHLTTrackgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<HLTTrack>[nElements] : new vector<HLTTrack>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEHLTTrackgR(void *p) {
      delete ((vector<HLTTrack>*)p);
   }
   static void deleteArray_vectorlEHLTTrackgR(void *p) {
      delete [] ((vector<HLTTrack>*)p);
   }
   static void destruct_vectorlEHLTTrackgR(void *p) {
      typedef vector<HLTTrack> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<HLTTrack>

namespace {
  void TriggerDictionaryInitialization_libNA62Persistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/NA62/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libNA62Persistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
namespace NA62Analysis{namespace Core{class __attribute__((annotate("$clingAutoload$AnalyzerIdentifier.hh")))  AnalyzerIdentifier;}}
class AnalysisInfo;
class BeamData;
class TargetInfo;
class MagnetInfo;
class ScalerInfo;
class PrimitiveInfo;
class BeamSpecialTrigger;
class DetectorParameter;
class __attribute__((annotate("$clingAutoload$EventBoundary.hh")))  EventBoundary;
class __attribute__((annotate("$clingAutoload$GenePart.hh")))  __attribute__((annotate("$clingAutoload$KinePart.hh")))  GenePart;
class __attribute__((annotate("$clingAutoload$KinePart.hh")))  KinePart;
class __attribute__((annotate("$clingAutoload$TEventInfo.hh")))  TEventInfo;
class Event;
class __attribute__((annotate("$clingAutoload$HLTEvent.hh")))  HLTTrack;
class __attribute__((annotate("$clingAutoload$HLTEvent.hh")))  HLTEvent;
class __attribute__((annotate("$clingAutoload$L0TPData.hh")))  L0TPData;
class __attribute__((annotate("$clingAutoload$L0TPData.hh")))  L0Primitive;
class __attribute__((annotate("$clingAutoload$L0TPSpecialTrigger.hh")))  L0TPSpecialTrigger;
class __attribute__((annotate("$clingAutoload$L0TPSpecialTrigger.hh")))  L0Mask;
class __attribute__((annotate("$clingAutoload$L1TPData.hh")))  L1TPData;
class __attribute__((annotate("$clingAutoload$L1TPData.hh")))  L1MaskBlock;
class __attribute__((annotate("$clingAutoload$L1TPData.hh")))  L1AlgoBlock;
class __attribute__((annotate("$clingAutoload$L1TPSpecialTrigger.hh")))  L1TPSpecialTrigger;
class __attribute__((annotate("$clingAutoload$L1TPSpecialTrigger.hh")))  L1PCSpecialBlock;
class __attribute__((annotate("$clingAutoload$L1TPSpecialTrigger.hh")))  L1MaskSpecialBlock;
class __attribute__((annotate("$clingAutoload$L2EBData.hh")))  L2EBData;
class __attribute__((annotate("$clingAutoload$L2EBData.hh")))  L2MaskBlock;
class __attribute__((annotate("$clingAutoload$L2EBData.hh")))  L2AlgoBlock;
class __attribute__((annotate("$clingAutoload$L2EBSpecialTrigger.hh")))  L2EBSpecialTrigger;
class __attribute__((annotate("$clingAutoload$L2EBSpecialTrigger.hh")))  L2PCSpecialBlock;
class __attribute__((annotate("$clingAutoload$L2EBSpecialTrigger.hh")))  L2MaskSpecialBlock;
class EventHeader;
class __attribute__((annotate("$clingAutoload$TVChannelID.hh")))  __attribute__((annotate("$clingAutoload$FADCVHit.hh")))  TVChannelID;
class __attribute__((annotate("$clingAutoload$TVHit.hh")))  __attribute__((annotate("$clingAutoload$FADCVHit.hh")))  TVHit;
class __attribute__((annotate("$clingAutoload$TVDigi.hh")))  __attribute__((annotate("$clingAutoload$FADCVHit.hh")))  TVDigi;
class __attribute__((annotate("$clingAutoload$FADCVHit.hh")))  FADCVHit;
class __attribute__((annotate("$clingAutoload$TDetectorVHit.hh")))  __attribute__((annotate("$clingAutoload$TDigiVEvent.hh")))  TDetectorVHit;
class __attribute__((annotate("$clingAutoload$TVEvent.hh")))  __attribute__((annotate("$clingAutoload$TDigiVEvent.hh")))  TVEvent;
class __attribute__((annotate("$clingAutoload$TDetectorVEvent.hh")))  __attribute__((annotate("$clingAutoload$TDigiVEvent.hh")))  TDetectorVEvent;
class __attribute__((annotate("$clingAutoload$TVCandidate.hh")))  __attribute__((annotate("$clingAutoload$TDigiVEvent.hh")))  TVCandidate;
class __attribute__((annotate("$clingAutoload$TDigiVCandidate.hh")))  __attribute__((annotate("$clingAutoload$TDigiVEvent.hh")))  TDigiVCandidate;
class __attribute__((annotate("$clingAutoload$TDigiVError.hh")))  __attribute__((annotate("$clingAutoload$TDigiVEvent.hh")))  TDigiVError;
class __attribute__((annotate("$clingAutoload$TDigiVEvent.hh")))  TDigiVEvent;
class FADCEvent;
class MCInfo;
class RecoInfo;
class Rndm;
class Stream;
class TDCError;
class __attribute__((annotate("$clingAutoload$TDCVHit.hh")))  TDCVHit;
class TDCEvent;
class __attribute__((annotate("$clingAutoload$TSpecialTrigger.hh")))  TSpecialTrigger;
class PrimRegister;
class PrimCounter;
class TPrimSpecialTrigger;
class TPrimitive;
class __attribute__((annotate("$clingAutoload$TRecoVHit.hh")))  TRecoVHit;
class TRecoVCandidate;
class TRecoVEvent;
class TSpecialTriggerEvent;
class TTDCBSpecialTrigger;
class TTimeCluster;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libNA62Persistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
// --------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// --------------------------------------------------------------

#ifndef ANALYSISINFO_h
#define ANALYSISINFO_h 1

#include "TObject.h"
#include "TString.h"
#include "AnalyzerIdentifier.hh"

class AnalysisInfo : public TObject {

public:

  AnalysisInfo();
  AnalysisInfo(AnalysisInfo &c);
  virtual ~AnalysisInfo() {};
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(AnalysisInfo &s);
  void MergeJobAttributes(AnalysisInfo &s);
  void UpdateAndMergeAttributes(AnalysisInfo &s);
  void AddJobInfo(TString fileName, TString rev);

  std::vector<NA62Analysis::Core::AnalyzerIdentifier> GetAnalyzers() { return fAnalyzerList;             }
  void AddAnalyzer(NA62Analysis::Core::AnalyzerIdentifier anIdent)   { fAnalyzerList.push_back(anIdent); }
  std::vector<TString> GetStreamName()                               { return fStreamList;               }
  void AddStreamName(TString name)                                   { fStreamList.push_back(name);      }

  std::vector<TString>  GetRevisions()  { return fRevisionList;               }
  void AddRevision(TString val)         { fRevisionList.push_back(val);	      }
  std::vector<TString>	GetInputFiles() { return fInputFileList;              }
  void AddInputFile(TString fileName)   { fInputFileList.push_back(fileName); }

private:
  // Unique attributes
  std::vector<NA62Analysis::Core::AnalyzerIdentifier> fAnalyzerList; ///< List of analyzer than ran on these data
  std::vector<TString> fStreamList;                                  ///< List of output stream that created these data

  // Job dependent attributes
  std::vector<TString> fRevisionList;   ///< List of software revision
  std::vector<TString> fInputFileList;	///< List of input files

  ClassDef(AnalysisInfo,2)
};

#endif
/*
 * AnalyzerIdentifier.hh
 *
 *  Created on: Feb 29, 2016
 *      Author: nlurkin
 */

#ifndef INCLUDE_ANALYZERIDENTIFIER_HH_
#define INCLUDE_ANALYZERIDENTIFIER_HH_

#include <TObjString.h>
#include <TObject.h>

namespace NA62Analysis {
namespace Core {

/// \class AnalyzerIdentifier
/// \Brief
/// Class used for storage in ROOT file containing identifier for an analyzer.
/// \EndBrief
///
/// \Detailed
/// Contains currently only the name of the analyzer but can easily be extended
/// to include version number, unique identifier, ...
/// \EndDetailed
class AnalyzerIdentifier: public TObject {
public:
	AnalyzerIdentifier();
	explicit AnalyzerIdentifier(TString name);
	virtual ~AnalyzerIdentifier();

	void print() const;

	bool operator==(TString value);

	const TObjString& GetAnalyzerName() const {	return fAnalyzerName;	}
private:
	TObjString fAnalyzerName; ///< Name of the analyzer

	ClassDef(AnalyzerIdentifier,1);
};

static const AnalyzerIdentifier ANIDNone("None");
} /* namespace Core */
} /* namespace NA62Analysis */

#endif /* INCLUDE_ANALYZERIDENTIFIER_HH_ */
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-02-12
//
// ---------------------------------------------------------------

#ifndef BeamData_H
#define BeamData_H 1
#include "TObject.h"

class BeamData : public TObject {

  public:

    BeamData();
    BeamData(const BeamData&);
    ~BeamData();
    void Clear(Option_t* = "");

    void AddInstantaneousIntensity(Double_t val)      { fInstantaneousIntensity.push_back(val);      }
    void AddInstantaneousIntensityError(Double_t val) { fInstantaneousIntensityError.push_back(val); }
    Double_t GetInstantaneousIntensity()       { if(fInstantaneousIntensity.size()>0) return fInstantaneousIntensity[0];
                                                 else return 0.;   }
    Double_t GetInstantaneousIntensityError()  { if(fInstantaneousIntensityError.size()>0) return fInstantaneousIntensityError[0];
                                                 else return 0.;   }

    void  PrintInfo();

  private:

    std::vector<Double_t> fInstantaneousIntensity;       ///< Evaluated by NA62Reco using GigaTracker
    std::vector<Double_t> fInstantaneousIntensityError;  ///< Evaluated by NA62Reco using GigaTracker

    ClassDef(BeamData,1);
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-23
//
// ---------------------------------------------------------------

#ifndef BeamSpecialTrigger_H
#define BeamSpecialTrigger_H 1
#include "Rtypes.h"
#include "TObject.h"

struct TargetInfoStruct;
struct MagnetInfoStruct;
struct ScalerInfoStruct;
struct PrimitiveInfoStruct;

//Beam DIM EOB info
class TargetInfo : public TObject{

  public:
    TargetInfo() : fIntensity(0), fIntensityDownstream(0), fIntensityNotNormalised(0),
	fMultiplicity(0), fSymmetry(0), fSymmetryWithoutBSM(0), fReserved(0){};
    ~TargetInfo(){};
    void SetInfo(TargetInfoStruct structInfo);
    void Clear(Option_t* option="");
    float GetIntensity()                { return fIntensity;              };
    void  SetIntensity(float val)       { fIntensity = val;               };
    float GetIntensityDownstream()      { return fIntensityDownstream;    };
    float GetIntensityNotNormalised()   { return fIntensityNotNormalised; };
    uint32_t GetMultiplicity()      { return fMultiplicity;           };
    uint32_t GetSymmetry()          { return fSymmetry;               };
    uint32_t GetSymmetryWithoutBSM(){ return fSymmetryWithoutBSM;     };
    uint32_t GetReserved()          { return fReserved;               };
  private:
    float fIntensity;
    float fIntensityDownstream;
    float fIntensityNotNormalised;
    uint32_t fMultiplicity;
    uint32_t fSymmetry;
    uint32_t fSymmetryWithoutBSM;
    uint32_t fReserved;
    ClassDef(TargetInfo,1);
};

class MagnetInfo : public TObject{
  public:
    MagnetInfo() : fAcqStamp(0), fCycleStamp(0), fReserved(0), fCurrentValue(0) {};
    ~MagnetInfo(){};
    void SetInfo(MagnetInfoStruct structInfo);
    void Clear(Option_t* option="");
    uint32_t GetAcqStamp()          { return fAcqStamp;           };
    uint32_t GetCycleStamp()        { return fCycleStamp;         };
    uint32_t GetReserved()          { return fReserved;           };
    float GetCurrentValue()         { return fCurrentValue;       };
  private:
    uint32_t fAcqStamp;
    uint32_t fCycleStamp;
    uint32_t fReserved;
    float fCurrentValue;
    ClassDef(MagnetInfo,1);
};

class ScalerInfo : public TObject{
  public:
    ScalerInfo() : fAcqStamp(0), fCounts(0), fCycleStamp(0), fReserved(0) {};
    ~ScalerInfo(){};
    void SetInfo(ScalerInfoStruct structInfo);
    void Clear(Option_t* option="");
    uint32_t GetAcqStamp()          { return fAcqStamp;           };
    uint32_t GetCounts()            { return fCounts;             };
    uint32_t GetCycleStamp()        { return fCycleStamp;         };
    uint32_t GetReserved()          { return fReserved;           };
  private:
    uint32_t fAcqStamp;
    uint32_t fCounts;
    uint32_t fCycleStamp;
    uint32_t fReserved;
    ClassDef(ScalerInfo,1);
};

class PrimitiveInfo : public TObject{
  public:
    PrimitiveInfo(): fBBQCHOD(0), fBBQRICH(0), fBBQLAV(0), fBBQMUV3(0),
	fBBQNewCHOD(0), fBBQTALK(0), fBBQLKr(0), fMeanRateCHOD(0), fMeanRateRICH(0),
	fMeanRateLAV(0), fMeanRateMUV3(0), fMeanRateNewCHOD(0), fMeanRateTALK(0),
	fMeanRateLKr(0) {};
    ~PrimitiveInfo(){};
    void SetInfo(PrimitiveInfoStruct structInfo);
    void Clear(Option_t* option="");
    float GetBBQCHOD()         { return fBBQCHOD;        };
    float GetBBQRICH()         { return fBBQRICH;        };
    float GetBBQLAV()          { return fBBQLAV;         };
    float GetBBQMUV3()         { return fBBQMUV3;        };
    float GetBBQNewCHOD()      { return fBBQNewCHOD;     };
    float GetBBQTALK()         { return fBBQTALK;        };
    float GetBBQLKr()          { return fBBQLKr;         };
    float GetMeanRateCHOD()    { return fMeanRateCHOD;   };
    float GetMeanRateRICH()    { return fMeanRateRICH;   };
    float GetMeanRateLAV()     { return fMeanRateLAV;    };
    float GetMeanRateMUV3()    { return fMeanRateMUV3;   };
    float GetMeanRateNewCHOD() { return fMeanRateNewCHOD;};
    float GetMeanRateTALK()    { return fMeanRateTALK;   };
    float GetMeanRateLKr()     { return fMeanRateLKr;    };
  private:
    float fBBQCHOD;
    float fBBQRICH;
    float fBBQLAV;
    float fBBQMUV3;
    float fBBQNewCHOD;
    float fBBQTALK;
    float fBBQLKr;
    float fMeanRateCHOD;
    float fMeanRateRICH;
    float fMeanRateLAV;
    float fMeanRateMUV3;
    float fMeanRateNewCHOD;
    float fMeanRateTALK;
    float fMeanRateLKr;
    ClassDef(PrimitiveInfo,1);
};


class BeamSpecialTrigger : public TObject {

  public:

    BeamSpecialTrigger();
    ~BeamSpecialTrigger();
    Bool_t  SetHeader(UInt_t *,UInt_t);
    void Clear(Option_t* option="");
    uint32_t   GetTimeStamp()            { return fTimeStamp;                       }
    TargetInfo GetT10()                  { return fT10;                             }
    MagnetInfo GetBEND_101_195()         { return fBEND_101_195;                    }
    MagnetInfo GetBEND_101_196()         { return fBEND_101_196;                    }
    MagnetInfo GetTRIM_101_102()         { return fTRIM_101_102;                    }
    ScalerInfo GetQX()                   { return fQX;                              }
    ScalerInfo GetQ1_OR()                { return fQ1_OR;                           }
    ScalerInfo GetMUV1_OR_MUV2()         { return fMUV1_OR_MUV2;                    }
    ScalerInfo GetMUV3()                 { return fMUV3;                            }
    ScalerInfo GetNHOD()                 { return fNHOD;                            }
    ScalerInfo GetIRC()                  { return fIRC;                             }
    ScalerInfo GetCHANTI()               { return fCHANTI;                          }
    ScalerInfo GetECN3_008()             { return fECN3_008;                        }
    ScalerInfo GetECN3_009()             { return fECN3_009;                        }
    ScalerInfo GetECN3_010()             { return fECN3_010;                        }
    ScalerInfo GetECN3_011()             { return fECN3_011;                        }
    ScalerInfo GetECN3_012()             { return fECN3_012;                        }
    ScalerInfo GetARGONION()             { return fARGONION;                        }
    PrimitiveInfo GetPrimitives()        { return fPrimitives;                      }
    // other useful variables
    Float_t GetIntensityT10()            { return fT10.GetIntensity();              }
    void    SetIntensityT10(float val)   { fT10.SetIntensity(val);                  }
    UInt_t  GetCountsQX()                { return fQX.GetCounts();                  }
    UInt_t  GetCountsQ1_OR()             { return fQ1_OR.GetCounts();               }
    UInt_t  GetCountsMUV1_OR_MUV2()      { return fMUV1_OR_MUV2.GetCounts();        }
    UInt_t  GetCountsMUV3()              { return fMUV3.GetCounts();                }
    UInt_t  GetCountsNHOD()              { return fNHOD.GetCounts();                }
    UInt_t  GetCountsIRC()               { return fIRC.GetCounts();                 }
    UInt_t  GetCountsCHANTI()            { return fCHANTI.GetCounts();              }
    UInt_t  GetCountsARGONION()          { return fARGONION.GetCounts();            }
    Float_t GetBBQCHOD()                 { return fPrimitives.GetBBQCHOD();         }
    Float_t GetBBQRICH()                 { return fPrimitives.GetBBQRICH();         }
    Float_t GetBBQLAV()                  { return fPrimitives.GetBBQLAV();          }
    Float_t GetBBQMUV3()                 { return fPrimitives.GetBBQMUV3();         }
    Float_t GetBBQNewCHOD()              { return fPrimitives.GetBBQNewCHOD();      }
    Float_t GetBBQTALK()                 { return fPrimitives.GetBBQTALK();         }
    Float_t GetBBQLKr()                  { return fPrimitives.GetBBQLKr();          }
    Float_t GetMeanRateCHOD()            { return fPrimitives.GetMeanRateCHOD();    }
    Float_t GetMeanRateRICH()            { return fPrimitives.GetMeanRateRICH();    }
    Float_t GetMeanRateLAV()             { return fPrimitives.GetMeanRateLAV();     }
    Float_t GetMeanRateMUV3()            { return fPrimitives.GetMeanRateMUV3();    }
    Float_t GetMeanRateNewCHOD()         { return fPrimitives.GetMeanRateNewCHOD(); }
    Float_t GetMeanRateTALK()            { return fPrimitives.GetMeanRateTALK();    }
    Float_t GetMeanRateLKr()             { return fPrimitives.GetMeanRateLKr();     }

  private:

    uint32_t fTimeStamp;
    TargetInfo fT10;
    MagnetInfo fBEND_101_195;
    MagnetInfo fBEND_101_196;
    MagnetInfo fTRIM_101_102;
    ScalerInfo fQX;
    ScalerInfo fQ1_OR;
    ScalerInfo fMUV1_OR_MUV2;
    ScalerInfo fMUV3;
    ScalerInfo fNHOD;
    ScalerInfo fIRC;
    ScalerInfo fCHANTI;
    ScalerInfo fECN3_008;
    ScalerInfo fECN3_009;
    ScalerInfo fECN3_010;
    ScalerInfo fECN3_011;
    ScalerInfo fECN3_012;
    ScalerInfo fARGONION;
    PrimitiveInfo fPrimitives;

    ClassDef(BeamSpecialTrigger,1);
};

// --- structure definitions [needed to extract values from buffer --- //
struct TargetInfoStruct {
  float Intensity;
  float IntensityDownstream;
  float IntensityNotNormalised;
  uint32_t Multiplicity;
  uint32_t Symmetry;
  uint32_t SymmetryWithoutBSM;
  uint32_t Reserved;
};

struct MagnetInfoStruct {
  uint32_t AcqStamp;
  uint32_t CycleStamp;
  uint32_t Reserved;
  float CurrentValue;
};

struct ScalerInfoStruct {
  uint32_t AcqStamp;
  uint32_t Counts;
  uint32_t CycleStamp;
  uint32_t Reserved;
};

struct PrimitiveInfoStruct {
  float BBQCHOD;
  float BBQRICH;
  float BBQLAV;
  float BBQMUV3;
  float BBQNewCHOD;
  float BBQTALK;
  float BBQLKr;
  float MeanRateCHOD;
  float MeanRateRICH;
  float MeanRateLAV;
  float MeanRateMUV3;
  float MeanRateNewCHOD;
  float MeanRateTALK;
  float MeanRateLKr;
};

struct BeamInfoStruct {
  MagnetInfoStruct BEND_101_195;
  MagnetInfoStruct BEND_101_196;
  MagnetInfoStruct TRIM_101_102;
  ScalerInfoStruct QX;
  ScalerInfoStruct Q1_OR;
  ScalerInfoStruct MUV1_OR_MUV2;
  ScalerInfoStruct MUV3;
  ScalerInfoStruct NHOD;
  ScalerInfoStruct IRC;
  ScalerInfoStruct CHANTI;
  ScalerInfoStruct ECN3_008;
  ScalerInfoStruct ECN3_009;
  ScalerInfoStruct ECN3_010;
  ScalerInfoStruct ECN3_011;
  ScalerInfoStruct ECN3_012;
  ScalerInfoStruct ARGONION;
  PrimitiveInfoStruct Primitives;
};

// ------------------------------------------------------------------- //
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef DetectorParameter_H
#define DetectorParameter_H 1

#include "TString.h"
#include "TObject.h"
#include "TObjArray.h"

class DetectorParameter : public TObject {

public:

  DetectorParameter(){};
  DetectorParameter(const char*,const char*,const char*,TObjArray);
  virtual ~DetectorParameter() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

public:

//   TString              GetName()                                          { return fName;                         };
//   void                 SetName(TString value)                             { fName = value;                        };
  TString              GetValue()                                         { return fValue;                        };
  void                 SetValue(TString value)                            { fValue = value;                       };
  TString              GetDescription()                                   { return fDescription;                  };
  void                 SetDescription(TString value)                      { fDescription = value;                 };

  TObjArray *          GetData()                                          { return &fData;                        };
  void                 SetData(TObjArray value)                           { fData = value;                        };

private:

  // N.B. spacial dimensions are always in mm
  TString fName;
  TString fValue;
  TString fDescription;

  TObjArray fData;

  ClassDef(DetectorParameter,1)
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Emanuele Leonardi (Emanuele.Leonardi@cern.ch) 2007-01-10
// Modified by Sergey Podolsky 2011-01-21
// --------------------------------------------------------------
#ifndef Event_h
#define Event_h 1

#include "EventBoundary.hh"
#include "KinePart.hh"
#include "TEventInfo.hh"
#include "TObject.h"
#include "TClonesArray.h"
#include "TRandom3.h"
#include "Riostream.h"

class Event : public TObject {

public:
  Event();
  virtual ~Event() { Clear(); };
  void Merge(Event*);
  void Purge(TEventInfo);
  EventBoundary* FindEventBoundary(Int_t);
  Int_t    GetID();
  void     SetID(Int_t);
  Int_t    GetStreamID();
  void     SetStreamID(Int_t);
  Double_t GetTime();
  void     SetTime(Double_t);
  Double_t GetEventWeight()            { return fEventWeight; }
  void     SetEventWeight(Double_t val){ fEventWeight = val;  }
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;
  void PrintAll() const;

  Int_t         GetNMergedEvents() const { return fNMergedEvents; }
  TClonesArray* GetEventBoundaries()  const { return fEventBoundaries; }
  Int_t         GetNPurgedKineParts() { return fNPurgedKineParts;  }

  Int_t GetEventNumber()          { return fEventNumber; }
  void  SetEventNumber(Int_t val) { fEventNumber = val;  }

  Int_t         GetNKineParts() const { return fNKineParts; }
  TClonesArray* GetKineParts()  const { return fKineParts; }
  KinePart*     AddKinePart();
  KinePart*     GetKinePart(Int_t);

  Int_t         GetNGeneParts() const { return fNGeneParts; }
  TClonesArray* GetGeneParts()  const { return fGeneParts; }
  GenePart*     AddGenePart();
  GenePart*     GetGenePart(Int_t);

  void StoreRandomState(TRandom3* RandomDecayState, Long_t *RanecuState);
  TRandom3* GetRandomDecayState () {return fRandomDecayState;}
  Long_t* GetRanecuState() {return fRanecuState;}

private:

  Int_t fEventNumber;       ///< MC event number
  Int_t fNGeneParts;        ///< Number of generated particles
  Int_t fNKineParts;        ///< Number of saved particles
  Int_t fNMergedEvents;     ///< Number of merged events
  Int_t fNPurgedKineParts;  ///< Number of removed KineParts
  Double_t fEventWeight;    ///< Weight of the MC event (to account for biasing)

  TClonesArray*  fGeneParts;            ///< Array of generated particles in K rest frame
  TClonesArray*  fKineParts;            ///< Array of saved true particles in lab frame
  TClonesArray*  fEventBoundaries;      ///< Array of event boundaries to separate merged events
  EventBoundary* fCurrentEventBoundary; //! Transient

  static TRandom3* fgRandomDecayState;
  TRandom3* fRandomDecayState; ///< NA62MC random generator state
  Long_t fRanecuState[2];      ///< G4 (CLHEP) random generator state

  ClassDef(Event,1)
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-01-23
//
// --------------------------------------------------------------
#ifndef EventBoundary_H
#define EventBoundary_H

#include "TObject.h"

class EventBoundary : public TObject {

    public:

        EventBoundary();
        virtual ~EventBoundary(){};
        void Clear(Option_t* option="");
        void AddGenePart();
        void AddKinePart();
        void Shift(Int_t, Int_t);

    public:

        Int_t                GetID()                                            { return fID;                           };
        void                 SetID(Int_t value)                                 { fID = value;                          };
        Int_t                GetStreamID()                                      { return fStreamID;                     };
        void                 SetStreamID(Int_t value)                           { fStreamID = value;                    };
        Double_t             GetTime()                                          { return fTime;                         };
        void                 SetTime(Double_t value)                            { fTime = value;                        };

        Int_t                GetFirstGenePartIndex()                            { return fFirstGenePartIndex;           };
        void                 SetFirstGenePartIndex(Int_t value)                 { fFirstGenePartIndex = value;          };
        Int_t                GetNGeneParts()                                    { return fNGeneParts;                   };
        void                 SetNGeneParts(Int_t value)                         { fNGeneParts = value;                  };
        Int_t                GetFirstKinePartIndex()                            { return fFirstKinePartIndex;           };
        void                 SetFirstKinePartIndex(Int_t value)                 { fFirstKinePartIndex = value;          };
        Int_t                GetNKineParts()                                    { return fNKineParts;                   };
        void                 SetNKineParts(Int_t value)                         { fNKineParts = value;                  };


    private:

        Int_t      fID;
        Int_t      fStreamID;
        Double_t   fTime;

        Int_t      fFirstGenePartIndex;
        Int_t      fNGeneParts;
        Int_t      fFirstKinePartIndex;
        Int_t      fNKineParts;

        ClassDef(EventBoundary,1);
};
#endif
#ifndef EVENTHEADER_H
#define EVENTHEADER_H 1

#include "Rtypes.h"
#include "TObject.h"
#include "BeamData.hh"
#include "BeamSpecialTrigger.hh"
#include "HLTEvent.hh"
#include "L0TPData.hh"
#include "L0TPSpecialTrigger.hh"
#include "L1TPData.hh"
#include "L1TPSpecialTrigger.hh"
#include "L2EBData.hh"
#include "L2EBSpecialTrigger.hh"

#define O_EVENTNUMBER 0
#define O_EVENTLENGTH 1
#define O_BURSTID 2
#define O_TIMESTAMP 3
#define O_TRIGGERTYPE 4
#define O_FINETIME 5
#define O_NSUBDET 5
#define O_DATAPROCESSINGIDENTIFIER 6
#define O_BURSTTIME 7
#define O_DETECTORTABLE 8

#define M_EVENTNUMBER 0x00ffffff
#define M_TRIGGERTYPE 0x00ffffff
#define M_FINETIME 0x000000ff
#define M_NSUBDET 0x0000ff00
#define M_DETECTOROFFSET 0x00ffffff
#define M_DETECTORID 0xff000000

#define S_NSUBDET 8
#define S_DETECTORID 26

class EventHeader : public TObject {

  public:

    EventHeader();
    ~EventHeader();
    Bool_t SetHeader(UInt_t *);
    Bool_t SetL0TPData(UInt_t *);
    Bool_t SetL1TPData(UInt_t *);
    Bool_t SetL2EBData(UInt_t *);
    void   SetBeamInstantaneousIntensity(Double_t, Double_t);
    Bool_t SetL0TPSpecialTrigger(UInt_t *);
    Bool_t SetL1TPSpecialTrigger(UInt_t *);
    Bool_t SetL2EBSpecialTrigger(UInt_t *);
    Bool_t SetBeamSpecialTrigger(UInt_t *,UInt_t);
    UInt_t GetEventNumber()                      { return fEventNumber;        }
    UInt_t GetEventLength()                      { return fEventLength;        }
    UInt_t GetBurstID()                          { return fBurstID;            }
    UInt_t GetRunID()                            { return fRunID;              }
    UInt_t GetTimeStamp()                        { return fTimeStamp;          }
    UInt_t GetPreviousTimeStamp()                { return fPreviousTimeStamp;  }
    UInt_t GetTriggerType()                      { return fTriggerType;        }
    void   SetTriggerType(UInt_t val)            { fTriggerType = val;         }
    UInt_t GetNumOfDetectors()                   { return fNumOfDetectors;     }
    UInt_t GetFineTime()                         { return fFineTime;           }
    UInt_t GetBurstTime()                        { return fBurstTime;          }
    L0TPData * GetL0TPData()                     { return fL0TPData;           }
    L1TPData * GetL1TPData()                     { return fL1TPData;           }
    L2EBData * GetL2EBData()                     { return fL2EBData;           }
    BeamData * GetBeamData()                     { return fBeamData;           }
    HLTEvent * GetHLTEvent()                     { return fHLTEvent;           }
    L0TPSpecialTrigger * GetL0TPSpecialTrigger() { return fL0TPSpecialTrigger; }
    L1TPSpecialTrigger * GetL1TPSpecialTrigger() { return fL1TPSpecialTrigger; }
    L2EBSpecialTrigger * GetL2EBSpecialTrigger() { return fL2EBSpecialTrigger; }
    BeamSpecialTrigger * GetBeamSpecialTrigger() { return fBeamSpecialTrigger; }
    UInt_t GetEventQualityMask()                 { return fEventQualityMask;   }

    // BurstID and RunID initilised by DecodeBurstHeader for data format >= 2015
    void SetRunID(UInt_t val)                    { fRunID = val;               }
    void SetBurstID(UInt_t val)                  { fBurstID = val;             }
    void SetBurstTime(UInt_t val)                { fBurstTime = val;           }
    void SetEventNumber(UInt_t val)              { fEventNumber = val;         }
    void SetFineTime(UInt_t val)                 { fFineTime = val;            }
    void SetTimeStamp(UInt_t val)                { fTimeStamp = val;           }
    void SetEventQualityMask(UInt_t val)         { fEventQualityMask = val;    }
    void UpdateEventQualityMask(UInt_t);

    void SetStartByte(ULong64_t value)           { fStartByte=value;           }
    ULong64_t GetStartByte()                     { return fStartByte;          }
    void SetDetectorID(ULong64_t value)          { fDetectorID = value;        }
    UInt_t GetDetectorID()                       { return fDetectorID;         }
    void SetEOBFileDescriptor(FILE *value)       { fEOBFileDescriptor=value;   }
    FILE * GetEOBFileDescriptor()                { return fEOBFileDescriptor;  }

    void ClearDIMBlock();

  private:

    UInt_t fEventNumber;
    UInt_t fEventLength;
    UInt_t fBurstID;
    UInt_t fRunID;
    UInt_t fTimeStamp;
    UInt_t fPreviousTimeStamp;
    UInt_t fTriggerType;
    UInt_t fNumOfDetectors;
    UInt_t fFineTime;
    UInt_t fBurstTime;
    UInt_t fDetectorID; //!  Transient data member
    ULong64_t fStartByte; //!  Transient data member
    FILE *fEOBFileDescriptor; //!  Transient data member
    L0TPData *fL0TPData; //!  Transient data member
    L1TPData *fL1TPData; //!  Transient data member
    L2EBData *fL2EBData; //!  Transient data member
    BeamData *fBeamData; //!  Transient data member
    HLTEvent *fHLTEvent; //!  Transient data member
    L0TPSpecialTrigger *fL0TPSpecialTrigger; //!  Transient data member
    L1TPSpecialTrigger *fL1TPSpecialTrigger; //!  Transient data member
    L2EBSpecialTrigger *fL2EBSpecialTrigger; //!  Transient data member
    BeamSpecialTrigger *fBeamSpecialTrigger; //!  Transient data member
    UInt_t fEventQualityMask;

    ClassDef(EventHeader,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Modified by Giuseppe Ruggiero 2012
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//            Evelina.Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#ifndef FADCEvent_H
#define FADCEvent_H
#include "TClass.h"
#include "FADCVHit.hh"
#include "TDigiVEvent.hh"


class FADCEvent : public TDigiVEvent {

  public:

    FADCEvent();
    explicit FADCEvent(TClass *);
    FADCEvent(TClass *, Int_t);
    FADCVHit* GetHit(Int_t);
    void Clear(Option_t* = "");

  public:
    Int_t    GetFADCID() {return fFADCID;};
    void     SetFADCID(Int_t val) {fFADCID = val;};
    Int_t    GetNSamples() {return fNSamples;};
    void     SetNSamples(Int_t val) {fNSamples = val;};
    Int_t    GetEventFlag() {return fEventFlag;};
    void     SetEventFlag(Int_t val) {fEventFlag = val;};

  private:
    Int_t fFADCID; // fFADCID = 10*a+b
    //                a = 1 if zero suppression algorithm applied, else 0
    //                b = 1 if pulses in energy units (GeV)
    //                b = 0 if pulses in units of ADC counts 
    Int_t fNSamples;  // Nb of ~25 nsec samplings stored
    Int_t fEventFlag; // Flag problems in this event
    //                   bit  1 set : at least one saturated pulse
    //                   bit  4 set : at least one pulse in underflow
    //                   bit 12 set : at least one L1 error

    ClassDef(FADCEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Modified by Giuseppe Ruggiero 2012
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#ifndef FADCVHit_H
#define FADCVHit_H
#include "TVDigi.hh"
#include "TArrayD.h"

class FADCVHit : public TVDigi {

  public:

    FADCVHit();
    explicit FADCVHit(Int_t);
    explicit FADCVHit(TVHit* MCHit);
    ~FADCVHit(){};
    void Clear(Option_t* = "");
    virtual void UpdateReferenceTime(Double_t value){ fPeakTime -= value; };
    virtual Double_t       GetTime() { return fPeakTime; };

  public:
    inline Double_t        GetPeakEnergy()                           { return fPeakEnergy;         }
    inline void            SetPeakEnergy(Double_t val)               { fPeakEnergy = val;          }
    inline Double_t        GetPeakTime()                             { return fPeakTime;           }
    inline void            SetPeakTime(Double_t val)                 { fPeakTime = val;            }
    inline Double_t        GetADCPeakEnergy()                        { return fADCPeakEnergy;      }
    inline void            SetADCPeakEnergy(Double_t val)            { fADCPeakEnergy = val;       }
    inline Double_t        GetADCPeakTime()                          { return fADCPeakTime;        }
    inline void            SetADCPeakTime(Double_t val)              { fADCPeakTime = val;         }
    inline Int_t           GetQuality()                              { return fQuality;            }
    inline void            SetQuality(Int_t val)                     { fQuality = val;             }
    inline Int_t           GetFlags()                                { return fFlags;              }
    inline void            SetFlags(Int_t val)                       { fFlags = val;               }
    inline Int_t           GetNSamples()                             { return fNSamples;           }
    void                   AddSample(Double_t Value);
    inline Double_t *      GetAllSamples()                           { return fSamples.GetArray(); }
    inline Int_t           GetGain()                                 { return fGain;               }
    inline void            SetGain(Int_t val)                        { fGain = val;                }

  private:

    Double_t fPeakEnergy;    // Parabola peak pulse height (in GeV)
    Double_t fPeakTime;      // Parabola peak time (the time is in ns)
    Double_t fADCPeakEnergy; // Parabola peak pulse height (= PEAK + 10000*maxgain with gain = 0,1,2, or 3)
    Double_t fADCPeakTime;   // Parabola peak time (the time is in unit of samples)
    Int_t fQuality;   // Quality estimator 
    //                   0 : no energy/time estimate
    //                   bit 0 set : energy/time from fixed sampling (in header)
    //                   bit 1 set : energy/time from max sampling
    //                   bit 2 set : energy/time from 3-point parabola
    Int_t fFlags;     // flag pulse or cell 
    //                   bit  0 set : pulse BELOW zero suppression threshold
    //                   bit  1 set : pulse saturated
    //                   bit  4 set : pulse in underflow
    //                   bit  5 set : no calibration
    //                   bit  6 set : non trivial pulse (gain>0)
    //                   bit  7 set : q(iplkr+3)=max count
    //                   bit 12 set : L1 error
    TArrayD  fSamples; //[fNMaxSamples] Pulse height (if in ADC counts is given as above)

    Int_t fNSamples;
    Int_t fNMaxSamples;
    Int_t fGain;

    ClassDef(FADCVHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 6 Apr 2011
//
// --------------------------------------------------------------
#ifndef GenePart_h
#define GenePart_h 1

#include "TObject.h"
#include "TVector3.h"
#include "TLorentzVector.h"

class GenePart : public TObject {

public:

  GenePart();
  virtual ~GenePart() {Clear();}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  inline void SetParticleName(TString val)     { fParticleName = val;          }
  inline void SetParticleName(char* val)       { fParticleName = TString(val); }
  inline void SetInitialEnergy(Double_t val)   { fInitialEnergy = val;         }
  inline void SetInitialMomentum(TVector3 val) { fInitialMomentum = val;       }

  void SetPDGcode(Int_t val);
  void SetParticleGroup(Int_t val);

  void SetInitial4Momentum(TLorentzVector val)
  { SetInitialMomentum(val.Vect()); SetInitialEnergy(val.T()); }

  inline Int_t    GetPDGcode()           { return fPDGcode;           }
  inline Int_t    GetCharge()            { return fCharge;            }
  inline TString  GetParticleName()      { return fParticleName;      }
  inline Double_t GetInitialEnergy()     { return fInitialEnergy;     }
  inline TVector3 GetInitialMomentum()   { return fInitialMomentum;   }
  inline Int_t    GetParticleGroup()     { return fParticleGroup;     }
  inline TString  GetParticleGroupName() { return fParticleGroupName; }

  TLorentzVector GetInitial4Momentum()
  { return TLorentzVector(fInitialMomentum,fInitialEnergy); }

protected:

  Int_t    fPDGcode;           ///< PDG code
  Int_t    fCharge;            ///< Electric charge, in units of the elementary charge
  TString  fParticleName;
  Double_t fInitialEnergy;     ///< Energy at the production position, MeV
  TVector3 fInitialMomentum;   ///< Momentum at the production position, MeV
  Int_t    fParticleGroup;     ///< Encoding of the "group" (0-3), see GenePart.cc for explanation
  TString  fParticleGroupName; ///< Self-explanatory name of the group

  ClassDef(GenePart,1)
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-12-05
//
// ---------------------------------------------------------------

#ifndef HLTEvent_H
#define HLTEvent_H 1
#include "Rtypes.h"
#include "TObject.h"
#include "TVector3.h"
#include "Riostream.h"

class HLTTrack : public TObject {

  public:

    HLTTrack();
    HLTTrack(const HLTTrack&);
    ~HLTTrack() {}
    void Clear(Option_t* = "");
    void PrintInfo();

    Int_t GetHLTTrackID()                      { return fHLTTrackID;             }
    Int_t GetNHoughIntersections()             { return fNHoughIntersections;    }
    Int_t GetNHoughAdjacents()                 { return fNHoughAdjacents;        }
    Float_t Getdydz()                          { return fdydz;                   }
    Float_t GetQy()                            { return fQy;                     }
    Float_t GetdxdzBeforeMagnet()              { return fdxdzBeforeMagnet;       }
    Float_t GetdxdzAfterMagnet()               { return fdxdzAfterMagnet;        }
    Float_t GetQxBeforeMagnet()                { return fQxBeforeMagnet;         }
    Float_t GetQxAfterMagnet()                 { return fQxAfterMagnet;          }
    Float_t GetZVertex()                       { return fZVertex;                }
    Float_t GetPz()                            { return fPz;                     }
    Float_t GetCDA()                           { return fCDA;                    }
    Float_t GetTrailing()                      { return fTrailing;               }
    TVector3 GetMomentumBeforeMagnet()         { return fMomentumBeforeMagnet;   }
    TVector3 GetMomentumAfterMagnet()          { return fMomentumAfterMagnet;    }
    TVector3 GetVertex()                       { return fVertex;                 }

    void SetHLTTrackID(Int_t element)          { fHLTTrackID = element;          }
    void SetNHoughIntersections(Int_t element) { fNHoughIntersections = element; }
    void SetNHoughAdjacents(Int_t element)     { fNHoughAdjacents = element;     }
    void Setdydz(Float_t element)              { fdydz = element;                }
    void SetQy(Float_t element)                { fQy = element;                  }
    void SetdxdzBeforeMagnet(Float_t element)  { fdxdzBeforeMagnet = element;    }
    void SetdxdzAfterMagnet(Float_t element)   { fdxdzAfterMagnet = element;     }
    void SetQxBeforeMagnet(Float_t element)    { fQxBeforeMagnet = element;      }
    void SetQxAfterMagnet(Float_t element)     { fQxAfterMagnet = element;       }
    void SetZVertex(Float_t element)           { fZVertex = element;             }
    void SetPz(Float_t element)                { fPz = element;                  }
    void SetCDA(Float_t element)               { fCDA = element;                 }
    void SetTrailing(Float_t element)          { fTrailing = element;            }
    void SetMomentumBeforeMagnet(Float_t mx, Float_t my, Float_t pz) { fMomentumBeforeMagnet.SetXYZ(mx*pz, my*pz, pz); }
    void SetMomentumAfterMagnet(Float_t mx, Float_t my, Float_t pz)  { fMomentumAfterMagnet.SetXYZ(mx*pz, my*pz, pz);  }
    void SetVertex(Float_t mx, Float_t qx, Float_t my, Float_t qy, Float_t zvertex) { fVertex.SetXYZ(mx*zvertex+qx, my*zvertex+qy, zvertex); }

  private:

    Int_t    fHLTTrackID;
    Int_t    fNHoughIntersections; // number of intersections in the Hough transform
    Int_t    fNHoughAdjacents;     // number of adjacent points in the Hough transform
    Float_t  fdydz;
    Float_t  fQy;
    Float_t  fdxdzBeforeMagnet;
    Float_t  fdxdzAfterMagnet;
    Float_t  fQxBeforeMagnet;
    Float_t  fQxAfterMagnet;
    Float_t  fZVertex;
    Float_t  fPz;
    Float_t  fCDA;
    Float_t  fTrailing;

    TVector3 fMomentumBeforeMagnet;
    TVector3 fMomentumAfterMagnet;
    TVector3 fVertex;

    ClassDef(HLTTrack,1);
};



class HLTEvent : public TObject {

  public:

    HLTEvent();
    HLTEvent(const HLTEvent&);
    ~HLTEvent();
    void Clear(Option_t* = "");
    void SetKTAGSectors(UInt_t NSecL0TP, UInt_t NSecCHOD);
    void SetKTAGProcessInfo(Bool_t is_l1_KTAG_processed, Bool_t is_l1_KTAG_empty_packet, Bool_t is_l1_KTAG_bad_data);
    void SetKTAGResponse(UChar_t ktag_response);

    void SetLAVHits(UInt_t l1_LAV_n_hits);
    void SetLAVProcessInfo(Bool_t is_l1_LAV_processed, Bool_t is_l1_LAV_empty_packet, Bool_t is_l1_LAV_bad_data);
    void SetLAVResponse(UChar_t lav_response);

    void SetSTRAWNTracks(UInt_t l1_straw_n_tracks);
    void SetSTRAWProcessInfo(Bool_t is_l1_straw_processed, Bool_t is_l1_straw_empty_packet, Bool_t is_l1_straw_bad_data, Bool_t is_l1_straw_overflow);
    void SetSTRAWResponse(UChar_t straw_response);

    UInt_t GetKTAGSectorsL0TP()          { return fNKTAGSectorsL0TP;    }
    UInt_t GetLAVHits()                  { return fNLAVHits;            }
    UInt_t GetNStrawTracks()             { return fNStrawTracks;        }
    std::vector<HLTTrack> GetHLTTracks() { return fHLTTracks;           }
    void AddHLTTracks(HLTTrack track)    { fHLTTracks.push_back(track); }
    UChar_t GetKTAGResponse()            { return fL1KTAGResponse;      }
    UChar_t GetLAVResponse()             { return fL1LAVResponse;       }
    UChar_t GetSTRAWResponse()           { return fL1StrawResponse;     }

    // Boolean functions
    Bool_t IsL1KTAGProcessed()    { return fL1KTAGProcessed;    }
    Bool_t IsL1KTAGEmptyPacket()  { return fL1KTAGEmptyPacket;  }
    Bool_t IsfL1KTAGBadData()     { return fL1KTAGBadData;      }
    Bool_t IsL1LAVProcessed()     { return fL1LAVProcessed;     }
    Bool_t IsL1LAVEmptyPacket()   { return fL1LAVEmptyPacket;   }
    Bool_t IsL1LAVBadData()       { return fL1LAVBadData;       }
    Bool_t IsL1StrawProcessed()   { return fL1StrawProcessed;   }
    Bool_t IsL1StrawEmptyPacket() { return fL1StrawEmptyPacket; }
    Bool_t IsL1StrawBadData()     { return fL1StrawBadData;     }
    Bool_t IsL1StrawOverflow()    { return fL1StrawOverflow;    }

    void  PrintInfo();

  private:

    Float_t fCHODTime;
    Float_t fNewCHODTime;
    UChar_t fL0TPRefTime;

    UChar_t fL1CHODResponse;
    UChar_t fL1KTAGResponse;
    UChar_t fL1LAVResponse;
    UChar_t fL1IRCSACResponse;
    UChar_t fL1StrawResponse;
    UChar_t fL1MUV3Response;
    UChar_t fL1NewCHODResponse;

    Bool_t fL1CHODProcessed;
    Bool_t fL1KTAGProcessed;
    Bool_t fL1LAVProcessed;
    Bool_t fL1IRCSACProcessed;
    Bool_t fL1StrawProcessed;
    Bool_t fL1MUV3TriggerMultiProcessed;
    Bool_t fL1MUV3TriggerLeftRightProcessed;
    Bool_t fL1MUV3TriggerNeighboursProcessed;
    Bool_t fL1NewCHODProcessed;

    Bool_t fL1CHODEmptyPacket;
    Bool_t fL1KTAGEmptyPacket;
    Bool_t fL1LAVEmptyPacket;
    Bool_t fL1IRCSACEmptyPacket;
    Bool_t fL1StrawEmptyPacket;
    Bool_t fL1MUV3EmptyPacket;
    Bool_t fL1NewCHODEmptyPacket;

    Bool_t fL1CHODBadData;
    Bool_t fL1KTAGBadData;
    Bool_t fL1LAVBadData;
    Bool_t fL1IRCSACBadData;
    Bool_t fL1StrawBadData;
    Bool_t fL1MUV3BadData;
    Bool_t fL1NewCHODBadData;

    Bool_t fL1StrawOverflow;

    UInt_t fNCHODHits;
    UInt_t fNKTAGSectorsL0TP;
    UInt_t fNKTAGSectorsCHOD;
    UInt_t fNLAVHits;
    UInt_t fNIRCSACHits;
    UInt_t fNStrawTracks;
    UInt_t fNMUV3Tiles;
    UInt_t fNNewCHODHits;

    std::vector<HLTTrack> fHLTTracks;

    ClassDef(HLTEvent,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)
//            Francesca Bucci (Francesca.Bucci@cern.ch) 2008-01-17
//
// Checkpoints added by E Goudzovski (eg@hep.ph.bham.ac.uk)
//
// Based on a project by Emanuele Leonardi (Emanuele Leonardi@cern.ch) 2007-01-10
//
// --------------------------------------------------------------

#ifndef KinePart_h
#define KinePart_h 1

#include "GenePart.hh"
#include "TVector3.h"
#include "TLorentzVector.h"

class KinePart : public GenePart {

public:

  KinePart();
  virtual ~KinePart() {Clear();}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;
  void PrintCheckPoint(Int_t) const;
  void PrintAllCheckPoints() const;

  inline void ShiftParentIndex(Int_t val)     { if (fParentIndex>=0) fParentIndex += val; }

  inline void SetFinalEnergy(Double_t val)    { fFinalEnergy = val;                       }
  inline void SetFinalMomentum(TVector3 val)  { fFinalMomentum = val;                     }
  inline void SetID(Int_t val)                { fID = val;                                }
  inline void SetParentID(Int_t val)          { fParentID = val;                          }
  inline void SetParentIndex(Int_t val)       { fParentIndex = val;                       }
  inline void SetProdPos(TLorentzVector val)  { fProdPos = val;                           }
  inline void SetEndPos(TLorentzVector val)   { fEndPos = val;                            }
  inline void SetProdProcessName(TString val) { fProdProcessName = val;                   }
  inline void SetProdProcessName(char* val)   { fProdProcessName = TString(val);          }
  inline void SetEndProcessName(TString val)  { fEndProcessName = val;                    }
  inline void SetEndProcessName(char* val)    { fEndProcessName = TString(val);           }
  inline void SetDirectParent(bool val)       { fDirectParent = val;                      }

  void SetFinal4Momentum(TLorentzVector val)
  { SetFinalMomentum(val.Vect()); SetFinalEnergy(val.T()); }

  inline Double_t GetFinalEnergy()            { return fFinalEnergy;                      }
  inline TVector3 GetFinalMomentum()          { return fFinalMomentum;                    }
  inline Int_t GetID()                        { return fID;                               }
  inline Int_t GetParentID()                  { return fParentID;                         }
  inline Int_t GetParentIndex()               { return fParentIndex;                      }
  inline TLorentzVector GetProdPos()          { return fProdPos;                          }
  inline TLorentzVector GetEndPos()           { return fEndPos;                           }
  inline TString GetProdProcessName()         { return fProdProcessName;                  }
  inline TString GetEndProcessName()          { return fEndProcessName;                   }
  inline TLorentzVector GetFinal4Momentum()
  { return TLorentzVector(fFinalMomentum,fFinalEnergy); }
  inline bool GetDirectParent()               { return fDirectParent;                     }

  Double_t xAt(Double_t z, Double_t MagFieldScaleFactor=1.0);
  Double_t yAt(Double_t z);
  Double_t xAtBackwards(Double_t z);
  Double_t yAtBackwards(Double_t z);

  TVector3 GetPosAtZ(Double_t z); ///< Obtained by extrapolating between checkpoints

  // Positions and momenta at the checkpoints:
  // a) addressing the checkpoints by name

  TVector3 GetPosCedarEntry()              { return fPosCedarEntry;        }
  TVector3 GetPosCedarExit()               { return fPosCedarExit;         }
  TVector3 GetPosGigaTrackerEntry()        { return fPosGigaTrackerEntry;  }
  TVector3 GetPosGigaTrackerExit()         { return fPosGigaTrackerExit;   }
  TVector3 GetPosSpectrometerEntry()       { return fPosSpectrometerEntry; }
  TVector3 GetPosMNP33Entry()              { return fPosMNP33Entry;        }
  TVector3 GetPosMNP33Exit()               { return fPosMNP33Exit;         }
  TVector3 GetPosRICHMirrorEntry()         { return fPosRICHMirrorEntry;   }
  TVector3 GetPosCHODEntry()               { return fPosCHODEntry;         }
  TVector3 GetPosLKrEntry()                { return fPosLKrEntry;          }
  TVector3 GetPosMUV1Entry()               { return fPosMUV1Entry;         }
  TVector3 GetPosMUV2Entry()               { return fPosMUV2Entry;         }
  TVector3 GetPosFISCEntry()               { return fPosFISCEntry;         }
  TVector3 GetPosMUV3Entry()               { return fPosMUV3Entry;         }
  TVector3 GetPosXWCMEntry()               { return fPosXWCMEntry;         }

  TLorentzVector GetMomCedarEntry()        { return fMomCedarEntry;        }
  TLorentzVector GetMomCedarExit()         { return fMomCedarExit;         }
  TLorentzVector GetMomGigaTrackerEntry()  { return fMomGigaTrackerEntry;  }
  TLorentzVector GetMomGigaTrackerExit()   { return fMomGigaTrackerExit;   }
  TLorentzVector GetMomSpectrometerEntry() { return fMomSpectrometerEntry; }
  TLorentzVector GetMomMNP33Entry()        { return fMomMNP33Entry;        }
  TLorentzVector GetMomMNP33Exit()         { return fMomMNP33Exit;         }
  TLorentzVector GetMomRICHMirrorEntry()   { return fMomRICHMirrorEntry;   }
  TLorentzVector GetMomCHODEntry()         { return fMomCHODEntry;         }
  TLorentzVector GetMomLKrEntry()          { return fMomLKrEntry;          }
  TLorentzVector GetMomMUV1Entry()         { return fMomMUV1Entry;         }
  TLorentzVector GetMomMUV2Entry()         { return fMomMUV2Entry;         }
  TLorentzVector GetMomFISCEntry()         { return fMomFISCEntry;         }
  TLorentzVector GetMomMUV3Entry()         { return fMomMUV3Entry;         }
  TLorentzVector GetMomXWCMEntry()         { return fMomXWCMEntry;         }

  void SetPosCedarEntry      (TVector3 val)       { fPosCedarEntry = val;       }
  void SetPosCedarExit       (TVector3 val)       { fPosCedarExit = val;        }
  void SetPosGigaTrackerEntry(TVector3 val)       { fPosGigaTrackerEntry = val; }
  void SetPosGigaTrackerExit (TVector3 val)       { fPosGigaTrackerExit = val;  }
  void SetPosMNP33Entry      (TVector3 val)       { fPosMNP33Entry = val;       }
  void SetPosMNP33Exit       (TVector3 val)       { fPosMNP33Exit = val;        }
  void SetPosRICHMirrorEntry (TVector3 val)       { fPosRICHMirrorEntry = val;  }
  void SetPosCHODEntry       (TVector3 val)       { fPosCHODEntry = val;        }
  void SetPosLKrEntry        (TVector3 val)       { fPosLKrEntry = val;         }
  void SetPosMUV1Entry       (TVector3 val)       { fPosMUV1Entry = val;        }
  void SetPosMUV2Entry       (TVector3 val)       { fPosMUV2Entry = val;        }
  void SetPosFISCEntry       (TVector3 val)       { fPosFISCEntry = val;        }
  void SetPosMUV3Entry       (TVector3 val)       { fPosMUV3Entry = val;        }
  void SetPosXWCMEntry       (TVector3 val)       { fPosXWCMEntry = val;        }

  void SetMomCedarEntry      (TLorentzVector val) { fMomCedarEntry = val;       }
  void SetMomCedarExit       (TLorentzVector val) { fMomCedarExit = val;        }
  void SetMomGigaTrackerEntry(TLorentzVector val) { fMomGigaTrackerEntry = val; }
  void SetMomGigaTrackerExit (TLorentzVector val) { fMomGigaTrackerExit = val;  }
  void SetMomMNP33Entry      (TLorentzVector val) { fMomMNP33Entry = val;       }
  void SetMomMNP33Exit       (TLorentzVector val) { fMomMNP33Exit = val;        }
  void SetMomRICHMirrorEntry (TLorentzVector val) { fMomRICHMirrorEntry = val;  }
  void SetMomCHODEntry       (TLorentzVector val) { fMomCHODEntry = val;        }
  void SetMomLKrEntry        (TLorentzVector val) { fMomLKrEntry = val;         }
  void SetMomMUV1Entry       (TLorentzVector val) { fMomMUV1Entry = val;        }
  void SetMomMUV2Entry       (TLorentzVector val) { fMomMUV2Entry = val;        }
  void SetMomFISCEntry       (TLorentzVector val) { fMomFISCEntry = val;        }
  void SetMomMUV3Entry       (TLorentzVector val) { fMomMUV3Entry = val;        }
  void SetMomXWCMEntry       (TLorentzVector val) { fMomXWCMEntry = val;        }

  // b) addressing the checkpoints by ID

  TVector3       GetPosAtCheckPoint(Int_t) const;
  TLorentzVector GetMomAtCheckPoint(Int_t) const;
  void           SetPosAtCheckPoint(Int_t, TVector3);
  void           SetMomAtCheckPoint(Int_t, TLorentzVector);

private:

  Double_t       fFinalEnergy;   ///< Energy at the end position, MeV
  TVector3       fFinalMomentum; ///< Momentum at the end position, MeV
  Int_t          fID; ///< Geant4 track ID
  Int_t          fParentID; ///< Geant4 parent track ID
  Int_t          fParentIndex; ///< Index in KinePart array of the parent particle
  TLorentzVector fProdPos; ///< Production position and time
  TLorentzVector fEndPos;  ///< End position and time
  TString        fProdProcessName; ///< Name of the production process (e.g. "Decay")
  TString        fEndProcessName;  ///< Name of the end process (e.g. "Decay")
  Bool_t         fDirectParent;

  // Positions and momenta at the checkpoints
  TVector3
    fPosCedarEntry,
    fPosCedarExit,
    fPosGigaTrackerEntry,
    fPosGigaTrackerExit,
    fPosSpectrometerEntry,
    fPosMNP33Entry,
    fPosMNP33Exit,
    fPosRICHMirrorEntry,
    fPosCHODEntry,
    fPosLKrEntry,
    fPosMUV1Entry,
    fPosMUV2Entry,
    fPosFISCEntry,
    fPosMUV3Entry,
    fPosXWCMEntry;

  TLorentzVector
    fMomCedarEntry,
    fMomCedarExit,
    fMomGigaTrackerEntry,
    fMomGigaTrackerExit,
    fMomSpectrometerEntry,
    fMomMNP33Entry,
    fMomMNP33Exit,
    fMomRICHMirrorEntry,
    fMomCHODEntry,
    fMomLKrEntry,
    fMomMUV1Entry,
    fMomMUV2Entry,
    fMomFISCEntry,
    fMomMUV3Entry,
    fMomXWCMEntry;

  ClassDef(KinePart,1)
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2014-11-20
//
// ---------------------------------------------------------------

#ifndef L0TPData_H
#define L0TPData_H 1
#include "Rtypes.h"
#include "TObject.h"
#include "NA62Global.hh"

#define L0NMAXDETECTORS 7

// Subdetector Block Header (generic part)
#define O_L0EVENTLENGTH 0
#define O_L0DETECTORID 0
#define O_L0DATABLOCKFORMAT 0
#define O_L0TIMESTAMP 1

#define M_L0EVENTLENGTH 0x0000ffff
#define M_L0DETECTORID 0x00ff0000
#define M_L0DATABLOCKFORMAT 0xff000000
#define M_L0TIMESTAMP 0xffffffff

#define S_L0EVENTLENGTH 0
#define S_L0DETECTORID 16
#define S_L0DATABLOCKFORMAT 24
#define S_L0TIMESTAMP 0

// Subdetector Block Header (L0TP-specific part)
#define O_L0REFFINETIME 0
#define O_L0DATATYPE 0
#define O_L0PRIMITIVES 0
// Primitives in words 0-3
#define O_L0PREVTIMESTAMP 4
#define O_L0TRIGTYPE 5
#define O_L0PREVTRIGTYPE 5
#define O_L0TRIGFLAGS 5
#define O_L0NDELTAPRIM 6
// 32 bits reserved (word 7)
#define O_L0FINETIMES_TRIGGERSLOT 8
// Different format for N-1 and N+1 slots
#define O_L0FINETIMES_OTHERSLOTS 4

#define M_L0REFFINETIME 0x000000ff
#define M_L0DATATYPE 0x0000ff00
#define M_L0PRIMITIVE_MSB 0xffff0000
#define M_L0PRIMITIVE_LSB 0x0000ffff
#define M_L0PREVTIMESTAMP 0xffffffff
#define M_L0TRIGTYPE 0x000000ff
#define M_L0PREVTRIGTYPE 0x0000ff00
#define M_L0TRIGFLAGS 0xffff0000
#define M_L0NDELTAPRIM 0x000000ff
// 32 bits reserved (word 7)
#define M_L0FINETIME 0x000000ff

#define S_L0REFFINETIME 0
#define S_L0DATATYPE 8
#define S_L0PRIMITIVE_MSB 16
#define S_L0PRIMITIVE_LSB 0
#define S_L0PREVTIMESTAMP 0
#define S_L0TRIGTYPE 0
#define S_L0PREVTRIGTYPE 8
#define S_L0TRIGFLAGS 16
#define S_L0NDELTAPRIM 0
// 32 bits reserved (word 7)
#define S_L0FINETIME 0

class L0Primitive;

class L0TPData : public TObject {

public:

  L0TPData();
  ~L0TPData();
  void Clear(Option_t* = "");
  Bool_t   SetHeader(UInt_t *);
  UInt_t   GetEventLength()                  { return fEventLength;         }
  ULong_t  GetTimeStamp()                    { return fTimeStamp;           }
  UChar_t  GetReferenceFineTime()            { return fReferenceFineTime;   }
  void     SetReferenceFineTime(UChar_t val) { fReferenceFineTime = val;    }
  UChar_t  GetDataType()                     { return fDataType;            }
  void     SetDataType(UChar_t val)          { fDataType = val;             }
  ULong_t  GetPreviousTimeStamp()            { return fPreviousTimeStamp;   }
  UChar_t  GetTriggerType()                  { return fTriggerType;         }
  void     SetTriggerType(UChar_t val)       { fTriggerType = val;          }
  UChar_t  GetPreviousTriggerType()          { return fPreviousTriggerType; }
  UShort_t GetTriggerFlags()                 { return fTriggerFlags;        }
  void     SetTriggerFlags(UShort_t val)     { fTriggerFlags = val;         }
  UChar_t  GetNDeltaPrimitives(UInt_t iL0Detector);
  Int_t    GetNPrimitives()                  { return fPrimitives.size();   }
  Bool_t   PrimitiveExists(UInt_t iL0Slot, UInt_t iL0Detector);
  L0Primitive GetPrimitive(UInt_t iL0Slot, UInt_t iL0Detector);
  Int_t    GetPrimitiveCorrectedFineTime(UInt_t iL0Slot, UInt_t iL0Detector,Int_t BitFineTime);
  void     SetPrimitives(std::vector<L0Primitive> &input);
  void     PrintInfo();

private:

  // Subdetector Block Header (generic part)
  UInt_t  fEventLength;
  UInt_t  fDetectorID;        //!  Transient data member
  UInt_t  fDataBlockFormat;   //!  Transient data member
  ULong_t fTimeStamp;
  UInt_t  fNBlockHeaderWords; //!  Transient data member

  // Subdetector Block Header (L0TP-specific part)
  UChar_t   fReferenceFineTime;
  UChar_t   fDataType;
  ULong_t   fPreviousTimeStamp;
  UChar_t   fTriggerType;
  UChar_t   fPreviousTriggerType;
  UShort_t  fTriggerFlags;
  ULong_t   fReserved;
  std::vector<UChar_t>     fNDeltaPrimitives;
  std::vector<Int_t>       fNDeltaPrimitivesMapping; //!  Transient data member
  std::vector<L0Primitive> fPrimitives;

  ClassDef(L0TPData,1);
};

class L0Primitive : public TObject {

  public:

    L0Primitive();
    L0Primitive(const L0Primitive&);
    ~L0Primitive(){};
    void Clear(Option_t* = "");
    UInt_t  GetPrimitiveID()           { return fPrimitiveID;       }
    void    SetPrimitiveID(UInt_t val) { fPrimitiveID = val;        }
    UChar_t GetFineTime()              { return fFineTime;          }
    void    SetFineTime(UChar_t val)   { fFineTime = val;           }
    Int_t   GetCorrectedFineTime(Int_t,Int_t,Int_t);
    void    PrintInfo();

  private:

    UShort_t fPrimitiveID;
    UChar_t fFineTime;

    ClassDef(L0Primitive,1);
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#ifndef L0TPSpecialTrigger_H
#define L0TPSpecialTrigger_H 1
#include "Rtypes.h"
#include "TObject.h"

// Subdetector Block Header (generic part)
#define O_L0EVENTLENGTH 0
#define O_L0DETECTORID 0
#define O_L0DATABLOCKFORMAT 0
#define O_L0TIMESTAMP 1

#define M_L0EVENTLENGTH 0x0000ffff
#define M_L0DETECTORID 0x00ff0000
#define M_L0DATABLOCKFORMAT 0xff000000
#define M_L0TIMESTAMP 0xffffffff

#define S_L0EVENTLENGTH 0
#define S_L0DETECTORID 16
#define S_L0DATABLOCKFORMAT 24
#define S_L0TIMESTAMP 0

// Subdetector Block Header (L0TP-specific part)
#define O_L0DATATYPE 0
#define O_L0LATENCY 0
#define O_L0FINETIMEBITS 1
#define O_L0REFDET 1
#define O_L0CTRLDET 1
#define O_L0CTRLDWN 2
#define O_L0NCTRLTRIGGEN 3
#define O_L0PREVTIMESTAMP 4
#define O_L0TRIGTYPE 5
#define O_L0PREVTRIGTYPE 5
#define O_L0TRIGFLAGS 5
#define O_L0NPRIMITIVES 6
// Primitives in words 6-12
#define O_L0NCHOKES 13
#define O_L0NERRORS 14
#define O_L0NPERIODIC 15
#define O_L0NCALIB  16
#define O_L0NCTRLTRIGSNT 17
// Info from the L0 masks
#define O_L0MASK_ID  18
#define O_L0MASK_DWN 19
#define O_L0MASK_NTRGSENT 20
#define O_L0MASK_NTRGGENERATED 21
#define O_L0MASK_REQUIREDPRIMBIT 22
#define O_L0MASK_DONTCAREPRIMBIT 22
#define O_L0MASK_RESERVED 29

#define M_L0DATATYPE 0x0000ff00
#define M_L0LATENCY 0xffff0000
#define M_L0FINETIMEBITS 0x000000ff
#define M_L0REFDET 0x0000ff00
#define M_L0CTRLDET 0x00ff0000
#define M_L0CTRLDWN 0xffff0000
#define M_L0NCTRLTRIGGEN 0xffffffff
#define M_L0PREVTIMESTAMP 0xffffffff
#define M_L0TRIGTYPE 0x000000ff
#define M_L0PREVTRIGTYPE 0x0000ff00
#define M_L0TRIGFLAGS 0xffff0000
#define M_L0NPRIMITIVES 0xffffffff
#define M_L0NCHOKES 0xffffffff
#define M_L0NERRORS 0xffffffff
#define M_L0NPERIODIC 0xffffffff
#define M_L0NCALIB  0xffffffff
#define M_L0NCTRLTRIGSNT 0xffffffff
// Info from the L0 masks
#define M_L0MASK_ID  0x000000ff
#define M_L0MASK_DWN 0xffffffff
#define M_L0MASK_NTRGSENT 0xffffffff
#define M_L0MASK_NTRGGENERATED 0xffffffff
#define M_L0MASK_REQUIREDPRIMBIT 0x0000ffff
#define M_L0MASK_DONTCAREPRIMBIT 0xffff0000
#define M_L0MASK_RESERVED 0xffffffff

#define S_L0DATATYPE 8
#define S_L0LATENCY 16
#define S_L0FINETIMEBITS 0
#define S_L0REFDET 8
#define S_L0CTRLDET 16
#define S_L0CTRLDWN 16
#define S_L0NCTRLTRIGGEN 0
#define S_L0PREVTIMESTAMP 0
#define S_L0TRIGTYPE 0
#define S_L0PREVTRIGTYPE 8
#define S_L0TRIGFLAGS 16
#define S_L0NPRIMITIVES 0
#define S_L0NCHOKES 0
#define S_L0NERRORS 0
#define S_L0NPERIODIC 0
#define S_L0NCALIB  0
#define S_L0NCTRLTRIGSNT 0
// Info from the L0 masks
#define S_L0MASK_ID  0
#define S_L0MASK_DWN 0
#define S_L0MASK_NTRGSENT 0
#define S_L0MASK_NTRGGENERATED 0
#define S_L0MASK_REQUIREDPRIMBIT 0
#define S_L0MASK_DONTCAREPRIMBIT 16
#define S_L0MASK_RESERVED 0

class L0Mask;

class L0TPSpecialTrigger : public TObject {

  public:

    L0TPSpecialTrigger();
    ~L0TPSpecialTrigger();
    void Clear(Option_t* = "");
    Bool_t  SetHeader(UInt_t *);
    UInt_t  GetEventLength()                     { return fEventLength;                     };
    ULong_t GetTimeStamp()                       { return fTimeStamp;                       };

    UInt_t  GetDataType()                        { return fDataType;                        };
    UInt_t  GetLatency()                         { return fLatency;                         };
    UInt_t  GetFineTimeBits()                    { return fFineTimeBits;                    };
    ULong_t GetReferenceDetector()               { return fReferenceDetector;               };
    ULong_t GetControlDetector()                 { return fControlDetector;                 };
    ULong_t GetPreviousTimeStamp()               { return fPreviousTimeStamp;               };
    UInt_t  GetTriggerType()                     { return fTriggerType;                     };
    UInt_t  GetPreviousTriggerType()             { return fPreviousTriggerType;             };
    UInt_t  GetTriggerFlags()                    { return fTriggerFlags;                    };
    UInt_t  GetNMaxDetectors()                   { return fNMaxDetectors;                   };
    std::vector<UInt_t> GetNPrimitives()         { return fNPrimitives;                     };
    UInt_t  GetNChokeTriggers()                  { return fNChokeTriggers;                  };
    UInt_t  GetNErrorTriggers()                  { return fNErrorTriggers;                  };
    UInt_t  GetNPeriodicTriggers()               { return fNPeriodicTriggers;               };
    UInt_t  GetNCalibTriggers()                  { return fNCalibTriggers;                  };
    UInt_t  GetNControlTriggersSent()            { return fNControlTriggersSent;            };
    UInt_t  GetNControlTriggersGenerated()       { return fNControlTriggersGenerated;       };
    UInt_t  GetControlTriggerDownscalingFactor() { return fControlTriggerDownscalingFactor; };
    std::vector<L0Mask> GetL0Masks()             { return fL0Masks;                         };

  private:

    // Subdetector Block Header (generic part)
    UInt_t  fEventLength;
    UInt_t  fDetectorID;        //!  Transient data member
    UInt_t  fDataBlockFormat;   //!  Transient data member
    ULong_t fTimeStamp;
    UInt_t  fNBlockHeaderWords; //!  Transient data member

    // Subdetector Block Header (L0TP-specific part)
    UInt_t  fDataType;            
    UInt_t  fLatency;             
    UInt_t  fFineTimeBits;        
    UInt_t  fReferenceDetector;  
    UInt_t  fControlDetector;  
    ULong_t fPreviousTimeStamp;   
    UInt_t  fTriggerType;         
    UInt_t  fPreviousTriggerType; 
    UInt_t  fTriggerFlags;        
    UInt_t  fNMaxDetectors;      //!  Transient data member    
    std::vector<UInt_t> fNPrimitives;         
    UInt_t  fNChokeTriggers;      
    UInt_t  fNErrorTriggers;      
    UInt_t  fNPeriodicTriggers;     
    UInt_t  fNCalibTriggers;      
    UInt_t  fNControlTriggersSent;  
    UInt_t  fNControlTriggersGenerated;  
    UInt_t  fControlTriggerDownscalingFactor;
    UInt_t  fReserved;      
    // Info from the L0 masks
    std::vector<L0Mask> fL0Masks;

    ClassDef(L0TPSpecialTrigger,1);
};

class L0Mask : public TObject {

  public:

    L0Mask();
    L0Mask(const L0Mask&);
    ~L0Mask(){};
    void Clear(Option_t* = "");
    UInt_t GetMaskID()                                     { return fMaskID;              };
    void   SetMaskID(UInt_t val)                           { fMaskID = val;               };
    UInt_t GetDownscalingFactor()                          { return fDownscalingFactor;   };
    void   SetDownscalingFactor(UInt_t val)                { fDownscalingFactor = val;    };
    UInt_t GetNTriggersSent()                              { return fNTriggersSent;       };
    void   SetNTriggersSent(UInt_t val)                    { fNTriggersSent = val;        };
    UInt_t GetNTriggersGenerated()                         { return fNTriggersGenerated;  };
    void   SetNTriggersGenerated(UInt_t val)               { fNTriggersGenerated = val;   };
    std::vector<UInt_t> GetRequiredPrimBitMask()           { return fRequiredPrimBitMask; };
    void   SetRequiredPrimBitMask(std::vector<UInt_t> &val){ fRequiredPrimBitMask = val;  };
    std::vector<UInt_t> GetDontcarePrimBitMask()           { return fDontcarePrimBitMask; };
    void   SetDontcarePrimBitMask(std::vector<UInt_t> &val){ fDontcarePrimBitMask = val;  };

  private:

    UInt_t              fMaskID;              
    UInt_t              fDownscalingFactor;         
    UInt_t              fNTriggersSent;       
    UInt_t              fNTriggersGenerated;  
    std::vector<UInt_t> fRequiredPrimBitMask; 
    std::vector<UInt_t> fDontcarePrimBitMask; 

    ClassDef(L0Mask,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-26
//
// ---------------------------------------------------------------

#ifndef L1TPData_H
#define L1TPData_H 1
#include "Rtypes.h"
#include "TObject.h"

// Subdetector Block Header (generic part)
#define O_L1EVENTLENGTH 0
#define O_L1DETECTORID 0
#define O_L1DATABLOCKFORMAT 0
#define O_L1TIMESTAMP 1

#define M_L1EVENTLENGTH 0x0000ffff
#define M_L1DETECTORID 0x00ff0000
#define M_L1DATABLOCKFORMAT 0xff000000
#define M_L1TIMESTAMP 0xffffffff

#define S_L1EVENTLENGTH 0
#define S_L1DETECTORID 16
#define S_L1DATABLOCKFORMAT 24
#define S_L1TIMESTAMP 0

// Subdetector Block Header (L1TP-specific part)
#define O_L1FORMAT 0
#define O_L1FLAGMODE 0
#define O_L1REFERENCEDETECTOR 0
#define O_L1REFERENCEFINETIME 0
#define O_L1GLOBALHEADERLENGTH 0
#define O_L1GLOBALREDUCTION 1
#define O_L1GLOBALDOWNSCALING 1 
#define O_L1AUTOFLAGFACTOR 2
#define O_L1BYPASSFACTOR 2
#define O_L1NENABLEDMASKS 2

#define M_L1FORMAT 0xff000000
#define M_L1FLAGMODE 0x00f00000
#define M_L1REFERENCEDETECTOR 0x000f0000
#define M_L1REFERENCEFINETIME 0x0000ff00
#define M_L1GLOBALHEADERLENGTH 0x000000ff
#define M_L1GLOBALREDUCTION 0xffff0000
#define M_L1GLOBALDOWNSCALING 0x0000ffff
#define M_L1AUTOFLAGFACTOR 0xffff0000
#define M_L1BYPASSFACTOR 0x0000ff00
#define M_L1NENABLEDMASKS 0x000000ff

#define S_L1FORMAT 24
#define S_L1FLAGMODE 20
#define S_L1REFERENCEDETECTOR 16
#define S_L1REFERENCEFINETIME 8
#define S_L1GLOBALHEADERLENGTH 0
#define S_L1GLOBALREDUCTION 16
#define S_L1GLOBALDOWNSCALING 0 
#define S_L1AUTOFLAGFACTOR 16
#define S_L1BYPASSFACTOR 8
#define S_L1NENABLEDMASKS 0

//L0 mask block
#define O_L1FLAGS 0
#define O_L1MASKID 0
#define O_L1TRIGGERWORD 0
#define O_L1NENABLEDALGOS 0
#define O_L1REFDETECTOR 1
#define O_L1REFFINETIME 1
#define O_L1REDUCTION 1

#define M_L1FLAGS 0xff000000
#define M_L1MASKID 0x00ff0000
#define M_L1TRIGGERWORD 0x0000ff00
#define M_L1NENABLEDALGOS 0x000000ff
#define M_L1REFDETECTOR 0xff000000
#define M_L1REFFINETIME 0x00ff0000
#define M_L1REDUCTION 0x0000ffff

#define S_L1FLAGS 24
#define S_L1MASKID 16
#define S_L1TRIGGERWORD 8
#define S_L1NENABLEDALGOS 0
#define S_L1REFDETECTOR 24
#define S_L1REFFINETIME 16
#define S_L1REDUCTION 0

//L1 algo block
#define O_L1QUALITYFLAGS 0
#define O_L1ALGOID 0
#define O_L1PROCESSID 0
#define O_L1NALGOWORDS 0
#define O_L1ALGOFLAGS 1
#define O_L1TIMEWINDOW 1
#define O_L1DOWNSCALING 1

#define M_L1QUALITYFLAGS 0xff000000
#define M_L1ALGOID 0x00ff0000
#define M_L1PROCESSID 0x0000ff00
#define M_L1NALGOWORDS 0x000000ff
#define M_L1ALGOFLAGS 0xff000000
#define M_L1TIMEWINDOW 0x00ff0000
#define M_L1DOWNSCALING 0x0000ffff

#define S_L1QUALITYFLAGS 24
#define S_L1ALGOID 16
#define S_L1PROCESSID 8
#define S_L1NALGOWORDS 0
#define S_L1ALGOFLAGS 24
#define S_L1TIMEWINDOW 16
#define S_L1DOWNSCALING 0

class L1AlgoBlock;
class L1MaskBlock;

class L1TPData : public TObject {

  public:

    L1TPData();
    L1TPData(const L1TPData&);
    ~L1TPData();
    void Clear(Option_t* = "");
    Bool_t   SetHeader(UInt_t *);
    UInt_t   GetEventLength()                { return fEventLength;         }
    ULong_t  GetTimeStamp()                  { return fTimeStamp;           }
    void     SetTimeStamp(ULong_t val)       { fTimeStamp = val;            }
    UChar_t  GetL1FlagMode()                 { return fL1FlagMode;          }
    UChar_t  GetL1ReferenceDetector()        { return fL1ReferenceDetector; }
    UChar_t  GetL1ReferenceFineTime()        { return fL1ReferenceFineTime; }
    void SetL1ReferenceFineTime(UChar_t val) { fL1ReferenceFineTime = val;  }
    UShort_t GetL1ReductionFactor()          { return fL1ReductionFactor;   }
    UShort_t GetL1DownscalingFactor()        { return fL1DownscalingFactor; }
    UShort_t GetL1AutoflagFactor()           { return fL1AutoflagFactor;    }
    UChar_t  GetL1BypassFactor()             { return fL1BypassFactor;      }
    std::vector<L1MaskBlock> GetL0Masks()    { return fL0Masks;             }
    void     PrintInfo();

  private:

    // Subdetector Block Header (generic part)
    UInt_t  fEventLength;
    UInt_t  fDetectorID;           //!  Transient data member
    UInt_t  fDataBlockFormat;      //!  Transient data member
    ULong_t fTimeStamp;
    UInt_t  fNBlockHeaderWords;    //!  Transient data member

    // Subdetector Block Header (L1TP-specific part)
    UChar_t  fL1Format;             //!  Transient data member
    UChar_t  fL1FlagMode;
    UChar_t  fL1ReferenceDetector;
    UChar_t  fL1ReferenceFineTime;
    UChar_t  fL1GlobalHeaderLength; //!  Transient data member
    UShort_t fL1ReductionFactor;
    UShort_t fL1DownscalingFactor;
    UShort_t fL1AutoflagFactor;
    UChar_t  fL1BypassFactor;
    UChar_t  fL1NEnabledMasks;      //! Transient data member
    std::vector<L1MaskBlock> fL0Masks;

    ClassDef(L1TPData,1);
};


class L1MaskBlock : public TObject {

  public:

    L1MaskBlock();
    L1MaskBlock(const L1MaskBlock&);
    ~L1MaskBlock() {}
    void Clear(Option_t* = "");
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              }
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               }
    UChar_t  GetL1Flags()                        { return fL1Flags;               }
    void     SetL1Flags(UChar_t val)             { fL1Flags = val;                }
    UChar_t  GetL1TriggerWord()                  { return fL1TriggerWord;         }
    void     SetL1TriggerWord(UChar_t val)       { fL1TriggerWord = val;          }
    UChar_t  GetL1NEnabledAlgos()                { return fL1NEnabledAlgos;       }
    void     SetL1NEnabledAlgos(UChar_t val)     { fL1NEnabledAlgos = val;        }
    UChar_t  GetL1ReferenceDetector()            { return fL1ReferenceDetector;   }
    void     SetL1ReferenceDetector(UChar_t val) { fL1ReferenceDetector = val;    }
    UChar_t  GetL1ReferenceFineTime()            { return fL1ReferenceFineTime;   }
    void     SetL1ReferenceFineTime(UChar_t val) { fL1ReferenceFineTime = val;    }
    UShort_t GetL1ReductionFactor()              { return fL1ReductionFactor;     }
    void     SetL1ReductionFactor(UShort_t val)  { fL1ReductionFactor = val;      }
    std::vector<L1AlgoBlock> GetL1Algorithms()   { return fL1Algorithms;          }
    void     SetL1Algorithms(std::vector<L1AlgoBlock> &val){ fL1Algorithms = val; }
    void     PrintInfo();

  private:

    UChar_t  fL0MaskID;
    UChar_t  fL1Flags;
    UChar_t  fL1TriggerWord;
    UChar_t  fL1NEnabledAlgos;      //! Transient data member
    UChar_t  fL1ReferenceDetector;
    UChar_t  fL1ReferenceFineTime;
    UShort_t fL1ReductionFactor;
    std::vector<L1AlgoBlock> fL1Algorithms;

    ClassDef(L1MaskBlock,1);
};

class L1AlgoBlock : public TObject {

  public:

    L1AlgoBlock();
    L1AlgoBlock(const L1AlgoBlock&);
    ~L1AlgoBlock(){};
    void Clear(Option_t* = "");
    UChar_t  GetL1AlgoID()                          { return fL1AlgoID;            }
    void     SetL1AlgoID(UChar_t val)               { fL1AlgoID = val;             }
    UChar_t  GetL1QualityFlags()                    { return fL1QualityFlags;      }
    void     SetL1QualityFlags(UChar_t val)         { fL1QualityFlags = val;       }
    UChar_t  GetL1ProcessID()                       { return fL1ProcessID;         }
    void     SetL1ProcessID(UChar_t val)            { fL1ProcessID = val;          }
    UChar_t  GetL1NAlgoWords()                      { return fL1NAlgoWords;        }
    void     SetL1NAlgoWords(UChar_t val)           { fL1NAlgoWords = val;         }
    UChar_t  GetL1AlgoFlags()                       { return fL1AlgoFlags;         }
    void     SetL1AlgoFlags(UChar_t val)            { fL1AlgoFlags = val;          }
    UChar_t  GetL1TimeWindow()                      { return fL1TimeWindow;        }
    void     SetL1TimeWindow(UChar_t val)           { fL1TimeWindow = val;         }
    UShort_t GetL1DownscalingFactor()               { return fL1DownscalingFactor; }
    void     SetL1DownscalingFactor(UShort_t val)   { fL1DownscalingFactor = val;  }
    std::vector<UInt_t> GetL1DataWords()            { return fL1DataWords;         }
    void     SetL1DataWords(std::vector<UInt_t> &val){ fL1DataWords = val;          }

  private:

    UChar_t  fL1AlgoID;             // 8-bit word
    UChar_t  fL1QualityFlags;
    UChar_t  fL1ProcessID;
    UChar_t  fL1NAlgoWords;         //! Transient data member
    UChar_t  fL1AlgoFlags;
    UChar_t  fL1TimeWindow;
    UShort_t fL1DownscalingFactor; // 16-bit word
    std::vector<UInt_t> fL1DataWords;

    ClassDef(L1AlgoBlock,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#ifndef L1TPSpecialTrigger_H
#define L1TPSpecialTrigger_H 1
#include "Rtypes.h"
#include "TObject.h"

#define O_L1SPTRGBLOCKLENGTH 0
#define O_L1SPTRGL1PCID 0
#define O_L1SPTRGDETECTORID 0
#define O_L1SPTRGTIMESTAMP 1
#define O_L1SPTRGDATAFORMAT 2
#define O_L1SPTRGTIMEOUTFLAG 2
// Reserved (word 3)
#define O_L1SPTRGNL1INPUTEVTS 4
#define O_L1SPTRGNL1SPECIALEVTS 5
#define O_L1SPTRGNL1CONTROLEVTS 6
#define O_L1SPTRGNL1PERIODICEVTS 7
#define O_L1SPTRGNL1PHYSICSEVTS 8
#define O_L1SPTRGNL1PHYSICSEVTSMM 9
#define O_L1SPTRGNL1DATAREQUESTS 10
#define O_L1SPTRGNL1OUTPUTEVTS 11
#define O_L1SPTRGNL1ACCEPTEDEVTS 12
#define O_L1SPTRGNL1TIMEOUTEVTS 13
#define O_L1SPTRGNL1ALLDISABLEDEVTS 14
#define O_L1SPTRGNL1BYPASSEDEVTS 15
#define O_L1SPTRGNL1FLAGALGOEVTS 16
#define O_L1SPTRGNL1AUTOPASSEVTS 17
#define O_L1SPTRGL1MASKSINFO 18

#define M_L1SPTRGBLOCKLENGTH 0x0000ffff
#define M_L1SPTRGL1PCID 0x00ff0000
#define M_L1SPTRGDETECTORID 0xff000000
#define M_L1SPTRGTIMESTAMP 0xffffffff
#define M_L1SPTRGDATAFORMAT 0x000000ff
#define M_L1SPTRGTIMEOUTFLAG 0x0000ff00

#define S_L1SPTRGBLOCKLENGTH 0
#define S_L1SPTRGL1PCID 16
#define S_L1SPTRGDETECTORID 24
#define S_L1SPTRGTIMESTAMP 0
#define S_L1SPTRGDATAFORMAT 0
#define S_L1SPTRGTIMEOUTFLAG 8

//--- L1Mask block
// Reserved (word 0)
#define O_L1SPMASKNINPUTEVTS 1
#define O_L1SPMASKNOUTPUTNEVTS 2

class L1MaskSpecialBlock;
class L1PCSpecialBlock;

class L1TPSpecialTrigger : public TObject {

  public:

    L1TPSpecialTrigger();
    ~L1TPSpecialTrigger();
    void Clear(Option_t* = "");
    Bool_t  AddPCInfo(UInt_t *);

    std::vector<L1PCSpecialBlock> GetL1PCsInfo()  { return fL1PCsInfo;   }

  private:

    std::vector<L1PCSpecialBlock> fL1PCsInfo;

    ClassDef(L1TPSpecialTrigger,1);
};

class L1PCSpecialBlock : public TObject {

public:

  L1PCSpecialBlock();
  L1PCSpecialBlock(const L1PCSpecialBlock&);
  ~L1PCSpecialBlock() {}
  void Clear(Option_t* = "");

  UInt_t  GetBlockLength()                                 { return  fBlockLength;                     }
  void    SetBlockLength(UInt_t value)                     { fBlockLength = value;                     }
  UChar_t GetL1PCID()                                      { return  fL1PCID;                          }
  void    SetL1PCID(UChar_t value)                         { fL1PCID = value;                          }
  ULong_t GetTimeStamp()                                   { return  fTimeStamp;                       }
  void    SetTimeStamp(ULong_t value)                      { fTimeStamp = value;                       }
  UChar_t GetDataFormat()                                  { return  fDataFormat;                      }
  void    SetDataFormat(UChar_t value)                     { fDataFormat = value;                      }
  UChar_t GetTimeoutFlag()                                 { return  fTimeoutFlag;                     }
  void    SetTimeoutFlag(UChar_t value)                    { fTimeoutFlag = value;                     }
  UInt_t  GetNL1InputEvents()                              { return  fNL1InputEvents;                  }
  void    SetNL1InputEvents(UInt_t value)                  { fNL1InputEvents = value;                  }
  UInt_t  GetNL1SpecialEvents()                            { return  fNL1SpecialEvents;                }
  void    SetNL1SpecialEvents(UInt_t value)                { fNL1SpecialEvents = value;                }
  UInt_t  GetNL1ControlEvents()                            { return  fNL1ControlEvents;                }
  void    SetNL1ControlEvents(UInt_t value)                { fNL1ControlEvents = value;                }
  UInt_t  GetNL1PeriodicEvents()                           { return  fNL1PeriodicEvents;               }
  void    SetNL1PeriodicEvents(UInt_t value)               { fNL1PeriodicEvents = value;               }
  UInt_t  GetNL1PhysicsEvents()                            { return  fNL1PhysicsEvents;                }
  void    SetNL1PhysicsEvents(UInt_t value)                { fNL1PhysicsEvents = value;                }
  UInt_t  GetNL1PhysicsEventsInMultipleMasks()             { return  fNL1PhysicsEventsInMultipleMasks; }
  void    SetNL1PhysicsEventsInMultipleMasks(UInt_t value) { fNL1PhysicsEventsInMultipleMasks = value; }
  UInt_t  GetNL1DataRequests()                             { return  fNL1DataRequests;                 }
  void    SetNL1DataRequests(UInt_t value)                 { fNL1DataRequests = value;                 }
  UInt_t  GetNL1OutputEvents()                             { return  fNL1OutputEvents;                 }
  void    SetNL1OutputEvents(UInt_t value)                 { fNL1OutputEvents = value;                 }
  UInt_t  GetNL1AcceptedEvents()                           { return  fNL1AcceptedEvents;               }
  void    SetNL1AcceptedEvents(UInt_t value)               { fNL1AcceptedEvents = value;               }
  UInt_t  GetNL1TimeoutEvents()                            { return  fNL1TimeoutEvents;                }
  void    SetNL1TimeoutEvents(UInt_t value)                { fNL1TimeoutEvents = value;                }
  UInt_t  GetNL1AllDisabledEvents()                        { return  fNL1AllDisabledEvents;            }
  void    SetNL1AllDisabledEvents(UInt_t value)            { fNL1AllDisabledEvents = value;            }
  UInt_t  GetNL1BypassedEvents()                           { return  fNL1BypassedEvents;               }
  void    SetNL1BypassedEvents(UInt_t value)               { fNL1BypassedEvents = value;               }
  UInt_t  GetNL1FlagAlgoEvents()                           { return  fNL1FlagAlgoEvents;               }
  void    SetNL1FlagAlgoEvents(UInt_t value)               { fNL1FlagAlgoEvents = value;               }
  UInt_t  GetNL1AutopassEvents()                           { return  fNL1AutopassEvents;               }
  void    SetNL1AutopassEvents(UInt_t value)               { fNL1AutopassEvents = value;               }
  std::vector<L1MaskSpecialBlock> GetL1MasksInfo()         { return  fL1MasksInfo;                     }
  void    SetL1MasksInfo(std::vector<L1MaskSpecialBlock> &value) { fL1MasksInfo = value;                }

private:

  UInt_t  fBlockLength;
  UChar_t fL1PCID;
  UInt_t  fDetectorID;        //!  Transient data member
  ULong_t fTimeStamp;
  UChar_t fDataFormat;
  UChar_t fTimeoutFlag;
  UInt_t  fReserved;
  UInt_t  fNL1InputEvents;      ///< Total number of input events
  UInt_t  fNL1SpecialEvents;
  UInt_t  fNL1ControlEvents;    ///< Number of input control triggers
  UInt_t  fNL1PeriodicEvents;   ///< Number of input periodic triggers
  UInt_t  fNL1PhysicsEvents;    ///< Number of input special triggers
  UInt_t  fNL1PhysicsEventsInMultipleMasks;
  UInt_t  fNL1DataRequests;
  UInt_t  fNL1OutputEvents;     ///< Total number of output events
  UInt_t  fNL1AcceptedEvents;
  UInt_t  fNL1TimeoutEvents;
  UInt_t  fNL1AllDisabledEvents;
  UInt_t  fNL1BypassedEvents;
  UInt_t  fNL1FlagAlgoEvents;
  UInt_t  fNL1AutopassEvents;
  std::vector<L1MaskSpecialBlock> fL1MasksInfo;

  ClassDef(L1PCSpecialBlock,1);
};

class L1MaskSpecialBlock : public TObject {

  public:

    L1MaskSpecialBlock();
    L1MaskSpecialBlock(const L1MaskSpecialBlock&);
    ~L1MaskSpecialBlock() {}
    void Clear(Option_t* = "");
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              }
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               }
    UInt_t   GetNL1InputEvents()                 { return fNL1InputEvents;        }
    void     SetNL1InputEvents(UInt_t val)       { fNL1InputEvents = val;         }
    UInt_t   GetNL1OutputEvents()                { return fNL1OutputEvents;       }
    void     SetNL1OutputEvents(UInt_t val)      { fNL1OutputEvents = val;        }

  private:

    UChar_t    fL0MaskID;
    UInt_t     fNL1InputEvents;
    UInt_t     fNL1OutputEvents;
    UInt_t     fReserved;

    ClassDef(L1MaskSpecialBlock,1);
};

#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-26
//
// ---------------------------------------------------------------

#ifndef L2EBData_H
#define L2EBData_H 1
#include "Rtypes.h"
#include "TObject.h"

// Subdetector Block Header (generic part)
#define O_L2EVENTLENGTH 0
#define O_L2DETECTORID 0
#define O_L2DATABLOCKFORMAT 0
#define O_L2TIMESTAMP 1

#define M_L2EVENTLENGTH 0x0000ffff
#define M_L2DETECTORID 0x00ff0000
#define M_L2DATABLOCKFORMAT 0xff000000
#define M_L2TIMESTAMP 0xffffffff

#define S_L2EVENTLENGTH 0
#define S_L2DETECTORID 16
#define S_L2DATABLOCKFORMAT 24
#define S_L2TIMESTAMP 0

// Subdetector Block Header (L2EB-specific part)
#define O_L2FORMAT 0
#define O_L2FLAGMODE 0
#define O_L2REFERENCEDETECTOR 0
#define O_L2REFERENCEFINETIME 0
#define O_L2GLOBALHEADERLENGTH 0
#define O_L2GLOBALREDUCTION 1
#define O_L2GLOBALDOWNSCALING 1 
#define O_L2AUTOFLAGFACTOR 2
#define O_L2BYPASSFACTOR 2
#define O_L2NENABLEDMASKS 2

#define M_L2FORMAT 0xff000000
#define M_L2FLAGMODE 0x00f00000
#define M_L2REFERENCEDETECTOR 0x000f0000
#define M_L2REFERENCEFINETIME 0x0000ff00
#define M_L2GLOBALHEADERLENGTH 0x000000ff
#define M_L2GLOBALREDUCTION 0xffff0000
#define M_L2GLOBALDOWNSCALING 0x0000ffff
#define M_L2AUTOFLAGFACTOR 0xffff0000
#define M_L2BYPASSFACTOR 0x0000ff00
#define M_L2NENABLEDMASKS 0x000000ff

#define S_L2FORMAT 24
#define S_L2FLAGMODE 20
#define S_L2REFERENCEDETECTOR 16
#define S_L2REFERENCEFINETIME 8
#define S_L2GLOBALHEADERLENGTH 0
#define S_L2GLOBALREDUCTION 16
#define S_L2GLOBALDOWNSCALING 0 
#define S_L2AUTOFLAGFACTOR 16
#define S_L2BYPASSFACTOR 8
#define S_L2NENABLEDMASKS 0

//L0 mask block
#define O_L2FLAGS 0
#define O_L2MASKID 0
#define O_L2TRIGGERWORD 0
#define O_L2NENABLEDALGOS 0
#define O_L2REFDETECTOR 1
#define O_L2REFFINETIME 1
#define O_L2REDUCTION 1

#define M_L2FLAGS 0xff000000
#define M_L2MASKID 0x00ff0000
#define M_L2TRIGGERWORD 0x0000ff00
#define M_L2NENABLEDALGOS 0x000000ff
#define M_L2REFDETECTOR 0xff000000
#define M_L2REFFINETIME 0x00ff0000
#define M_L2REDUCTION 0x0000ffff

#define S_L2FLAGS 24
#define S_L2MASKID 16
#define S_L2TRIGGERWORD 8
#define S_L2NENABLEDALGOS 0
#define S_L2REFDETECTOR 24
#define S_L2REFFINETIME 16
#define S_L2REDUCTION 0

//L2 algo block
#define O_L2QUALITYFLAGS 0
#define O_L2ALGOID 0
#define O_L2PROCESSID 0
#define O_L2NALGOWORDS 0
#define O_L2ALGOFLAGS 1
#define O_L2TIMEWINDOW 1
#define O_L2DOWNSCALING 1

#define M_L2QUALITYFLAGS 0xff000000
#define M_L2ALGOID 0x00ff0000
#define M_L2PROCESSID 0x0000ff00
#define M_L2NALGOWORDS 0x000000ff
#define M_L2ALGOFLAGS 0xff000000
#define M_L2TIMEWINDOW 0x00ff0000
#define M_L2DOWNSCALING 0x0000ffff

#define S_L2QUALITYFLAGS 24
#define S_L2ALGOID 16
#define S_L2PROCESSID 8
#define S_L2NALGOWORDS 0
#define S_L2ALGOFLAGS 24
#define S_L2TIMEWINDOW 16
#define S_L2DOWNSCALING 0

class L2AlgoBlock;
class L2MaskBlock;

class L2EBData : public TObject {

  public:

    L2EBData();
    L2EBData(const L2EBData&);
    ~L2EBData();
    void Clear(Option_t* = "");
    Bool_t   SetHeader(UInt_t *);
    UInt_t   GetEventLength()                    { return fEventLength;         }
    ULong_t  GetTimeStamp()                      { return fTimeStamp;           }
    void     SetTimeStamp(ULong_t val)       { fTimeStamp = val;            }
    UChar_t  GetL2FlagMode()                     { return fL2FlagMode;          }
    UChar_t  GetL2ReferenceDetector()            { return fL2ReferenceDetector; }
    UChar_t  GetL2ReferenceFineTime()            { return fL2ReferenceFineTime; }
    void     SetL2ReferenceFineTime(UChar_t val) { fL2ReferenceFineTime = val;  }
    UShort_t GetL2ReductionFactor()              { return fL2ReductionFactor;   }
    UShort_t GetL2DownscalingFactor()            { return fL2DownscalingFactor; }
    UShort_t GetL2AutoflagFactor()               { return fL2AutoflagFactor;    }
    UChar_t  GetL2BypassFactor()                 { return fL2BypassFactor;      }
    std::vector<L2MaskBlock> GetL0Masks()        { return fL0Masks;             }
    void     PrintInfo();

  private:

    // Subdetector Block Header (generic part)
    UInt_t  fEventLength;
    UInt_t  fDetectorID;           //!  Transient data member
    UInt_t  fDataBlockFormat;      //!  Transient data member
    ULong_t fTimeStamp;
    UInt_t  fNBlockHeaderWords;    //!  Transient data member

    // Subdetector Block Header (L2EB-specific part)
    UChar_t  fL2Format;             //!  Transient data member
    UChar_t  fL2FlagMode;
    UChar_t  fL2ReferenceDetector;
    UChar_t  fL2ReferenceFineTime;
    UChar_t  fL2GlobalHeaderLength; //!  Transient data member
    UShort_t fL2ReductionFactor;
    UShort_t fL2DownscalingFactor;
    UShort_t fL2AutoflagFactor;
    UChar_t  fL2BypassFactor;
    UChar_t  fL2NEnabledMasks;      //! Transient data member
    std::vector<L2MaskBlock> fL0Masks;

    ClassDef(L2EBData,1);
};


class L2MaskBlock : public TObject {

  public:

    L2MaskBlock();
    L2MaskBlock(const L2MaskBlock&);
    ~L2MaskBlock(){};
    void Clear(Option_t* = "");
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              };
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               };
    UChar_t  GetL2Flags()                        { return fL2Flags;               };
    void     SetL2Flags(UChar_t val)             { fL2Flags = val;                };
    UChar_t  GetL2TriggerWord()                  { return fL2TriggerWord;         };
    void     SetL2TriggerWord(UChar_t val)       { fL2TriggerWord = val;          };
    UChar_t  GetL2NEnabledAlgos()                { return fL2NEnabledAlgos;       };
    void     SetL2NEnabledAlgos(UChar_t val)     { fL2NEnabledAlgos = val;        };
    UChar_t  GetL2ReferenceDetector()            { return fL2ReferenceDetector;   };
    void     SetL2ReferenceDetector(UChar_t val) { fL2ReferenceDetector = val;    };
    UChar_t  GetL2ReferenceFineTime()            { return fL2ReferenceFineTime;   };
    void     SetL2ReferenceFineTime(UChar_t val) { fL2ReferenceFineTime = val;    };
    UShort_t GetL2ReductionFactor()              { return fL2ReductionFactor;     };
    void     SetL2ReductionFactor(UShort_t val)  { fL2ReductionFactor = val;      };
    std::vector<L2AlgoBlock> GetL2Algorithms()   { return fL2Algorithms;          };
    void     SetL2Algorithms(std::vector<L2AlgoBlock> &val){ fL2Algorithms = val; };
    void     PrintInfo();

  private:

    UChar_t  fL0MaskID;
    UChar_t  fL2Flags;
    UChar_t  fL2TriggerWord;
    UChar_t  fL2NEnabledAlgos;      //! Transient data member
    UChar_t  fL2ReferenceDetector;
    UChar_t  fL2ReferenceFineTime;
    UShort_t fL2ReductionFactor;
    std::vector<L2AlgoBlock> fL2Algorithms;

    ClassDef(L2MaskBlock,1);
};

class L2AlgoBlock : public TObject {

  public:

    L2AlgoBlock();
    L2AlgoBlock(const L2AlgoBlock&);
    ~L2AlgoBlock(){};
    void Clear(Option_t* = "");
    UChar_t  GetL2AlgoID()                          { return fL2AlgoID;            };
    void     SetL2AlgoID(UChar_t val)               { fL2AlgoID = val;             };
    UChar_t  GetL2QualityFlags()                    { return fL2QualityFlags;      };
    void     SetL2QualityFlags(UChar_t val)         { fL2QualityFlags = val;       };
    UChar_t  GetL2ProcessID()                       { return fL2ProcessID;         };
    void     SetL2ProcessID(UChar_t val)            { fL2ProcessID = val;          };
    UChar_t  GetL2NAlgoWords()                      { return fL2NAlgoWords;        };
    void     SetL2NAlgoWords(UChar_t val)           { fL2NAlgoWords = val;         };
    UChar_t  GetL2AlgoFlags()                       { return fL2AlgoFlags;         };
    void     SetL2AlgoFlags(UChar_t val)            { fL2AlgoFlags = val;          };
    UChar_t  GetL2TimeWindow()                      { return fL2TimeWindow;        };
    void     SetL2TimeWindow(UChar_t val)           { fL2TimeWindow = val;         };
    UShort_t GetL2DownscalingFactor()               { return fL2DownscalingFactor; };
    void     SetL2DownscalingFactor(UShort_t val)   { fL2DownscalingFactor = val;  };
    std::vector<UInt_t> GetL2DataWords()            { return fL2DataWords;         };
    void     SetL2DataWords(std::vector<UInt_t> &val){ fL2DataWords = val;          };

  private:

    UChar_t  fL2AlgoID;
    UChar_t  fL2QualityFlags;
    UChar_t  fL2ProcessID;
    UChar_t  fL2NAlgoWords;      //! Transient data member
    UChar_t  fL2AlgoFlags;
    UChar_t  fL2TimeWindow;
    UShort_t fL2DownscalingFactor;
    std::vector<UInt_t> fL2DataWords;

    ClassDef(L2AlgoBlock,1);
};
#endif
// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#ifndef L2EBSpecialTrigger_H
#define L2EBSpecialTrigger_H 1
#include "Rtypes.h"
#include "TObject.h"

#define O_L2SPTRGBLOCKLENGTH 0
#define O_L2SPTRGL2PCID 0
#define O_L2SPTRGDETECTORID 0
#define O_L2SPTRGTIMESTAMP 1
#define O_L2SPTRGDATAFORMAT 2
#define O_L2SPTRGTIMEOUTFLAG 2
// Reserved (word 3)
#define O_L2SPTRGNL2INPUTEVTS 4
#define O_L2SPTRGNL2SPECIALEVTS 5
#define O_L2SPTRGNL2CONTROLEVTS 6
#define O_L2SPTRGNL2PERIODICEVTS 7
#define O_L2SPTRGNL2PHYSICSEVTS 8
#define O_L2SPTRGNL2PHYSICSEVTSMM 9
#define O_L2SPTRGNL2OUTPUTEVTS 10
#define O_L2SPTRGNL2ACCEPTEDEVTS 11
#define O_L2SPTRGNL2TIMEOUTEVTS 12
#define O_L2SPTRGNL2ALLDISABLEDEVTS 13
#define O_L2SPTRGNL2BYPASSEDEVTS 14
#define O_L2SPTRGNL2FLAGALGOEVTS 15
#define O_L2SPTRGNL2AUTOPASSEVTS 16
#define O_L2SPTRGL2MASKSINFO 17

#define M_L2SPTRGBLOCKLENGTH 0x0000ffff
#define M_L2SPTRGL2PCID 0x00ff0000
#define M_L2SPTRGDETECTORID 0xff000000
#define M_L2SPTRGTIMESTAMP 0xffffffff
#define M_L2SPTRGDATAFORMAT 0x000000ff
#define M_L2SPTRGTIMEOUTFLAG 0x0000ff00

#define S_L2SPTRGBLOCKLENGTH 0
#define S_L2SPTRGL2PCID 16
#define S_L2SPTRGDETECTORID 24
#define S_L2SPTRGTIMESTAMP 0
#define S_L2SPTRGDATAFORMAT 0
#define S_L2SPTRGTIMEOUTFLAG 8

//--- L2Mask block
// Reserved (word 0)
#define O_L2SPMASKNINPUTEVTS 1
#define O_L2SPMASKNOUTPUTNEVTS 2

class L2MaskSpecialBlock;
class L2PCSpecialBlock;

class L2EBSpecialTrigger : public TObject {

  public:

    L2EBSpecialTrigger();
    ~L2EBSpecialTrigger();
    void Clear(Option_t* = "");
    Bool_t  AddPCInfo(UInt_t *);

    std::vector<L2PCSpecialBlock> GetL2PCsInfo()  { return fL2PCsInfo;   };

  private:

    std::vector<L2PCSpecialBlock> fL2PCsInfo;

    ClassDef(L2EBSpecialTrigger,1);
};

class L2PCSpecialBlock : public TObject {

  public:

    L2PCSpecialBlock();
    L2PCSpecialBlock(const L2PCSpecialBlock&);
    ~L2PCSpecialBlock() {}
    void Clear(Option_t* = "");

    UInt_t  GetBlockLength()                                 { return  fBlockLength;                     };
    void    SetBlockLength(UInt_t value)                     { fBlockLength = value;                     };
    UChar_t GetL2PCID()                                      { return  fL2PCID;                          };
    void    SetL2PCID(UChar_t value)                         { fL2PCID = value;                          };
    ULong_t GetTimeStamp()                                   { return  fTimeStamp;                       };
    void    SetTimeStamp(ULong_t value)                      { fTimeStamp = value;                       };
    UChar_t GetDataFormat()                                  { return  fDataFormat;                      };
    void    SetDataFormat(UChar_t value)                     { fDataFormat = value;                      };
    UChar_t GetTimeoutFlag()                                 { return  fTimeoutFlag;                     };
    void    SetTimeoutFlag(UChar_t value)                    { fTimeoutFlag = value;                     };
    UInt_t  GetNL2InputEvents()                              { return  fNL2InputEvents;                  };
    void    SetNL2InputEvents(UInt_t value)                  { fNL2InputEvents = value;                  };
    UInt_t  GetNL2SpecialEvents()                            { return  fNL2SpecialEvents;                };
    void    SetNL2SpecialEvents(UInt_t value)                { fNL2SpecialEvents = value;                };
    UInt_t  GetNL2ControlEvents()                            { return  fNL2ControlEvents;                };
    void    SetNL2ControlEvents(UInt_t value)                { fNL2ControlEvents = value;                };
    UInt_t  GetNL2PeriodicEvents()                           { return  fNL2PeriodicEvents;               };
    void    SetNL2PeriodicEvents(UInt_t value)               { fNL2PeriodicEvents = value;               };
    UInt_t  GetNL2PhysicsEvents()                            { return  fNL2PhysicsEvents;                };
    void    SetNL2PhysicsEvents(UInt_t value)                { fNL2PhysicsEvents = value;                };
    UInt_t  GetNL2PhysicsEventsInMultipleMasks()             { return  fNL2PhysicsEventsInMultipleMasks; };
    void    SetNL2PhysicsEventsInMultipleMasks(UInt_t value) { fNL2PhysicsEventsInMultipleMasks = value; };
    UInt_t  GetNL2OutputEvents()                             { return  fNL2OutputEvents;                 };
    void    SetNL2OutputEvents(UInt_t value)                 { fNL2OutputEvents = value;                 };
    UInt_t  GetNL2AcceptedEvents()                           { return  fNL2AcceptedEvents;               };
    void    SetNL2AcceptedEvents(UInt_t value)               { fNL2AcceptedEvents = value;               };
    UInt_t  GetNL2TimeoutEvents()                            { return  fNL2TimeoutEvents;                };
    void    SetNL2TimeoutEvents(UInt_t value)                { fNL2TimeoutEvents = value;                };
    UInt_t  GetNL2AllDisabledEvents()                        { return  fNL2AllDisabledEvents;            };
    void    SetNL2AllDisabledEvents(UInt_t value)            { fNL2AllDisabledEvents = value;            };
    UInt_t  GetNL2BypassedEvents()                           { return  fNL2BypassedEvents;               };
    void    SetNL2BypassedEvents(UInt_t value)               { fNL2BypassedEvents = value;               };
    UInt_t  GetNL2FlagAlgoEvents()                           { return  fNL2FlagAlgoEvents;               };
    void    SetNL2FlagAlgoEvents(UInt_t value)               { fNL2FlagAlgoEvents = value;               };
    UInt_t  GetNL2AutopassEvents()                           { return  fNL2AutopassEvents;               };
    void    SetNL2AutopassEvents(UInt_t value)               { fNL2AutopassEvents = value;               };
    std::vector<L2MaskSpecialBlock> GetL2MasksInfo()         { return  fL2MasksInfo;                     };
    void    SetL2MasksInfo(std::vector<L2MaskSpecialBlock> &value) { fL2MasksInfo = value;                };

  private:

    UInt_t  fBlockLength;
    UChar_t fL2PCID;
    UInt_t  fDetectorID;        //!  Transient data member
    ULong_t fTimeStamp;
    UChar_t fDataFormat;
    UChar_t fTimeoutFlag;
    UInt_t  fReserved;
    UInt_t  fNL2InputEvents;
    UInt_t  fNL2SpecialEvents;
    UInt_t  fNL2ControlEvents;
    UInt_t  fNL2PeriodicEvents;
    UInt_t  fNL2PhysicsEvents;
    UInt_t  fNL2PhysicsEventsInMultipleMasks;
    UInt_t  fNL2OutputEvents;
    UInt_t  fNL2AcceptedEvents;
    UInt_t  fNL2TimeoutEvents;
    UInt_t  fNL2AllDisabledEvents;
    UInt_t  fNL2BypassedEvents;
    UInt_t  fNL2FlagAlgoEvents;
    UInt_t  fNL2AutopassEvents;
    std::vector<L2MaskSpecialBlock> fL2MasksInfo;

    ClassDef(L2PCSpecialBlock,1);
};

class L2MaskSpecialBlock : public TObject {

  public:

    L2MaskSpecialBlock();
    L2MaskSpecialBlock(const L2MaskSpecialBlock&);
    ~L2MaskSpecialBlock() {}
    void Clear(Option_t* = "");
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              };
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               };
    UInt_t   GetNL2InputEvents()                 { return fNL2InputEvents;        };
    void     SetNL2InputEvents(UInt_t val)       { fNL2InputEvents = val;         };
    UInt_t   GetNL2OutputEvents()                { return fNL2OutputEvents;       };
    void     SetNL2OutputEvents(UInt_t val)      { fNL2OutputEvents = val;        };

  private:

    UChar_t    fL0MaskID;
    UInt_t     fNL2InputEvents;
    UInt_t     fNL2OutputEvents;
    UInt_t     fReserved;

    ClassDef(L2MaskSpecialBlock,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// --------------------------------------------------------------

#ifndef MCINFO_H
#define MCINFO_H 1

#include "TObject.h"
#include "TString.h"

class MCInfo : public TObject {

public:

  MCInfo();
  virtual ~MCInfo() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(MCInfo &s);
  void MergeJobAttributes(MCInfo &s);
  void UpdateAndMergeAttributes(MCInfo &s);

  TString  GetRevision()                    { return fRevision;           }
  void     SetRevision(TString val)         { fRevision = val;            }
  Int_t    GetBeamType()                    { return fBeamType;           }
  void     SetBeamType(Int_t val)           { fBeamType = val;            }
  Int_t    GetFastSimulationMode()          { return fFastSimulationMode; }
  void     SetFastSimulationMode(Int_t val) { fFastSimulationMode = val;  }
  Double_t GetBrPie2()                      { return fBrPie2;             }
  void     SetBrPie2(Double_t val)          { fBrPie2 = val;              }
  Bool_t   GetForcedDecay()                 { return fForcedDecay;        }
  void     SetForcedDecay(Bool_t val)       { fForcedDecay = val;         }
  Bool_t   GetForcedMuonDecay()             { return fForcedMuonDecay;    }
  void     SetForcedMuonDecay(Bool_t val)   { fForcedMuonDecay = val;     }
  Bool_t   GetForcedPionDecay()             { return fForcedPionDecay;    }
  void     SetForcedPionDecay(Bool_t val)   { fForcedPionDecay = val;     }
  Int_t    GetDecayType()             	    { return fDecayType;          }
  void     SetDecayType(Int_t val)    	    { fDecayType = val;           }
  Int_t    GetRadCor()                      { return fRadCor;             }
  void     SetRadCor(Int_t val)             { fRadCor = val;              }
  Int_t    GetPiZeroDecay()                 { return fPiZeroDecay;        }
  void     SetPiZeroDecay(Int_t val)        { fPiZeroDecay = val;         }
  Double_t GetZDecayMin()                   { return fZDecayMin;          }
  void     SetZDecayMin(Double_t val)       { fZDecayMin = val;           }
  Double_t GetZDecayMax()                   { return fZDecayMax;          }
  void     SetZDecayMax(Double_t val)       { fZDecayMax = val;           }

  Double_t GetExoticParticleMass()                 { return fExoticParticleMass;      }
  void     SetExoticParticleMass(Double_t val)     { fExoticParticleMass = val;       }
  Int_t    GetExoticParticleDecayMode()            { return fExoticParticleDecayMode; }
  void     SetExoticParticleDecayMode(Int_t val)   { fExoticParticleDecayMode = val;  }
  Double_t GetExoticParticleLifetime()             { return fExoticParticleLifetime;  }
  void     SetExoticParticleLifetime(Double_t val) { fExoticParticleLifetime = val;   }

  std::vector<TString> GetFileName()            { return fFileName;           }
  void                 AddFileName(TString val) { fFileName.push_back(val);   }
  std::vector<Int_t>   GetRunNumber()           { return fRunNumber;          }
  void                 AddRunNumber(Int_t val)  { fRunNumber.push_back(val);  }
  std::vector<Int_t>   GetRandomSeed()          { return fRandomSeed;         }
  void                 AddRandomSeed(Int_t val) { fRandomSeed.push_back(val); }
  std::vector<Int_t>   GetNEvents()             { return fNEvents;            }
  void                 AddNEvents(Int_t val)    { fNEvents.push_back(val);    }

private:

  // Unique attributes
  TString  fRevision;        ///< Software revision
  Int_t    fBeamType;        ///< Beam type: turtle, gps, external, ...
  Bool_t   fForcedDecay;     ///< Forced decay requested?
  Bool_t   fForcedMuonDecay; ///< Forced muon decay requested?
  Int_t    fDecayType;       ///< Decay type
  Int_t    fRadCor;          ///< Radiative corrections mode
  Int_t    fPiZeroDecay;     ///< pi0 decay modes (encoded for up to 3 pi0 mesons)
  Double_t fZDecayMin;       ///< Lower limit of the decay region generated
  Double_t fZDecayMax;       ///< Upper limit of the decay region generated
  Double_t fExoticParticleMass;
  Int_t    fExoticParticleDecayMode;
  Double_t fExoticParticleLifetime;

  // Job-dependent attributes
  std::vector<TString> fFileName;
  std::vector<Int_t>   fRunNumber;
  std::vector<Int_t>   fRandomSeed;
  std::vector<Int_t>   fNEvents;

  // These members are last in the list: they have been introduced in v2 of the class
  Int_t    fFastSimulationMode;
  Double_t fBrPie2;          ///< BR(pi+ --> e+ nu)
  Bool_t   fForcedPionDecay; ///< Forced pion decay requested?

  ClassDef(MCInfo,2)
};

#endif
// --------------------------------------------------------------
// History:
//
// Variables added  Karim Massri (karim.massri@cern.ch) 2016-12-08
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// --------------------------------------------------------------

#ifndef RECOINFO_H
#define RECOINFO_H 1

#include "TObject.h"
#include "TString.h"

class RecoInfo : public TObject {

public:

  RecoInfo();
  virtual ~RecoInfo() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(RecoInfo &s);
  void MergeJobAttributes(RecoInfo &s);
  void UpdateAndMergeAttributes(RecoInfo &s);

  TString               GetRevision() const                   { return fRevision;               }
  void                  SetRevision(TString val)              { fRevision = val;                }
  std::vector<UInt_t>   GetRunID() const                      { return fRunID;                  }
  void                  SetRunID(UInt_t val)                  { fRunID.push_back(val);          }
  std::vector<UInt_t>   GetBurstID() const                    { return fBurstID;                }
  void                  SetBurstID(UInt_t val)                { fBurstID.push_back(val);        }
  std::vector<UInt_t>   GetBurstTime() const                  { return fBurstTime;              }
  void                  SetBurstTime(UInt_t val)              { fBurstTime.push_back(val);      }
  UInt_t                GetNReadEvents() const                { return fNReadEvents;            }
  void                  SetNReadEvents(UInt_t val)            { fNReadEvents = val;             }
  UInt_t                GetNProcessedEvents() const           { return fNProcessedEvents;       }
  void                  SetNProcessedEvents(UInt_t val)       { fNProcessedEvents = val;        }
  UInt_t                GetNSkippedEvents() const             { return fNSkippedEvents;         }
  void                  SetNSkippedEvents(UInt_t val)         { fNSkippedEvents = val;          }
  UInt_t                GetNCriticalEvents() const            { return fNCriticalEvents;        }
  void                  SetNCriticalEvents(UInt_t val)        { fNCriticalEvents = val;         }
  UInt_t                GetNPhysicsTriggerEvents() const      { return fNPhysicsTriggerEvents;  }
  void                  SetNPhysicsTriggerEvents(UInt_t val)  { fNPhysicsTriggerEvents = val;   }
  UInt_t                GetNControlTriggerEvents() const      { return fNControlTriggerEvents;  }
  void                  SetNControlTriggerEvents(UInt_t val)  { fNControlTriggerEvents = val;   }
  UInt_t                GetNPeriodicTriggerEvents() const     { return fNPeriodicTriggerEvents; }
  void                  SetNPeriodicTriggerEvents(UInt_t val) { fNPeriodicTriggerEvents = val;  }
  UInt_t                GetNSpecialTriggerEvents() const      { return fNSpecialTriggerEvents;  }
  void                  SetNSpecialTriggerEvents(UInt_t val)  { fNSpecialTriggerEvents = val;   }
  Double_t              GetKaonRate() const                   { return fKaonRate;               }
  void                  SetKaonRate(Double_t val)             { fKaonRate = val;                }
  Double_t              GetKaonRateError() const              { return fKaonRateError;          }
  void                  SetKaonRateError(Double_t val)        { fKaonRateError = val;           }
  Double_t              GetChokeONTime() const                { return fChokeONTime;            }
  void                  SetChokeONTime(Double_t val)          { fChokeONTime = val;             }

private:

  TString  fRevision;                ///< Software revision
  UInt_t   fNReadEvents;             ///< Number of read events in the file
  UInt_t   fNProcessedEvents;        ///< Number of processed events in the file
  UInt_t   fNSkippedEvents;          ///< Number of skipped events in the file
  UInt_t   fNCriticalEvents;         ///< Number of events in the file with critical errors
  UInt_t   fNPhysicsTriggerEvents;   ///< Number of PhysicsTrigger events in the file
  UInt_t   fNControlTriggerEvents;   ///< Number of ControlTrigger in the file
  UInt_t   fNPeriodicTriggerEvents;  ///< Number of PeriodicTrigger in the file
  UInt_t   fNSpecialTriggerEvents;   ///< Number of SpecialTrigger in the file
  Double_t fKaonRate;                ///< Kaon rate [MHz] estimated from Cedar
  Double_t fKaonRateError;           ///< Error of the Kaon rate [MHz] estimated from Cedar

  std::vector<UInt_t>   fRunID;      ///< Run number
  std::vector<UInt_t>   fBurstID;    ///< Burst number
  std::vector<UInt_t>   fBurstTime;  ///< Burst time (UNIX)
  
  // These members are last in the list: they have been introduced in v2 of the class
  Double_t fChokeONTime;

  ClassDef(RecoInfo,2)
};

#endif

#ifndef Rndm_h
#define Rndm_h 1

#include "TObject.h"
#include "TRandom3.h"

class Rndm : public TObject
{
public :
  Rndm();
  void StoreRandomState(TRandom3* RandomDecayState, TRandom3* RandomBeamState, long *RanecuState);
  TRandom3* GetRandomDecayState () {return fRandomDecayState;}
  TRandom3* GetRandomBeamState () {return fRandomBeamState;}
  long* GetRanecuState() {return fRanecuState;}


private:

  TRandom3* fRandomDecayState;
  TRandom3* fRandomBeamState;
  Long_t fRanecuState[2];
  ClassDef(Rndm,1)
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-02-01
//
// --------------------------------------------------------------

#ifndef STREAM_H
#define STREAM_H 1

#include "TObject.h"
#include "AnalysisInfo.hh"
#include "MCInfo.hh"
#include "RecoInfo.hh"

class Stream : public TObject {

public:
  Stream();
  virtual ~Stream() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(Stream &s);
  void MergeJobAttributes(Stream &s);
  void UpdateAndMergeAttributes(Stream &s);

  MCInfo&       GetMCInfo()                        { return fMCInfo;       }
  void          SetMCInfo(MCInfo &val)             { fMCInfo = val;        }
  RecoInfo&     GetRecoInfo()                      { return fRecoInfo;     }
  void          SetRecoInfo(RecoInfo &val)         { fRecoInfo = val;      }
  AnalysisInfo& GetAnalysisInfo()                  { return fAnalysisInfo; }
  void          SetAnalysisInfo(AnalysisInfo &val) { fAnalysisInfo = val;  }

private:
  MCInfo       fMCInfo;
  RecoInfo     fRecoInfo;
  AnalysisInfo fAnalysisInfo;

  ClassDef(Stream,1)
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#ifndef TDCError_H
#define TDCError_H
#include "TDigiVError.hh"

class TDCError : public TDigiVError {

  public:

    TDCError();
    ~TDCError(){};
    void Clear(Option_t* = "");

  public:

    Int_t                GetTDCID()                                         { return fTDCID;                        };
    void                 SetTDCID(Int_t value)                              { fTDCID = value;                       };
    Int_t                GetTDCBID()                                        { return GetROMezzanineID();            };
    void                 SetTDCBID(Int_t value)                             { SetROMezzanineID(value);              };
    Int_t                GetTEL62ID()                                       { return GetROBoardID();                };
    void                 SetTEL62ID(Int_t value)                            { SetROBoardID(value);                  };

  private:

    Int_t fTDCID;

    ClassDef(TDCError,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TDCEvent_H
#define TDCEvent_H
#include "TClass.h"
#include "TDCVHit.hh"
#include "TDigiVEvent.hh"

class TDCEvent : public TDigiVEvent {

  public:

    TDCEvent();
    explicit TDCEvent(TClass *);
    TDCVHit* GetHit(Int_t);
    void Clear(Option_t* = "");

  private:

    ClassDef(TDCEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#ifndef TDCVHit_H
#define TDCVHit_H
#include "TVDigi.hh"
#include "Riostream.h"

class TDCVHit : public TVDigi {

    public:

        TDCVHit();
        explicit TDCVHit(Int_t);
        explicit TDCVHit(TVHit* MCHit);
        virtual ~TDCVHit(){};
        void Clear(Option_t* = "");
        virtual void UpdateReferenceTime(Double_t value){ fLeadingEdge -= value; fTrailingEdge -= value; };
        Int_t Compare(const TObject *obj) const;
        Bool_t IsSortable() const { return kTRUE; }
        virtual Double_t GetTime() { return fLeadingEdge; };
        //virtual Int_t GetStationID() { cerr << "ERROR: GetStationID() must be overloaded in the concrete implementetion of Digi" << endl; return 0; };

    public:

        inline Int_t                GetDetectedEdge() const                      { return fDetectedEdge;                 };
        inline void                 SetDetectedEdge(Int_t value)                 { fDetectedEdge = value;                };
        inline Double_t             GetLeadingEdge() const                       { return fLeadingEdge;                  };
        inline void                 SetLeadingEdge(Double_t value)               { fLeadingEdge = value;                 };
        inline Double_t             GetTrailingEdge() const                      { return fTrailingEdge;                 };
        inline void                 SetTrailingEdge(Double_t value)              { fTrailingEdge = value;                };
        inline Int_t                GetFPGAID() const                            { return fFPGAID;                       };
        inline void                 SetFPGAID(Int_t value)                       { fFPGAID = value;                      };
        inline Int_t                GetSlot() const                              { return fSlot;                         };
        inline void                 SetSlot(Int_t value)                         { fSlot = value;                        };
        inline Double_t             GetSlotTS() const                            { return fSlotTS;                       };
        inline void                 SetSlotTS(Double_t value)                    { fSlotTS = value;                      };

        inline void                 UpdateDetectedEdge(Int_t e)                  { fDetectedEdge |= e;                   };

    protected:

        Double_t fLeadingEdge;
        Double_t fTrailingEdge;

    private:

        Int_t   fDetectedEdge; // Bits: 1=Leading,2=Trailing
        Int_t fFPGAID;
        Int_t fSlot;
        Double_t fSlotTS;

        ClassDef(TDCVHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TDetectorVEvent_H
#define TDetectorVEvent_H

#include "TClass.h"

#include "TDetectorVHit.hh"
#include "TVEvent.hh"
#include "TVDigi.hh"
#include "TVHit.hh"
#include "TClonesArray.h"

class TDetectorVEvent : public TVEvent {

public:

  TDetectorVEvent();
  TDetectorVEvent(const TDetectorVEvent &);
  TDetectorVEvent(TClass * Class, Int_t NMaxHits=1000);
  ~TDetectorVEvent();
  TVHit * AddHit();
  TVHit * AddHit(Int_t iCh);
  TDetectorVHit * AddHit(TDetectorVHit *);
  TVHit * GetLastHitOnChannel(Int_t iCh);
  TVHit * GetHit(Int_t iHit);
  TVHit * GetLastHit();
  void RemoveHit(Int_t iHit);
  void Clear(Option_t* = "");

  Int_t         GetNHits() { return fNHits; }
  TClonesArray* GetHits()  { return fHits;  }

private:

  Int_t         fNHits;
  TClonesArray* fHits;
  ClassDef(TDetectorVEvent,1);
};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TDetectorVHit_H
#define TDetectorVHit_H

#include "TVHit.hh"
#include "TVector3.h"

class TDetectorVHit : public TVHit {

    public:

        TDetectorVHit();
        TDetectorVHit(const TDetectorVHit &);
        explicit TDetectorVHit(Int_t);
        virtual ~TDetectorVHit(){};
        void Clear(Option_t* = "");
        virtual void UpdateReferenceTime(Double_t value){ fTime -= value; };
        void Print(Option_t* option="") const;

    public:

        TVector3             GetPosition() const                                { return fPosition;                     };
        void                 SetPosition(TVector3 value)                        { fPosition = value;                    };
        Double_t             GetEnergy() const                                  { return fEnergy;                       };
        void                 SetEnergy(Double_t value)                          { fEnergy = value;                      };
        void                 AddEnergy(Double_t value)                          { fEnergy += value;                     };
        Double_t             GetTime() const                                    { return fTime;                         };
        void                 SetTime(Double_t value)                            { fTime = value;                        };

    private:

        TVector3   fPosition;
        Double_t   fEnergy;
        Double_t   fTime;

        ClassDef(TDetectorVHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-30
//
// --------------------------------------------------------------
#ifndef TDigiVCandidate_H
#define TDigiVCandidate_H

#include "TVCandidate.hh"
#include "TVDigi.hh"

class TDigiVCandidate : public TVCandidate {

    public:

        TDigiVCandidate();
        explicit TDigiVCandidate(Int_t);
        virtual ~TDigiVCandidate();
        void Clear(Option_t* = "");

        Bool_t AddDigi(Int_t);
        Int_t GetNDigis();
        TVDigi * GetDigi(Int_t);

    public:

    private:

        ClassDef(TDigiVCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-12-31
//
// --------------------------------------------------------------
#ifndef TDigiVError_H
#define TDigiVError_H
#include "TObject.h"

class TDigiVError : public TObject {

  public:

    TDigiVError();
    ~TDigiVError(){};
    void Clear(Option_t* = "");

  public:

    Int_t                GetROMezzanineID()               { return fROMezzanineID;   };
    void                 SetROMezzanineID(Int_t value)    { fROMezzanineID = value;  };
    Int_t                GetROBoardID()                   { return fROBoardID;       };
    void                 SetROBoardID(Int_t value)        { fROBoardID = value;      };
    Int_t                GetType()                        { return fType;            };
    void                 SetType(Int_t value)             { fType = value;           };
    Bool_t               GetFatal()                       { return fFatal;           };
    void                 SetFatal(Bool_t value)           { fFatal = value;          };

  private:

    Int_t  fROMezzanineID;
    Int_t  fROBoardID;
    Int_t  fType;
    Bool_t fFatal;

    ClassDef(TDigiVError,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-02-18
//
// --------------------------------------------------------------
#ifndef TDigiVEvent_H
#define TDigiVEvent_H

#include "TDetectorVEvent.hh"
#include "TDigiVCandidate.hh"
#include "TDigiVError.hh"
#include "TClass.h"

#include "TDetectorVHit.hh"
#include "TVDigi.hh"

class TDigiVEvent : public TDetectorVEvent {

  public:

    TDigiVEvent();
    TDigiVEvent(const TDigiVEvent &);
    TDigiVEvent(TClass *, TClass *, TClass *);
    TDigiVEvent(TClass *, TClass *, TClass *, Int_t);
    ~TDigiVEvent();

    TVDigi * AddDigi();
    TVDigi * AddDigi(Int_t);
    TVDigi * AddDigi(TDetectorVHit *);
    Int_t GetNDigis();
    TClonesArray * GetDigis();
    TDigiVCandidate * AddCandidate();
    TDigiVCandidate * GetCandidate(Int_t);
    TDigiVError * AddError(Int_t ErrorType);
    TDigiVError * GetError(Int_t iError);
    void UpdateErrorMask(Int_t ErrorType);
    void Clear(Option_t* = "");
    void RemoveCandidate(Int_t);

    Int_t          GetNCandidates()                   { return fNCandidates;   }
    void           SetNCandidates(Int_t value)        { fNCandidates = value;  }
    TClonesArray*  GetCandidates()                    { return fCandidates;    }
    void           SetCandidates(TClonesArray* value) { fCandidates = value;   }
    ULong64_t      GetErrorMask()                     { return fErrorMask;     }
    Int_t          GetNErrors()                       { return fNErrors;       }
    void           SetNErrors(Int_t value)            { fNErrors = value;      }
    TClonesArray*  GetErrors()                        { return fErrors;        }

  private:

    Int_t         fNCandidates;
    TClonesArray* fCandidates;
    ULong64_t     fErrorMask;   //!  Transient data member
    Int_t         fNErrors;
    TClonesArray* fErrors;

    ClassDef(TDigiVEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-02-26
//
// --------------------------------------------------------------
#ifndef TEventInfo_H
#define TEventInfo_H

#include "TObject.h"

#define NMAXEVINFO 1000

class TEventInfo : public TObject {

    public:

        TEventInfo();
        ~TEventInfo();
        void Clear(Option_t* = "");
        void  ProposeLatestHitTime(Double_t value){ fLatestHitTime = value > fLatestHitTime ? value : fLatestHitTime; };

    public:

        Int_t                GetID()                                            { return fID;                           };
        void                 SetID(Int_t value)                                 { fID = value;                          };
        Int_t                GetStreamID()                                      { return fStreamID;                     };
        void                 SetStreamID(Int_t value)                           { fStreamID = value;                    };
        Double_t             GetTime()                                          { return fTime;                         };
        void                 SetTime(Double_t value)                            { fTime = value;                        };
        Bool_t               GetValidity()                                      { return fValidity;                     };
        void                 SetValidity(Bool_t value)                          { fValidity = value;                    };

        Int_t                GetFirstHit()                                      { return fFirstHit;                     };
        void                 SetFirstHit(Int_t value)                           { fFirstHit = value;                    };
        Int_t                GetNHits()                                         { return fNHits;                        };
        void                 SetNHits(Int_t value)                              { fNHits = value;                       };
        Double_t             GetLatestHitTime()                                 { return fLatestHitTime;                };
        void                 SetLatestHitTime(Double_t value)                   { fLatestHitTime = value;               };
        Int_t                GetNKineParts()                                    { return fNKineParts;                   };
        void                 SetNKineParts(Int_t value)                         { fNKineParts = value;                  };

    private:

        Int_t      fID;
        Int_t      fStreamID;
        Double_t   fTime;
        Bool_t     fValidity;

        Int_t      fFirstHit;
        Int_t      fNHits;
        Double_t   fLatestHitTime;
        Int_t      fNKineParts;

        ClassDef(TEventInfo,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-30
//
// --------------------------------------------------------------
#ifndef TPrimSpecialTrigger_H
#define TPrimSpecialTrigger_H
#include "TObject.h"

#include "TSpecialTrigger.hh"
#include "TString.h"

class PrimRegister : public TObject{

  public:

    PrimRegister() { Clear(); }
    PrimRegister(const PrimRegister &c) : TObject(c), fLabel(c.fLabel), fValue(c.fValue) { }
    PrimRegister(TString Label, UInt_t Value);
    ~PrimRegister() {}
    void Clear(Option_t* = "");
    TString GetLabel()          { return fLabel;  }
    UInt_t GetValue()           { return fValue;  }
    void SetValue(UInt_t Value) { fValue = Value; }

  private:

    TString fLabel;
    UInt_t  fValue;
    ClassDef(PrimRegister,1);
};

class PrimCounter : public TObject{

  public:

    PrimCounter(){ Clear(); };
    PrimCounter(const PrimCounter&);
    PrimCounter(TString Label, Int_t ChannelID, UInt_t Value);
    ~PrimCounter(){};
    void Clear(Option_t* = "");
    TString GetLabel()                   { return fLabel;                }
    std::vector<Int_t> GetChannelIDs()   { return fChannelIDs;           }
    std::vector<UInt_t> GetValues()      { return fValues;               }
    Int_t GetChannelID(Int_t iCounter)   { return fChannelIDs[iCounter]; }
    UInt_t GetValue(UInt_t iCounter)     { return fValues[iCounter];     }
    UInt_t GetNEntries() { return fValues.size(); }
    void AddEntry(Int_t ChannelID, UInt_t Value);

  private:

    TString fLabel;
    std::vector<Int_t>  fChannelIDs;
    std::vector<UInt_t> fValues;
    ClassDef(PrimCounter,1);
};


class TPrimSpecialTrigger : public TSpecialTrigger {

  public:

    TPrimSpecialTrigger();
    ~TPrimSpecialTrigger(){};
    void Clear(Option_t* = "");

    std::vector<PrimRegister> GetRegisters()    { return fRegisters;            };
    std::vector<PrimCounter>  GetCounters()     { return fCounters;             };
    PrimRegister* GetRegister(UInt_t iRegister) { if(iRegister<fRegisters.size()) return &(*(fRegisters.begin()+iRegister)); else return 0; };
    PrimCounter*  GetCounter(UInt_t iCounter)   { if(iCounter<fCounters.size()) return &(*(fCounters.begin()+iCounter)); else return 0;     };
    PrimRegister* GetRegister(TString Label);
    PrimCounter*  GetCounter(TString Label);
    Int_t FindRegister(TString Label);
    Int_t FindCounter(TString Label);
    void AddRegister(TString Label, UInt_t Value);
    void AddCounter(TString Label, Int_t ChannelID, UInt_t Value);

  private:

    std::vector<PrimRegister> fRegisters;
    std::vector<PrimCounter>  fCounters;

    ClassDef(TPrimSpecialTrigger,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2015-10-28
// Modified by Francesco Gonnella 2016-05-05 (added Run and burst ID)
//
// --------------------------------------------------------------
#ifndef TPRIMITIVE_H
#define TPRIMITIVE_H

#include "TObject.h"

class TPrimitive : public TObject {

public:

  TPrimitive();
  virtual ~TPrimitive() {}

  void Clear(Option_t* = "");

  Int_t   GetTimeStamp()     { return fTimeStamp; }
  Short_t GetFineTime()      { return fFineTime; }
  Int_t   GetPrimitiveID()   { return fPrimitiveID; }
  Short_t GetSourceID()      { return fSourceID; }
  Short_t GetSubID()         { return fSubID; }
  Int_t   GetMTP()           { return fMTP; }
  Int_t   GetSendTimeStamp() { return fSendTimeStamp; }
  Int_t   GetRunID()         { return fRunID; }
  Int_t   GetBurstID()       { return fBurstID; }

  void SetTimeStamp     (Int_t   timestamp)     { fTimeStamp = timestamp; }
  void SetFineTime      (Short_t finetime)      { fFineTime = finetime; }
  void SetPrimitiveID   (Int_t   primitiveID)   { fPrimitiveID = primitiveID; }
  void SetSourceID      (Short_t sourceID)      { fSourceID = sourceID; }
  void SetSubID         (Short_t subID)         { fSubID = subID; }
  void SetMTP           (Int_t   MTP)           { fMTP = MTP; }
  void SetSendTimeStamp (Int_t   sendTimeStamp) { fSendTimeStamp = sendTimeStamp; }
  void SetRunID         (Int_t   RunID)         { fRunID = RunID; }
  void SetBurstID       (Int_t   BurstID)       { fBurstID = BurstID; }


  void Print(Option_t *option="") const;
  Double_t GetTime();

private:

  Int_t   fTimeStamp;
  Short_t fFineTime;
  Int_t   fPrimitiveID;
  Short_t fSourceID;
  Short_t fSubID;
  Int_t   fMTP;
  Int_t   fSendTimeStamp;
  Int_t   fRunID;
  Int_t   fBurstID;

  ClassDef(TPrimitive,1);

};

#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-24
//
// --------------------------------------------------------------
#ifndef TRecoVCandidate_H
#define TRecoVCandidate_H

#include "TRecoVHit.hh"
#include "TVCandidate.hh"

class TRecoVCandidate : public TVCandidate {

public:

  TRecoVCandidate();
  TRecoVCandidate(const TRecoVCandidate &);
  explicit TRecoVCandidate(Int_t);
  virtual ~TRecoVCandidate();
  void Clear(Option_t* = "");

  TRecoVHit * GetHit(Int_t);

  Double_t GetTime() const         { return fTime;  }
  void     SetTime(Double_t value) { fTime = value; }

private:

  Double_t fTime;
  ClassDef(TRecoVCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoVEvent_H
#define TRecoVEvent_H

#include "TDetectorVEvent.hh"
#include "TDigiVEvent.hh"
#include "TRecoVCandidate.hh"
#include "TClonesArray.h"

class TRecoVEvent : public TDetectorVEvent {

  public:

    TRecoVEvent();
    TRecoVEvent(const TRecoVEvent &);
    TRecoVEvent(TClass *, TClass *);
    ~TRecoVEvent();

    TRecoVHit * AddHit(TDetectorVHit*);
    TRecoVHit * AddHit(TVDigi*);
    TRecoVCandidate * AddCandidate();
    TRecoVCandidate * GetCandidate(Int_t);
    void Clear(Option_t* = "");
    void RemoveCandidate(Int_t);

    Bool_t               GetStatus()                        { return fStatus;       }
    void                 SetStatus(Bool_t value)            { fStatus = value;      }
    Int_t                GetNCandidates()                   { return fNCandidates;  }
    void                 SetNCandidates(Int_t value)        { fNCandidates = value; }
    TClonesArray *       GetCandidates()                    { return fCandidates;   }
    void                 SetCandidates(TClonesArray* value) { fCandidates = value;  }
    TDigiVEvent *        GetDigiEvent()                     { return fDigiEvent;    }
    void                 SetDigiEvent(TDigiVEvent* value)   { fDigiEvent = value;   }
    ULong64_t            GetErrorMask()                     { return fErrorMask;    }
    void                 SetErrorMask(ULong64_t value)      { fErrorMask = value;   }
    void                 SetErrorMaskBit(Int_t bit,Bool_t value);

  private:

    Bool_t        fStatus;
    Int_t         fNCandidates;
    TClonesArray* fCandidates;
    TDigiVEvent*  fDigiEvent;    //!  Transient data member
    ULong64_t     fErrorMask;

    ClassDef(TRecoVEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoVHit_H
#define TRecoVHit_H

#include "TDetectorVHit.hh"
#include "TVDigi.hh"

class TRecoVHit : public TDetectorVHit {

  public:

    TRecoVHit();
    TRecoVHit(const TRecoVHit &);
    explicit TRecoVHit(Int_t);
    explicit TRecoVHit(TVDigi*);
    explicit TRecoVHit(TDetectorVHit*);
    ~TRecoVHit();

    void Clear(Option_t* = "");

  public:

    Bool_t               GetDigiOwner()                                     { return fDigiOwner;                    };
    void                 SetDigiOwner(Bool_t value)                         { fDigiOwner = value;                   };

    TVDigi *             GetDigi()                                          { return fDigi;                         };
    void                 SetDigi(TVDigi * value)                            { fDigi = value;                        };

  private:

    Bool_t fDigiOwner; //!  Transient data member
    TVDigi * fDigi; //!  Transient data member for MCTruth Association 

    ClassDef(TRecoVHit,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#ifndef TSpecialTrigger_H
#define TSpecialTrigger_H
#include "TObject.h"

class TSpecialTrigger : public TObject {

  public:

    TSpecialTrigger();
    ~TSpecialTrigger(){};

    void Clear(Option_t* = "");

  public:

    void SetDataSourceID(Int_t value)       { fDataSourceID=value;      };
    Int_t GetDataSourceID()                 { return fDataSourceID;     };
    void SetROBoardID(Int_t value)          { fROBoardID=value;         };
    Int_t GetROBoardID()                    { return fROBoardID;        };
    void SetTimeStamp(UInt_t value)         { fTimeStamp=value;         };
    UInt_t GetTimeStamp()                   { return fTimeStamp;        };
    void SetTriggerType(Int_t value)        { fTriggerType=value;       };
    Int_t GetTriggerType()                  { return fTriggerType;      };
    void SetTriggerCount(UInt_t value)      { fTriggerCount=value;      };
    UInt_t GetTriggerCount()                { return fTriggerCount;     };
    void SetErrorsFlag(UInt_t value)        { fErrorsFlag=value;        };
    UInt_t GetErrorsFlag()                  { return fErrorsFlag;       };

  private:

    Int_t   fDataSourceID;
    Int_t   fROBoardID;
    UInt_t  fTimeStamp;
    Int_t   fTriggerType;
    UInt_t  fTriggerCount;
    UInt_t  fErrorsFlag;

    ClassDef(TSpecialTrigger,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TSpecialTriggerEvent_H
#define TSpecialTriggerEvent_H
#include "TClass.h"
#include "TDetectorVEvent.hh"
#include "TSpecialTrigger.hh"

class TSpecialTriggerEvent : public TDetectorVEvent {

  public:

        TSpecialTriggerEvent();
        explicit TSpecialTriggerEvent(TClass *);
        TSpecialTrigger * AddSpecialTrigger();
        TSpecialTrigger * LastSpecialTrigger();
        void Clear(Option_t* = "");
        ~TSpecialTriggerEvent();

    public:

        Int_t GetNSpecialTriggers()                      { return GetNHits();               };
        //TSpecialTrigger * GetSpecialTrigger()            { return fSOB;                     };


    private:

        //TSpecialTrigger *fSOB;

        ClassDef(TSpecialTriggerEvent,1);

};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-07-16
//
// --------------------------------------------------------------
#ifndef TTDCBSpecialTrigger_H
#define TTDCBSpecialTrigger_H
#include "TPrimSpecialTrigger.hh"

class TTDCBSpecialTrigger : public TPrimSpecialTrigger {

  public:

    TTDCBSpecialTrigger();
    ~TTDCBSpecialTrigger(){};
    void Clear(Option_t* = "");

  public:

    void SetFPGAID(Int_t value)             { fFPGAID=value;            };
    Int_t GetFPGAID()                       { return fFPGAID;           };

  private:

    Int_t   fFPGAID;

    ClassDef(TTDCBSpecialTrigger,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-24
//
// --------------------------------------------------------------
#ifndef TTimeCluster_H
#define TTimeCluster_H

#include "TDigiVCandidate.hh"

class TTimeCluster : public TDigiVCandidate {

    public:

        TTimeCluster();
        ~TTimeCluster(){};
        void Clear(Option_t* = "");

        Bool_t AddDigi(Int_t);
        Int_t Compare(const TObject *obj) const;
        Bool_t IsSortable() const { return kTRUE; }

    public:

        Double_t             GetAverage() const                                 { return fAverage;                      };
        void                 SetAverage(Double_t value)                         { fAverage = value;                     };
        Double_t             GetRMS() const                                     { return fRMS;                          };
        void                 SetRMS(Double_t value)                             { fRMS = value;                         };
        Double_t             GetMinTime() const                                 { return fMinTime;                      };
        void                 SetMinTime(Double_t value)                         { fMinTime = value;                     };
        Double_t             GetMaxTime() const                                 { return fMaxTime;                      };
        void                 SetMaxTime(Double_t value)                         { fMaxTime = value;                     };
        Int_t                GetStationID() const                               { return fStationID;                    };
        void                 SetStationID(Int_t value)                          { fStationID = value;                   };

    private:

        Double_t fAverage;
        Double_t fRMS;
        Double_t fMinTime;
        Double_t fMaxTime;
        Int_t fStationID;

        ClassDef(TTimeCluster,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TVCandidate_H
#define TVCandidate_H

#include "TObject.h"
#include "TClass.h"

#include "TArrayI.h"

#include <vector>
#include "TDetectorVEvent.hh"
#include "TDetectorVHit.hh"

class TVCandidate : public TObject {

    public:

        TVCandidate();
        TVCandidate(const TVCandidate &);
        explicit TVCandidate(Int_t);
        virtual ~TVCandidate();

        Bool_t AddHit(Int_t);
        TDetectorVHit * GetHit(Int_t);
        void Clear(Option_t* = "");
        void RemoveHit(Int_t);
        void Merge(TVCandidate*);

    public:

        Int_t                GetNHits()                        { return fNHits;       }
        Int_t *              GetHitsIndexes()                  { return fHitsIndexes.GetArray(); }
        TDetectorVEvent *    GetEvent()                        { return fEvent;       }
        void                 SetEvent(TDetectorVEvent * value) { fEvent = value;      }

    private:

        Int_t fNHits;
        Int_t fNMaxHits;

        TArrayI  fHitsIndexes; //[fNMaxHits];

        TDetectorVEvent * fEvent; //! Transient data member for simpler manipulation

        ClassDef(TVCandidate,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------

#ifndef TVChannelID_H
#define TVChannelID_H

#include "TObject.h"
#include "NA62Global.hh"

class TVChannelID {

public:

  TVChannelID() : fChannelID(-1) {}
  TVChannelID(const TVChannelID & ch) : fChannelID(ch.fChannelID){}
  explicit TVChannelID(Int_t iCh) : fChannelID(iCh) {}
  virtual ~TVChannelID() {}
  void Clear(Option_t* = "");

  void Print(Option_t* option="") const;
  Int_t GetChannelID() const      { return fChannelID;         }
  Int_t SetChannelID(Int_t value) { return fChannelID = value; }

protected:

  Int_t fChannelID;
  ClassDef(TVChannelID,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-09-20
//
// --------------------------------------------------------------
#ifndef TVDigi_H
#define TVDigi_H

#include "TVHit.hh"

class TVDigi : public TVHit {

    public:

        TVDigi();
        TVDigi(Int_t);
        TVDigi(TVHit*);
        virtual ~TVDigi(){};
        void Clear(Option_t* = "");
        virtual Double_t GetTime() = 0;
        virtual Int_t GetStationID() = 0;
        virtual Int_t EncodeChannelID() = 0;
        virtual void  DecodeChannelID() = 0;

    public:

        TVHit *              GetMCHit()                                         { return fMCHit;                        };
        void                 SetMCHit(TVHit * value)                            { fMCHit = value;                       };

    protected:

        TVHit*      fMCHit; //!  Transient data member for MCTruth Association

        ClassDef(TVDigi,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TVEvent_H
#define TVEvent_H

#include "TObject.h"
#include "NA62Global.hh"

class TVEvent : public TObject {

    public:

        TVEvent();
        TVEvent(TVEvent &);
        virtual ~TVEvent();
        void Clear(Option_t* = "");
        Int_t Compare(const TObject *obj) const;
        Bool_t IsSortable() const { return kTRUE; }

        ULong64_t        GetStartByte() const                               { return fStartByte;                    }
        void             SetStartByte(ULong64_t value)                      { fStartByte = value;                   }
        Int_t            GetID() const                                      { return fID;                           }
        void             SetID(Int_t value)                                 { fID = value;                          }
        Int_t            GetBurstID() const                                 { return fBurstID;                      }
        void             SetBurstID(Int_t value)                            { fBurstID = value;                     }
        Int_t            GetRunID() const                                   { return fRunID;                        }
        void             SetRunID(Int_t value)                              { fRunID = value;                       }
        Bool_t           GetIsMC() const                                    { return fIsMC;                         }
        void             SetIsMC(Bool_t value)                              { fIsMC = value;                        }
        ULong64_t        GetTriggerType() const                             { return fTriggerType;                  }
        void             SetTriggerType(ULong64_t value)                    { fTriggerType = value;                 }
        Int_t            GetL0TriggerType() const                           { return fL0TriggerType;                }
        void             SetL0TriggerType(Int_t value)                      { fL0TriggerType = value;               }
        ULong_t          GetTimeStamp() const                               { return fTimeStamp;                    }
        void             SetTimeStamp(ULong_t value)                        { fTimeStamp = value;                   }

    private:

        ULong64_t  fStartByte;
        Int_t      fID;
        Int_t      fBurstID;
        Int_t      fRunID;
        Bool_t     fIsMC;
        ULong64_t  fTriggerType;
        Int_t      fL0TriggerType;
        ULong_t fTimeStamp;
        Float_t fFineTime;     // useless? should be removed if true..
        Float_t fLatency;      // useless? should be removed if true..

        ClassDef(TVEvent,1);
};
#endif
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TVHit_H
#define TVHit_H

#include "TVChannelID.hh"
#include "TObject.h"


class TVHit : public TObject, public TVChannelID {

    public:

        TVHit();
	    TVHit(const TVHit &);
	    explicit TVHit(Int_t);
        virtual ~TVHit(){};
        void Clear(Option_t* = "");
        virtual void UpdateReferenceTime(Double_t) = 0;
        void ShiftMCTrackID(Int_t value){ fMCTrackID += value; };
        Bool_t IsSortable() const { return kTRUE; }
        Int_t Compare(const TObject *obj) const {if(fChannelID < static_cast<const TVHit*>(obj)->GetChannelID()) return -1;
                                                 else if(fChannelID > static_cast<const TVHit*>(obj)->GetChannelID()) return 1;
                                                 else return 0;}
        void Print(Option_t* option="") const;

    public:

        Int_t                GetMCTrackID()                                     { return fMCTrackID;                    };
        void                 SetMCTrackID(Int_t value)                          { fMCTrackID = value;                   };
        Bool_t               GetDirectInteraction()                             { return fDirectInteraction;            };
        void                 SetDirectInteraction(Bool_t value)                 { fDirectInteraction = value;           };


    private:

        Int_t      fMCTrackID; // For MCTruth Association
        Bool_t     fDirectInteraction;

        ClassDef(TVHit,1);
};
#endif

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"AnalysisInfo", payloadCode, "@",
"BeamData", payloadCode, "@",
"BeamSpecialTrigger", payloadCode, "@",
"DetectorParameter", payloadCode, "@",
"Event", payloadCode, "@",
"EventBoundary", payloadCode, "@",
"EventHeader", payloadCode, "@",
"FADCEvent", payloadCode, "@",
"FADCVHit", payloadCode, "@",
"GenePart", payloadCode, "@",
"HLTEvent", payloadCode, "@",
"HLTTrack", payloadCode, "@",
"KinePart", payloadCode, "@",
"L0Mask", payloadCode, "@",
"L0Primitive", payloadCode, "@",
"L0TPData", payloadCode, "@",
"L0TPSpecialTrigger", payloadCode, "@",
"L1AlgoBlock", payloadCode, "@",
"L1MaskBlock", payloadCode, "@",
"L1MaskSpecialBlock", payloadCode, "@",
"L1PCSpecialBlock", payloadCode, "@",
"L1TPData", payloadCode, "@",
"L1TPSpecialTrigger", payloadCode, "@",
"L2AlgoBlock", payloadCode, "@",
"L2EBData", payloadCode, "@",
"L2EBSpecialTrigger", payloadCode, "@",
"L2MaskBlock", payloadCode, "@",
"L2MaskSpecialBlock", payloadCode, "@",
"L2PCSpecialBlock", payloadCode, "@",
"MCInfo", payloadCode, "@",
"MagnetInfo", payloadCode, "@",
"NA62Analysis::Core::AnalyzerIdentifier", payloadCode, "@",
"PrimCounter", payloadCode, "@",
"PrimRegister", payloadCode, "@",
"PrimitiveInfo", payloadCode, "@",
"RecoInfo", payloadCode, "@",
"Rndm", payloadCode, "@",
"ScalerInfo", payloadCode, "@",
"Stream", payloadCode, "@",
"TDCError", payloadCode, "@",
"TDCEvent", payloadCode, "@",
"TDCVHit", payloadCode, "@",
"TDetectorVEvent", payloadCode, "@",
"TDetectorVHit", payloadCode, "@",
"TDigiVCandidate", payloadCode, "@",
"TDigiVError", payloadCode, "@",
"TDigiVEvent", payloadCode, "@",
"TEventInfo", payloadCode, "@",
"TPrimSpecialTrigger", payloadCode, "@",
"TPrimitive", payloadCode, "@",
"TRecoVCandidate", payloadCode, "@",
"TRecoVEvent", payloadCode, "@",
"TRecoVHit", payloadCode, "@",
"TSpecialTrigger", payloadCode, "@",
"TSpecialTriggerEvent", payloadCode, "@",
"TTDCBSpecialTrigger", payloadCode, "@",
"TTimeCluster", payloadCode, "@",
"TVCandidate", payloadCode, "@",
"TVChannelID", payloadCode, "@",
"TVDigi", payloadCode, "@",
"TVEvent", payloadCode, "@",
"TVHit", payloadCode, "@",
"TargetInfo", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libNA62Persistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libNA62Persistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libNA62Persistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libNA62Persistency() {
  TriggerDictionaryInitialization_libNA62Persistency_Impl();
}
