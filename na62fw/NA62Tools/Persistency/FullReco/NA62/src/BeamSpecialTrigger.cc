// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-23
//
// ---------------------------------------------------------------

#include "BeamSpecialTrigger.hh"

#include "Riostream.h"

ClassImp(BeamSpecialTrigger)

void TargetInfo::Clear(Option_t* /*option*/){
  fIntensity              = 0;
  fIntensityDownstream    = 0;
  fIntensityNotNormalised = 0;
  fMultiplicity           = 0;
  fSymmetry               = 0;
  fSymmetryWithoutBSM     = 0;
  fReserved               = 0;
}

void MagnetInfo::Clear(Option_t* /*option*/){
  fAcqStamp     = 0;
  fCycleStamp   = 0;
  fReserved     = 0;
  fCurrentValue = 0;
}

void ScalerInfo::Clear(Option_t* /*option*/){
  fAcqStamp     = 0;              
  fCounts       = 0;
  fCycleStamp   = 0;
  fReserved     = 0;
}

void PrimitiveInfo::Clear(Option_t* /*option*/){
  fBBQCHOD         = 0;
  fBBQRICH         = 0;
  fBBQLAV          = 0;
  fBBQMUV3         = 0;
  fBBQNewCHOD      = 0;
  fBBQTALK         = 0;
  fBBQLKr          = 0;
  fMeanRateCHOD    = 0;
  fMeanRateRICH    = 0;
  fMeanRateLAV     = 0;
  fMeanRateMUV3    = 0;
  fMeanRateNewCHOD = 0;
  fMeanRateTALK    = 0;
  fMeanRateLKr     = 0;
}

void TargetInfo::SetInfo(TargetInfoStruct structInfo){
  fIntensity              = structInfo.Intensity;             
  fIntensityDownstream    = structInfo.IntensityDownstream;   
  fIntensityNotNormalised = structInfo.IntensityNotNormalised;
  fMultiplicity           = structInfo.Multiplicity;          
  fSymmetry               = structInfo.Symmetry;              
  fSymmetryWithoutBSM     = structInfo.SymmetryWithoutBSM;    
  fReserved               = structInfo.Reserved;
}

void MagnetInfo::SetInfo(MagnetInfoStruct structInfo){
  fAcqStamp     = structInfo.AcqStamp;              
  fCycleStamp   = structInfo.CycleStamp;
  fReserved     = structInfo.Reserved;
  fCurrentValue = structInfo.CurrentValue;
}

void ScalerInfo::SetInfo(ScalerInfoStruct structInfo){
  fAcqStamp     = structInfo.AcqStamp;              
  fCounts       = structInfo.Counts;
  fCycleStamp   = structInfo.CycleStamp;
  fReserved     = structInfo.Reserved;
}

void PrimitiveInfo::SetInfo(PrimitiveInfoStruct structInfo){
  fBBQCHOD         = structInfo.BBQCHOD;
  fBBQRICH         = structInfo.BBQRICH;
  fBBQLAV          = structInfo.BBQLAV;
  fBBQMUV3         = structInfo.BBQMUV3;
  fBBQNewCHOD      = structInfo.BBQNewCHOD;
  fBBQTALK         = structInfo.BBQTALK;
  fBBQLKr          = structInfo.BBQLKr;
  fMeanRateCHOD    = structInfo.MeanRateCHOD;
  fMeanRateRICH    = structInfo.MeanRateRICH;
  fMeanRateLAV     = structInfo.MeanRateLAV;
  fMeanRateMUV3    = structInfo.MeanRateMUV3;
  fMeanRateNewCHOD = structInfo.MeanRateNewCHOD;
  fMeanRateTALK    = structInfo.MeanRateTALK;
  fMeanRateLKr     = structInfo.MeanRateLKr;
}

BeamSpecialTrigger::BeamSpecialTrigger(){
  Clear();
}

BeamSpecialTrigger::~BeamSpecialTrigger(){}

void BeamSpecialTrigger::Clear(Option_t* option){
  fTimeStamp=0;
  fT10.Clear(option);
  fBEND_101_195.Clear(option);
  fBEND_101_196.Clear(option);
  fTRIM_101_102.Clear(option);
  fQX.Clear(option);
  fQ1_OR.Clear(option);
  fMUV1_OR_MUV2.Clear(option);
  fMUV3.Clear(option);
  fNHOD.Clear(option);
  fIRC.Clear(option);
  fCHANTI.Clear(option);
  fECN3_008.Clear(option);
  fECN3_009.Clear(option);
  fECN3_010.Clear(option);
  fECN3_011.Clear(option);
  fECN3_012.Clear(option);
  fARGONION.Clear(option);
  fPrimitives.Clear(option);    
}

Bool_t BeamSpecialTrigger::SetHeader(UInt_t * pDataBuffer,UInt_t NumberOfWords){

  uint32_t TimeStamp = *(pDataBuffer+1);
  TargetInfoStruct structTargetInfo; //T10 info, changed in 2017
  memcpy(&(structTargetInfo),pDataBuffer+2,sizeof(TargetInfoStruct));
  Int_t NTargetInfoWords = sizeof(TargetInfoStruct)/sizeof(uint32_t);
  if(NumberOfWords>87) {
    // Added in 2017 (during run 7701),
    // not propagated to the persistency: 
    // - uint32_t intensityDowstreamSignalOK;
    // - uint32_t intensitySignalOK;
    // - uint32_t multiplicitySignalOK;
    // - uint32_t symmetrySignalOK;
    NTargetInfoWords+=4;
  }
  BeamInfoStruct structBeamInfo;
  memcpy(&(structBeamInfo),pDataBuffer+2+NTargetInfoWords,sizeof(BeamInfoStruct));

  //structBeamInfo.ARGONION.Counts *= 7400; //normalisation factor: 7400
  structBeamInfo.ARGONION.Counts *= 10000; //new normalisation factor: 10000
  // store values in the persistent variables
  fTimeStamp = TimeStamp;
  fT10.SetInfo(structTargetInfo);
  fBEND_101_195.SetInfo(structBeamInfo.BEND_101_195);
  fBEND_101_196.SetInfo(structBeamInfo.BEND_101_196);
  fTRIM_101_102.SetInfo(structBeamInfo.TRIM_101_102);
  fQX.SetInfo(structBeamInfo.QX);
  fQ1_OR.SetInfo(structBeamInfo.Q1_OR);
  fMUV1_OR_MUV2.SetInfo(structBeamInfo.MUV1_OR_MUV2);
  fMUV3.SetInfo(structBeamInfo.MUV3);
  fNHOD.SetInfo(structBeamInfo.NHOD);
  fIRC.SetInfo(structBeamInfo.IRC);
  fCHANTI.SetInfo(structBeamInfo.CHANTI);
  fECN3_008.SetInfo(structBeamInfo.ECN3_008);
  fECN3_009.SetInfo(structBeamInfo.ECN3_009);
  fECN3_010.SetInfo(structBeamInfo.ECN3_010);
  fECN3_011.SetInfo(structBeamInfo.ECN3_011);
  fECN3_012.SetInfo(structBeamInfo.ECN3_012);
  fARGONION.SetInfo(structBeamInfo.ARGONION);
  if(NumberOfWords<=73) return kTRUE; //Block length without primitive info (<= run 6815)
  fPrimitives.SetInfo(structBeamInfo.Primitives);
  return kTRUE;
}
