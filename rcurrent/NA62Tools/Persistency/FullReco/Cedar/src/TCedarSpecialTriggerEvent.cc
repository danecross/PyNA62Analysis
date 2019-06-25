// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-24
//
// --------------------------------------------------------------
#include "TCedarSpecialTriggerEvent.hh"
#include "Riostream.h"

ClassImp(TCedarSpecialTriggerEvent)

TCedarSpecialTriggerEvent::TCedarSpecialTriggerEvent() : TSpecialTriggerEvent(){
  Clear();
}

TCedarSpecialTriggerEvent::TCedarSpecialTriggerEvent(TClass* Class) : TSpecialTriggerEvent(Class){
  Clear();
}

CedarDIMInfo::CedarDIMInfo():fTimeStamp(0),fTempRear(0),fTempFront(0),fTempDiaph(0),fMotorPosX(0),fMotorPosY(0),fDiaphragmAperture(0),fPressureState(0),fPressure(0),fPressureSetPoint(0),fHVStatus(0),fFEStatus(0),fCedarStatus(0),fAlignStatus(0),fKTAGEnvStatus(0),fKTAGStatus(0),fWienerStatus(0){}

void CedarDIMInfo::Clear(Option_t* /*option*/){
  fTimeStamp         = 0;
  fTempRear          = 0;
  fTempFront         = 0;
  fTempDiaph         = 0;
  fMotorPosX         = 0;
  fMotorPosY         = 0;
  fDiaphragmAperture = 0;
  fPressureState     = 0;
  fPressure          = 0;
  fPressureSetPoint  = 0;
  fHVStatus          = 0;
  fFEStatus          = 0;
  fCedarStatus       = 0;
  fAlignStatus       = 0;
  fKTAGEnvStatus     = 0;
  fKTAGStatus        = 0;
  fWienerStatus      = 0;
}

void CedarDIMInfo::SetInfo(CedarDIMInfoStruct structInfo){
  fTimeStamp         = structInfo.TimeStamp;
  fTempRear          = ((Float_t)structInfo.TempRear)/1000.;
  fTempFront         = ((Float_t)structInfo.TempFront)/1000.;
  fTempDiaph         = ((Float_t)structInfo.TempDiaph)/1000.;
  fMotorPosX         = ((Float_t)structInfo.MotorPosX)/1000.;
  fMotorPosY         = ((Float_t)structInfo.MotorPosY)/1000.;
  fDiaphragmAperture = ((Float_t)structInfo.DiaphragmAperture)/1000.;
  fPressureState     = structInfo.PressureState;
  fPressure          = ((Float_t)structInfo.Pressure)/1000.;
  fPressureSetPoint  = ((Float_t)structInfo.PressureSetPoint)/1000.;
  fHVStatus          = structInfo.HVStatus;
  fFEStatus          = structInfo.FEStatus;
  fCedarStatus       = structInfo.CedarStatus;
  fAlignStatus       = structInfo.AlignStatus;
  fKTAGEnvStatus     = structInfo.KTAGEnvStatus;
  fKTAGStatus        = structInfo.KTAGStatus;
  fWienerStatus      = structInfo.WienerStatus;
}

CedarDIMInfo& CedarDIMInfo::operator=(const CedarDIMInfo& rhs){
  fTimeStamp         = rhs.fTimeStamp;
  fTempRear          = rhs.fTempRear;
  fTempFront         = rhs.fTempFront;
  fTempDiaph         = rhs.fTempDiaph;
  fMotorPosX         = rhs.fMotorPosX;
  fMotorPosY         = rhs.fMotorPosY;
  fDiaphragmAperture = rhs.fDiaphragmAperture;
  fPressureState     = rhs.fPressureState;
  fPressure          = rhs.fPressure;
  fPressureSetPoint  = rhs.fPressureSetPoint;
  fHVStatus          = rhs.fHVStatus;
  fFEStatus          = rhs.fFEStatus;
  fCedarStatus       = rhs.fCedarStatus;
  fAlignStatus       = rhs.fAlignStatus;
  fKTAGEnvStatus     = rhs.fKTAGEnvStatus;
  fKTAGStatus        = rhs.fKTAGStatus;
  fWienerStatus      = rhs.fWienerStatus;

  return *this;
}

void TCedarSpecialTriggerEvent::Clear(Option_t* option){
  TSpecialTriggerEvent::Clear(option);
  fDIMInfo.Clear(option);
}

void TCedarSpecialTriggerEvent::SetDIMInfo(UInt_t* pDataBuffer){
  CedarDIMInfoStruct structCedarDIMInfo;
  memcpy(&(structCedarDIMInfo),pDataBuffer+1,sizeof(CedarDIMInfoStruct));
  // store values in the persistent variables
  fDIMInfo.SetInfo(structCedarDIMInfo);
}
