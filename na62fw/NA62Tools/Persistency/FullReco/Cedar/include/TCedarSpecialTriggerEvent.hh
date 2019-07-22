// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-24
//
// --------------------------------------------------------------
#ifndef TCedarSpecialTriggerEvent_H
#define TCedarSpecialTriggerEvent_H
#include "TClass.h"
#include "TSpecialTriggerEvent.hh"

struct CedarDIMInfoStruct;

class CedarDIMInfo : public TObject {
  public:
    CedarDIMInfo();
    ~CedarDIMInfo(){};
    void SetInfo(CedarDIMInfoStruct);
    void Clear(Option_t* = "");
    UInt_t  GetTimeStamp()          { return fTimeStamp;          }
    Float_t GetTempRear()           { return fTempRear;           }
    Float_t GetTempFront()          { return fTempFront;          }
    Float_t GetTempDiaph()          { return fTempDiaph;          }
    Float_t GetMotorPosX()          { return fMotorPosX;          }
    Float_t GetMotorPosY()          { return fMotorPosY;          }
    Float_t GetDiaphragmAperture()  { return fDiaphragmAperture;  }
    UInt_t  GetPressureState()      { return fPressureState;      }
    Float_t GetPressure()           { return fPressure;           }
    Float_t GetPressureSetPoint()   { return fPressureSetPoint;   }
    UInt_t  GetHVStatus()           { return fHVStatus;           }
    UInt_t  GetFEStatus()           { return fFEStatus;           }
    UInt_t  GetCedarStatus()        { return fCedarStatus;        }
    UInt_t  GetAlignStatus()        { return fAlignStatus;        }
    UInt_t  GetKTAGEnvStatus()      { return fKTAGEnvStatus;      }
    UInt_t  GetKTAGStatus()         { return fKTAGStatus;         }
    UInt_t  GetWienerStatus()       { return fWienerStatus;       }

    CedarDIMInfo& operator=(const CedarDIMInfo& obj);

  private:
    UInt_t  fTimeStamp;
    Float_t fTempRear;
    Float_t fTempFront;
    Float_t fTempDiaph;
    Float_t fMotorPosX;
    Float_t fMotorPosY;
    Float_t fDiaphragmAperture;
    UInt_t  fPressureState;
    Float_t fPressure;
    Float_t fPressureSetPoint;
    UInt_t  fHVStatus;
    UInt_t  fFEStatus;
    UInt_t  fCedarStatus;
    UInt_t  fAlignStatus;
    UInt_t  fKTAGEnvStatus;
    UInt_t  fKTAGStatus;
    UInt_t  fWienerStatus;

    ClassDef(CedarDIMInfo,1);
};

class TCedarSpecialTriggerEvent : public TSpecialTriggerEvent {

  public:
    TCedarSpecialTriggerEvent();
    explicit TCedarSpecialTriggerEvent(TClass *);
    ~TCedarSpecialTriggerEvent(){};
    void Clear(Option_t* = "");
    void SetDIMInfo(UInt_t * value);
    CedarDIMInfo GetDIMInfo()        { return fDIMInfo;                        }
    UInt_t  GetTimeStamp()           { return fDIMInfo.GetTimeStamp();         }
    Float_t GetTempRear()            { return fDIMInfo.GetTempRear();          }
    Float_t GetTempFront()           { return fDIMInfo.GetTempFront();         }
    Float_t GetTempDiaph()           { return fDIMInfo.GetTempDiaph();         }
    Float_t GetMotorPosX()           { return fDIMInfo.GetMotorPosX();         }
    Float_t GetMotorPosY()           { return fDIMInfo.GetMotorPosY();         }
    Float_t GetDiaphragmAperture()   { return fDIMInfo.GetDiaphragmAperture(); }
    UInt_t  GetPressureState()       { return fDIMInfo.GetPressureState();     }
    Float_t GetPressure()            { return fDIMInfo.GetPressure();          }
    Float_t GetPressureSetPoint()    { return fDIMInfo.GetPressureSetPoint();  }
    UInt_t  GetHVStatus()            { return fDIMInfo.GetHVStatus();          }
    UInt_t  GetFEStatus()            { return fDIMInfo.GetFEStatus();          }
    UInt_t  GetCedarStatus()         { return fDIMInfo.GetCedarStatus();       }
    UInt_t  GetAlignStatus()         { return fDIMInfo.GetAlignStatus();       }
    UInt_t  GetKTAGEnvStatus()       { return fDIMInfo.GetKTAGEnvStatus();     }
    UInt_t  GetKTAGStatus()          { return fDIMInfo.GetKTAGStatus();        }
    UInt_t  GetWienerStatus()        { return fDIMInfo.GetWienerStatus();      }

  private:

    CedarDIMInfo fDIMInfo;

    ClassDef(TCedarSpecialTriggerEvent,1);

};

// --- structure definitions [needed to extract values from buffer --- //

struct CedarDIMInfoStruct {
  uint32_t TimeStamp;           //unix time (s)
  uint32_t TempRear;            //(temperature in degrees C)*1000 [rear      sensor]
  uint32_t TempFront;           //(temperature in degrees C)*1000 [front     sensor]
  uint32_t TempDiaph;           //(temperature in degrees C)*1000 [diaphragm sensor]
  uint32_t MotorPosX;           //(x motor position in mm)*1000
  uint32_t MotorPosY;           //(y motor position in mm)*1000
  uint32_t DiaphragmAperture;   //(diaphragm aperture in mm)*1000
  uint32_t PressureState;       //pressure state (1: stable, 2: ramping up, 3: ramping down, 4: stabilising, 5: unknown, 6: error)
  uint32_t Pressure;            //(Cedar pressure in bar)*1000
  uint32_t PressureSetPoint;    //(Cedar pressure set point in bar)*1000
  uint32_t HVStatus;            //High Voltage Status (1: OK, 0: Error)
  uint32_t FEStatus;            //Front-End    Status (1: OK, 0: Error)
  uint32_t CedarStatus;         //Cedar        Status (1: OK, 0: Error)
  uint32_t AlignStatus;         //Alignment    Status (1: OK, 0: Error)
  uint32_t KTAGEnvStatus;       //Environment  Status (1: OK, 0: Error)
  uint32_t KTAGStatus;          //KTAG         Status (1: OK, 0: Error)
  uint32_t WienerStatus;        //Wiener       Status (1: OK, 0: Error)
};
// ------------------------------------------------------------------- //
#endif
