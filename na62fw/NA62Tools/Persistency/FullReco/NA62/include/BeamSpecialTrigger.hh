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
