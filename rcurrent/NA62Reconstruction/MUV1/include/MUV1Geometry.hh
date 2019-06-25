#ifndef MUV1Geometry_H
#define MUV1Geometry_H 1

#include "TVector3.h"

class MUV1Geometry
{

  public:

    MUV1Geometry();
    static MUV1Geometry* GetInstance();
    double  GetWorldZLength()                           { return fWorldZLength;             };
    void      SetWorldZLength(double value)             { fWorldZLength = value;            };
    double  GetWorldXLength()                           { return fWorldXLength;             };
    void      SetWorldXLength(double value)             { fWorldXLength = value;            };
    double  GetWorldYLength()                           { return fWorldYLength;             };
    void      SetWorldYLength(double value)             { fWorldYLength = value;            };

    double  GetMUV1ResponsibilityRegionXLength()                 { return fMUV1ResponsibilityRegionXLength;     };
    void      SetMUV1ResponsibilityRegionXLength(double value)   { fMUV1ResponsibilityRegionXLength = value;    };
    double  GetMUV1ResponsibilityRegionYLength()                 { return fMUV1ResponsibilityRegionYLength;     };
    void      SetMUV1ResponsibilityRegionYLength(double value)   { fMUV1ResponsibilityRegionYLength = value;    };
    double  GetMUV1ResponsibilityRegionZBeginning()              { return fMUV1ResponsibilityRegionZBeginning;  };
    void      SetMUV1ResponsibilityRegionZBeginning(double value){ fMUV1ResponsibilityRegionZBeginning = value; };
    double  GetMUV1ResponsibilityRegionZEnd()                    { return fMUV1ResponsibilityRegionZEnd;        };
    void      SetMUV1ResponsibilityRegionZEnd(double value)      { fMUV1ResponsibilityRegionZEnd = value;       };

    double  GetMUV1DetectorFrontZPosition()                      { return fMUV1DetectorFrontZPosition;          };
    void      SetMUV1DetectorFrontZPosition(double value)        { fMUV1DetectorFrontZPosition = value;         };

    double  GetScintillatorThickness()                  { return  fScintillatorThickness;   };
    void      SetScintillatorThickness(double value)    { fScintillatorThickness = value;   };
    double  GetIronThickness()                          { return  fIronThickness;           };
    void      SetIronThickness(double value)            { fIronThickness = value;           };
    double  GetRubberThickness()                        { return fRubberThickness;          };
    void      SetRubberThickness(double value)          { fRubberThickness = value;         };


    int     GetNIronPlate()                             { return  fNIronPlate;              };
    void      SetNIronPlate(int value)                  { fNIronPlate = value;              };

    int     GetIronPlateSizeX()                         { return  fIronPlateSizeX;          };
    void      SetIronPlateSizeX(int value)              { fIronPlateSizeX = value;          };
    int     GetIronPlateSizeY()                         { return  fIronPlateSizeY;          };
    void      SetIronPlateSizeY(int value)              { fIronPlateSizeY = value;          };
    int     GetIronPlateOuterSizeX()                    { return  fIronPlateOuterSizeX;     };
    void      SetIronPlateOuterSizeX(int value)         { fIronPlateOuterSizeX = value;     };
    int     GetIronPlateOuterSizeY()                    { return  fIronPlateOuterSizeY;     };
    void      SetIronPlateOuterSizeY(int value)         { fIronPlateOuterSizeY = value;     };
    double  GetConnectionHoleRadius()                   {  return fConnectionHoleRadius;    };
    void      SettConnectionHoleRadius(double value)    { fConnectionHoleRadius = value;    };
    double  GetBoltPositionX()                          { return fBoltPositionX;            };
    void      SetBoltPositionX(double value)            { fBoltPositionX = value;           };
    double  GetBoltPositionY()                          { return fBoltPositionY;            };
    void      SetBoltPositionY(double value)            { fBoltPositionY = value;           };

    int     GetNRubberPlate()                           { return  fNRubberPlate;            };
    void      SetNRubberPlate(int value)                { fNRubberPlate = value;            };

    double  GetMUV1XSize()                              { return fMUV1XSize;                };
    void      SetMUV1XSize(double value)                { fMUV1XSize = value;               };
    double  GetMUV1YSize()                              { return fMUV1YSize;                };
    void      SetMUV1YSize(double value)                { fMUV1YSize = value;               };
    //new
    double  GetMUV1Length()                             { return fMUV1Length;               };
    void      SetMUV1Length(double value)               { fMUV1Length = value;              };
    double  GetMUV1ZPosition()                          { return fMUV1ZPosition;            };
    void      SetMUV1ZPosition(double value)            { fMUV1ZPosition = value;           };

    double  GetScintWidthStandard()                     { return fScintWidthStandard;       };
    void      SetScintWidthStandard(double value)       { fScintWidthStandard = value;      };
    double  GetScintWidthMiddle()                       { return fScintWidthMiddle;         };
    void      SetScintWidthMiddle(double value)         { fScintWidthMiddle = value;        };
    double  GetScintWidthOuter()                        { return fScintWidthOuter;          };
    void      SetScintWidthOuter(double value)          { fScintWidthOuter = value;         };

    double  GetScintLengthStandard()                    { return fScintLengthStandard;      };
    void      SetScintLengthStandard(double value)      { fScintLengthStandard = value;     };
    double  GetScintLengthMiddleStd()                   { return fScintLengthMiddleStd;     };
    void      SetScintLengthMiddleStd(double value)     { fScintLengthMiddleStd = value;    };
    double  GetScintLengthMiddleOuter()                 { return fScintLengthMiddleOuter;   };
    void      SetScintLengthMiddleOuter(double value)   { fScintLengthMiddleOuter = value;  };
    double* GetScintLengthOuter()                       { return fScintLengthOuter;         };
    void      SetScintLengthOuter(double* value)        { fScintLengthOuter = value;        };
    double  GetmOCounterCutHeight()                     { return fmOCounterCutHeight;       };
    void      SetmOCounterCutHeight(double value)       { fmOCounterCutHeight = value;      };
    double  GetSkinWidth()                              { return fSkinWidth;                };
    void      SetSkinWidth(double value)                { fSkinWidth = value;               };


    int     GetNMUV1CounterStandard()                   { return  fNMUV1CounterStandard;    };
    void      SetNMUV1CounterStandard(int value)        { fNMUV1CounterStandard = value;    };
    int     GetNMUV1CounterMiddle()                     { return  fNMUV1CounterMiddle;      };
    void      SetNMUV1CounterMiddle(int value)          { fNMUV1CounterMiddle = value;      };
    int     GetNMUV1CounterMiddleStd()                  { return  fNMUV1CounterMiddleStd;   };
    void      SetNMUV1CounterMiddleStd(int value)       { fNMUV1CounterMiddleStd = value;   };
    int     GetNMUV1CounterMiddleOuter()                { return  fNMUV1CounterMiddleOuter; };
    void      SetNMUV1CounterMiddleOuter(int value)     { fNMUV1CounterMiddleOuter = value; };
    int     GetNMUV1CounterOuter()                      { return  fNMUV1CounterOuter;       };
    void      SetNMUV1CounterOuter(int value)           { fNMUV1CounterOuter = value;       };

    int     GetNScintVertical()                         { return  fNScintVertical;          };
    void      SetNScintVertical(int value)              { fNScintVertical = value;          };
    int     GetNScintHorizontal()                       { return  fNScintHorizontal;        };
    void      SetNScintHorizontal(int value)            { fNScintHorizontal = value;        };

    double  GetHoleDiameter()                           { return fHoleDiameter;             };
    void      SetHoleDiameter(double value)             { fHoleDiameter = value;            };

    void      SetNFiber(int value)                      { fNFiber = value;                  };
    int     GetNFiber()                                 { return fNFiber;                   };
    void      SetFiberRadius(double value)              { fFiberRadius = value;             };
    double  GetFiberRadius()                            { return fFiberRadius;              };
    void      SetFiberSeperation(double value)          { fFiberSeperation = value;         };
    double  GetFiberSeperation()                        { return fFiberSeperation;          };
    void      SetFirstFiberXPosition(double value)      { fFirstFiberXPosition = value;     };
    double  GetFirstFiberXPosition()                    { return fFirstFiberXPosition;      };
    void      SetFiberLengthOut(double value)           { fFiberLengthOut = value;          };
    double  GetFiberLengthOut()                         { return fFiberLengthOut;           };
    void      SetFiberPhotonEnergyCut(double value)     { fFiberPhotonEnergyCut = value;    };
    double  GetFiberPhotonEnergyCut()                   { return fFiberPhotonEnergyCut;     };
    void      SetGrooveWidth(double value)              { fGrooveWidth = value;             };
    double  GetGrooveWidth()                            { return fGrooveDepth;              };
    void      SetGrooveDepth(double value)              { fGrooveDepth = value;             };
    double  GetGrooveDepth()                            { return fGrooveDepth;              };
    void      SetGroovePositionInStandardScintillator(double value) { fGroovePositionInStandardScintillator = value; };
    double  GetGroovePositionInStandardScintillator()               { return fGroovePositionInStandardScintillator;  };
    void      SetPMTSize(TVector3 value)                { fPMTSize = value;                 };
    TVector3  GetPMTSize()                              { return fPMTSize;                  };
    void      SetPMTAirGap(double value)                { fPMTAirGap = value;               };
    double  GetPMTAirGap()                              { return fPMTAirGap;                };
    void      SetPMTGlassGap(double value)              { fPMTGlassGap = value;             };
    double  GetPMTGlassGap()                            { return fPMTGlassGap;              };
    void      SetCutDepth(double value)                 { fCutDepth = value;                };
    double  GetCutDepth()                               { return fCutDepth;                 };
    void      SetCutWidth(double value)                 { fCutWidth = value;                };
    double  GetCutWidth()                               { return fCutWidth;                 };
    void      SetScintillatorSpacerSize(TVector3 value) {fScintillatorSpacerSize = value;   };
    TVector3 GetScintillatorSpacerSize()                { return fScintillatorSpacerSize;   };
    void      SetCutBoxOuterSize(TVector3 value)        { fCutBoxOuterSize = value;         };
    TVector3 GetCutBoxOuterSize()                       { return fCutBoxOuterSize;          };
    void      SetCutBoxMiddleOuterSize(TVector3 value)  { fCutBoxMiddleOuterSize = value;   };
    TVector3 GetCutBoxMiddleOuterSize()                 { return fCutBoxMiddleOuterSize;    };
    void      SetCutBoxMiddleInnerSize(TVector3 value)  { fCutBoxMiddleInnerSize = value;   };
    TVector3 GetCutBoxMiddleInnerSize()                 { return fCutBoxMiddleInnerSize;    };
    void      SetCutBoxInnerSize(TVector3 value)        { fCutBoxInnerSize = value;         };
    TVector3 GetCutBoxInnerSize()                       { return fCutBoxInnerSize;          };
    void      SetInnerRadiusSpacer(double value)        { fInnerRadiusSpacer = value;       };
    double   GetInnerRadiusSpacer()	                { return fInnerRadiusSpacer;        };
    void      SetInnerBeamPipeRadius(double value)      { fInnerBeamPipeRadius = value;     };
    double   GetInnerBeamPipeRadius()                   { return fInnerBeamPipeRadius;      };

    double   GetOuterBeamPipeRadius()                   { return fOuterBeamPipeRadius; };
    void      SetOuterBeamPipeRadius(double value)      { fOuterBeamPipeRadius = value; };
    double   GetLongitudinalLengthBeamPipe()            { return fLongitudinalLengthBeamPipe; };
    void      SetLongitudinalLengthBeamPipe(double value){ fLongitudinalLengthBeamPipe = value; };

    int      GetScintillatorAt (double X);
  private:

    double fScintillatorPosition[45];

    double  fWorldZLength;
    double  fWorldXLength;
    double  fWorldYLength;

    double  fMUV1ResponsibilityRegionXLength;
    double  fMUV1ResponsibilityRegionYLength;
    double  fMUV1ResponsibilityRegionZBeginning;
    double  fMUV1ResponsibilityRegionZEnd;

    double  fMUV1DetectorFrontZPosition;

    double  fScintillatorThickness;
    double  fIronThickness;
    double  fRubberThickness;

    int     fNIronPlate;
    double fIronPlateSizeX;
    double fIronPlateSizeY;
    double fIronPlateOuterSizeX;
    double fIronPlateOuterSizeY;
    double fConnectionHoleRadius;
    double fBoltPositionX;
    double fBoltPositionY;


    int     fNRubberPlate;

    double  fMUV1XSize;
    double  fMUV1YSize;
    double  fMUV1Length;

    double  fMUV1ZPosition;

    double  fScintWidthStandard;
    double  fScintWidthMiddle;
    double  fScintWidthOuter;

    double  fScintLengthStandard;
    double  fScintLengthMiddleStd;
    //  double* fScintLengthMiddleOuter;
    double fScintLengthMiddleOuter;
    double* fScintLengthOuter;
    double  fmOCounterCutHeight;

    double fSkinWidth;   // Skin width of the aluminium and air layers of the scintillator

    int     fNMUV1CounterStandard;
    int     fNMUV1CounterMiddleStd;
    int     fNMUV1CounterMiddleOuter;
    int     fNMUV1CounterMiddle;
    int     fNMUV1CounterOuter;

    int     fNScintVertical;
    int     fNScintHorizontal;

    double  fHoleDiameter;

    double  fFiberRadius;
    int     fNFiber;
    double  fFiberSeperation;
    double  fFirstFiberXPosition;
    double  fCutWidth;
    double  fCutDepth;
    double  fFiberLengthOut;
    double  fFiberPhotonEnergyCut;

    //Scintillator Spacer
    TVector3 fScintillatorSpacerSize;
    TVector3 fCutBoxOuterSize;
    TVector3 fCutBoxMiddleOuterSize;
    TVector3 fCutBoxMiddleInnerSize;
    TVector3 fCutBoxInnerSize;
    double fInnerRadiusSpacer;

    //Groove
    double fGrooveWidth ;
    double fGrooveDepth;
    double fGroovePositionInStandardScintillator;

    //PMT

    TVector3 fPMTSize;
    double fPMTAirGap;
    double fPMTGlassGap;
    //Beam pipe

    double fInnerBeamPipeRadius;
    double fOuterBeamPipeRadius;

    double fLongitudinalLengthBeamPipe;



    static MUV1Geometry* fInstance;

  private:

    void CreateGeometry();

  public:


  private:

};
#endif
