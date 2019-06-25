#ifndef RICHGeometryParameters_H
#define RICHGeometryParameters_H 1

#include "globals.hh"
#include "TVector2.h"
#include "TObjArray.h"
#include "G4TwoVector.hh"

#include "NA62VGeometryParameters.hh"

class RICHGeometryParameters : public NA62VGeometryParameters {

public:
  
  ~RICHGeometryParameters() {}
  static RICHGeometryParameters* GetInstance();
  void Print();
  void fromLabtoRICHBox(G4double,G4double,G4double*,G4double*);
  void fromRICHBoxtoLab(G4double,G4double,G4double*,G4double*);

private:
  
  static RICHGeometryParameters* fInstance;
  
protected:
  
  RICHGeometryParameters();
  
public:

  G4int                GetNChannels()                                     { return fNChannels;                    };
  void                 SetNChannels(G4int value)                          { fNChannels = value;                   };
  
  G4double             GetRespRegionZStart()                              { return fRespRegionZStart;              }
  G4double             GetRespRegionZEnd()                                { return fRespRegionZEnd;                }
  G4double             GetRespRegionZLength()                             { return fRespRegionZLength;             }
  G4double             GetRespRegionUpstreamLength()                      { return fRespRegionUpstreamLength;      }
  G4double             GetRespRegionUpstreamRadius()                      { return fRespRegionUpstreamRadius;      }
  G4double             GetRespRegionDownstreamLength()                    { return fRespRegionDownstreamLength;    }
  G4double             GetRespRegionDownstreamRadius()                    { return fRespRegionDownstreamRadius;    }

  G4double             GetRICHDetectorZStart()                            { return fRICHDetectorZStart;           };
  void                 SetRICHDetectorZStart(G4double value)              { fRICHDetectorZStart = value;          };
  G4double             GetRICHDetectorZEnd()                              { return fRICHDetectorZEnd;             };
  void                 SetRICHDetectorZEnd(G4double value)                { fRICHDetectorZEnd = value;            };
  G4double             GetRICHDetectorZPosition()                         { return fRICHDetectorZPosition;        };
  void                 SetRICHDetectorZPosition(G4double value)           { fRICHDetectorZPosition = value;       };

  G4double             GetRICHDetectorRadius()                            { return fRICHDetectorRadius;           };
  void                 SetRICHDetectorRadius(G4double value)              { fRICHDetectorRadius = value;          };

  G4double             GetRICHDetectorZLength()                           { return fRICHDetectorZLength;          };
  void                 SetRICHDetectorZLength(G4double value)             { fRICHDetectorZLength = value;         };
  G4double             GetRICHDetectorXLength()                           { return fRICHDetectorXLength;          };
  void                 SetRICHDetectorXLength(G4double value)             { fRICHDetectorXLength = value;         };
  G4double             GetRICHDetectorYLength()                           { return fRICHDetectorYLength;          };
  void                 SetRICHDetectorYLength(G4double value)             { fRICHDetectorYLength = value;         };

  G4double             GetMirrorWindowZLength()                           { return fMirrorWindowZLength;          };
  void                 SetMirrowWindowZLength(G4double value)             { fMirrorWindowZLength = value;         };
  G4double             GetMirrorWindowThickness()                         { return fMirrorWindowThickness;        };
  void                 SetMirrowWindowThickness(G4double value)           { fMirrorWindowThickness = value;       };
  G4double             GetMirrorWindowInnerRadius()                       { return fMirrorWindowInnerRadius;      };
  void                 SetMirrowWindowInnerRadius(G4double value)         { fMirrorWindowInnerRadius = value;     };
  G4double             GetMirrorWindowOuterRadius()                       { return fMirrorWindowOuterRadius;      };
  void                 SetMirrowWindowOuterRadius(G4double value)         { fMirrorWindowOuterRadius = value;     };
  G4double             GetMirrorWindowZPosition()                         { return fMirrorWindowZPosition;        };
  void                 SetMirrowWindowZPosition(G4double value)           { fMirrorWindowZPosition = value;       };

  G4double             GetMirrorWindowInnerFlangeZLength()                       { return fMirrorWindowInnerFlangeZLength;      };
  void                 SetMirrowWindowInnerFlangeZlength(G4double value)         { fMirrorWindowInnerFlangeZLength = value;     };
  G4double             GetMirrorWindowInnerFlangeZPosition()                     { return fMirrorWindowInnerFlangeZPosition;    };
  void                 SetMirrowWindowInnerFlangeZPosition(G4double value)       { fMirrorWindowInnerFlangeZPosition = value;   };
  G4double             GetMirrorWindowInnerFlangeInnerRadius()                   { return fMirrorWindowInnerFlangeInnerRadius;  };
  void                 SetMirrowWindowInnerFlangeInnerRadius(G4double value)     { fMirrorWindowInnerFlangeInnerRadius = value;     };
  G4double             GetMirrorWindowInnerFlangeRadialThickness()               { return fMirrorWindowInnerFlangeRadialThickness;  };
  void                 SetMirrowWindowInnerFlangeRadialThickness(G4double value) { fMirrorWindowInnerFlangeRadialThickness = value; };

  G4double             GetMirrorWindowOuterFlangeZLength()                       { return fMirrorWindowOuterFlangeZLength;      };
  void                 SetMirrowWindowOuterFlangeZlength(G4double value)         { fMirrorWindowOuterFlangeZLength = value;     };
  G4double             GetMirrorWindowOuterFlangeInnerRadius()                   { return fMirrorWindowOuterFlangeInnerRadius;  };
  void                 SetMirrowWindowOuterFlangeInnerRadius(G4double value)     { fMirrorWindowOuterFlangeInnerRadius = value;     };
  G4double             GetMirrorWindowOuterFlangeRadialThickness()               { return fMirrorWindowOuterFlangeRadialThickness;  };
  void                 SetMirrowWindowOuterFlangeRadialThickness(G4double value) { fMirrorWindowOuterFlangeRadialThickness = value; };
  G4double             GetMirrorWindowOuterFlangeZPosition()                     { return fMirrorWindowOuterFlangeZPosition;    };
  void                 SetMirrowWindowOuterFlangeZPosition(G4double value)       { fMirrorWindowOuterFlangeZPosition = value;   };


  G4double             GetInterfaceRingZLength()                                 { return fInterfaceRingZLength;            };
  void                 SetInterfaceRingZLength(G4double value)                   { fInterfaceRingZLength = value;           };
  G4double             GetInterfaceRingRadialThickness()                         { return fInterfaceRingRadialThickness;    };
  void                 SetInterfaceRingRadialThickness(G4double value)           { fInterfaceRingRadialThickness = value;   };
  G4double             GetInterfaceRingInnerRadius()                             { return fInterfaceRingInnerRadius;        };
  void                 SetInterfaceRingInnerRadius(G4double value)               { fInterfaceRingInnerRadius = value;       };
  G4double             GetInterfaceRingZPosition()                               { return fInterfaceRingZPosition;          };
  void                 SetInterfaceRingZPosition(G4double value)                 { fInterfaceRingZPosition = value;         };

  
  G4int                GetNVesselSections()                               { return fNVesselSections;              };
  void                 SetNVesselSections(G4int value)                    { fNVesselSections = value;             };
  G4double             GetVesselRadialThickness()                         { return fVesselRadialThickness;        };
  void                 SetVesselRadialThickness(G4double value)           { fVesselRadialThickness = value;       };
  G4double             GetVesselZLength()                                 { return fVesselZLength;                };
  void                 SetVesselZLength(G4double value)                   { fVesselZLength = value;               };
  G4double             GetVesselZPosition()                               { return fVesselZPosition;              };
  void                 SetVesselZPosition(G4double value)                 { fVesselZPosition = value;             };


  G4double *           GetVesselSectionZLength()                          { return fVesselSectionZLength;                     };
  G4double *           GetVesselSectionInnerRadius()                      { return fVesselSectionInnerRadius;                 };

  G4double *           GetVesselSectionPressureFlangeZLength()             { return fVesselSectionPressureFlangeZLength;               };
  G4double *           GetVesselSectionPressureFlangeRadialThickness()     { return fVesselSectionPressureFlangeRadialThickness;       };
  G4double *           GetVesselSectionPressureFlangeInnerRadius()         { return fVesselSectionPressureFlangeInnerRadius;           };
  G4double *           GetVesselSectionPressureFlangeZShift()              { return fVesselSectionPressureFlangeZShift;                };
  G4double *           GetVesselSectionPressureFlangeZPosition()           { return fVesselSectionPressureFlangeZPosition;             };

  G4double *           GetVesselSectionUpstreamFlangeZLength()             { return fVesselSectionUpstreamFlangeZLength;               };
  G4double *           GetVesselSectionUpstreamFlangeRadialThickness()     { return fVesselSectionUpstreamFlangeRadialThickness;       };
  G4double *           GetVesselSectionUpstreamFlangeInnerRadius()         { return fVesselSectionUpstreamFlangeInnerRadius;           };
  G4double *           GetVesselSectionUpstreamFlangeZShift()              { return fVesselSectionUpstreamFlangeZShift;                };
  G4double *           GetVesselSectionUpstreamFlangeZPosition()           { return fVesselSectionUpstreamFlangeZPosition;             };

  G4double *           GetVesselSectionDownstreamFlangeZLength()           { return fVesselSectionDownstreamFlangeZLength;            };
  G4double *           GetVesselSectionDownstreamFlangeRadialThickness()   { return fVesselSectionDownstreamFlangeRadialThickness;    };
  G4double *           GetVesselSectionDownstreamFlangeInnerRadius()       { return fVesselSectionDownstreamFlangeInnerRadius;        };
  G4double *           GetVesselSectionDownstreamFlangeZPosition()         { return fVesselSectionDownstreamFlangeZPosition;          };

  G4double             GetConicalWindowOuterFlangeZLength()                      { return fConicalWindowOuterFlangeZLength;                 };
  void                 SetConicalWindowOuterFlangeZLength(G4double value)        { fConicalWindowOuterFlangeZLength = value;                };
  G4double             GetConicalWindowOuterFlangeInnerRadius()                  { return fConicalWindowOuterFlangeInnerRadius;             };
  void                 SetConicalWindowOuterFlangeInnerRadius(G4double value)    { fConicalWindowOuterFlangeInnerRadius = value;            }; 
  G4double             GetConicalWindowOuterFlangeRadialThickness()              { return fConicalWindowOuterFlangeRadialThickness;         };
  void                 SetConicalWindowOuterFlangeRadialThickness(G4double value){ fConicalWindowOuterFlangeRadialThickness = value;        };
  G4double             GetConicalWindowOuterFlangeZPosition()                    { return fConicalWindowOuterFlangeZPosition;               };
  void                 SetConicalWindowOuterFlangeZPosition(G4double value)      { fConicalWindowOuterFlangeZPosition = value;              };

  G4double             GetConicalWindowZLength()                      { return fConicalWindowZLength;                 };
  void                 SetConicalWindowZLength(G4double value)        { fConicalWindowZLength = value;                };
  G4double             GetConicalWindowThickness()                    { return fConicalWindowThickness;               };
  void                 SetConicalWindowThickness(G4double value)      { fConicalWindowThickness = value;              };
  G4double             GetConicalWindowOuterRadius()                  { return fConicalWindowOuterRadius;             };
  void                 SetConicalWindowOuterRadius(G4double value)    { fConicalWindowOuterRadius = value;            };
  G4double             GetConicalWindowInnerRadius()                  { return fConicalWindowInnerRadius;             };
  void                 SetConicalWindowInnerRadius(G4double value)    { fConicalWindowInnerRadius = value;            };
  G4double             GetConicalWindowZPosition()                    { return fConicalWindowZPosition;               };
  void                 SetConicalWindowZPosition(G4double value)      { fConicalWindowZPosition = value;              };

  G4double             GetConicalWindowInnerFlangeZLength()                      { return fConicalWindowInnerFlangeZLength;                 };
  void                 SetConicalWindowInnerFlangeZLength(G4double value)        { fConicalWindowInnerFlangeZLength = value;                };
  G4double             GetConicalWindowInnerFlangeInnerRadius()                  { return fConicalWindowInnerFlangeInnerRadius;             };
  void                 SetConicalWindowInnerFlangeInnerRadius(G4double value)    { fConicalWindowInnerFlangeInnerRadius = value;            }; 
  G4double             GetConicalWindowInnerFlangeRadialThickness()              { return fConicalWindowInnerFlangeRadialThickness;         };
  void                 SetConicalWindowInnerFlangeRadialThickness(G4double value){ fConicalWindowInnerFlangeRadialThickness = value;        };
  G4double             GetConicalWindowInnerFlangeZPosition()                    { return fConicalWindowInnerFlangeZPosition;               };
  void                 SetConicalWindowInnerFlangeZPosition(G4double value)      { fConicalWindowInnerFlangeZPosition = value;              };

  G4double             GetPMTsTubeZLength()                                { return fPMTsTubeZLength;          };
  void                 SetPMTsTubeZLength(G4double value)                  { fPMTsTubeZLength = value;         }; 
  G4TwoVector *        GetPMTsTubeCenter()                                 { return fPMTsTubeCenter;           };
  void                 SetPMTsTubeCenter(G4TwoVector * value)              { fPMTsTubeCenter = value;          };
  G4double             GetPMTsTubeInnerRadius()                            { return fPMTsTubeInnerRadius;      };
  void                 SetPMTsTubeInnerRadius(G4double value)              { fPMTsTubeInnerRadius = value;     }; 
  G4double             GetPMTsTubeRadialThickness()                        { return fPMTsTubeRadialThickness;  };
  void                 SetPMTsTubeRadialThickness(G4double value)          { fPMTsTubeRadialThickness = value; }; 
  G4double             GetPMTsTubeZPosition()                              { return fPMTsTubeZPosition;        };
  void                 SetPMTsTubeZPosition(G4double value)                { fPMTsTubeZPosition = value;       }; 

  G4double             GetPMsTubeRadius()                                 { return fPMsTubeRadius;     };  // Fast Sim?
  void                 SetPMsTubeRadius(G4double value)                   { fPMsTubeRadius = value;    };

  G4double             GetPMTsDiskZLength()                                { return fPMTsDiskZLength;           };
  void                 SetPMTsDiskZLength(G4double value)                  { fPMTsDiskZLength = value;          };
  G4TwoVector *        GetPMTsDiskCenter()                                 { return fPMTsDiskCenter;            };
  void                 SetPMTsDiskCenter(G4TwoVector * value)              { fPMTsDiskCenter = value;           };
  G4TwoVector *        GetPMTsDiskCenter_Jura_lab()                        { return fPMTsDiskCenter_Jura_lab;   };
  void                 SetPMTsDiskCenter_Jura_lab(G4TwoVector * value)     { fPMTsDiskCenter_Jura_lab = value;  };
  G4TwoVector *        GetPMTsDiskCenter_Saleve_lab()                      { return fPMTsDiskCenter_Saleve_lab; };
  void                 SetPMTsDiskCenter_Saleve_lab(G4TwoVector * value)   { fPMTsDiskCenter_Saleve_lab = value;};
  G4double             GetPMTsDiskInnerRadius()                            { return fPMTsDiskInnerRadius;       };
  void                 SetPMTsDiskInnerRadius(G4double value)              { fPMTsDiskInnerRadius = value;      };
  G4double             GetPMTsDiskOuterRadius()                            { return fPMTsDiskOuterRadius;       };
  void                 SetPMTsDiskOuterRadius(G4double value)              { fPMTsDiskOuterRadius = value;      };
  G4double             GetPMTsDiskZPosition()                              { return fPMTsDiskZPosition;       };
  void                 SetPMTsDiskZPosition(G4double value)                { fPMTsDiskZPosition = value;      };

  G4double             GetBeamWindowOuterFlangeZLength()                         { return fBeamWindowOuterFlangeZLength;          };
  void                 SetBeamWindowOuterFlangeZLength(G4double value)           { fBeamWindowOuterFlangeZLength = value;         };
  G4double             GetBeamWindowOuterFlangeInnerRadius()                     { return fBeamWindowOuterFlangeInnerRadius;      };
  void                 SetBeamWindowOuterFlangeInnerRadius(G4double value)       { fBeamWindowOuterFlangeInnerRadius = value;     };
  G4double             GetBeamWindowOuterFlangeRadialThickness()                 { return fBeamWindowOuterFlangeRadialThickness;  };
  void                 SetBeamWindowOuterFlangeRadialThickness(G4double value)   { fBeamWindowOuterFlangeRadialThickness = value; };
  G4double             GetBeamWindowOuterFlangeZShift()                          { return fBeamWindowOuterFlangeZShift;     };
  void                 SetBeamWindowOuterFlangeZShift(G4double value)            { fBeamWindowOuterFlangeZShift = value;    };
  G4double             GetBeamWindowOuterFlangeZPosition()                       { return fBeamWindowOuterFlangeZPosition;  };
  void                 SetBeamWindowOuterFlangeZPosition(G4double value)         { fBeamWindowOuterFlangeZPosition = value; };

  G4double             GetBeamWindowZLength()                                    { return fBeamWindowZLength;      };
  void                 SetBeamWindowZLength(G4double value)                      { fBeamWindowZLength = value;     };
  G4double             GetBeamWindowOuterRadius()                                { return fBeamWindowOuterRadius;  };
  void                 SetBeamWindowOuterRadius(G4double value)                  { fBeamWindowOuterRadius = value; };
  G4double             GetBeamWindowInnerRadius()                                { return fBeamWindowInnerRadius;  };
  void                 SetBeamWindowInnerRadius(G4double value)                  { fBeamWindowInnerRadius = value; };
  G4double             GetBeamWindowThickness()                                  { return fBeamWindowThickness;    };
  void                 SetBeamWindowThickness(G4double value)                    { fBeamWindowThickness = value;   };
  G4double             GetBeamWindowZPosition()                                  { return fBeamWindowZPosition;    };
  void                 SetBeamWindowZPosition(G4double value)                    { fBeamWindowZPosition = value;   };

  G4double             GetBeamWindowInnerFlangeZLength()                         { return fBeamWindowInnerFlangeZLength;          };
  void                 SetBeamWindowInnerFlangeZLength(G4double value)           { fBeamWindowInnerFlangeZLength = value;         };
  G4double             GetBeamWindowInnerFlangeZShift()                          { return fBeamWindowInnerFlangeZShift;     };
  void                 SetBeamWindowInnerFlangeZShift(G4double value)            { fBeamWindowInnerFlangeZShift = value;    };
  G4double             GetBeamWindowInnerFlangeZPosition()                       { return fBeamWindowInnerFlangeZPosition;  };
  void                 SetBeamWindowInnerFlangeZPosition(G4double value)         { fBeamWindowInnerFlangeZPosition = value; };
  G4double             GetBeamWindowInnerFlangeInnerRadius()                     { return fBeamWindowInnerFlangeInnerRadius;      };
  void                 SetBeamWindowInnerFlangeInnerRadius(G4double value)       { fBeamWindowInnerFlangeInnerRadius = value;     };
  G4double             GetBeamWindowInnerFlangeRadialThickness()                 { return fBeamWindowInnerFlangeRadialThickness;  };
  void                 SetBeamWindowInnerFlangeRadialThickness(G4double value)   { fBeamWindowInnerFlangeRadialThickness = value; };

  G4double             GetInputDisplacementBeatch()                             { return fInputDisplacementBeatch;              };
  void                 SetInputDisplacementBeatch(G4double value)               { fInputDisplacementBeatch = value;             };
  G4double             GetInputDisplacementZPositionBeatch()                    { return fInputDisplacementZPositionBeatch;     };
  void                 SetInputDisplacementZPositionBeatch(G4double value)      { fInputDisplacementZPositionBeatch = value;    };
  G4double             GetOutputDisplacementBeatch()                            { return fOutputDisplacementBeatch;              };
  void                 SetOutputDisplacementBeatch(G4double value)              { fOutputDisplacementBeatch = value;             };
  G4double             GetOutputDisplacementZPositionBeatch()                   { return fOutputDisplacementZPositionBeatch;     };
  void                 SetOutputDisplacementZPositionBeatch(G4double value)     { fOutputDisplacementZPositionBeatch = value;    };

  G4double             GetAngleWRTXaxis()                                    { return fAngleWRTXaxis;     };
  void                 SetAngleWRTXaxis(G4double value)                      { fAngleWRTXaxis = value;    };

  G4double             GetInputDisplacementWRTXaxis()                           { return fInputDisplacementWRTXaxis;     };
  void                 SetInputDisplacementWRTXaxis(G4double value)             { fInputDisplacementWRTXaxis = value;    };
  G4double             GetOutputDisplacementWRTXaxis()                          { return fOutputDisplacementWRTXaxis;    };
  void                 SetOutputDisplacementWRTXaxis(G4double value)            { fOutputDisplacementWRTXaxis = value;   };

  G4double             GetRadiatorZLength()                               { return fRadiatorZLength;              };
  void                 SetRadiatorZLength(G4double value)                 { fRadiatorZLength = value;             };
  G4double             GetRadiatorInnerRadius()                           { return fRadiatorInnerRadius;          };
  void                 SetRadiatorInnerRadius(G4double value)             { fRadiatorInnerRadius = value;         };
  G4double             GetRadiatorOuterRadius()                           { return fRadiatorOuterRadius;          };
  void                 SetRadiatorOuterRadius(G4double value)             { fRadiatorOuterRadius = value;         };

// inizio parametri nuovi

  G4double             GetMirrorFocalLength()                             { return fMirrorFocalLength;            };
  void                 SetMirrorFocalLength(G4double value)               { fMirrorFocalLength = value;           };
  G4double             GetMirrorThickness()                               { return fMirrorThickness;              };
  void                 SetMirrorThickness(G4double value)                 { fMirrorThickness = value;             };
  G4double             GetMirrorSphereInnerRadius()                       { return fMirrorSphereInnerRadius;      };
  void                 SetMirrorSphereInnerRadius(G4double value)         { fMirrorSphereInnerRadius = value;     };
  G4double             GetMirrorSphereOuterRadius()                       { return fMirrorSphereOuterRadius;      };
  void                 SetMirrorSphereOuterRadius(G4double value)         { fMirrorSphereOuterRadius = value;     };
  G4double             GetSubMirrorExternalRadius()                       { return fSubMirrorExternalRadius;      };
  void                 SetSubMirrorExternalRadius(G4double value)         { fSubMirrorExternalRadius = value;     };  
  G4double *           GetSubMirrorGap()                                  { return fSubMirrorGap;                 };
  G4double             GetSubHalfMirrorHoleRadius()                       { return fSubHalfMirrorHoleRadius;      };
  void                 SetSubHalfMirrorHoleRadius(G4double value)         { fSubHalfMirrorHoleRadius = value;     };
  G4double *           GetMirrorCenterOfCurvature_Jura()                  { return fMirrorCenterOfCurvature_Jura; };
  G4double *           GetMirrorCenterOfCurvature_Saleve()                { return fMirrorCenterOfCurvature_Saleve; };

  void                 ShiftMirrorCenterOfCurvature_Jura(G4double dx, G4double dy)  
                         {fMirrorCenterOfCurvature_Jura[0]+=dx; fMirrorCenterOfCurvature_Jura[1]+=dy; };
  void                 ShiftMirrorCenterOfCurvature_Saleve(G4double dx, G4double dy)
                         {fMirrorCenterOfCurvature_Saleve[0]+=dx; fMirrorCenterOfCurvature_Saleve[1]+=dy; };

  G4double             GetMirrorCenterOfCurvatureShift_Jura(G4int irow, G4int icolumn)    { return fMirrorCenterOfCurvatureShift_Jura[irow][icolumn]; };
  G4double             GetMirrorCenterOfCurvatureShift_Saleve(G4int irow, G4int icolumn)  { return fMirrorCenterOfCurvatureShift_Saleve[irow][icolumn]; };
  void                 SetMirrorCenterOfCurvatureShift_Jura(G4int irow, G4double dx, G4double dy)
                         {fMirrorCenterOfCurvatureShift_Jura[irow][0] = dx; fMirrorCenterOfCurvatureShift_Jura[irow][1] = dy; };
  void                 SetMirrorCenterOfCurvatureShift_Saleve(G4int irow, G4double dx, G4double dy)
                         {fMirrorCenterOfCurvatureShift_Saleve[irow][0] = dx; fMirrorCenterOfCurvatureShift_Saleve[irow][1] = dy;};

  G4double             GetMirrorZPosition()                               { return fMirrorZPosition;              };
  void                 SetMirrorZPosition(G4double value)                 { fMirrorZPosition = value;             };
  G4double             GetMirrorZPositionRadRef()                         { return fMirrorZPositionRadRef;        };
  void                 SetMirrorZPositionRadRef(G4double value)           { fMirrorZPositionRadRef = value;       };

  G4double             GetMirrorActuatorPinRadius()                       { return fMirrorActuatorPinRadius;      };
  void                 SetMirrorActuatorPinRadius(G4double value)         { fMirrorActuatorPinRadius = value;     };
  G4double             GetMirrorActuatorPinHeight()                       { return fMirrorActuatorPinHeight;      };
  void                 SetMirrorActuatorPinHeight(G4double value)         { fMirrorActuatorPinHeight = value;     };
  G4double *           GetMirrorActuatorPinR1_Jura()                      { return fMirrorActuatorPinR1_Jura;     };  
  G4double *           GetMirrorActuatorPinR2_Jura()                      { return fMirrorActuatorPinR2_Jura;     };
  G4double *           GetMirrorActuatorPinR1_Saleve()                    { return fMirrorActuatorPinR1_Saleve;     };
  G4double *           GetMirrorActuatorPinR2_Saleve()                    { return fMirrorActuatorPinR2_Saleve;     };

  G4double             GetMirrorStabilizerPinRadius()                     { return fMirrorStabilizerPinRadius;      };
  void                 SetMirrorStabilizerPinRadius(G4double value)       { fMirrorStabilizerPinRadius = value;     };
  G4double *           GetMirrorStabilizerPinHeight_Jura()                { return fMirrorStabilizerPinHeight_Jura;     };
  G4double *           GetMirrorStabilizerPinD_Jura()                      { return fMirrorStabilizerPinD_Jura;     };
  G4double *           GetMirrorStabilizerPinHeight_Saleve()                { return fMirrorStabilizerPinHeight_Saleve;     };
  G4double *           GetMirrorStabilizerPinD_Saleve()                   { return fMirrorStabilizerPinD_Saleve;     };




  G4double             GetMirrorFlangeDistance()                          { return fMirrorFlangeDistance;         };   //per la FastSim
  void                 SetMirrorFlangeDistance(G4double value)            { fMirrorFlangeDistance = value;        };

  G4double             GetMirrorSupportPanelOuterRadius()                 { return fMirrorSupportPanelOuterRadius;  };
  void                 SetMirrorSupportPanelOuterRadius(G4double value)   { fMirrorSupportPanelOuterRadius=value;   };
  G4double             GetMirrorSupportPanelInnerRadius()                 { return fMirrorSupportPanelInnerRadius;  };
  void                 SetMirrorSupportPanelInnerRadius(G4double value)   { fMirrorSupportPanelInnerRadius=value;   };
  G4double             GetMirrorSupportPanelEquivalentThickness()                 { return fMirrorSupportPanelEquivalentThickness;  };
  void                 SetMirrorSupportPanelEquivalentThickness(G4double value)   { fMirrorSupportPanelEquivalentThickness=value;   };
  G4double             GetMirrorSupportPanelZPosition()                   { return fMirrorSupportPanelZPosition;  };
  void                 SetMirrorSupportPanelZPosition(G4double value)     { fMirrorSupportPanelZPosition=value;   };
  G4double             GetMirrorSupportPanelZPositionRadRef()                 { return fMirrorSupportPanelZPositionRadRef;  };
  void                 SetMirrorSupportPanelZPositionRadRef(G4double value)   { fMirrorSupportPanelZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelToothXLenght()                 { return fMirrorSupportPanelToothXLenght;  };
  void                 SetMirrorSupportPanelToothXLenght(G4double value)   { fMirrorSupportPanelToothXLenght=value;   };
  G4double             GetMirrorSupportPanelToothYLenght()                 { return fMirrorSupportPanelToothYLenght;  };
  void                 SetMirrorSupportPanelToothYLenght(G4double value)   { fMirrorSupportPanelToothYLenght=value;   };
  G4double             GetMirrorSupportPanelToothZLenght()                 { return fMirrorSupportPanelToothZLenght;  };
  void                 SetMirrorSupportPanelToothZLenght(G4double value)   { fMirrorSupportPanelToothZLenght=value;   };
  G4double             GetMirrorSupportPanelToothZPosition()                 { return fMirrorSupportPanelToothZPosition;  };
  void                 SetMirrorSupportPanelToothZPosition(G4double value)   { fMirrorSupportPanelToothZPosition=value;   };
  G4double             GetMirrorSupportPanelToothZPositionRadRef()                 { return fMirrorSupportPanelToothZPositionRadRef;  };
  void                 SetMirrorSupportPanelToothZPositionRadRef(G4double value)   { fMirrorSupportPanelToothZPositionRadRef=value;   };


  G4double             GetMirrorSupportPanelRingOuterRadius()                 { return fMirrorSupportPanelRingOuterRadius;  };
  void                 SetMirrorSupportPanelRingOuterRadius(G4double value)   { fMirrorSupportPanelRingOuterRadius=value;   };
  G4double             GetMirrorSupportPanelRingInnerRadius()                 { return fMirrorSupportPanelRingInnerRadius;  };
  void                 SetMirrorSupportPanelRingInnerRadius(G4double value)   { fMirrorSupportPanelRingInnerRadius=value;   };
  G4double             GetMirrorSupportPanelRingThickness()                   { return fMirrorSupportPanelRingThickness;    };
  void                 SetMirrorSupportPanelRingThickness(G4double value)     { fMirrorSupportPanelRingThickness=value;     };
  G4double             GetMirrorSupportPanelRingZPosition()                   { return fMirrorSupportPanelRingZPosition;    };
  void                 SetMirrorSupportPanelRingZPosition(G4double value)     { fMirrorSupportPanelRingZPosition=value;     };
  G4double             GetMirrorSupportPanelRingZPositionRadRef()                  { return fMirrorSupportPanelRingZPositionRadRef;  };
  void                 SetMirrorSupportPanelRingZPositionRadRef(G4double value)   { fMirrorSupportPanelRingZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelRotation_Jura()                   { return fMirrorSupportPanelRotation_Jura;  };
  void                 SetMirrorSupportPanelRotation_Jura(G4double value)     { fMirrorSupportPanelRotation_Jura=value;   };
  G4double             GetMirrorSupportPanelRotation_Saleve()                 { return fMirrorSupportPanelRotation_Saleve;  };
  void                 SetMirrorSupportPanelRotation_Saleve(G4double value)   { fMirrorSupportPanelRotation_Saleve=value;   };

  G4double             GetMirrorSupportPanelConeBottomOuterRadius()                { return fMirrorSupportPanelConeBottomOuterRadius;  };
  void                 SetMirrorSupportPanelConeBottomOuterRadius(G4double value)  { fMirrorSupportPanelConeBottomOuterRadius=value;   };
  G4double             GetMirrorSupportPanelConeUpOuterRadius()                    { return fMirrorSupportPanelConeUpOuterRadius;  };
  void                 SetMirrorSupportPanelConeUpOuterRadius(G4double value)      { fMirrorSupportPanelConeUpOuterRadius=value;   };
  G4double             GetMirrorSupportPanelConeThickness()                        { return fMirrorSupportPanelConeThickness;  };
  void                 SetMirrorSupportPanelConeThickness(G4double value)          { fMirrorSupportPanelConeThickness=value;   };
  G4double *           GetMirrorSupportPanelConeHeight()                           { return fMirrorSupportPanelConeHeight; };
//  G4double             GetMirrorSupportPanelConeHeight()                           { return fMirrorSupportPanelConeHeight;  };
//  void                 SetMirrorSupportPanelConeHeight(G4double value)             { fMirrorSupportPanelConeHeight=value;   };
  G4double*             GetMirrorSupportPanelConeZPosition()                        { return fMirrorSupportPanelConeZPosition;  };
//  void                 SetMirrorSupportPanelConeZPosition(G4double value)          { fMirrorSupportPanelConeZPosition=value;   };
  G4double*             GetMirrorSupportPanelConeZPositionRadRef()                  { return fMirrorSupportPanelConeZPositionRadRef;  };
//  void                 SetMirrorSupportPanelConeZPositionRadRef(G4double value)    { fMirrorSupportPanelConeZPositionRadRef=value;   };


  G4double             GetMirrorSupportPanelConeBaseOuterRadius()                      { return fMirrorSupportPanelConeBaseOuterRadius;  };
  void                 SetMirrorSupportPanelConeBaseOuterRadius(G4double value)        { fMirrorSupportPanelConeBottomOuterRadius=value;   };
  G4double             GetMirrorSupportPanelConeBaseInnerRadius()                      { return fMirrorSupportPanelConeBaseInnerRadius;  };
  void                 SetMirrorSupportPanelConeBaseInnerRadius(G4double value)        { fMirrorSupportPanelConeBaseInnerRadius=value;   };
  G4double             GetMirrorSupportPanelConeBaseThickness()                        { return fMirrorSupportPanelConeBaseThickness;  };
  void                 SetMirrorSupportPanelConeBaseThickness(G4double value)          { fMirrorSupportPanelConeBaseThickness=value;   };
  G4double             GetMirrorSupportPanelConeBaseZPosition()                        { return fMirrorSupportPanelConeBaseZPosition;  };
  void                 SetMirrorSupportPanelConeBaseZPosition(G4double value)          { fMirrorSupportPanelConeBaseZPosition=value;   };
  G4double             GetMirrorSupportPanelConeBaseZPositionRadRef()                  { return fMirrorSupportPanelConeBaseZPositionRadRef;  };
  void                 SetMirrorSupportPanelConeBaseZPositionRadRef(G4double value)    { fMirrorSupportPanelConeBaseZPositionRadRef=value;   };


  G4double             GetMirrorSupportPanelConeHatOuterRadius()                   { return fMirrorSupportPanelConeHatOuterRadius;  };
  void                 SetMirrorSupportPanelConeHatOuterRadius(G4double value)     { fMirrorSupportPanelConeHatOuterRadius=value;   };
  G4double             GetMirrorSupportPanelConeHatInnerRadius()                   { return fMirrorSupportPanelConeHatInnerRadius;  };
  void                 SetMirrorSupportPanelConeHatInnerRadius(G4double value)     { fMirrorSupportPanelConeHatInnerRadius=value;   };
  G4double             GetMirrorSupportPanelConeHatThickness()                     { return fMirrorSupportPanelConeHatThickness;  };
  void                 SetMirrorSupportPanelConeHatThickness(G4double value)       { fMirrorSupportPanelConeHatThickness=value;   };
  G4double*            GetMirrorSupportPanelConeHatZPosition()                     { return fMirrorSupportPanelConeHatZPosition;  };
//  void                 SetMirrorSupportPanelConeHatZPosition(G4double value)       { fMirrorSupportPanelConeHatZPosition=value;   };
  G4double*            GetMirrorSupportPanelConeHatZPositionRadRef()               { return fMirrorSupportPanelConeHatZPositionRadRef;  };
//  void                 SetMirrorSupportPanelConeHatZPositionRadRef(G4double value) { fMirrorSupportPanelConeHatZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelConeScrewRadius()                      { return fMirrorSupportPanelConeScrewRadius;  };
  void                 SetMirrorSupportPanelConeScrewRadius(G4double value)        { fMirrorSupportPanelConeScrewRadius=value;   };
  G4double             GetMirrorSupportPanelConeScrewHeight()                      { return fMirrorSupportPanelConeScrewHeight;  };
  void                 SetMirrorSupportPanelConeScrewHeight(G4double value)        { fMirrorSupportPanelConeScrewHeight=value;   };
  G4double             GetMirrorSupportPanelConeScrewZPosition()                   { return fMirrorSupportPanelConeScrewZPosition;  };
  void                 SetMirrorSupportPanelConeScrewZPosition(G4double value)     { fMirrorSupportPanelConeScrewZPosition=value;   };
  G4double             GetMirrorSupportPanelConeScrewZPositionRadRef()             { return fMirrorSupportPanelConeScrewZPositionRadRef;  };
  void                 SetMirrorSupportPanelConeScrewZPositionRadRef(G4double value) { fMirrorSupportPanelConeScrewZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelTurnerBaseXLenght()               { return fMirrorSupportPanelTurnerBaseXLenght;  };
  void                 SetMirrorSupportPanelTurnerBaseXLenght(G4double value) { fMirrorSupportPanelTurnerBaseXLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerBaseYLenght()               { return fMirrorSupportPanelTurnerBaseYLenght;  };
  void                 SetMirrorSupportPanelTurnerBaseYLenght(G4double value) { fMirrorSupportPanelTurnerBaseYLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerBaseZLenght()               { return fMirrorSupportPanelTurnerBaseZLenght;  };
  void                 SetMirrorSupportPanelTurnerBaseZLenght(G4double value) { fMirrorSupportPanelTurnerBaseZLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerBaseZPosition()               { return fMirrorSupportPanelTurnerBaseZPosition;  };
  void                 SetMirrorSupportPanelTurnerBaseZPosition(G4double value) { fMirrorSupportPanelTurnerBaseZPosition=value;   };
  G4double             GetMirrorSupportPanelTurnerBaseZPositionRadRef()         { return fMirrorSupportPanelTurnerBaseZPositionRadRef;  };
  void                 SetMirrorSupportPanelTurnerBaseZPositionRadRef(G4double value) { fMirrorSupportPanelTurnerBaseZPositionRadRef=value;   };



  G4double             GetMirrorSupportPanelTurnerXMaxLenght()               { return fMirrorSupportPanelTurnerXMaxLenght;  };
  void                 SetMirrorSupportPanelTurnerXMaxLenght(G4double value) { fMirrorSupportPanelTurnerXMaxLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerXMinLenght()               { return fMirrorSupportPanelTurnerXMinLenght;  };
  void                 SetMirrorSupportPanelTurnerXMinLenght(G4double value) { fMirrorSupportPanelTurnerXMinLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerYMaxLenght()               { return fMirrorSupportPanelTurnerYMaxLenght;  };
  void                 SetMirrorSupportPanelTurnerYMaxLenght(G4double value) { fMirrorSupportPanelTurnerYMaxLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerYMinLenght()               { return fMirrorSupportPanelTurnerYMinLenght;  };
  void                 SetMirrorSupportPanelTurnerYMinLenght(G4double value) { fMirrorSupportPanelTurnerYMinLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerZLenght()               { return fMirrorSupportPanelTurnerZLenght;  };
  void                 SetMirrorSupportPanelTurnerZLenght(G4double value) { fMirrorSupportPanelTurnerZLenght=value;   };  
  G4double             GetMirrorSupportPanelTurnerZPosition()               { return fMirrorSupportPanelTurnerZPosition;  };
  void                 SetMirrorSupportPanelTurnerZPosition(G4double value) { fMirrorSupportPanelTurnerZPosition=value;   };
  G4double             GetMirrorSupportPanelTurnerZPositionRadRef()               { return fMirrorSupportPanelTurnerZPositionRadRef;  };
  void                 SetMirrorSupportPanelTurnerZPositionRadRef(G4double value) { fMirrorSupportPanelTurnerZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelTurnerHoleXMaxLenght()               { return fMirrorSupportPanelTurnerHoleXMaxLenght;  };
  void                 SetMirrorSupportPanelTurnerHoleXMaxLenght(G4double value) { fMirrorSupportPanelTurnerHoleXMaxLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerHoleXMinLenght()               { return fMirrorSupportPanelTurnerHoleXMinLenght;  };
  void                 SetMirrorSupportPanelTurnerHoleXMinLenght(G4double value) { fMirrorSupportPanelTurnerHoleXMinLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerHoleYMaxLenght()               { return fMirrorSupportPanelTurnerHoleYMaxLenght;  };
  void                 SetMirrorSupportPanelTurnerHoleYMaxLenght(G4double value) { fMirrorSupportPanelTurnerHoleYMaxLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerHoleYMinLenght()               { return fMirrorSupportPanelTurnerHoleYMinLenght;  };
  void                 SetMirrorSupportPanelTurnerHoleYMinLenght(G4double value) { fMirrorSupportPanelTurnerHoleYMinLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerHoleZLenght()               { return fMirrorSupportPanelTurnerHoleZLenght;  };
  void                 SetMirrorSupportPanelTurnerHoleZLenght(G4double value) { fMirrorSupportPanelTurnerHoleZLenght=value;   };
  G4double             GetMirrorSupportPanelTurnerHoleZPosition()               { return fMirrorSupportPanelTurnerHoleZPosition;  };
  void                 SetMirrorSupportPanelTurnerHoleZPosition(G4double value) { fMirrorSupportPanelTurnerHoleZPosition=value;   };
  G4double             GetMirrorSupportPanelTurnerHoleZPositionRadRef()               { return fMirrorSupportPanelTurnerHoleZPositionRadRef;  };
  void                 SetMirrorSupportPanelTurnerHoleZPositionRadRef(G4double value) { fMirrorSupportPanelTurnerHoleZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelPrismHatXLenght()               { return fMirrorSupportPanelPrismHatXLenght;  };
  void                 SetMirrorSupportPanelPrismHatXLenght(G4double value) { fMirrorSupportPanelPrismHatXLenght=value;   };
  G4double             GetMirrorSupportPanelPrismHatYLenght()               { return fMirrorSupportPanelPrismHatYLenght;  };
  void                 SetMirrorSupportPanelPrismHatYLenght(G4double value) { fMirrorSupportPanelPrismHatYLenght=value;   };
  G4double             GetMirrorSupportPanelPrismHatZLenght()               { return fMirrorSupportPanelPrismHatZLenght;  };
  void                 SetMirrorSupportPanelPrismHatZLenght(G4double value) { fMirrorSupportPanelPrismHatZLenght=value;   };
  G4double             GetMirrorSupportPanelPrismHatZPosition()               { return fMirrorSupportPanelPrismHatZPosition;  };
  void                 SetMirrorSupportPanelPrismHatZPosition(G4double value) { fMirrorSupportPanelPrismHatZPosition=value;   };
  G4double             GetMirrorSupportPanelPrismHatZPositionRadRef()         { return fMirrorSupportPanelPrismHatZPositionRadRef;  };
  void                 SetMirrorSupportPanelPrismHatZPositionRadRef(G4double value) { fMirrorSupportPanelPrismHatZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelPrismBaseXLenght()               { return fMirrorSupportPanelPrismBaseXLenght;  };
  void                 SetMirrorSupportPanelPrismBaseXLenght(G4double value) { fMirrorSupportPanelPrismBaseXLenght=value;   };
  G4double             GetMirrorSupportPanelPrismBaseYLenght()               { return fMirrorSupportPanelPrismBaseYLenght;  };
  void                 SetMirrorSupportPanelPrismBaseYLenght(G4double value) { fMirrorSupportPanelPrismBaseYLenght=value;   };
  G4double             GetMirrorSupportPanelPrismBaseZLenght()               { return fMirrorSupportPanelPrismBaseZLenght;  };
  void                 SetMirrorSupportPanelPrismBaseZLenght(G4double value) { fMirrorSupportPanelPrismBaseZLenght=value;   };
  G4double             GetMirrorSupportPanelPrismBaseZPosition()               { return fMirrorSupportPanelPrismBaseZPosition;  };
  void                 SetMirrorSupportPanelPrismBaseZPosition(G4double value) { fMirrorSupportPanelPrismBaseZPosition=value;   };
  G4double             GetMirrorSupportPanelPrismBaseZPositionRadRef()         { return fMirrorSupportPanelPrismBaseZPositionRadRef;  };
  void                 SetMirrorSupportPanelPrismBaseZPositionRadRef(G4double value) { fMirrorSupportPanelPrismBaseZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelPrismX1Lenght()               { return fMirrorSupportPanelPrismX1Lenght;  };
  void                 SetMirrorSupportPanelPrismX1Lenght(G4double value) { fMirrorSupportPanelPrismX1Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismX2Lenght()               { return fMirrorSupportPanelPrismX2Lenght;  };
  void                 SetMirrorSupportPanelPrismX2Lenght(G4double value) { fMirrorSupportPanelPrismX2Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismY1Lenght()               { return fMirrorSupportPanelPrismY1Lenght;  };
  void                 SetMirrorSupportPanelPrismY1Lenght(G4double value) { fMirrorSupportPanelPrismY1Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismY2Lenght()               { return fMirrorSupportPanelPrismY2Lenght;  };
  void                 SetMirrorSupportPanelPrismY2Lenght(G4double value) { fMirrorSupportPanelPrismY2Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismZLenght()               { return fMirrorSupportPanelPrismZLenght;  };
  void                 SetMirrorSupportPanelPrismZLenght(G4double value) { fMirrorSupportPanelPrismZLenght=value;   };
  G4double             GetMirrorSupportPanelPrismZPosition()               { return fMirrorSupportPanelPrismZPosition;  };
  void                 SetMirrorSupportPanelPrismZPosition(G4double value) { fMirrorSupportPanelPrismZPosition=value;   };
  G4double             GetMirrorSupportPanelPrismZPositionRadRef()               { return fMirrorSupportPanelPrismZPositionRadRef;  };
  void                 SetMirrorSupportPanelPrismZPositionRadRef(G4double value) { fMirrorSupportPanelPrismZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelPrismHoleX1Lenght()               { return fMirrorSupportPanelPrismHoleX1Lenght;  };
  void                 SetMirrorSupportPanelPrismHoleX1Lenght(G4double value) { fMirrorSupportPanelPrismHoleX1Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismHoleX2Lenght()               { return fMirrorSupportPanelPrismHoleX2Lenght;  };
  void                 SetMirrorSupportPanelPrismHoleX2Lenght(G4double value) { fMirrorSupportPanelPrismHoleX2Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismHoleY1Lenght()               { return fMirrorSupportPanelPrismHoleY1Lenght;  };
  void                 SetMirrorSupportPanelPrismHoleY1Lenght(G4double value) { fMirrorSupportPanelPrismHoleY1Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismHoleY2Lenght()               { return fMirrorSupportPanelPrismHoleY2Lenght;  };
  void                 SetMirrorSupportPanelPrismHoleY2Lenght(G4double value) { fMirrorSupportPanelPrismHoleY2Lenght=value;   };
  G4double             GetMirrorSupportPanelPrismHoleZLenght()               { return fMirrorSupportPanelPrismHoleZLenght;  };
  void                 SetMirrorSupportPanelPrismHoleZLenght(G4double value) { fMirrorSupportPanelPrismHoleZLenght=value;   };
  G4double             GetMirrorSupportPanelPrismHoleZPosition()               { return fMirrorSupportPanelPrismHoleZPosition;  };
  void                 SetMirrorSupportPanelPrismHoleZPosition(G4double value) { fMirrorSupportPanelPrismHoleZPosition=value;   };
  G4double             GetMirrorSupportPanelPrismHoleZPositionRadRef()               { return fMirrorSupportPanelPrismHoleZPositionRadRef;  };
  void                 SetMirrorSupportPanelPrismHoleZPositionRadRef(G4double value) { fMirrorSupportPanelPrismHoleZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelPrismXPosition_Jura()               { return fMirrorSupportPanelPrismXPosition_Jura;  };
  void                 SetMirrorSupportPanelPrismXPosition_Jura(G4double value) { fMirrorSupportPanelPrismXPosition_Jura=value;   };
  G4double             GetMirrorSupportPanelPrismXPosition_Saleve()               { return fMirrorSupportPanelPrismXPosition_Saleve;  };
  void                 SetMirrorSupportPanelPrismXPosition_Saleve(G4double value) { fMirrorSupportPanelPrismXPosition_Saleve=value;   };

  G4double             GetMirrorSupportPanelPrismScrewRadius()                     { return fMirrorSupportPanelPrismScrewRadius;  };
  void                 SetMirrorSupportPanelPrismScrewRadius(G4double value)       { fMirrorSupportPanelPrismScrewRadius=value;   };
  G4double             GetMirrorSupportPanelPrismScrewHeight()                     { return fMirrorSupportPanelPrismScrewHeight;  };
  void                 SetMirrorSupportPanelPrismScrewHeight(G4double value)       { fMirrorSupportPanelPrismScrewHeight=value;   };
  G4double             GetMirrorSupportPanelPrismScrewZPosition()                  { return fMirrorSupportPanelPrismScrewZPosition;  };
  void                 SetMirrorSupportPanelPrismScrewZPosition(G4double value)    { fMirrorSupportPanelPrismScrewZPosition=value;   };
  G4double             GetMirrorSupportPanelPrismScrewZPositionRadRef()            { return fMirrorSupportPanelPrismScrewZPositionRadRef;  };
  void                 SetMirrorSupportPanelPrismScrewZPositionRadRef(G4double value) { fMirrorSupportPanelPrismScrewZPositionRadRef=value;   };

  G4double             GetMirrorSupportPanelDowelRadius()                          { return fMirrorSupportPanelDowelRadius;   };
  void                 SetMirrorSupportPanelDowelRadius(G4double value)            { fMirrorSupportPanelDowelRadius=value;    };
  G4double             GetMirrorSupportPanelDowelHeight()                          { return fMirrorSupportPanelDowelHeight;   };
  void                 SetMirrorSupportPanelDowelHeight(G4double value)            { fMirrorSupportPanelDowelHeight=value;    };

  G4double             GetConeInputDiameter()                             { return fConeInputDiameter;            };
  void                 SetConeInputDiameter(G4double value)               { fConeInputDiameter = value;           };
  G4double             GetConeInputRadius()                               { return fConeInputRadius;              };
  void                 SetConeInputRadius(G4double value)                 { fConeInputRadius = value;             };
  G4double             GetConeOutputDiameter()                            { return fConeOutputDiameter;           };
  void                 SetConeOutputDiameter(G4double value)              { fConeOutputDiameter = value;          };
  G4double             GetConeOutputRadius()                              { return fConeOutputRadius;             };
  void                 SetConeOutputRadius(G4double value)                { fConeOutputRadius = value;            };
  G4double             GetConeLongitudinalLength()                        { return fConeLongitudinalLength;       };
  void                 SetConeLongitudinalLength(G4double value)          { fConeLongitudinalLength = value;      };

  G4double             GetMylarConeThickness()                            { return fMylarConeThickness;           };
  void                 SetMylarConeThickness(G4double value)              { fMylarConeThickness = value;          };
  G4double             GetMylarConeInputRadius()                          { return fMylarConeInputRadius;         };
  void                 SetMylarConeInputRadius(G4double value)            { fMylarConeInputRadius = value;        };
  G4double             GetMylarConeOutputRadius()                         { return fMylarConeOutputRadius;        };
  void                 SetMylarConeOutputRadius(G4double value)           { fMylarConeOutputRadius = value;       };
  G4double             GetMylarConeLongitudinalLength()                   { return fMylarConeLongitudinalLength;  };
  void                 SetMylarConeLongitudinalLength(G4double value)     { fMylarConeLongitudinalLength = value; };

  G4double             GetQuartzWindowInnerRadius()                       { return fQuartzWindowInnerRadius;      };
  void                 SetQuartzWindowInnerRadius(G4double value)         { fQuartzWindowInnerRadius = value;     };
  G4double             GetQuartzWindowOuterRadius()                       { return fQuartzWindowOuterRadius;      };
  void                 SetQuartzWindowOuterRadius(G4double value)         { fQuartzWindowOuterRadius = value;     };
  G4double             GetQuartzWindowThickness()                         { return fQuartzWindowThickness;        };
  void                 SetQuartzWindowThickness(G4double value)           { fQuartzWindowThickness = value;       };

  // da capire cosa sono

  G4double             GetPMWindowInnerRadius()                           { return fPMWindowInnerRadius;          };
  void                 SetPMWindowInnerRadius(G4double value)             { fPMWindowInnerRadius = value;         };
  G4double             GetPMWindowOuterRadius()                           { return fPMWindowOuterRadius;          };
  void                 SetPMWindowOuterRadius(G4double value)             { fPMWindowOuterRadius = value;         };
  G4double             GetPMWindowThickness()                             { return fPMWindowThickness;            };
  void                 SetPMWindowThickness(G4double value)               { fPMWindowThickness = value;           };
  
  ////////////////////

   G4double             GetPMInnerRadius()                                 { return fPMInnerRadius;                };
   void                 SetPMInnerRadius(G4double value)                   { fPMInnerRadius = value;               };
   G4double             GetPMOuterRadius()                                 { return fPMOuterRadius;                };
   void                 SetPMOuterRadius(G4double value)                   { fPMOuterRadius = value;               };
   G4double             GetPMLength()                                      { return fPMLength;                     };
   void                 SetPMLength(G4double value)                        { fPMLength = value;                    };

   G4double             GetHoneyCombDistance()                             { return fHoneyCombDistance;            };
   void                 SetHoneyCombDistance(G4double value)               { fHoneyCombDistance = value;           };
   G4double             GetRMin()                                          { return fRMin;                         };
   void                 SetRMin(G4double value)                            { fRMin = value;                        };
   G4double             GetRMax()                                          { return fRMax;                         };
   void                 SetRMax(G4double value)                            { fRMax = value;                        };
   G4double             GetEcc2()                                          { return fEcc2;                         };
   void                 SetEcc2(G4double value)                            { fEcc2 = value;                        };
   G4double             GetMinDistance()                                   { return fMinDistance;                  };
   void                 SetMinDistance(G4double value)                     { fMinDistance = value;                 };
   G4double             GetMaxDistance()                                   { return fMaxDistance;                  };
   void                 SetMaxDistance(G4double value)                     { fMaxDistance = value;                 };
   G4int **             GetPMsIDs()                                        { return (G4int**)fPMsIDs;              };
   //void                 SetPMsIDs(G4int ** value)                          { fPMsIDs = value;                      };
   TVector2 *           GetPMsPositions()                                  { return fPMsPositions;                 };
   void                 SetPMsPositions(TVector2 * value)                  { fPMsPositions = value;                };
   G4int                GetNPMs()                                          { return fNPMs;                         };
   void                 SetNPMs(G4int value)                               { fNPMs = value;                        };
   G4int *             GetGeoIDs()                                        { return (G4int*)fGeoIDs;              };

private:

  G4int     fNChannels;
  
  G4double  fRespRegionZStart;
  G4double  fRespRegionZEnd;
  G4double  fRespRegionZLength;
  G4double  fRespRegionUpstreamLength;
  G4double  fRespRegionUpstreamRadius;
  G4double  fRespRegionDownstreamLength;
  G4double  fRespRegionDownstreamRadius;

  G4double  fRICHDetectorZStart;
  G4double  fRICHDetectorZEnd;
  G4double  fRICHDetectorZPosition;
  G4double  fRICHDetectorRadius; ///< for FastSim
  G4double  fRICHDetectorZLength;
  G4double  fRICHDetectorXLength;
  G4double  fRICHDetectorYLength;

  G4double  fMirrorWindowZLength; 
  G4double  fMirrorWindowThickness; 
  G4double  fMirrorWindowInnerRadius;
  G4double  fMirrorWindowOuterRadius;
  G4double  fMirrorWindowZPosition;

  G4double  fMirrorWindowInnerFlangeZLength;
  G4double  fMirrorWindowInnerFlangeZPosition;
  G4double  fMirrorWindowInnerFlangeInnerRadius;
  G4double  fMirrorWindowInnerFlangeRadialThickness;

  G4double  fMirrorWindowOuterFlangeZLength; 
  G4double  fMirrorWindowOuterFlangeInnerRadius;
  G4double  fMirrorWindowOuterFlangeRadialThickness;
  G4double  fMirrorWindowOuterFlangeZPosition;

  G4double  fInterfaceRingZLength;
  G4double  fInterfaceRingRadialThickness;
  G4double  fInterfaceRingInnerRadius;
  G4double  fInterfaceRingZPosition;
  
  G4int     fNVesselSections;
  G4double  fVesselRadialThickness;  
  G4double  fVesselZLength;
  G4double  fVesselZPosition;
  
  G4double  fVesselSectionZLength[10];
  G4double  fVesselSectionInnerRadius[10];

  G4double  fVesselSectionPressureFlangeZLength[10];
  G4double  fVesselSectionPressureFlangeRadialThickness[10];
  G4double  fVesselSectionPressureFlangeInnerRadius[10];
  G4double  fVesselSectionPressureFlangeZShift[10];
  G4double  fVesselSectionPressureFlangeZPosition[10];      

  G4double  fVesselSectionUpstreamFlangeZLength[10];
  G4double  fVesselSectionUpstreamFlangeRadialThickness[10];
  G4double  fVesselSectionUpstreamFlangeInnerRadius[10];
  G4double  fVesselSectionUpstreamFlangeZShift[10];
  G4double  fVesselSectionUpstreamFlangeZPosition[10];      

  G4double  fVesselSectionDownstreamFlangeZLength[10];
  G4double  fVesselSectionDownstreamFlangeRadialThickness[10];
  G4double  fVesselSectionDownstreamFlangeInnerRadius[10];
  G4double  fVesselSectionDownstreamFlangeZPosition[10];

  G4double fConicalWindowOuterFlangeZLength; 
  G4double fConicalWindowOuterFlangeInnerRadius;
  G4double fConicalWindowOuterFlangeRadialThickness;
  G4double fConicalWindowOuterFlangeZPosition;

  G4double fConicalWindowZLength;
  G4double fConicalWindowThickness;
  G4double fConicalWindowOuterRadius;
  G4double fConicalWindowInnerRadius;
  G4double fConicalWindowZPosition;

  G4double fConicalWindowInnerFlangeZLength; 
  G4double fConicalWindowInnerFlangeInnerRadius;
  G4double fConicalWindowInnerFlangeRadialThickness;
  G4double fConicalWindowInnerFlangeZPosition;

  G4double  fPMTsTubeZLength;
  G4TwoVector* fPMTsTubeCenter;
  G4double  fPMTsTubeInnerRadius;
  G4double  fPMTsTubeRadialThickness;
  G4double  fPMTsTubeZPosition;

  G4double  fPMsTubeRadius; // Fast Sim ??

  G4double  fPMTsDiskZLength;
  G4TwoVector* fPMTsDiskCenter;
  G4TwoVector* fPMTsDiskCenter_Jura_lab;
  G4TwoVector* fPMTsDiskCenter_Saleve_lab;
  G4double  fPMTsDiskInnerRadius;
  G4double  fPMTsDiskOuterRadius;
  G4double  fPMTsDiskZPosition;

  G4double  fBeamWindowOuterFlangeZLength;
  G4double  fBeamWindowOuterFlangeInnerRadius;
  G4double  fBeamWindowOuterFlangeRadialThickness;
  G4double  fBeamWindowOuterFlangeZShift;
  G4double  fBeamWindowOuterFlangeZPosition;

  G4double  fBeamWindowZLength; 
  G4double  fBeamWindowOuterRadius;
  G4double  fBeamWindowInnerRadius;
  G4double  fBeamWindowThickness;
  G4double  fBeamWindowZPosition;

  G4double  fBeamWindowInnerFlangeZLength;
  G4double  fBeamWindowInnerFlangeZShift;
  G4double  fBeamWindowInnerFlangeZPosition;
  G4double  fBeamWindowInnerFlangeInnerRadius; 
  G4double  fBeamWindowInnerFlangeRadialThickness;
 
  G4double fInputDisplacementBeatch;
  G4double fInputDisplacementZPositionBeatch;
  G4double fOutputDisplacementBeatch;
  G4double fOutputDisplacementZPositionBeatch;

  G4double  fAngleWRTXaxis;

  G4double fInputDisplacementWRTXaxis;
  G4double fOutputDisplacementWRTXaxis;

  G4double fNewChodStartingZ;

  G4double  fRadiatorZLength;
  G4double  fRadiatorInnerRadius;
  G4double  fRadiatorOuterRadius;

  G4double  fMirrorFocalLength;
  G4double  fMirrorThickness;
  G4double  fMirrorSphereInnerRadius;
  G4double  fMirrorSphereOuterRadius;
  G4double  fSubMirrorExternalRadius;
  G4double  fSubMirrorGap[2];
  G4double  fSubHalfMirrorHoleRadius;
  G4double  fMirrorCenterOfCurvature_Jura[3];
  G4double  fMirrorCenterOfCurvature_Saleve[3];
  G4double  fMirrorCenterOfCurvatureShift_Jura[10][2];
  G4double  fMirrorCenterOfCurvatureShift_Saleve[10][2];
  G4double  fMirrorZPosition;
  G4double  fMirrorZPositionRadRef;

  G4double  fMirrorActuatorPinRadius;
  G4double  fMirrorActuatorPinHeight;

  G4double  fMirrorActuatorPinR1_Jura[10]; 
  G4double  fMirrorActuatorPinR2_Jura[10];
  G4double  fMirrorActuatorPinR1_Saleve[10];
  G4double  fMirrorActuatorPinR2_Saleve[10];

  G4double  fMirrorStabilizerPinRadius;

  G4double  fMirrorStabilizerPinHeight_Jura[9];
  G4double  fMirrorStabilizerPinD_Jura[9];
  G4double  fMirrorStabilizerPinHeight_Saleve[9];
  G4double  fMirrorStabilizerPinD_Saleve[9];         

  G4double  fMirrorFlangeDistance; // per la FastSim
 
  G4double  fMirrorSupportPanelOuterRadius;	
  G4double  fMirrorSupportPanelInnerRadius;
  G4double  fMirrorSupportPanelEquivalentThickness;
  G4double  fMirrorSupportPanelZPosition;
  G4double  fMirrorSupportPanelZPositionRadRef;

  G4double fMirrorSupportPanelToothXLenght;
  G4double fMirrorSupportPanelToothYLenght;
  G4double fMirrorSupportPanelToothZLenght;
  G4double fMirrorSupportPanelToothZPosition;
  G4double fMirrorSupportPanelToothZPositionRadRef;


  G4double  fMirrorSupportPanelRingOuterRadius;
  G4double  fMirrorSupportPanelRingInnerRadius;
  G4double  fMirrorSupportPanelRingThickness;
  G4double  fMirrorSupportPanelRingZPosition;
  G4double  fMirrorSupportPanelRingZPositionRadRef;

  G4double  fMirrorSupportPanelRotation_Jura;     //angolo di rotazione nel riferimento del RICH
  G4double  fMirrorSupportPanelRotation_Saleve;   //angolo di rotazione nel riferimento del RICH

  G4double  fMirrorSupportPanelConeBottomOuterRadius;
  G4double  fMirrorSupportPanelConeUpOuterRadius;
  G4double  fMirrorSupportPanelConeThickness;
  G4double  fMirrorSupportPanelConeHeight[9]; 
//  G4double  fMirrorSupportPanelConeHeight;
  G4double  fMirrorSupportPanelConeZPosition[9];
  G4double  fMirrorSupportPanelConeZPositionRadRef[9];

  G4double  fMirrorSupportPanelConeBaseOuterRadius;
  G4double  fMirrorSupportPanelConeBaseInnerRadius;
  G4double  fMirrorSupportPanelConeBaseThickness;
  G4double  fMirrorSupportPanelConeBaseZPosition;
  G4double  fMirrorSupportPanelConeBaseZPositionRadRef;

  G4double  fMirrorSupportPanelConeHatOuterRadius;
  G4double  fMirrorSupportPanelConeHatInnerRadius;
  G4double  fMirrorSupportPanelConeHatThickness;
  G4double  fMirrorSupportPanelConeHatZPosition[9];
  G4double  fMirrorSupportPanelConeHatZPositionRadRef[9];

  G4double  fMirrorSupportPanelConeScrewRadius;
  G4double  fMirrorSupportPanelConeScrewHeight;
  G4double  fMirrorSupportPanelConeScrewZPosition;
  G4double  fMirrorSupportPanelConeScrewZPositionRadRef;

  G4double fMirrorSupportPanelTurnerBaseXLenght;
  G4double fMirrorSupportPanelTurnerBaseYLenght;
  G4double fMirrorSupportPanelTurnerBaseZLenght;
  G4double fMirrorSupportPanelTurnerBaseZPosition;
  G4double fMirrorSupportPanelTurnerBaseZPositionRadRef;

  G4double fMirrorSupportPanelTurnerXMaxLenght;
  G4double fMirrorSupportPanelTurnerXMinLenght;
  G4double fMirrorSupportPanelTurnerYMaxLenght;
  G4double fMirrorSupportPanelTurnerYMinLenght;
  G4double fMirrorSupportPanelTurnerZLenght;
  G4double fMirrorSupportPanelTurnerZPosition;
  G4double fMirrorSupportPanelTurnerZPositionRadRef;
 
  G4double fMirrorSupportPanelTurnerHoleXMaxLenght;
  G4double fMirrorSupportPanelTurnerHoleXMinLenght;
  G4double fMirrorSupportPanelTurnerHoleYMaxLenght;
  G4double fMirrorSupportPanelTurnerHoleYMinLenght;
  G4double fMirrorSupportPanelTurnerHoleZLenght;
  G4double fMirrorSupportPanelTurnerHoleZPosition;
  G4double fMirrorSupportPanelTurnerHoleZPositionRadRef;

  G4double fMirrorSupportPanelPrismHatXLenght;
  G4double fMirrorSupportPanelPrismHatYLenght;
  G4double fMirrorSupportPanelPrismHatZLenght;
  G4double fMirrorSupportPanelPrismHatZPosition;
  G4double fMirrorSupportPanelPrismHatZPositionRadRef;

  G4double fMirrorSupportPanelPrismBaseXLenght;
  G4double fMirrorSupportPanelPrismBaseYLenght;
  G4double fMirrorSupportPanelPrismBaseZLenght;
  G4double fMirrorSupportPanelPrismBaseZPosition;
  G4double fMirrorSupportPanelPrismBaseZPositionRadRef;

  G4double fMirrorSupportPanelPrismX1Lenght;
  G4double fMirrorSupportPanelPrismX2Lenght;
  G4double fMirrorSupportPanelPrismY1Lenght;
  G4double fMirrorSupportPanelPrismY2Lenght;
  G4double fMirrorSupportPanelPrismZLenght;
  G4double fMirrorSupportPanelPrismZPosition;
  G4double fMirrorSupportPanelPrismZPositionRadRef;

  G4double fMirrorSupportPanelPrismHoleX1Lenght;
  G4double fMirrorSupportPanelPrismHoleX2Lenght;
  G4double fMirrorSupportPanelPrismHoleY1Lenght;
  G4double fMirrorSupportPanelPrismHoleY2Lenght;
  G4double fMirrorSupportPanelPrismHoleZLenght;
  G4double fMirrorSupportPanelPrismHoleZPosition;
  G4double fMirrorSupportPanelPrismHoleZPositionRadRef;

  G4double  fMirrorSupportPanelPrismScrewRadius;
  G4double  fMirrorSupportPanelPrismScrewHeight;
  G4double  fMirrorSupportPanelPrismScrewZPosition;
  G4double  fMirrorSupportPanelPrismScrewZPositionRadRef;

  G4double fMirrorSupportPanelPrismXPosition_Jura;
  G4double fMirrorSupportPanelPrismXPosition_Saleve;

  G4double fMirrorSupportPanelDowelRadius;
  G4double fMirrorSupportPanelDowelHeight;

  G4double fConeInputDiameter;
  G4double fConeInputRadius;
  G4double fConeOutputDiameter;
  G4double fConeOutputRadius;
  G4double fConeLongitudinalLength;
  
  G4double fMylarConeThickness;
  G4double fMylarConeInputRadius;
  G4double fMylarConeOutputRadius;
  G4double fMylarConeLongitudinalLength;
  
  G4double fQuartzWindowInnerRadius;
  G4double fQuartzWindowOuterRadius;
  G4double fQuartzWindowThickness;
  
  // da capire cosa sono
  G4double fPMWindowInnerRadius;
  G4double fPMWindowOuterRadius;
  G4double fPMWindowThickness;

  G4double fPMInnerRadius;
  G4double fPMOuterRadius;
  G4double fPMLength;
  
  // eliminare le cose che non servono
  G4double fHoneyCombDistance;
  G4double fRMin;
  G4double fRMax;
  G4double fEcc2;
  G4double fMinDistance;
  G4double fMaxDistance;
  G4int ** fPMsIDs;
  TVector2* fPMsPositions;
  G4int    fNPMs;
  G4int * fGeoIDs;
  
};
#endif
