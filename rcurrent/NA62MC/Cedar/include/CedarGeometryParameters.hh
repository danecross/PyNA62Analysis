// --------------------------------------------------------------
// History:
//
// 2012-12-21 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - parameters are read from a configuration file
//
// 2012-08-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - various geometry updates
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - various geometry updates
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more photodetector configurations added
//
// 2011-11-18 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - a series of geometry updates
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - lightguide parameters; more realistic geometry
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - vessel optics parameters
//
// 2009-11-16 Evgueni Goudzovski
// - parameters of passive Cedar geometry + two quadrupoles (QFS077,079)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef CedarGeometryParameters_H
#define CedarGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"

#include "NA62VGeometryParameters.hh"
#include "G4ThreeVector.hh"

class CedarGeometryParameters : public NA62VGeometryParameters {

public:

  ~CedarGeometryParameters() {}
  static CedarGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  void ParseConfFile(TString);
  static CedarGeometryParameters* fInstance;

protected:

  CedarGeometryParameters();

public:

  G4int    GetNSectors() const                      { return fNSectors;                 }
  G4int    GetGasH2() const                         { return iGasH2;                    }
  G4int    GetLightGuideType() const                { return iLGType;                   }
  G4int    GetOctantsEnabled() const                { return iOctantsEnabled;           }

  G4double GetCedarRegionZPositionLab()             { return fCedarRegionZPositionLab;  }
  void     SetCedarRegionZPositionLab(G4double val) { fCedarRegionZPositionLab = val;   }

  G4double      GetRotBoxXLength()                  { return fRotBoxXLength;            }
  G4double      GetRotBoxYLength()                  { return fRotBoxYLength;            }
  G4double      GetRotBoxZLength()                  { return fRotBoxZLength;            }
  G4ThreeVector GetRotBoxPosition()                 { return fRotBoxPosition;           }
  G4ThreeVector GetRotCentrePosition()              { return fRotCentrePosition;        }
  G4double      GetRotAngleX()                      { return fRotAngleX;                }
  G4double      GetRotAngleY()                      { return fRotAngleY;                }

  G4double GetCedarRegionZLength()                  { return fCedarRegionZLength;       }
  void     SetCedarRegionZLength(G4double val)      { fCedarRegionZLength = val;        }
  G4double GetCedarRegionXLength()                  { return fCedarRegionXLength;       }
  void     SetCedarRegionXLength(G4double val)      { fCedarRegionXLength = val;        }
  G4double GetCedarRegionYLength()                  { return fCedarRegionYLength;       }
  void     SetCedarRegionYLength(G4double val)      { fCedarRegionYLength = val;        }

  G4double GetZSphericalMirrorCapCentre(G4int i)               { return fZSphericalMirrorCapCentre[i-1]; }
  void     SetZSphericalMirrorCapCentre(G4int i, G4double val) { fZSphericalMirrorCapCentre[i-1] = val;  }
  G4double GetZSphericalMirrorCentre(G4int i)                  { return fZSphericalMirrorCentre[i-1];    }
  void     SetZSphericalMirrorCentre(G4int i, G4double val)    { fZSphericalMirrorCentre[i-1] = val;     }

  G4double GetZLightGuideCentre()                       { return fZLightGuideCentre;           }
  void     SetZLightGuideCentre(G4double val)           { fZLightGuideCentre = val;            }
  G4double GetZFrontPipeStart()                         { return fZFrontPipeStart;             }
  void     SetZFrontPipeStart(G4double val)             { fZFrontPipeStart = val;              }
  G4double GetZSphericalMirrorMountStart()              { return fZSphericalMirrorMountStart;  }
  void     SetZSphericalMirrorMountStart(G4double val)  { fZSphericalMirrorMountStart = val;   }
  G4double GetZExternalLensStart()                      { return fZExternalLensStart;          }
  void     SetZExternalLensStart(G4double val)          { fZExternalLensStart = val;           }
  G4double GetZQuartzWindowStart()                      { return fZQuartzWindowStart;          }
  void     SetZQuartzWindowStart(G4double val)          { fZQuartzWindowStart = val;           }
  G4double GetZMainVesselStart()                        { return fZMainVesselStart;            }
  void     SetZMainVesselStart(G4double val)            { fZMainVesselStart = val;             }
  G4double GetZExitWindowStart()                        { return fZExitWindowStart;            }
  void     SetZExitWindowStart(G4double val)            { fZExitWindowStart = val;             }
  G4double GetZManginMirrorStart()                      { return fZManginMirrorStart;          }
  void     SetZManginMirrorStart(G4double val)          { fZManginMirrorStart = val;           }
  G4double GetZChromaticCorrectorStart()                { return fZChromaticCorrectorStart;    }
  void     SetZChromaticCorrectorStart(G4double val)    { fZChromaticCorrectorStart = val;     }
  G4double GetZDiaphragmCentre()                        { return fZDiaphragmCentre;            }
  void     SetZDiaphragmCentre(G4double val)            { fZDiaphragmCentre = val;             }
  G4double GetZCondenserStart()                         { return fZCondenserStart;             }
  void     SetZCondenserStart(G4double val)             { fZCondenserStart = val;              }
  G4double GetZAtexCylinderStart()                      { return fZAtexCylinderStart;          }
  void     SetZAtexCylinderStart(G4double val)          { fZAtexCylinderStart = val;           }

  G4double GetEntranceWindowZLength()                   { return fEntranceWindowZLength;       }
  void     SetEntranceWindowZLength(G4double val)       { fEntranceWindowZLength = val;        }
  G4double GetExitWindowZLength()                       { return fExitWindowZLength;           }
  void     SetExitWindowZLength(G4double val)           { fExitWindowZLength = val;            }
  G4double GetFrontPipeZLength()                        { return fFrontPipeZLength;            }
  void     SetFrontPipeZLength(G4double val)            { fFrontPipeZLength = val;             }
  G4double GetQuartzWindowZLength()                     { return fQuartzWindowZLength;         }
  void     SetQuartzWindowZLength(G4double val)         { fQuartzWindowZLength = val;          }
  G4double GetFrontVesselZLength()                      { return fFrontVesselZLength;          }
  void     SetFrontVesselZLength(G4double val)          { fFrontVesselZLength = val;           }
  G4double GetMainVesselCylinderZLength()               { return fMainVesselCylinderZLength;   }
  void     SetMainVesselCylinderZLength(G4double val)   { fMainVesselCylinderZLength = val;    }
  G4double GetExitPipeZLength()                         { return fExitPipeZLength;             }
  void     SetExitPipeZLength(G4double val)             { fExitPipeZLength = val;              }
  G4double GetManginMirrorZLength()                     { return fManginMirrorZLength;         }
  void     SetManginMirrorZLength(G4double val)         { fManginMirrorZLength = val;          }
  G4double GetManginMirrorCoatingZLength()              { return fManginMirrorCoatingZLength;  }
  void     SetManginMirrorCoatingZLength(G4double val)  { fManginMirrorCoatingZLength = val;   }
  G4double GetChromaticCorrectorZLength()               { return fChromaticCorrectorZLength;   }
  void     SetChromaticCorrectorZLength(G4double val)   { fChromaticCorrectorZLength = val;    }
  G4double GetDiaphragmZLength()                        { return fDiaphragmZLength;            }
  void     SetDiaphragmZLength(G4double val)            { fDiaphragmZLength = val;             }
  G4double GetCondenserZLength()                        { return fCondenserZLength;            }
  void     SetCondenserZLength(G4double val)            { fCondenserZLength = val;             }
  G4double GetAtexCylinderZLength()                     { return fAtexCylinderZLength;         }
  void     SetAtexCylinderZLength(G4double val)         { fAtexCylinderZLength = val;          }
  G4double GetSphericalMirrorMountZLength()             { return fSphericalMirrorMountZLength; }
  void     SetSphericalMirrorMountZLength(G4double val) { fSphericalMirrorMountZLength = val;  }
  G4double GetSphericalMirrorMountSupportRingZLength()  { return fSphericalMirrorMountSupportRingZLength; }
  G4double GetSphericalMirrorMountSupportRingRin()      { return fSphericalMirrorMountSupportRingRin;     }
  G4double GetSphericalMirrorMountSupportRingRout()     { return fSphericalMirrorMountSupportRingRout;    }
  G4double GetSphericalMirrorMountSupportRingHoleDia()  { return fSphericalMirrorMountSupportRingHoleDia; }
  G4double GetFrontPipeInnerRadius()                    { return fFrontPipeInnerRadius;        }
  void     SetFrontPipeInnerRadius(G4double val)        { fFrontPipeInnerRadius = val;         }
  G4double GetFrontPipeOuterRadius()                    { return fFrontPipeOuterRadius;        }
  void     SetFrontPipeOuterRadius(G4double val)        { fFrontPipeOuterRadius = val;         }
  G4double GetFrontVesselInnerRadius()                  { return fFrontVesselInnerRadius;      }
  void     SetFrontVesselInnerRadius(G4double val)      { fFrontVesselInnerRadius = val;       }
  G4double GetFrontVesselOuterRadius()                  { return fFrontVesselOuterRadius;      }
  void     SetFrontVesselOuterRadius(G4double val)      { fFrontVesselOuterRadius = val;       }
  G4double GetMainVesselInnerRadius()                   { return fMainVesselInnerRadius;       }
  void     SetMainVesselInnerRadius(G4double val)       { fMainVesselInnerRadius = val;        }
  G4double GetMainVesselOuterRadius()                   { return fMainVesselOuterRadius;       }
  void     SetMainVesselOuterRadius(G4double val)       { fMainVesselOuterRadius = val;        }
  G4double GetExitPipeInnerRadius1()                    { return fExitPipeInnerRadius1;        }
  void     SetExitPipeInnerRadius1(G4double val)        { fExitPipeInnerRadius1 = val;         }
  G4double GetExitPipeOuterRadius1()                    { return fExitPipeOuterRadius1;        }
  void     SetExitPipeOuterRadius1(G4double val)        { fExitPipeOuterRadius1 = val;         }
  G4double GetExitPipeInnerRadius2()                    { return fExitPipeInnerRadius2;        }
  void     SetExitPipeInnerRadius2(G4double val)        { fExitPipeInnerRadius2 = val;         }
  G4double GetExitPipeOuterRadius2()                    { return fExitPipeOuterRadius2;        }
  void     SetExitPipeOuterRadius2(G4double val)        { fExitPipeOuterRadius2 = val;         }
  G4double GetManginMirrorInnerRadius()                 { return fManginMirrorInnerRadius;     }
  void     SetManginMirrorInnerRadius(G4double val)     { fManginMirrorInnerRadius = val;      }
  G4double GetManginMirrorOuterRadius()                 { return fManginMirrorOuterRadius;     }
  void     SetManginMirrorOuterRadius(G4double val)     { fManginMirrorOuterRadius = val;      }
  G4double GetChromaticCorrectorInnerRadius()             { return fChromaticCorrectorInnerRadius; }
  void     SetChromaticCorrectorInnerRadius(G4double val) { fChromaticCorrectorInnerRadius = val;  }
  G4double GetChromaticCorrectorOuterRadius()             { return fChromaticCorrectorOuterRadius; }
  void     SetChromaticCorrectorOuterRadius(G4double val) { fChromaticCorrectorOuterRadius = val;  }
  G4double GetDiaphragmInnerRadius()                    { return fDiaphragmInnerRadius;        }
  void     SetDiaphragmInnerRadius(G4double val)        { fDiaphragmInnerRadius = val;         }
  G4double GetDiaphragmOpeningRadius()                  { return fDiaphragmOpeningRadius;      }
  void     SetDiaphragmOpeningRadius(G4double val)      { fDiaphragmOpeningRadius = val;       }
  G4double GetDiaphragmApertureR()                      { return fDiaphragmApertureR;          }
  void     SetDiaphragmApertureR(G4double val)          { fDiaphragmApertureR = val;           }
  G4double GetDiaphragmAperturePhi()                    { return fDiaphragmAperturePhi;        }
  void     SetDiaphragmAperturePhi(G4double val)        { fDiaphragmAperturePhi = val;         }
  G4double GetCondenserOuterRadius()                    { return fCondenserOuterRadius;        }
  void     SetCondenserOuterRadius(G4double val)        { fCondenserOuterRadius = val;         }
  G4double GetLightGuideInnerRadius()                   { return fLightGuideInnerRadius;       }
  void     SetLightGuideInnerRadius(G4double val)       { fLightGuideInnerRadius = val;        }
  G4double GetLightGuideOuterRadius()                   { return fLightGuideOuterRadius;       }
  void     SetLightGuideOuterRadius(G4double val)       { fLightGuideOuterRadius = val;        }
  G4double GetLightGuideDiameter()                      { return fLightGuideDiameter;          }
  void     SetLightGuideDiameter(G4double val)          { fLightGuideDiameter = val;           }

  G4double GetLightGuideDepth() { return fLightGuideOuterRadius - fLightGuideInnerRadius; }

  G4double GetManginMirrorReflectingSurfaceRadius() { return fManginMirrorReflectingSurfaceRadius; }
  void     SetManginMirrorReflectingSurfaceRadius(G4double val)
  { fManginMirrorReflectingSurfaceRadius = val; }
  G4double GetManginMirrorRefractingSurfaceRadius() { return fManginMirrorRefractingSurfaceRadius; }
  void     SetManginMirrBorRefractingSurfaceRadius(G4double val)
  { fManginMirrorRefractingSurfaceRadius = val; }
  G4double GetChromaticCorrectorRearSurfaceRadius() { return fChromaticCorrectorRearSurfaceRadius; }
  void     SetChromaticCorrectorRearSurfaceRadius(G4double val)
  { fChromaticCorrectorRearSurfaceRadius = val; }
  G4double GetCondenserFrontSurfaceRadius() { return fCondenserFrontSurfaceRadius; }
  void     SetCondenserFrontSurfaceRadius(G4double val)
  { fCondenserFrontSurfaceRadius = val; }

  G4double GetQFSMagnetZLength()                        { return fQFSMagnetZLength;            }
  void     SetQFSMagnetZLength(G4double val)            { fQFSMagnetZLength = val;             }
  G4double GetQFSMagnet077NominalGradient()             { return fQFSMagnet077NominalGradient; }
  void     SetQFSMagnet077NominalGradient(G4double val) { fQFSMagnet077NominalGradient = val;  }
  G4double GetQFSMagnet079NominalGradient()             { return fQFSMagnet079NominalGradient; }
  void     SetQFSMagnet079NominalGradient(G4double val) { fQFSMagnet079NominalGradient = val;  }

  /***************************************************************/
  /** positions with respect to the CEDAR responsibility region **/
  /***************************************************************/

  G4ThreeVector GetEntranceWindowPosition()                      { return fEntranceWindowPosition;     }
  void          SetEntranceWindowPosition(G4ThreeVector val)     { fEntranceWindowPosition = val;      }
  G4ThreeVector GetFrontPipePosition()                           { return fFrontPipePosition;          }
  void          SetFrontPipePosition(G4ThreeVector val)          { fFrontPipePosition = val;           }
  G4ThreeVector GetQuartzWindowDiskPosition()                    { return fQuartzWindowDiskPosition;   }
  void          SetQuartzWindowDiskPosition(G4ThreeVector val)   { fQuartzWindowDiskPosition = val;    }
  G4ThreeVector GetFrontVesselPosition()                         { return fFrontVesselPosition;        }
  void          SetFrontVesselPosition(G4ThreeVector val)        { fFrontVesselPosition = val;         }
  G4ThreeVector GetMainVesselCylinderPosition()                  { return fMainVesselCylinderPosition; }
  void          SetMainVesselCylinderPosition(G4ThreeVector val) { fMainVesselCylinderPosition = val;  }
  G4ThreeVector GetExitWindowPosition()                          { return fExitWindowPosition;         }
  void          SetExitWindowPosition(G4ThreeVector val)         { fExitWindowPosition = val;          }
  G4ThreeVector GetExitPipePosition()                            { return fExitPipePosition;           }
  void          SetExitPipePosition(G4ThreeVector val)           { fExitPipePosition = val;            }
  G4ThreeVector GetManginMirrorPosition()                        { return fManginMirrorPosition;       }
  void          SetManginMirrorPosition(G4ThreeVector val)       { fManginMirrorPosition = val;        }
  G4ThreeVector GetChromaticCorrectorPosition()                  { return fChromaticCorrectorPosition; }
  void          SetChromaticCorrectorPosition(G4ThreeVector val) { fChromaticCorrectorPosition = val;  }
  G4ThreeVector GetDiaphragmPosition()                           { return fDiaphragmPosition;          }
  void          SetDiaphragmPosition(G4ThreeVector val)          { fDiaphragmPosition = val;           }
  G4ThreeVector GetCondenserPosition()                           { return fCondenserPosition;          }
  void          SetCondenserPosition(G4ThreeVector val)          { fCondenserPosition = val;           }

  G4ThreeVector GetQFSMagnet077Position()                        { return fQFSMagnet077Position;       }
  void          SetQFSMagnet077Position(G4ThreeVector val)       { fQFSMagnet077Position = val;        }
  G4ThreeVector GetQFSMagnet079Position()                        { return fQFSMagnet079Position;       }
  void          SetQFSMagnet079Position(G4ThreeVector val)       { fQFSMagnet079Position = val;        }

  /***********************************************************/
  /** positions with respect to the octant central point    **/
  /***********************************************************/

  G4ThreeVector GetAtexCylinderPosition()                              { return fAtexCylinderPosition;         }
  void          SetAtexCylinderPosition(G4ThreeVector val)             { fAtexCylinderPosition = val;          }
  G4ThreeVector GetExternalLensPosition()                              { return fExternalLensPosition;         }
  void          SetExternalLensPosition(G4ThreeVector val)             { fExternalLensPosition = val;          }
  G4ThreeVector GetSphericalMirrorPosition(G4int i)                    { return fSphericalMirrorPosition[i-1]; }
  void          SetSphericalMirrorPosition(G4int i, G4ThreeVector val) { fSphericalMirrorPosition[i-1] = val;  }
  G4ThreeVector GetSphericalMirrorMountPosition()                      { return fSphericalMirrorMountPosition; }
  void          SetSphericalMirrorMountPosition(G4ThreeVector val)     { fSphericalMirrorMountPosition = val;  }
  G4ThreeVector GetLightGuidePosition()                                { return fLightGuidePosition;           }
  void          SetLightGuidePosition(G4ThreeVector val)               { fLightGuidePosition = val;            }
  G4ThreeVector GetOldPMTPosition()                                    { return fOldPMTPosition;               }
  void          SetOldPMTPosition(G4ThreeVector val)                   { fOldPMTPosition = val;                }

  G4ThreeVector GetAtexCylinderHolePosition()                          { return fAtexCylinderHolePosition;     }
  void          SetAtexCylinderHolePosition(G4ThreeVector val)         { fAtexCylinderHolePosition = val;      }

  /********************************************************************/

  G4double GetCondenserRadialOffset()                         { return fCondenserRadialOffset;     }
  void     SetCondenserRadialOffset(G4double val)             { fCondenserRadialOffset = val;      }
  G4double GetCondenserDistanceToCentre()                     { return fCondenserDistanceToCentre; }
  void     SetCondenserDistanceToCentre(G4double val)         { fCondenserDistanceToCentre = val;  }
  G4double GetInterCondenserAngle()                           { return fInterCondenserAngle;       }
  void     SetInterCondenserAngle(G4double val)               { fInterCondenserAngle = val;        }
  G4double GetQuartzWindowRadialOffset()                      { return fQuartzWindowRadialOffset;  }
  void     SetQuartzWindowRadialOffset(G4double val)          { fQuartzWindowRadialOffset = val;   }
  G4double GetQuartzWindowRadius()                            { return fQuartzWindowRadius;        }
  void     SetQuartzWindowRadius(G4double val)                { fQuartzWindowRadius = val;         }

  G4double GetSphericalMirrorCapRadialOffset()                { return fSphericalMirrorCapRadialOffset;    }
  void     SetSphericalMirrorCapRadialOffset(G4double val)    { fSphericalMirrorCapRadialOffset=val;       }
  G4double GetSphericalMirrorCentreRadialOffset()             { return fSphericalMirrorCentreRadialOffset; }
  void     SetSphericalMirrorCentreRadialOffset(G4double val) { fSphericalMirrorCentreRadialOffset=val;    }
  G4double GetSphericalMirrorSurfaceRadius()                  { return fSphericalMirrorSurfaceRadius;      }
  void     SetSphericalMirrorSurfaceRadius(G4double val)      { fSphericalMirrorSurfaceRadius = val;       }
  G4double GetSphericalMirrorDiameter()                       { return fSphericalMirrorDiameter;           }
  void     SetSphericalMirrorDiameter(G4double val)           { fSphericalMirrorDiameter = val;            }
  G4double GetSphericalMirrorCentralAngle()                   { return fSphericalMirrorCentralAngle;       }
  void     SetSphericalMirrorCentralAngle(G4double val)       { fSphericalMirrorCentralAngle = val;        }

  G4double GetAtexCylinderMinRadius()                         { return fAtexCylinderMinRadius;     }
  void     SetAtexCylinderMinRadius(G4double val)             { fAtexCylinderMinRadius = val;      }
  G4double GetAtexCylinderMaxRadius()                         { return fAtexCylinderMaxRadius;     }
  void     SetAtexCylinderMaxRadius(G4double val)             { fAtexCylinderMaxRadius = val;      }
  G4double GetAtexCylinderHoleDiameter()                      { return fAtexCylinderHoleDiameter;  }
  void     SetAtexCylinderHoleDiameter(G4double val)          { fAtexCylinderHoleDiameter = val;   }
  G4double GetExternalLensDiameter()                          { return fExternalLensDiameter;      }
  void     SetExternalLensDiameter(G4double val)              { fExternalLensDiameter = val;       }
  G4double GetExternalLensMinThickness()                      { return fExternalLensMinThickness;  }
  void     SetExternalLensMinThickness(G4double val)          { fExternalLensMinThickness = val;   }
  G4double GetExternalLensSurfaceRadius()                     { return fExternalLensSurfaceRadius; }
  void     SetExternalLensSurfaceRadius(G4double val)         { fExternalLensSurfaceRadius = val;  }

  G4double GetExternalLensMaxThickness()
  { return fExternalLensMinThickness + fExternalLensSurfaceRadius -
      0.5*sqrt(4.0*fExternalLensSurfaceRadius*fExternalLensSurfaceRadius -
	       fExternalLensDiameter*fExternalLensDiameter); }

  G4double GetfLightGuideCentreRadialOffset()                 { return fLightGuideCentreRadialOffset; }
  void     SetfLightGuideCentreRadialOffset(G4double val)     { fLightGuideCentreRadialOffset = val;  }

  /**************************************************************************/

  /* Lightguide geometry */

  // Lightguide
  G4int     GetLightGuideNConesTotal()                 { return fLightGuideNConesTotal;     }
  void      SetLightGuideNConesTotal(G4int val)        { fLightGuideNConesTotal = val;      }
  G4int     GetLightGuideNPMTsTotal()                  { return fLightGuideNPMTsTotal;      }
  void      SetLightGuideNPMTsTotal(G4int val)         { fLightGuideNPMTsTotal = val;       }
  G4int     GetLightGuideNofRows()                     { return fLightGuideNofRows;         }
  void      SetLightGuideNofRows(G4int val)            { fLightGuideNofRows = val;          }
  G4int*    GetLightGuideNofCones()                    { return fLightGuideNofCones;        }
  void      SetLightGuideNofCones(G4int *val)          { fLightGuideNofCones = val;         }
  G4int*    GetLightGuideInstrumented()                { return fLightGuideInstrumented;    }
  void      SetLightGuideInstrumented(G4int *val)      { fLightGuideInstrumented = val;     }
  G4double  GetLightGuideConesPhiShift()               { return fLightGuideConesPhiShift;   }
  void      SetLightGuideConesPhiShift(G4double val)   { fLightGuideConesPhiShift = val;    }
  G4double  GetLightGuideConesThetaShift()             { return fLightGuideConesThetaShift; }
  void      SetLightGuideConesThetaShift(G4double val) { fLightGuideConesThetaShift = val;  }
  G4double* GetLightGuideRowsPhiShift()                { return fLightGuideRowsPhiShift;    }
  void      SetLightGuideRowsPhiShift(G4double *val)   { fLightGuideRowsPhiShift = val;     }

  // Cones & PMTs
  G4double GetLightGuideConeInnerRadius()              { return fLightGuideConeInnerRadius;  }
  void     SetLightGuideConeInnerRadius(G4double val)  { fLightGuideConeInnerRadius = val;   }
  G4double GetLightGuideConeOuterRadius()              { return fLightGuideConeOuterRadius;  }
  void     SetLightGuideConeOuterRadius(G4double val)  { fLightGuideConeOuterRadius = val;   }
  G4double GetLightGuideConeLength()                   { return fLightGuideConeLength;       }
  void     SetLightGuideConeLength(G4double val)       { fLightGuideConeLength = val;        }
  G4double GetLightGuideConeOpeningAngle()             { return fLightGuideConeOpeningAngle; }
  void     SetLightGuideConeOpeningAngle(G4double val) { fLightGuideConeOpeningAngle = val;  }

  G4double GetPMTDiameter()                            { return fPMTDiameter;                }
  void     SetPMTDiameter(G4double val)                { fPMTDiameter = val;                 }
  G4double GetPMTWindowDiameter()                      { return fPMTWindowDiameter;          }
  void     SetPMTWindowDiameter(G4double val)          { fPMTWindowDiameter = val;           }
  G4double GetPMTPhotoCathodeDiameter()                { return fPMTPhotoCathodeDiameter;    }
  void     SetPMTPhotoCathodeDiameter(G4double val)    { fPMTPhotoCathodeDiameter = val;     }

  G4double GetPMTLength()                              { return fPMTLength;                  }
  void     SetPMTLength(G4double val)                  { fPMTLength = val;                   }
  G4double GetPMTPreWindowLength()                     { return fPMTPreWindowLength;         }
  void     SetPMTPreWindowLength(G4double val)         { fPMTPreWindowLength = val;          }
  G4double GetPMTWindowLength()                        { return fPMTWindowLength;            }
  void     SetPMTWindowLength(G4double val)            { fPMTWindowLength = val;             }
  G4double GetPMTPhotoCathodeLength()                  { return fPMTPhotoCathodeLength;      }
  void     SetPMTPhotoCathodeLength(G4double val)      { fPMTPhotoCathodeLength = val;       }

  G4double GetOldPMTLength()                           { return fOldPMTLength;               }
  void     SetOldPMTLength(G4double val)               { fOldPMTLength = val;                }
  G4double GetOldPMTRadius()                           { return fOldPMTRadius;               }
  void     SetOldPMTRadius(G4double val)               { fOldPMTRadius = val;                }
  G4double GetOldPMTWindowLength()                     { return fOldPMTWindowLength;         }
  void     SetOldPMTWindowLength(G4double val)         { fOldPMTWindowLength = val;          }
  G4double GetOldPMTWindowRadius()                     { return fOldPMTWindowRadius;         }
  void     SetOldPMTWindowRadius(G4double val)         { fOldPMTWindowRadius = val;          }
  G4double GetOldPMTPhotoCathodeLength()               { return fOldPMTPhotoCathodeLength;   }
  void     SetOldPMTPhotoCathodeLength(G4double val)   { fOldPMTPhotoCathodeLength = val;    }
  G4double GetOldPMTPhotoCathodeRadius()               { return fOldPMTPhotoCathodeRadius;   }
  void     SetOldPMTPhotoCathodeRadius(G4double val)   { fOldPMTPhotoCathodeRadius = val;    }

  /********************************************************************************************/

  /* PMT numbering */
  G4int GetPMTSequentialID (G4int fPositionID);

  /********************************************************************************************/
private:

  G4int    fNSectors;
  G4int    iGasH2;

  G4double fZminCedarRegionLab;
  G4double fZmaxCedarRegionLab;
  G4double fCedarRegionZPositionLab;
  G4double fCedarRegionZLength;
  G4double fCedarRegionXLength;
  G4double fCedarRegionYLength;

  G4double fZRotBoxStart;
  G4double fZRotBoxEnd;
  G4double fRotBoxXLength;
  G4double fRotBoxYLength;
  G4double fRotBoxZLength;
  G4double fRotBoxZCentre;

  G4double fZCedarOriginLab;
  G4double fZFrontPipeStart;
  G4double fZAtexCylinderStart;
  G4double fZSphericalMirrorCapCentreNominal;
  G4double *fZSphericalMirrorCapCentreMisalignmentX;
  G4double *fZSphericalMirrorCapCentreMisalignmentZ;
  G4double *fZSphericalMirrorCapCentre;
  G4double *fZSphericalMirrorCentre;
  G4double fZLightGuideCentre;

  G4double fZSphericalMirrorMountStart;
  G4double fZExternalLensStart;
  G4double fZQuartzWindowStart;
  G4double fZMainVesselStart;
  G4double fZExitWindowStart;
  G4double fZExitPipeStart;
  G4double fZManginMirrorStart;
  G4double fZChromaticCorrectorStart;
  G4double fZDiaphragmCentre;
  G4double fZCondenserStart;

  // Cedar misalignment
  G4double fAlignmentMotorX;
  G4double fAlignmentMotorY;
  G4double fDistanceBetweenFeet;
  G4double fRotAngleX;
  G4double fRotAngleY;
  G4double fXRotCentre;
  G4double fYRotCentre;
  G4double fZRotCentre;

  // Longitudinal sizes of elements
  G4double fEntranceWindowZLength;
  G4double fExitWindowZLength;
  G4double fFrontPipeZLength;
  G4double fQuartzWindowZLength;
  G4double fFrontVesselZLength;
  G4double fMainVesselCylinderZLength;
  G4double fExitPipeZLength;
  G4double fManginMirrorZLength;
  G4double fManginMirrorCoatingZLength;
  G4double fChromaticCorrectorZLength;
  G4double fDiaphragmZLength;
  G4double fCondenserZLength;
  G4double fAtexCylinderZLength;
  G4double fSphericalMirrorMountZLength;
  G4double fSphericalMirrorMountSupportRingZLength;
  G4double fSphericalMirrorMountSupportRingRin;
  G4double fSphericalMirrorMountSupportRingRout;
  G4double fSphericalMirrorMountSupportRingHoleDia;

  // Radii of elements
  G4double fFrontPipeInnerRadius;
  G4double fFrontPipeOuterRadius;
  G4double fFrontVesselInnerRadius;
  G4double fFrontVesselOuterRadius;
  G4double fMainVesselInnerRadius;
  G4double fMainVesselOuterRadius;
  G4double fExitPipeInnerRadius1;
  G4double fExitPipeOuterRadius1;
  G4double fExitPipeInnerRadius2;
  G4double fExitPipeOuterRadius2;
  G4double fManginMirrorInnerRadius;
  G4double fManginMirrorOuterRadius;
  G4double fManginMirrorReflectingSurfaceRadius;
  G4double fManginMirrorRefractingSurfaceRadius;
  G4double fChromaticCorrectorInnerRadius;
  G4double fChromaticCorrectorOuterRadius;
  G4double fChromaticCorrectorRearSurfaceRadius;
  G4double fDiaphragmInnerRadius;
  G4double fDiaphragmOpeningRadius;
  G4double fDiaphragmApertureR;
  G4double fDiaphragmAperturePhi;
  G4double fCondenserOuterRadius;
  G4double fCondenserFrontSurfaceRadius;
  G4double fLightGuideInnerRadius;
  G4double fLightGuideOuterRadius;
  G4double fLightGuideDiameter;

  // Quadrupole magnets
  G4double fQFS077ZPositionLab;
  G4double fQFS079ZPositionLab;
  G4double fQFSMagnetZLength;
  G4double fQFSMagnet077NominalGradient;
  G4double fQFSMagnet079NominalGradient;

  /*************************************************/

  // wrt CEDAR responsilibity region

  G4ThreeVector fRotBoxPosition;
  G4ThreeVector fRotCentrePosition;
  G4ThreeVector fEntranceWindowPosition;
  G4ThreeVector fFrontPipePosition;
  G4ThreeVector fQuartzWindowDiskPosition;
  G4ThreeVector fFrontVesselPosition;
  G4ThreeVector fMainVesselCylinderPosition;
  G4ThreeVector fExitWindowPosition;
  G4ThreeVector fExitPipePosition;
  G4ThreeVector fManginMirrorPosition;
  G4ThreeVector fChromaticCorrectorPosition;
  G4ThreeVector fDiaphragmPosition;
  G4ThreeVector fCondenserPosition;
  G4ThreeVector fQFSMagnet077Position;
  G4ThreeVector fQFSMagnet079Position;

  // wrt octant cenral point
  G4ThreeVector fAtexCylinderPosition;
  G4ThreeVector *fSphericalMirrorPosition;
  G4ThreeVector fSphericalMirrorMountPosition;
  G4ThreeVector fLightGuidePosition;
  G4ThreeVector fExternalLensPosition;
  G4ThreeVector fOldPMTPosition;

  // wrt atex cylinder
  G4ThreeVector fAtexCylinderHolePosition;

  /**************************************************/

  G4double fCondenserRadialOffset;
  G4double fCondenserDistanceToCentre;
  G4double fInterCondenserAngle;
  G4double fQuartzWindowRadialOffset; 
  G4double fQuartzWindowRadius;
  G4double fSphericalMirrorCapRadialOffset;
  G4double fSphericalMirrorCentreRadialOffset;
  G4double fSphericalMirrorSurfaceRadius;
  G4double fSphericalMirrorDiameter;
  G4double fSphericalMirrorCentralAngle;
  G4double fAtexCylinderMinRadius;
  G4double fAtexCylinderMaxRadius;
  G4double fAtexCylinderHoleDiameter;
  G4double fExternalLensDiameter;
  G4double fExternalLensMinThickness;
  G4double fExternalLensSurfaceRadius;

  G4double fLightGuideCentreRadialOffset;

  /****************************************/
  /* Lightguide geometry                  */
  /****************************************/

  /////////////////////////////////////////////////////////////////////////////
  // Definition of lightguide angles.
  //
  // Theta is the polar angle between the Z axis and
  // the vector normal to the LG surface. In the LG centre, Theta = pi/2.
  // A row is defined by cones with the same Theta.
  //
  // Phi is the polar angle between the Y axis (in the octant reference frame)
  // and the vector normal to the LG surface.
  // In the LG centre, Phi=0.
  // LG geometrical centre on the Y axis in the octant reference frame.
  /////////////////////////////////////////////////////////////////////////////

  G4int     iLGType;
  G4int     iOctantsEnabled;            // bit-encoded: which octants are enabled

  G4int     fLightGuideNConesTotal;     // total number of cones in the lightguide
  G4int     fLightGuideNPMTsTotal;      // total number of PMTs installed in the lightguide
  G4int     fLightGuideNofRows;         // row = group cones with the same Theta
  G4int*    fLightGuideNofCones;        // for each row
  G4int*    fLightGuideInstrumented;    // for each row
  G4double  fLightGuideConesPhiShift;   // shift between two adjacent cones in the same row
  G4double  fLightGuideConesThetaShift; // shift between two adjacent rows
  G4double* fLightGuideRowsPhiShift;    // shift of each row (wrt the LG centre)

  // Cone attributes
  G4double fLightGuideConeInnerRadius;
  G4double fLightGuideConeOuterRadius;
  G4double fLightGuideConeLength;
  G4double fLightGuideConeOpeningAngle;

  // PMTs
  G4double fPMTDiameter;
  G4double fPMTWindowDiameter;
  G4double fPMTPhotoCathodeDiameter;
  G4double fPMTLength;
  G4double fPMTPreWindowLength;
  G4double fPMTWindowLength;
  G4double fPMTPhotoCathodeLength;

  G4double fOldPMTLength;
  G4double fOldPMTRadius;
  G4double fOldPMTWindowLength;
  G4double fOldPMTWindowRadius;
  G4double fOldPMTPhotoCathodeLength;
  G4double fOldPMTPhotoCathodeRadius;

};
#endif
