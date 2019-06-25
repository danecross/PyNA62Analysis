//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-10
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
// Copied from Harish/MUV/MUVGeometryParameters
// Changes to MUV1 by ykohl in March 2010
//
// --------------------------------------------------------------------
#ifndef MUV1GeometryParameters_H
#define MUV1GeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"

#include "G4RotationMatrix.hh"
#include "NA62VGeometryParameters.hh"

class MUV1GeometryParameters : public NA62VGeometryParameters
{

public:

  ~MUV1GeometryParameters();
  static MUV1GeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();
  static G4RotationMatrix stringToRotationMatrix(G4String rotation);

private:

  static MUV1GeometryParameters* fInstance;

protected:

  MUV1GeometryParameters();

public:

  G4double  GetWorldZLength()                           { return fWorldZLength;             }
  void      SetWorldZLength(G4double value)             { fWorldZLength = value;            }
  G4double  GetWorldXLength()                           { return fWorldXLength;             }
  void      SetWorldXLength(G4double value)             { fWorldXLength = value;            }
  G4double  GetWorldYLength()                           { return fWorldYLength;             }
  void      SetWorldYLength(G4double value)             { fWorldYLength = value;            }

  G4double  GetMUV1ResponsibilityRegionXLength()                 { return fMUV1ResponsibilityRegionXLength;     }
  void      SetMUV1ResponsibilityRegionXLength(G4double value)   { fMUV1ResponsibilityRegionXLength = value;    }
  G4double  GetMUV1ResponsibilityRegionYLength()                 { return fMUV1ResponsibilityRegionYLength;     }
  void      SetMUV1ResponsibilityRegionYLength(G4double value)   { fMUV1ResponsibilityRegionYLength = value;    }
  G4double  GetMUV1ResponsibilityRegionZBeginning()              { return fMUV1ResponsibilityRegionZBeginning;  }
  void      SetMUV1ResponsibilityRegionZBeginning(G4double value){ fMUV1ResponsibilityRegionZBeginning = value; }
  G4double  GetMUV1ResponsibilityRegionZEnd()                    { return fMUV1ResponsibilityRegionZEnd;        }
  void      SetMUV1ResponsibilityRegionZEnd(G4double value)      { fMUV1ResponsibilityRegionZEnd = value;       }
  G4double  GetMUV1DetectorFrontZPosition()                      { return fMUV1DetectorFrontZPosition;          }
  void      SetMUV1DetectorFrontZPosition(G4double value)        { fMUV1DetectorFrontZPosition = value;         }

  G4double  GetBareScintillatorThickness()                       { return  fBareScintillatorThickness;   }
  void      SetBareScintillatorThickness(G4double value)         { fBareScintillatorThickness = value;   }
  G4double  GetScintillatorThickness()                           { return  fScintillatorThickness;       }
  void      SetScintillatorThickness(G4double value)             { fScintillatorThickness = value;       }
  G4double  GetIronThickness()                                   { return  fIronThickness;               }
  void      SetIronThickness(G4double value)                     { fIronThickness = value;               }
  G4double  GetRubberThickness()                                 { return fRubberThickness;              }
  void      SetRubberThickness(G4double value)                   { fRubberThickness = value;             }
  G4double  GetRubberRealThickness()                             { return fRubberRealThickness;          }
  void      SetRubberRealThickness(G4double value)               { fRubberRealThickness = value;         }

  G4int     GetNIronPlate()                             { return  fNIronPlate;              }
  void      SetNIronPlate(G4int value)                  { fNIronPlate = value;              }

  G4int     GetIronPlateSizeX()                         { return  fIronPlateSizeX;          }
  void      SetIronPlateSizeX(G4int value)              { fIronPlateSizeX = value;          }
  G4int     GetIronPlateSizeY()                         { return  fIronPlateSizeY;          }
  void      SetIronPlateSizeY(G4int value)              { fIronPlateSizeY = value;          }
  G4int     GetIronPlateOuterSizeX()                    { return  fIronPlateOuterSizeX;     }
  void      SetIronPlateOuterSizeX(G4int value)         { fIronPlateOuterSizeX = value;     }
  G4int     GetIronPlateOuterSizeY()                    { return  fIronPlateOuterSizeY;     }
  void      SetIronPlateOuterSizeY(G4int value)         { fIronPlateOuterSizeY = value;     }

  G4double  GetConnectionHoleRadius()					{  return fConnectionHoleRadius;    }
  void		SettConnectionHoleRadius(G4double value)	{ fConnectionHoleRadius = value;    }
  G4double  GetBoltPositionX()							{ return fBoltPositionX;		    }
  void      SetBoltPositionX(G4double value)			{ fBoltPositionX = value;		    }
  G4double  GetBoltPositionY()							{ return fBoltPositionY;		    }
  void      SetBoltPositionY(G4double value)			{ fBoltPositionY = value;		    }
  G4double  GetOuterSpacerOuterRadius()					{ return fOuterSpacerOuterRadius;	}
  void      SetOuterSpacerOuterRadius(G4double value)	{ fOuterSpacerOuterRadius = value;	}

  G4int     GetNRubberPlate()                           { return  fNRubberPlate;            }
  void      SetNRubberPlate(G4int value)                { fNRubberPlate = value;            }

  G4double  GetMUV1XSize()                              { return fMUV1XSize;                }
  void      SetMUV1XSize(G4double value)                { fMUV1XSize = value;               }
  G4double  GetMUV1YSize()                              { return fMUV1YSize;                }
  void      SetMUV1YSize(G4double value)                { fMUV1YSize = value;               }

  G4double  GetMUV1Length()                             { return fMUV1Length;               }
  void      SetMUV1Length(G4double value)               { fMUV1Length = value;              }
  G4double  GetMUV1ZPosition()                          { return fMUV1ZPosition;            }
  void      SetMUV1ZPosition(G4double value)            { fMUV1ZPosition = value;           }

  G4double  GetBareScintWidthStandard()                 { return fBareScintWidthStandard;   }
  void      SetBareScintWidthStandard(G4double value)   { fBareScintWidthStandard = value;  }
  G4double  GetBareScintWidthMiddle()                   { return fBareScintWidthMiddle;     }
  void      SetBareScintWidthMiddle(G4double value)     { fBareScintWidthMiddle = value;    }
  G4double  GetBareScintWidthOuter()                    { return fBareScintWidthOuter;      }
  void      SetBareScintWidthOuter(G4double value)      { fBareScintWidthOuter = value;     }

  G4double  GetBareScintLengthStandard()                    { return fBareScintLengthStandard;      }
  void      SetBareScintLengthStandard(G4double value)      { fBareScintLengthStandard = value;     }
  G4double  GetBareScintLengthMiddleStd()                   { return fBareScintLengthMiddleStd;     }
  void      SetBareScintLengthMiddleStd(G4double value)     { fBareScintLengthMiddleStd = value;    }
  G4double  GetBareScintLengthMiddleOuter()                 { return fBareScintLengthMiddleOuter;   }
  void      SetBareScintLengthMiddleOuter(G4double value)   { fBareScintLengthMiddleOuter = value;  }

  G4double  GetSkinWidth()                              { return fSkinWidth;                }
  void      SetSkinWidth(G4double value)                { fSkinWidth = value;               }
  G4double  GetAirGapWidth()                            { return fAirGapWidth;              }
  void      SetAirGapWidth(G4double value)              { fAirGapWidth = value;             }

  G4double  GetScintWidthStandard()                     { return fScintWidthStandard;       }
  void      SetScintWidthStandard(G4double value)       { fScintWidthStandard = value;      }
  G4double  GetScintWidthMiddle()                       { return fScintWidthMiddle;         }
  void      SetScintWidthMiddle(G4double value)         { fScintWidthMiddle = value;        }
  G4double  GetScintWidthOuter()                        { return fScintWidthOuter;          }
  void      SetScintWidthOuter(G4double value)          { fScintWidthOuter = value;         }

  G4double  GetScintLengthStandard()                    { return fScintLengthStandard;      }
  void      SetScintLengthStandard(G4double value)      { fScintLengthStandard = value;     }
  G4double  GetScintLengthMiddleStd()                   { return fScintLengthMiddleStd;     }
  void      SetScintLengthMiddleStd(G4double value)     { fScintLengthMiddleStd = value;    }
  G4double  GetScintLengthMiddleOuter()                 { return fScintLengthMiddleOuter;   }
  void      SetScintLengthMiddleOuter(G4double value)   { fScintLengthMiddleOuter = value;  }
  G4double* GetScintLengthOuter()                       { return fScintLengthOuter;         }
  void      SetScintLengthOuter(G4double* value)        { fScintLengthOuter = value;        }

  G4double  GetScintMotherWidth() 					    { return fScintMotherWidth;         }
  void      SetScintMotherWidth(G4double value)		    { fScintMotherWidth = value;        }
  G4double	GetScintMotherLength()					    { return fScintMotherLenght;        }
  void      SetScintMotherLength(G4double value)	    { fScintMotherLenght = value;       }
  G4double	GetScintMotherThickness()				    { return fScintMotherThickness;     }
  void      SetScintMotherThickness(G4double value)	    { fScintMotherThickness = value;    }
  G4double	GetScintLayerThickness()				    { return fScintLayerThickness;      }
  void      SetScintLayerThickness(G4double value)	    { fScintLayerThickness = value;     }


  G4double  GetmOCounterCutHeight()                     { return fmOCounterCutHeight;       }
  void      SetmOCounterCutHeight(G4double value)       { fmOCounterCutHeight = value;      }

  G4int     GetNMUV1CounterStandard()                   { return  fNMUV1CounterStandard;    }
  void      SetNMUV1CounterStandard(G4int value)        { fNMUV1CounterStandard = value;    }
  G4int     GetNMUV1CounterMiddle()                     { return  fNMUV1CounterMiddle;      }
  void      SetNMUV1CounterMiddle(G4int value)          { fNMUV1CounterMiddle = value;      }
  G4int     GetNMUV1CounterMiddleStd()                  { return  fNMUV1CounterMiddleStd;   }
  void      SetNMUV1CounterMiddleStd(G4int value)       { fNMUV1CounterMiddleStd = value;   }
  G4int     GetNMUV1CounterMiddleOuter()                { return  fNMUV1CounterMiddleOuter; }
  void      SetNMUV1CounterMiddleOuter(G4int value)     { fNMUV1CounterMiddleOuter = value; }
  G4int     GetNMUV1CounterOuter()                      { return  fNMUV1CounterOuter;       }
  void      SetNMUV1CounterOuter(G4int value)           { fNMUV1CounterOuter = value;       }
  
  G4int     GetNScintVertical()                         { return  fNScintVertical;          }
  void      SetNScintVertical(G4int value)              { fNScintVertical = value;          }
  G4int     GetNScintHorizontal()                       { return  fNScintHorizontal;        }
  void      SetNScintHorizontal(G4int value)            { fNScintHorizontal = value;        }

  G4double  GetBirksConstant()                          { return  fBirksConstant;           }
  void      SetBirksConstant(G4double value)            { fBirksConstant = value;           }

  G4double  GetHitContainerTimeLimit()                                 { return  fHitContainerTimeLimit;                  }
  void      SetHitContainerTimeLimit(G4double value)                   { fHitContainerTimeLimit = value;                  }
  G4double  GetHitContainerScintillatorSegmentation()                  { return  fHitContainerScintillatorSegmentation;   }
  void      SetHitContainerScintillatorSegmentation(G4double value)    { fHitContainerScintillatorSegmentation = value;   }
  G4double  GetHitContainerDimension()                                 { return fHitContainerDimension;                   }
  void      SetHitContainerDimension( G4double dim )                   { fHitContainerDimension = dim;                    }

  void      SetNFiber(G4int value)                      { fNFiber = value;                  }
  G4int     GetNFiber()                                 { return fNFiber;                   }
  void      SetFiberRadius(G4double value)              { fFiberRadius = value;             }
  G4double  GetFiberRadius()                            { return fFiberRadius;              }
  void      SetFiberSeperation(G4double value)          { fFiberSeperation = value;         }
  G4double  GetFiberSeperation()                        { return fFiberSeperation;          }
  void      SetFirstFiberXPosition(G4double value)      { fFirstFiberXPosition = value;     }
  G4double  GetFirstFiberXPosition()                    { return fFirstFiberXPosition;      }
  void      SetFiberLengthOut(G4double value)           { fFiberLengthOut = value;          }
  G4double  GetFiberLengthOut() 					    {return fFiberLengthOut;            }
  void      SetFiberPhotonEnergyCut(G4double value)     { fFiberPhotonEnergyCut = value;    }
  G4double  GetFiberPhotonEnergyCut() 					{return fFiberPhotonEnergyCut;      }

  void      SetGrooveWidth(G4double value)              { fGrooveWidth = value;             }
  G4double  GetGrooveWidth()                            { return fGrooveDepth;              }
  void      SetGrooveDepth(G4double value)              { fGrooveDepth = value;             }
  G4double  GetGrooveDepth()                            { return fGrooveDepth;              }
  void      SetGroovePositionInStandardScintillator(G4double value)      { fGroovePositionInStandardScintillator = value;     }
  G4double  GetGroovePositionInStandardScintillator()                    { return fGroovePositionInStandardScintillator;      }

  void           SetPMTSize(G4ThreeVector value)        { fPMTSize = value;              }
  G4ThreeVector  GetPMTSize()                           { return fPMTSize;               }
  void      SetPMTAirGap(G4double value)                { fPMTAirGap = value;            }
  G4double  GetPMTAirGap()                              { return fPMTAirGap;             }
  void      SetPMTGlassGap(G4double value)              { fPMTGlassGap = value;          }
  G4double  GetPMTGlassGap()                            { return fPMTGlassGap;           }

  void      SetCutDepth(G4double value)                 { fCutDepth = value;             }
  G4double  GetCutDepth()                               { return fCutDepth;              }
  void      SetCutDepthShort(G4double value)            { fCutDepthShort = value;        }
  G4double  GetCutDepthShort()                          { return fCutDepthShort;         }
  void      SetCutDepthLong(G4double value)             { fCutDepthLong = value;         }
  G4double  GetCutDepthLong()                           { return fCutDepthLong;          }
  void      SetCutWidth(G4double value)                 { fCutWidth = value;             }
  G4double  GetCutWidth()                               { return fCutWidth;              }
  
  void		    SetScintillatorSpacerSize(G4ThreeVector value)  {fScintillatorSpacerSize = value;     }
  G4ThreeVector GetScintillatorSpacerSize()				        { return fScintillatorSpacerSize;	  }
  void		    SetCutBoxOuterSize(G4ThreeVector value)		    { fCutBoxOuterSize = value;			  }
  G4ThreeVector GetCutBoxOuterSize()					        { return fCutBoxOuterSize;			  }
  void          SetCutBoxMiddleOuterSize(G4ThreeVector value)   { fCutBoxMiddleOuterSize = value;	  }
  G4ThreeVector GetCutBoxMiddleOuterSize()				        { return fCutBoxMiddleOuterSize;	  }
  void		    SetCutBoxMiddleInnerSize(G4ThreeVector value)   { fCutBoxMiddleInnerSize = value;     }
  G4ThreeVector GetCutBoxMiddleInnerSize()				        { return fCutBoxMiddleInnerSize;	  }
  void		    SetCutBoxInnerSize(G4ThreeVector value)		    { fCutBoxInnerSize = value;           }
  G4ThreeVector GetCutBoxInnerSize()					        { return fCutBoxInnerSize;			  }
  G4double      GetHoleDiameter()                               { return fHoleDiameter;               }
  void          SetHoleDiameter(G4double value)                 { fHoleDiameter = value;              }
  void		    SetHoleInnerRadius(G4double value)		        { fHoleInnerRadius = value;		      }
  G4double	    GetHoleInnerRadius()						    { return fHoleInnerRadius;		      }
  void		    SetTubeInnerRadius(G4double value)		        { fTubeInnerRadius = value;		      }
  G4double	    GetTubeInnerRadius()						    { return fTubeInnerRadius;		      }
  G4double      GetInnerBeamPipeRadius()                        { return fInnerBeamPipeRadius;        }
  void          SetInnerBeamPipeRadius(G4double value)          { fInnerBeamPipeRadius = value;       }
  G4double      GetOuterBeamPipeRadius()                        { return fOuterBeamPipeRadius;        }
  void          SetOuterBeamPipeRadius(G4double value)          { fOuterBeamPipeRadius = value;       }
  G4double      GetLongitudinalLengthBeamPipe()                 { return fLongitudinalLengthBeamPipe; }
  void          SetLongitudinalLengthBeamPipe(G4double value)   { fLongitudinalLengthBeamPipe = value;}

private:

  G4double  fWorldZLength;
  G4double  fWorldXLength;
  G4double  fWorldYLength;

  G4double  fMUV1ResponsibilityRegionXLength;
  G4double  fMUV1ResponsibilityRegionYLength;
  G4double  fMUV1ResponsibilityRegionZBeginning;
  G4double  fMUV1ResponsibilityRegionZEnd;

  G4double  fMUV1DetectorFrontZPosition;
  
  G4double  fBareScintillatorThickness; // w/o wrapping
  G4double  fScintillatorThickness; // wrapped
  G4double  fIronThickness;
  G4double  fRubberThickness;
   
  G4int    fNIronPlate;
  G4double fIronPlateSizeX;
  G4double fIronPlateSizeY;
  G4double fIronPlateOuterSizeX;
  G4double fIronPlateOuterSizeY;
  G4double fBoltPositionX;
  G4double fBoltPositionY;
  G4double fConnectionHoleRadius;
  G4double fOuterSpacerOuterRadius;

  G4int     fNRubberPlate;
  
  G4double  fMUV1XSize;
  G4double  fMUV1YSize;
  G4double  fMUV1Length;

  G4double  fMUV1ZPosition;

  //Dimensions of scintillator mother volume
  G4double  fScintMotherWidth; //dx
  G4double  fScintMotherLenght; //dy
  G4double  fScintMotherThickness; //dz

  //Scintillator layer box thickness
  G4double  fScintLayerThickness; //dz

  //Dimensions of bare scintillators --> no wrapper and air gap included
  G4double  fBareScintWidthStandard;
  G4double  fBareScintWidthMiddle;
  G4double  fBareScintWidthOuter;

  G4double  fBareScintLengthStandard;
  G4double  fBareScintLengthMiddleStd;
  G4double  fBareScintLengthMiddleOuter;

  G4double  fRubberRealThickness;

  //Scintillator wrapper material and air gap (between scintillator and wrapper) thickness
  G4double fSkinWidth;   // Skin width of the aluminum and air layers of the scintillator
  G4double fAirGapWidth; //air gap between scintillator and its wrapper on the pressed side (along the x-coordinate)

  //Dimensions of wrapped scintillators
  G4double  fScintWidthStandard;
  G4double  fScintWidthMiddle;
  G4double  fScintWidthOuter;
  G4double  fScintLengthStandard;
  G4double  fScintLengthMiddleStd;
  G4double  fScintLengthMiddleOuter;
  G4double* fScintLengthOuter;
  G4double  fmOCounterCutHeight;

  //MUV1 hits
  G4double fBirksConstant;
  G4double fHitContainerTimeLimit;
  G4double fHitContainerScintillatorSegmentation;
  G4double fHitContainerDimension;


  G4int     fNMUV1CounterStandard;
  G4int     fNMUV1CounterMiddleStd;
  G4int     fNMUV1CounterMiddleOuter;
  G4int     fNMUV1CounterMiddle;
  G4int     fNMUV1CounterOuter;

  G4int     fNScintVertical;
  G4int     fNScintHorizontal;


  G4double  fFiberRadius;
  G4int     fNFiber;
  G4double  fFiberSeperation;
  G4double  fFirstFiberXPosition;
  G4double  fCutWidth;
  G4double  fCutDepth;
  G4double  fCutDepthShort;
  G4double  fCutDepthLong;
  G4double  fFiberLengthOut;
  G4double  fFiberPhotonEnergyCut;

  //Scintillator Spacer & Transportation Tube & Beam Pipe Hole
  G4ThreeVector fScintillatorSpacerSize;
  G4ThreeVector fCutBoxOuterSize;
  G4ThreeVector fCutBoxMiddleOuterSize;
  G4ThreeVector fCutBoxMiddleInnerSize;
  G4ThreeVector fCutBoxInnerSize;
  G4double fHoleDiameter;
  G4double fHoleInnerRadius;
  G4double fTubeInnerRadius;

  //Groove
  G4double fGrooveWidth ;
  G4double fGrooveDepth;
  G4double fGroovePositionInStandardScintillator;

  //PMT
  G4ThreeVector fPMTSize;
  G4double fPMTAirGap;
  G4double fPMTGlassGap;

  //Beam pipe
  G4double fInnerBeamPipeRadius;
  G4double fOuterBeamPipeRadius;

  G4double fLongitudinalLengthBeamPipe;


};
#endif
