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
// Copied from MUVGeometryParameters
// Changes to MUV2 and commented out by ykohl in March 2010
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
// --------------------------------------------------------------------
#ifndef MUV2GeometryParameters_H
#define MUV2GeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"

#include "G4RotationMatrix.hh"
#include "NA62VGeometryParameters.hh"

class MUV2GeometryParameters : public NA62VGeometryParameters
{

public:

    ~MUV2GeometryParameters();
    static MUV2GeometryParameters* GetInstance();
    TObjArray GetHashTable();
    void Print();
    static G4RotationMatrix stringToRotationMatrix(G4String rotation);

private:

    static MUV2GeometryParameters* fInstance;

protected:

    MUV2GeometryParameters();

public:

    G4double GetWorldZLength()                        { return fWorldZLength;             };
    void     SetWorldZLength(G4double value)          { fWorldZLength = value;            };
    G4double GetWorldXLength()                        { return fWorldXLength;             };
    void     SetWorldXLength(G4double value)          { fWorldXLength = value;            };
    G4double GetWorldYLength()                        { return fWorldYLength;             };
    void     SetWorldYLength(G4double value)          { fWorldYLength = value;            };

    G4double GetMUV2ResponsibilityRegionXLength()                 { return fMUV2ResponsibilityRegionXLength;     };
    void     SetMUV2ResponsibilityRegionXLength(G4double value)   { fMUV2ResponsibilityRegionXLength = value;    };
    G4double GetMUV2ResponsibilityRegionYLength()                 { return fMUV2ResponsibilityRegionYLength;     };
    void     SetMUV2ResponsibilityRegionYLength(G4double value)   { fMUV2ResponsibilityRegionYLength = value;    };
    G4double GetMUV2ResponsibilityRegionZBeginning()              { return fMUV2ResponsibilityRegionZBeginning;  };
    void     SetMUV2ResponsibilityRegionZBeginning(G4double value){ fMUV2ResponsibilityRegionZBeginning = value; };
    G4double GetMUV2ResponsibilityRegionZEnd()                    { return fMUV2ResponsibilityRegionZEnd;        };
    void     SetMUV2ResponsibilityRegionZEnd(G4double value)      { fMUV2ResponsibilityRegionZEnd = value;       };

    G4double GetMUV2DetectorFrontZPosition()                      { return fMUV2DetectorFrontZPosition;          };
    void     SetMUV2DetectorFrontZPosition(G4double value)        { fMUV2DetectorFrontZPosition = value;         };

    G4double GetScintillatorThickness()               { return  fScintillatorThickness;   };
    void     SetScintillatorThickness(G4double value) { fScintillatorThickness = value;   };
    G4double GetIronThickness()                       { return  fIronThickness;           };
    void     SetIronThickness(G4double value)         { fIronThickness = value;           };
    G4double GetGapThickness()                        { return fGapThickness;             };
    void     SetGapThickeness(G4double value)         { fGapThickness = value;            };

    G4double  GetSkinAluminumWidth()                  { return fSkinAluminumWidth;        }
    void      SetSkinAluminumWidth(G4double value)    { fSkinAluminumWidth = value;       }
    G4double  GetSkinTapeWidth()                      { return fSkinTapeWidth;            }
    void      SetSkinTapeWidth(G4double value)        { fSkinTapeWidth = value;           }
    G4double  GetAirGapWidth()                        { return fAirGapWidth;              }
    void      SetAirGapWidth(G4double value)          { fAirGapWidth = value;             }

    G4int    GetNIronPlate()                          { return  fNIronPlate;              };
    void     SetNIronPlate(G4int value)               { fNIronPlate = value;              };

    G4double GetMUV2Size()                            { return fMUV2Size;                 };
    void     SetMUV2Size(G4double value)              { fMUV2Size = value;                };
    G4double GetMUV2Length()                          { return fMUV2Length;               };
    void     SetMUV2Length(G4double value)            { fMUV2Length = value;              };
    G4double GetMUV2ZPosition()                       { return fMUV2ZPosition;            };
    void     SetMUV2ZPosition(G4double value)         { fMUV2ZPosition = value;           };

    G4double GetScintWidthStandard()                  { return fScintWidthStandard;       };
    void     SetScintWidthStandard(G4double value)    { fScintWidthStandard = value;      };
    G4double GetScintWidthMiddle()                    { return fScintWidthMiddle;         };
    void     SetScintWidthMiddle(G4double value)      { fScintWidthMiddle = value;        };
    G4double GetScintWidthOuter()                     { return fScintWidthOuter;          };
    void     SetScintWidthOuter(G4double value)       { fScintWidthOuter = value;         };

    G4double GetScintLengthStandard()                 { return fScintLengthStandard;      };
    void     SetScintLengthStandard(G4double value)   { fScintLengthStandard = value;     };
    G4double GetScintLengthMiddle()                   { return fScintLengthMiddle;        };
    void     SetScintLengthMiddle(G4double value)     { fScintLengthMiddle = value;       };
    G4double GetScintLengthOuter()                    { return fScintLengthOuter;         };
    void     SetScintLengthOuter(G4double value)      { fScintLengthOuter = value;        };

    G4int    GetNMUV2CounterStandard()                { return  fNMUV2CounterStandard;    };
    void     SetNMUV2CounterStandard(G4int value)     { fNMUV2CounterStandard = value;    };
    G4int    GetNMUV2CounterMiddle()                  { return  fNMUV2CounterMiddle;      };
    void     SetNMUV2CounterMiddle(G4int value)       { fNMUV2CounterMiddle = value;      };
    G4int    GetNMUV2CounterOuter()                   { return  fNMUV2CounterOuter;       };
    void     SetNMUV2CounterOuter(G4int value)        { fNMUV2CounterOuter = value;       };

    G4int    GetNScintVertical()                      { return  fNScintVertical;          };
    void     SetNScintVertical(G4int value)           { fNScintVertical = value;          };
    G4int    GetNScintHorizontal()                    { return  fNScintHorizontal;        };
    void     SetNScintHorizontal(G4int value)         { fNScintHorizontal = value;        };

    G4double GetHoleDiameter()                        { return fHoleDiameter;             };
    void     SetHoleDiameter(G4double value)          { fHoleDiameter = value;            };
    G4double GetInnerTubeDiameter()                   { return fInnerTubeDiameter;        };
    void     SetInnerTubeDiameter(G4double value)     { fInnerTubeDiameter = value;        };

    G4double  GetBirksConstant()                      { return  fBirksConstant;           };
    void      SetBirksConstant(G4double value)        { fBirksConstant = value;           };


    G4double  GetHitContainerTimeLimit()                  { return  fHitContainerTimeLimit;        };
    void      SetHitContainerTimeLimit(G4double value)    { fHitContainerTimeLimit = value;        };
    G4double  GetHitContainerScintillatorSegmentation()                  { return  fHitContainerScintillatorSegmentation;        };
    void      SetHitContainerScintillatorSegmentation(G4double value)    { fHitContainerScintillatorSegmentation = value;        };

    G4int  GetHitContainerDimension()                  { return  fHitContainerDimension;        };
    void      SetHitContainerDimension(G4int value)    { fHitContainerDimension = value;        };



    G4double  GetBoxCutDepth()                            { return fBoxCutDepth;              };
    void      SetBoxCutDepth(G4double value)              { fBoxCutDepth = value;             };
    G4double  GetmOScintillatorCutHeight()                     { return fmOScintillatorCutHeight;       };
    void      SetmOScintillatorCutHeight(G4double value)       { fmOScintillatorCutHeight = value;      };
    G4double  GetmOScintillatorCutHeight2()                     { return fmOScintillatorCutHeight2;       };
    void      SetmOScintillatorCutHeight2(G4double value)       { fmOScintillatorCutHeight2 = value;      };
    G4double  GetScintillatorCutWidth()                     { return fScintillatorCutWidth;       };
    void      SetScintillatorCutWidth(G4double value)       { fScintillatorCutWidth = value;      };
    G4double  GetScintillatorCutWidth2()                     { return fScintillatorCutWidth2;       };
    void      SetScintillatorCutWidth2(G4double value)       { fScintillatorCutWidth2 = value;      };
    G4double      GetInnerBeamPipeRadius()                   { return fInnerBeamPipeRadius; };
    void          SetInnerBeamPipeRadius(G4double value)     { fInnerBeamPipeRadius = value; };

    G4double      GetOuterBeamPipeRadius()                   { return fOuterBeamPipeRadius; };
    void          SetOuterBeamPipeRadius(G4double value)     { fOuterBeamPipeRadius = value; };
    G4double      GetLongitudinalLengthBeamPipe()            { return fLongitudinalLengthBeamPipe; };
    void          SetLongitudinalLengthBeamPipe(G4double value) { fLongitudinalLengthBeamPipe = value; };

private:

    G4double  fWorldZLength;
    G4double  fWorldXLength;
    G4double  fWorldYLength;

    G4double  fMUV2ResponsibilityRegionXLength;
    G4double  fMUV2ResponsibilityRegionYLength;
    G4double  fMUV2ResponsibilityRegionZBeginning;
    G4double  fMUV2ResponsibilityRegionZEnd;

    G4double  fMUV2DetectorFrontZPosition;

    G4double  fScintillatorThickness;
    G4double  fIronThickness;
    G4double  fGapThickness;

    G4double  fSkinAluminumWidth;   // Skin width of the aluminum and of the scintillator
    G4double  fSkinTapeWidth;   // Skin width of the aluminum and of the scintillator
    G4double  fAirGapWidth; //air gap between scintillator and its wrapper

    G4int     fNIronPlate;

    G4double  fMUV2Size;
    G4double  fMUV2Length;
    G4double  fMUV2ZPosition;

    G4double  fScintWidthStandard;
    G4double  fScintWidthMiddle;
    G4double  fScintWidthOuter;

    G4double  fBoxCutDepth;
    G4double fScintillatorCutWidth;
    G4double fScintillatorCutWidth2;
    G4double fmOScintillatorCutHeight;
    G4double fmOScintillatorCutHeight2;

    G4double  fScintLengthStandard;
    G4double  fScintLengthMiddle;
    G4double  fScintLengthOuter;

    G4int     fNMUV2CounterStandard;
    G4int     fNMUV2CounterMiddle;
    G4int     fNMUV2CounterOuter;

    G4int     fNScintVertical;
    G4int     fNScintHorizontal;

    G4double  fHoleDiameter;
    G4double  fInnerTubeDiameter;

    G4double fBirksConstant;
    G4double fHitContainerTimeLimit;
    G4double fHitContainerScintillatorSegmentation;
    G4int fHitContainerDimension;


    //Beam pipe

    G4double fInnerBeamPipeRadius;
    G4double fOuterBeamPipeRadius;

    G4double fLongitudinalLengthBeamPipe;

};
#endif
