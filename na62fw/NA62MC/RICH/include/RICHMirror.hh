#ifndef RICHMirror_H
#define RICHMirror_H 1

#include "NA62VComponent.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"

class RICHMirror : public NA62VComponent
{

public:
  
  RICHMirror(G4int,G4Material*, G4LogicalVolume*);
  ~RICHMirror();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  void DefineOpticalSurface();

public:

  G4int                GetSegmentIndex()                                  { return fSegmentIndex;                 };
  void                 SetSegmentIndex(G4int value)                       { fSegmentIndex = value;                };

  G4double             GetInnerRadius()                                   { return fInnerRadius;                  };
  void                 SetInnerRadius(G4double value)                     { fInnerRadius = value;                 };
  G4double             GetOuterRadius()                                   { return fOuterRadius;                  };
  void                 SetOuterRadius(G4double value)                     { fOuterRadius = value;                 };

  G4double             GetExternalRadius()                                { return fExternalRadius;               };
  void                 SetExternalRadius(G4double value)                  { fExternalRadius = value;              };

  G4double             GetZPosition()                                     { return fZPosition;                    };
  void                 SetZPosition(G4double value)                       { fZPosition = value;                   };

  G4VPhysicalVolume *  GetPhysicalMirror_Jura(G4int iMirr)                { return fPhysicalMirror_Jura[iMirr];   };
  void                 SetPhysicalMirror_Jura(G4VPhysicalVolume * value, G4int iMirr)
                                                                          { fPhysicalMirror_Jura[iMirr] = value;  };

  G4VPhysicalVolume *  GetPhysicalMirror_Saleve(G4int iMirr)              { return fPhysicalMirror_Saleve[iMirr]; };
  void                 SetPhysicalMirror_Saleve(G4VPhysicalVolume * value, G4int iMirr)
                                                                          { fPhysicalMirror_Saleve[iMirr] = value;};

  G4OpticalSurface *   GetOpticalSurface()                                { return fOpticalSurface;               };
  void                 SetOpticalSurface(G4OpticalSurface * value)        { fOpticalSurface = value;              };

private:
 
  G4int      fSegmentIndex;
  G4double*  fCenterOfCurvature_Jura;
  G4double*  fCenterOfCurvature_Saleve;
  G4double   fInnerRadius;
  G4double   fOuterRadius;
  G4double   fExternalRadius;
  G4double*  fMirrorGap;
  G4double   fMirrorShift_Jura[10][2];
  G4double   fMirrorShift_Saleve[10][2];
  G4double   fZPosition;

  G4double   fActuatorPinRadius; 
  G4double   fActuatorPinHeight;
  G4double*  fActuatorPinR1_Jura;
  G4double*  fActuatorPinR2_Jura;
  G4double*  fActuatorPinR1_Saleve;
  G4double*  fActuatorPinR2_Saleve;


  G4double   fStabilizerPinRadius;
  G4double*  fStabilizerPinHeight_Jura;
  G4double*  fStabilizerPinD_Jura;
  G4double*  fStabilizerPinHeight_Saleve;
  G4double*  fStabilizerPinD_Saleve;


  G4VSolid** fSolidMirror_Jura;
  G4VSolid*  fSolidHalfMirror_Jura;	
  G4LogicalVolume** fLogicalMirror_Jura;
  G4VPhysicalVolume** fPhysicalMirror_Jura;

  G4VSolid** fSolidActuatorPin1_Jura;
  G4LogicalVolume** fLogicalActuatorPin1_Jura;
  G4VPhysicalVolume** fPhysicalActuatorPin1_Jura;

  G4VSolid** fSolidActuatorPin2_Jura;
  G4LogicalVolume** fLogicalActuatorPin2_Jura;
  G4VPhysicalVolume** fPhysicalActuatorPin2_Jura;

  G4VSolid** fSolidStabilizerPin_Jura;
  G4LogicalVolume** fLogicalStabilizerPin_Jura;
  G4VPhysicalVolume** fPhysicalStabilizerPin_Jura;



  G4VSolid** fSolidMirror_Saleve;
  G4VSolid*  fSolidHalfMirror_Saleve;	
  G4LogicalVolume** fLogicalMirror_Saleve;
  G4VPhysicalVolume** fPhysicalMirror_Saleve;

  G4VSolid** fSolidActuatorPin1_Saleve;
  G4LogicalVolume** fLogicalActuatorPin1_Saleve;
  G4VPhysicalVolume** fPhysicalActuatorPin1_Saleve;

  G4VSolid** fSolidActuatorPin2_Saleve;
  G4LogicalVolume** fLogicalActuatorPin2_Saleve;
  G4VPhysicalVolume** fPhysicalActuatorPin2_Saleve;

  G4VSolid** fSolidStabilizerPin_Saleve;
  G4LogicalVolume** fLogicalStabilizerPin_Saleve;
  G4VPhysicalVolume** fPhysicalStabilizerPin_Saleve;

  G4OpticalSurface* fOpticalSurface;

};

#endif
