#ifndef RICHRadiator_H
#define RICHRadiator_H 1

#include "NA62VComponent.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"
#include "G4Tubs.hh"

class RICHRadiator : public NA62VComponent
{

public:
  
  RICHRadiator(G4Material*, G4LogicalVolume*);
  ~RICHRadiator();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

private:

  G4int fNSections;
  G4double   fInnerRadius;
  G4double   fZLength; 
  G4double   fZPosition;
  G4double*  fOuterRadii;
  G4double*  fZLengths; 

  G4double* fDownstreamFlangeZLengths;
  G4double* fDownstreamFlangeThickness;
  G4double* fDownstreamFlangeInnerRadii;
  G4double* fDownstreamFlangeZPosition;

  G4Tubs** fSolidDownstreamFlange;
};

#endif
