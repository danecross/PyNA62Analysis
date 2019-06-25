#ifndef RICHVessel_H
#define RICHVessel_H 1

#include "NA62VComponent.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"
#include "G4Tubs.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class RICHVessel : public NA62VComponent
{

public:
  
  RICHVessel(G4Material*, G4LogicalVolume*);
  ~RICHVessel();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  void DefineOpticalSurface();

public:

  G4OpticalSurface * GetOpticalSurface(){return fOpticalSurface;};
  void SetOpticalSurface(G4OpticalSurface * value){fOpticalSurface=value;};

private:

  G4double   fZPosition; 
  G4int      fNSections;
  G4double   fZLength; 
  G4double   fInnerRadius;
  G4double*  fInnerRadii;
  G4double   fThickness;
  G4double*  fZLengths; 

  G4double*  fPressureFlangeZLengths;  
  G4double*  fPressureFlangeThickness; 
  G4double*  fPressureFlangeInnerRadii;
  G4double*  fPressureFlangeZShift;
  G4double*  fPressureFlangeZPosition;

   G4double*  fUpstreamFlangeZLengths;  
   G4double*  fUpstreamFlangeThickness; 
   G4double*  fUpstreamFlangeInnerRadii; 
   G4double*  fUpstreamFlangeZShift;
   G4double*  fUpstreamFlangeZPosition;

  G4double*  fDownstreamFlangeZLengths;            
  G4double*  fDownstreamFlangeThickness;                   
  G4double*  fDownstreamFlangeInnerRadii;
  G4double*  fDownstreamFlangeZPosition;


  G4OpticalSurface* fOpticalSurface;

  G4Tubs** fSolidUpstreamFlange;
  G4LogicalVolume** fLogicUpstreamFlange;
  G4VPhysicalVolume** fPhysicalUpstreamFlange;

  G4Tubs** fSolidPressureFlange;
  G4LogicalVolume** fLogicPressureFlange;
  G4VPhysicalVolume** fPhysicalPressureFlange;

  G4Tubs** fSolidDownstreamFlange;
  G4LogicalVolume** fLogicDownstreamFlange;
  G4VPhysicalVolume** fPhysicalDownstreamFlange;


};

#endif
