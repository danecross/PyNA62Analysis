#ifndef RICHMirrorSupports_H
#define RICHMirrorSupports_H 1

#include "NA62VComponent.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class RICHMirrorSupports : public NA62VComponent
{

public:
  
  RICHMirrorSupports(G4Material*, G4LogicalVolume*);
  ~RICHMirrorSupports();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  void DefineOpticalSurface();


public:

  G4double  GetZLength()               {return fZLength;}
  void      SetZLength(G4double  value){fZLength=value;}
  G4double  GetInnerRadius()           {return fInnerRadius;}
  void      SetInnerRadius(G4double  value) {fInnerRadius=value;}
  G4double  GetOuterRadius()           {return fOuterRadius;}
  void      SetOuterRadius(G4double  value){fOuterRadius=value;}
  G4double  GetZPosition(){return fZPosition;}
  void      SetZPosition(G4double  value){fZPosition=value;}
  G4double  GetExternalRadius()                                { return fExternalRadius;               };
  void      SetExternalRadius(G4double value)                  { fExternalRadius = value;              };


  G4VPhysicalVolume* GetPhysicalSupport_Jura()                             { return fPhysicalSupport_Jura;  };	
  void               SetPhysicalSupport_Jura(G4VPhysicalVolume * value)    { fPhysicalSupport_Jura=value;   };

  G4VPhysicalVolume* GetPhysicalSupport_Saleve()                           { return fPhysicalSupport_Saleve;  };   
  void               SetPhysicalSupport_Saleve(G4VPhysicalVolume * value)  { fPhysicalSupport_Saleve=value;   };

  G4OpticalSurface *   GetOpticalSurface()                                { return fOpticalSurface;               };
  void                 SetOpticalSurface(G4OpticalSurface * value)        { fOpticalSurface = value;              };

  
private:

  G4double   fZLength; //length of the mirror Supports volume along z
  G4double   fInnerRadius; //mirror Supports inner radius 
  G4double   fOuterRadius; //mirror Supports outer radius
  G4double   fZPosition; //mirror Supports Z distance from center of vessel

  G4double   fRotation_Jura;
  G4double   fRotation_Saleve;

  G4double fToothXLenght;
  G4double fToothYLenght;
  G4double fToothZLenght;
  G4double fToothZPosition;
  G4double fToothZPositionRadRef;

  G4double   fRingZLength; 
  G4double   fRingInnerRadius;                  
  G4double   fRingOuterRadius; 
  G4double   fRingZPosition; 

  G4double   fConeBottomOuterRadius;
  G4double   fConeUpOuterRadius;
  G4double   fConeThickness;
  G4double*  fConeHeight;
//  G4double   fConeHeight;
  G4double*  fConeZPosition;
//  G4double   fConeZPosition;

  G4double   fConeBaseOuterRadius;
  G4double   fConeBaseInnerRadius;
  G4double   fConeBaseThickness;
  G4double   fConeBaseZPosition;

  G4double   fConeHatOuterRadius;
  G4double   fConeHatInnerRadius;
  G4double   fConeHatThickness;
  G4double*   fConeHatZPosition;
//  G4double   fConeHatZPosition;

  G4double   fConeScrewRadius;
  G4double   fConeScrewHeight;
  G4double   fConeScrewZPosition;

  G4double fTurnerBaseXLenght;
  G4double fTurnerBaseYLenght;
  G4double fTurnerBaseZLenght;
  G4double fTurnerBaseZPosition;

  G4double fTurnerXMaxLenght;
  G4double fTurnerXMinLenght;
  G4double fTurnerYMaxLenght;
  G4double fTurnerYMinLenght;
  G4double fTurnerZLenght;
  G4double fTurnerZPosition;

  G4double fTurnerHoleXMaxLenght;
  G4double fTurnerHoleXMinLenght;
  G4double fTurnerHoleYMaxLenght;
  G4double fTurnerHoleYMinLenght;
  G4double fTurnerHoleZLenght;
  G4double fTurnerHoleZPosition;

  G4double fPrismHatXLenght;
  G4double fPrismHatYLenght;
  G4double fPrismHatZLenght;
  G4double fPrismHatZPosition;
  G4double fPrismHatZPositionRadRef;

  G4double fPrismBaseXLenght;
  G4double fPrismBaseYLenght;
  G4double fPrismBaseZLenght;
  G4double fPrismBaseZPosition;
  G4double fPrismBaseZPositionRadRef;

  G4double fPrismX1Lenght;
  G4double fPrismX2Lenght;
  G4double fPrismY1Lenght;
  G4double fPrismY2Lenght;
  G4double fPrismZLenght;
  G4double fPrismZPosition;
  G4double fPrismZPositionRadRef;

  G4double fPrismHoleX1Lenght;
  G4double fPrismHoleX2Lenght;
  G4double fPrismHoleY1Lenght;
  G4double fPrismHoleY2Lenght;
  G4double fPrismHoleZLenght;
  G4double fPrismHoleZPosition;
  G4double fPrismHoleZPositionRadRef;

  G4double fPrismXPosition_Jura;
  G4double fPrismXPosition_Saleve; 

  G4double   fPrismScrewRadius;
  G4double   fPrismScrewHeight;
  G4double   fPrismScrewZPosition;

  G4double   fDowelRadius;
  G4double   fDowelHeight;

  G4double   fExternalRadius;
  G4double*  fMirrorGap;

  G4VSolid*  fSolidSupport_Jura;    
  G4LogicalVolume* fLogicalSupport_Jura;
  G4VPhysicalVolume* fPhysicalSupport_Jura;

  G4VSolid*  fSolidSupport_Saleve;    
  G4LogicalVolume* fLogicalSupport_Saleve;
  G4VPhysicalVolume* fPhysicalSupport_Saleve;   

  G4OpticalSurface* fOpticalSurface; 

};

#endif
