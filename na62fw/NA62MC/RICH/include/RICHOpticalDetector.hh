#ifndef RICHOpticalDetector_H
#define RICHOpticalDetector_H 1

#include "globals.hh"
#include "TVector2.h"

#include "G4OpticalSurface.hh"
#include "NA62VComponent.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;

class RICHOpticalDetector : public NA62VComponent
{

public:
  
  RICHOpticalDetector(G4LogicalVolume*, G4Material*);
  ~RICHOpticalDetector();
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurfaces();
  void SetProperties();

public:

  G4double  GetConeInputDiameter(){return fConeInputDiameter;}
  void SetConeInputDiameter(G4double  value){fConeInputDiameter=value;}
  G4double  GetConeInputRadius(){return fConeInputRadius;}
  void SetConeInputRadius(G4double  value){fConeInputRadius=value;}
  G4double  GetConeOutputDiameter(){return fConeOutputDiameter;}
  void SetConeOutputDiameter(G4double  value){fConeOutputDiameter=value;}
  G4double  GetConeOutputRadius(){return fConeOutputRadius;}
  void SetConeOutputRadius(G4double  value){fConeOutputRadius=value;}
  G4double  GetConeLongitudinalLength(){return fConeLongitudinalLength;}
  void SetConeLongitudinalLength(G4double  value){fConeLongitudinalLength=value;}

  G4double  GetMylarConeThickness(){return fMylarConeThickness;}
  void SetMylarConeThickness(G4double  value){fMylarConeThickness=value;}
  G4double  GetMylarConeInputRadius(){return fMylarConeInputRadius;}
  void SetMylarConeInputRadius(G4double  value){fMylarConeInputRadius=value;}
  G4double  GetMylarConeOutputRadius(){return fMylarConeOutputRadius;}
  void SetMylarConeOutputRadius(G4double  value){fMylarConeOutputRadius=value;}
  G4double  GetMylarConeLongitudinalLength(){return fMylarConeLongitudinalLength;}
  void SetMylarConeLongitudinalLength(G4double  value){fMylarConeLongitudinalLength=value;}

  G4double  GetQuartzWindowInnerRadius(){return fQuartzWindowInnerRadius;}
  void SetQuartzWindowInnerRadius(G4double  value){fQuartzWindowInnerRadius=value;}
  G4double  GetQuartzWindowOuterRadius(){return fQuartzWindowOuterRadius;}
  void SetQuartzWindowOuterRadius(G4double  value){fQuartzWindowOuterRadius=value;}
  G4double  GetQuartzWindowThickness(){return fQuartzWindowThickness;}
  void SetQuartzWindowThickness(G4double  value){fQuartzWindowThickness=value;}

  G4double  GetPMWindowInnerRadius(){return fPMWindowInnerRadius;}
  void SetPMWindowInnerRadius(G4double  value){fPMWindowInnerRadius=value;}
  G4double  GetPMWindowOuterRadius(){return fPMWindowOuterRadius;}
  void SetPMWindowOuterRadius(G4double  value){fPMWindowOuterRadius=value;}
  G4double  GetPMWindowThickness(){return fPMWindowThickness;}
  void SetPMWindowThickness(G4double  value){fPMWindowThickness=value;}

  G4double  GetPMOuterRadius(){return fPMOuterRadius;}
  void SetPMOuterRadius(G4double  value){fPMOuterRadius=value;}

  G4double  GetPMLength(){return fPMLength;}
  void SetPMLength(G4double  value){fPMLength=value;}
  
  G4LogicalVolume * GetLogicHole(){return fLogicHole;}
  void SetLogicHole(G4LogicalVolume * value){fLogicHole=value;}
  
  G4VPhysicalVolume * GetPhysicalQuartzWindow(){return fPhysicalQuartzWindow;}
  void SetPhysicalQuartzWindow(G4VPhysicalVolume * value){fPhysicalQuartzWindow=value;}
  G4VPhysicalVolume * GetPhysicalPMWindow(){return fPhysicalPMWindow;}
  void SetPhysicalPMWindow(G4VPhysicalVolume * value){fPhysicalPMWindow=value;}
  G4VPhysicalVolume * GetPhysicalMylarCone(){return fPhysicalMylarCone;}
  void SetPhysicalMylarCone(G4VPhysicalVolume * value){fPhysicalMylarCone=value;}
  G4VPhysicalVolume * GetPhysicalConeRadiator(){return fPhysicalConeRadiator;}
  void SetPhysicalConeRadiator(G4VPhysicalVolume * value){fPhysicalConeRadiator=value;}
  G4VPhysicalVolume * GetPhysicalPhotocatode(){return fPhysicalPhotocatode;}
  void SetPhysicalPhotocatode(G4VPhysicalVolume * value){fPhysicalPhotocatode=value;}
  
  TVector2 * GetPMsPositions(){return fPMsPositions;}
  void SetPMsPositions(TVector2 * value){fPMsPositions=value;}

  G4OpticalSurface * GetPhotocatodeOpticalSurface(){return fPhotocatodeOpticalSurface;}
  void SetPhotocatodeOpticalSurface(G4OpticalSurface * value){fPhotocatodeOpticalSurface=value;}
  G4OpticalSurface * GetConeOpticalSurface(){return fConeOpticalSurface;}
  void SetConeOpticalSurface(G4OpticalSurface * value){fConeOpticalSurface=value;}

private:

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

  G4double fPMWindowInnerRadius;
  G4double fPMWindowOuterRadius;
  G4double fPMWindowThickness;

  G4double fPMOuterRadius;
  G4double fPMLength;

  G4LogicalVolume* fLogicHole;

  G4VPhysicalVolume* fPhysicalHole;
  G4VPhysicalVolume* fPhysicalQuartzWindow;
  G4VPhysicalVolume* fPhysicalPMWindow;
  G4VPhysicalVolume* fPhysicalMylarCone;
  G4VPhysicalVolume* fPhysicalConeRadiator;
  G4VPhysicalVolume* fPhysicalPhotocatode;
#ifdef RICHNEWOPT
  G4VPhysicalVolume* fPhysicalPhotocatode0;
#endif

  TVector2* fPMsPositions;

  G4OpticalSurface* fPhotocatodeOpticalSurface;
  G4OpticalSurface* fConeOpticalSurface;

};

#endif
