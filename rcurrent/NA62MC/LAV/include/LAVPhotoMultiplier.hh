// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2011-01-24
//
// --------------------------------------------------------------------

#ifndef LAVPhotoMultiplier_HH
#define  LAVPhotoMultiplier_HH

#include "G4LogicalVolume.hh"
#include "G4VPhysicalVolume.hh"
#include "G4VSolid.hh"

#define __MAX_EVENT_PHS__ 10000

class LAVPhotoMultiplier{
public:
  explicit LAVPhotoMultiplier(G4int Id);

  void UpdateGeometry();

  void SetGuide(G4double a, G4double b) {LightGuideDiameter = a;LightGuideZLength = b;}
  void SetCathode(G4double a){PmtDiameter = a;}
  void SetCover(G4double a, G4double b, G4double c){CoverDiameter = a;CoverInner=b;CoverLength=c;}
  void SetThickness(G4double a){Thick = a;}

  G4double GetDiameter()   { return CoverDiameter; }
  G4double GetLength()     { return CoverLength/*LightGuideZLength+3*Thick+(CoverDiameter-CoverInner)/2*/; }

  G4LogicalVolume *GetLogicalVolume(){return fLogicalVolume;}
  G4VPhysicalVolume *GetGuidePhysic(){return fGuidePhysic;}

private:
  G4VPhysicalVolume * fGuidePhysic;
  G4LogicalVolume * fLogicalVolume;
  G4int fId;
  G4double LightGuideZLength, LightGuideDiameter, CoverDiameter, CoverInner, CoverLength, PmtDiameter, Thick;

};

#endif
