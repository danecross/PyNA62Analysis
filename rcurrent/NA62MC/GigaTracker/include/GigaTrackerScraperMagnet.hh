// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (Evgueni.Goudzovski@cern.ch) 2016-02-16
// ---------------------------------------------------------------------

#ifndef GigaTrackerScraperMagnet_H
#define GigaTrackerScraperMagnet_H 1

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "globals.hh"
#include "GigaTrackerScraperField.hh"
#include "G4FieldManager.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class GigaTrackerScraperMagnet : public NA62VComponent {

public:
  
  GigaTrackerScraperMagnet(G4Material*, G4LogicalVolume*, G4ThreeVector);
  ~GigaTrackerScraperMagnet() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties() {}

public:

  G4ThreeVector GetPosition()                      { return fPosition;           }
  void          SetPosition(G4ThreeVector value)   { fPosition = value;          }
  G4double      GetApertureHalfWidth()             { return fApertureHalfWidth;  }
  G4double      GetApertureHalfHeight()            { return fApertureHalfHeight; }
  G4double      GetOverallHalfHeight()             { return fOverallHalfHeight;  }

  //G4FieldManager*       GetFieldMgr()                             { return fFieldMgr;  }
  //void                  SetFieldMgr(G4FieldManager * value)       { fFieldMgr = value; }
  //G4QuadrupoleMagField* GetMagField()                             { return fMagField;  }
  //void                  SetMagField(G4QuadrupoleMagField * value) { fMagField = value; }

private:

  G4ThreeVector fPosition;   ///< Magnet position in the Gigatracker reference frame
  G4double      fZReference; ///< Origin of the Gigatracker reference frame
  G4double      fZLength;
  G4double      fApertureHalfWidth;
  G4double      fApertureHalfHeight;
  G4double      fOverallHalfHeight;
  G4double      fFieldStrength; ///< Magnetic field strength

  GigaTrackerScraperField* fMagField;
  G4FieldManager*          fFieldMgr;
};

#endif
