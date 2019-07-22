// ------------------------------------------------------------------
// History:
//
// Created by Simone Schuchmann 2019-07-03
//
// Class to use G4beamline magnet field maps in Geant4
// 
// ------------------------------------------------------------------



#include "G4BLMagnetFields.hh"
#include "G4SystemOfUnits.hh"
#include "G4ThreeVector.hh"
#include "G4BLMagneticFieldMap.hh"


G4BLMagnetFields::G4BLMagnetFields(G4double ZRef, G4bool Rotation, G4ThreeVector Position, double FieldScaleFactor,G4BLMagneticFieldMap *FieldMap,G4bool FlipBxBy): fZPos(ZRef),fRot(Rotation),fPosition(Position),fFieldScale(FieldScaleFactor),fMap(FieldMap),fFlipBxBy(FlipBxBy){
    
    fPosition +=G4ThreeVector(0.0,0.0,fZPos);
    if(!fMap) G4cout<<"G4BLMagnetFields NO FIELD MAP available!!!"<<G4endl;
}

G4BLMagnetFields::~G4BLMagnetFields() {
  delete fMap;
}

/////////////////////////////////////////////////////////////
// Evaluation of the field value: used by Geant4 for tracking

void G4BLMagnetFields::GetFieldValue(const G4double Point[4], G4double *B) const {
  
  G4double g4local[4];

  g4local[0] = 0.0;
  g4local[1] = 0.0;
  g4local[2] = 0.0;
  g4local[3] = 0.0;

  if(fFlipBxBy){
    //e.g. MCB iron is rotated 90 deg.
    g4local[0] = Point[1]-fPosition.getX();
    g4local[1] = Point[0]-fPosition.getY();
    g4local[2] = Point[2]-fPosition.getZ();
   }
  else{ 
    g4local[0] = Point[0]-fPosition.getX();
    g4local[1] = Point[1]-fPosition.getY();
    g4local[2] = Point[2]-fPosition.getZ();
  }
  
  
  if(fRot){
    //    G4cout<<"G4BLMagnetFields:: ROTATE!"<<G4endl;
    g4local[0] = - g4local[0];
    g4local[1] = - g4local[1];  
  }
 
  B[0] = B[1] = B[2] = 0.0;
  
  if(!fMap){
    G4cout<<"G4BLMagnetFields:: NO FIELD MAP"<<G4endl;
    return;  
  } 

  //  G4cout<<"G4BLMagnetFields::GetFieldValue Field Pos at "<<Point[0]<<" "<<Point[1]<<" "<<Point[2]<<G4endl;
  //  G4cout<<"G4BLMagnetFields::GetFieldValue Field local Pos at "<<g4local[0]<<" "<<g4local[1]<<" "<<g4local[2]<<G4endl;

  double local[3];
  for(int i = 0;i<3;i++) local[i] = double(g4local[i]/(1.0*mm));
  TVector3 Field = fMap->GetFieldValue(local);
  if(fFlipBxBy){
    //e.g. MCB iron is rotated 90 deg.
    B[1] = Field.X()*fFieldScale*tesla; //Y in global coord = X in MCB map.
    B[0] = Field.Y()*fFieldScale*tesla;
    B[2] = Field.Z()*fFieldScale*tesla;
  }
  else{
    B[0] = Field.X()*fFieldScale*tesla;
    B[1] = Field.Y()*fFieldScale*tesla;
    B[2] = Field.Z()*fFieldScale*tesla;
  }

  //  G4cout<<"G4BLMagnetFields::GetFieldValue B field "<<B[0]/(1*tesla)<<" "<<B[1]/(1*tesla)<<" "<<B[2]/(1*tesla)<<" "<<fFieldScale<<"\n"<<G4endl;
  return;
}
