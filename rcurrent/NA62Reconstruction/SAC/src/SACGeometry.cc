// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "SACGeometry.hh"

SACGeometry* SACGeometry::fInstance = 0;

SACGeometry::SACGeometry(){
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  CreateGeometry();
}

SACGeometry * SACGeometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new SACGeometry(); }
  return fInstance;
}

void SACGeometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class
}
