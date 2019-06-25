// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "HACGeometry.hh"

HACGeometry* HACGeometry::fInstance = 0;

HACGeometry::HACGeometry(){
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  CreateGeometry();
}

HACGeometry * HACGeometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new HACGeometry(); }
  return fInstance;
}

void HACGeometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class
}
