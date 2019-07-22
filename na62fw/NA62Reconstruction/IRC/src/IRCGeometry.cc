// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "IRCGeometry.hh"

IRCGeometry* IRCGeometry::fInstance = 0;

IRCGeometry::IRCGeometry(){
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  CreateGeometry();
}

IRCGeometry * IRCGeometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new IRCGeometry(); }
  return fInstance;
}

void IRCGeometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class
}
