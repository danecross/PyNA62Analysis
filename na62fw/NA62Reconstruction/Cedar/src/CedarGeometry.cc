// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#include "CedarGeometry.hh"

CedarGeometry::CedarGeometry() {
  fNSectors = 8;
  CreateGeometry();
}

CedarGeometry::~CedarGeometry() {}

void CedarGeometry::CreateGeometry() {
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class
}
