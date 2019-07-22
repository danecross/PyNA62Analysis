// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------

#include "LKrGeometry.hh"
#include "TMath.h"
#include <CLHEP/Units/PhysicalConstants.h>
using namespace CLHEP;
#include "LKrCommon.hh"

LKrGeometry* LKrGeometry::fInstance = 0;

LKrGeometry::LKrGeometry() {
  fCom = LKrCommon::GetInstance();
  CreateGeometry();
}

LKrGeometry * LKrGeometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new LKrGeometry(); }
  return fInstance;
}

void LKrGeometry::CreateGeometry() {
  fXfrontReferenceCell           = 9.868167*mm;
  fThermalContractionConstantG10 = 0.9983;
  fHalfCellSizeAtFrontWall       = (0.98859*cm)*fThermalContractionConstantG10;
  fLKrCellLength                 = 2*fHalfCellSizeAtFrontWall;

  fCom->GetCUN()->XCELL = 1.97383881;
  fCom->GetCUN()->YCELL = 1.97383881;
  fCom->GetCUN()->EDGE_X = fCom->GetCUN()->XCELL*126/2.;
  fCom->GetCUN()->EDGE_Y = fCom->GetCUN()->YCELL*126/2.;
  fCom->GetCUN()->EDGE_D =sqrt(fCom->GetCUN()->EDGE_X*fCom->GetCUN()->EDGE_X+fCom->GetCUN()->EDGE_Y*fCom->GetCUN()->EDGE_Y);
  for (Int_t i=0; i<128; i++) fCom->GetCUN()->XCELL_LKR[i] = fCom->GetCUN()->XCELL*((float)i-0.5*127);
//  for (Int_t i=0; i<128; i++) fCom->GetCUN()->XCELL_LKR[i] = fCom->GetCUN()->XCELL*((float)i-0.5*128);
  for (Int_t i=0; i<128; i++) fCom->GetCUN()->YCELL_LKR[i] = fCom->GetCUN()->YCELL*((float)i-0.5*127); 
}
