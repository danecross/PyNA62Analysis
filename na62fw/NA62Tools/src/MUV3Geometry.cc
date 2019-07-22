// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
// Major updates: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
//
// --------------------------------------------------------------

#include "MUV3Geometry.hh"
#include "TMath.h"

/// \class MUV3Geometry
/// \Brief
/// MUV3 geometry definition
/// \EndBrief
/// \Detailed
/// Used by MUV3Reconstruction and for Slim-to-Standard MUV3 candidate conversion.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

MUV3Geometry* MUV3Geometry::fInstance = nullptr;

MUV3Geometry::MUV3Geometry() {
  CreateGeometry();
}

MUV3Geometry* MUV3Geometry::GetInstance() {
  if (!fInstance) fInstance = new MUV3Geometry();
  return fInstance;
}

void MUV3Geometry::CreateGeometry() {

  // Centre positions: outer tiles
  fStandardSize = 220.0;
  fGapWidth     = 0.8;
  for (Int_t icell=0; icell<144; icell++) {
    Int_t ix = icell%12;
    Int_t iy = icell/12;
    fTileCentreX[icell] = fStandardSize * (ix-5.5);
    fTileCentreY[icell] = fStandardSize * (iy-5.5);
    fTileCentreX[icell] += (ix>=6) ? 0.5*fGapWidth : -0.5*fGapWidth;
    fPMCentreX[icell]   = fTileCentreX[icell];
    fPMCentreY[icell]   = fTileCentreY[icell];
  }

  // Centre positions: inner tiles
  fTileCentreX[144] = -2.0/3.0*fStandardSize - 0.5*fGapWidth;
  fTileCentreY[144] = -2.0/3.0*fStandardSize;
  fTileCentreX[145] = 0.0;
  fTileCentreY[145] = +fTileCentreY[144];
  fTileCentreX[146] = -fTileCentreX[144];
  fTileCentreY[146] = +fTileCentreY[144];
  fTileCentreX[147] = +fTileCentreX[144];
  fTileCentreY[147] = 0.0;
  fTileCentreX[148] = +fTileCentreX[146];
  fTileCentreY[148] = 0.0;
  fTileCentreX[149] = +fTileCentreX[144];
  fTileCentreY[149] = -fTileCentreY[144];
  fTileCentreX[150] = 0.0;
  fTileCentreY[150] = +fTileCentreY[149];
  fTileCentreX[151] = +fTileCentreX[146];
  fTileCentreY[151] = +fTileCentreY[149];

  ///////////////////////////////////////////////////////////////////////////////
  // Define the offsets of the two PMs: PM 0 is the one on top in the large cells
  // and the one on top or left in the central cells.
  // These offsets are used in the detailes MUV3 digitizer.
  // The detailed digitizer is still to be copleted and tested.

  // Outer cells
  fPMOffsetXG[0] = +26.5165;
  fPMOffsetYG[0] = +26.5165;
  fPMOffsetXG[1] = -26.5165;
  fPMOffsetYG[1] = -26.5165;

  // Inner cells
  fPMOffsetXP[0][0] =  26.51; //cells 144 & 151
  fPMOffsetXP[1][0] = -26.51;
  fPMOffsetXP[0][1] = -26.51; //cells 146 & 149
  fPMOffsetXP[1][1] =  26.51;
  fPMOffsetXP[0][2] =  37.33; //cells 145 & 150
  fPMOffsetXP[1][2] =  37.33;
  fPMOffsetXP[0][3] =   0.0 ; //cells 147 & 148
  fPMOffsetXP[1][3] =   0.0 ;
  fPMOffsetYP[0][0] =  26.51; //cells 144 & 151
  fPMOffsetYP[1][0] = -26.51;
  fPMOffsetYP[0][1] =  26.51; //cells 146 & 149
  fPMOffsetYP[1][1] = -26.51;
  fPMOffsetYP[0][2] =   0.0 ; //cells 145 & 150
  fPMOffsetYP[1][2] =   0.0 ;
  fPMOffsetYP[0][3] =  37.33; //cells 147 & 148
  fPMOffsetYP[1][3] = -37.33;
}

Int_t MUV3Geometry::GetTileID(TVector2 Pos) {
  Double_t MUV3HalfSize = 6.*fStandardSize;
  Double_t x = Pos.X() + MUV3HalfSize;
  Double_t y = Pos.Y() + MUV3HalfSize;
  x += (Pos.X()<0.) ? 0.5*fGapWidth : -0.5*fGapWidth;

  Double_t InnerTileSize = fStandardSize*2./3.;

  // outside MUV3 acceptance, return a dummy value
  if (!(x>0. && y>0. && x<2.*MUV3HalfSize && y<2.*MUV3HalfSize)) return 200;
  if (TMath::Sqrt((x-MUV3HalfSize)*(x-MUV3HalfSize) +
		  (y-MUV3HalfSize)*(y-MUV3HalfSize))<103.) return 200;

  // inside MUV3 acceptance
  if ((TMath::Abs(x-MUV3HalfSize) > fStandardSize) || (TMath::Abs(y-MUV3HalfSize) > fStandardSize)) {
    //outer cells
    return 12*(int)(y/fStandardSize) + (int)(x/fStandardSize);
  }
  else {
    //inner cells
    if (y<=MUV3HalfSize - InnerTileSize/2.) {
      return 144 + (int)(TMath::Floor((x-(MUV3HalfSize-fStandardSize))/InnerTileSize)); //3 lower cells
    }
    else {
      if (y>=MUV3HalfSize + InnerTileSize/2.) {
        return 149 + (int)(TMath::Floor((x-(MUV3HalfSize-fStandardSize))/InnerTileSize)); //3 upper cells
      }
      else {
        if (x<MUV3HalfSize - InnerTileSize/2.) return 147; 
        if (x>MUV3HalfSize + InnerTileSize/2.) return 148;
      }
    }
  }
  return 200; // dummy value
}

Double_t MUV3Geometry::GetPMOffsetX(Int_t iCell, Int_t iPM) {
  Double_t offset = fPMOffsetXG[iPM];
  if (iCell==144 || iCell==151) offset = fPMOffsetXP[iPM][0];
  if (iCell==146 || iCell==149) offset = fPMOffsetXP[iPM][1];
  if (iCell==145 || iCell==150) offset = fPMOffsetXP[iPM][2];
  if (iCell==147 || iCell==148) offset = fPMOffsetXP[iPM][3];
  return offset;
}

Double_t MUV3Geometry::GetPMOffsetY(Int_t iCell, Int_t iPM) {
  Double_t offset = fPMOffsetYG[iPM];
  if (iCell==144 || iCell==151) offset = fPMOffsetYP[iPM][0];
  if (iCell==146 || iCell==149) offset = fPMOffsetYP[iPM][1];
  if (iCell==145 || iCell==150) offset = fPMOffsetYP[iPM][2];
  if (iCell==147 || iCell==148) offset = fPMOffsetYP[iPM][3];
  return offset;
}

Bool_t MUV3Geometry::FindNeighbours(Int_t iTile1, Int_t iTile2) {
  if (iTile1<0 || iTile1>143) return false; // outer tiles only
  if (iTile2<0 || iTile2>143) return false; // outer tiles only
  Int_t row_check = (iTile1 + iTile2) % 24;
  Int_t col_check = iTile2 - iTile1;
  if (abs(col_check) == 1) {
    if (row_check != 23) return true; // horizontal neighbours
    else return false;
  }
  if (abs(col_check)==12) return true; // vertical neighbours
  return false;
}

Bool_t MUV3Geometry::IsRight(Int_t iTileID) {
  return ((iTileID % 12)>5);
}

Bool_t MUV3Geometry::LeftRight(Int_t iTile1, Int_t iTile2) {
  return ((IsRight(iTile1) && !IsRight(iTile2)) ||
	  (IsRight(iTile2) && !IsRight(iTile1)));
}

Bool_t MUV3Geometry::LeftRight(std::vector<Int_t> &vTileIDs) {
  Bool_t left_right_flag;
  Int_t left_counter = 0, right_counter = 0;
  for (UInt_t i=0; i<vTileIDs.size(); i++) {
    left_right_flag = IsRight(vTileIDs[i]);
    if (left_right_flag) right_counter++;
    else if (!left_right_flag) left_counter++;
  }
  if (left_counter && right_counter) return true;
  return false;
}
