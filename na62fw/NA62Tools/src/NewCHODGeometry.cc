// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#include "NewCHODGeometry.hh"

/// \class NewCHODGeometry
/// \Brief
/// NewCHOD geometry definition
/// \EndBrief
/// \Detailed
/// Used by NewCHODReconstruction and for Slim-to-Standard NewCHOD hit conversion.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

NewCHODGeometry* NewCHODGeometry::fInstance = nullptr;

NewCHODGeometry::NewCHODGeometry() {
  CreateGeometry();
}

NewCHODGeometry* NewCHODGeometry::GetInstance() {
  if (!fInstance) fInstance = new NewCHODGeometry();
  return fInstance;
}

void NewCHODGeometry::CreateGeometry() {

  fBrickSizeX   = 134;
  fBrickSizeY   = 107;
  fInnerRadius =  140;
  fOuterRadius = 1070;

  ////////////////////////////////////
  // Brick to tile mapping: tiles 1-38

  Double_t Map[100] =
    {01,  1,  2,  3,  4,  4,  5,  5,  0,  0,
     06,  7,  8,  9, 10, 10, 11, 11,  0,  0,
     12, 13, 14, 14, 15, 15, 16, 16,  0,  0,
     17, 18, 19, 19, 20, 20, 21, 21,  0,  0,
     22, 22, 23, 23, 24, 24, 25, 25,  0,  0,
     26, 26, 27, 27, 28, 28, 29,  0,  0,  0,
     30, 30, 31, 31, 32, 32, 29,  0,  0,  0,
     33, 33, 34, 34, 35, 35,  0,  0,  0,  0,
     36, 36, 37, 37, 35,  0,  0,  0,  0,  0,
     38, 38, 37, 37,  0,  0,  0,  0,  0,  0};

  /////////////////////////////////////////////////////////////
  // Centres of the tiles = (x,y) position of the candidates.
  // They are defined arbitrarily for tiles of irregular shape.
  // Tiles are numbered 0-37, not 1-38!

  Double_t TileX[38] =
    {1.5, 2.5, 3.5, 5.0, 7.0,      // row 1
     0.5, 1.5, 2.5, 3.5, 5.0, 7.0, // row 2
     0.5, 1.5, 3.0, 5.0, 6.9,      // row 3
     0.5, 1.5, 3.0, 5.0, 6.7,      // row 4
     1.0, 3.0, 5.0, 6.5,           // row 5
     1.0, 3.0, 5.0, 6.3,           // row 6
     1.0, 3.0, 5.0,                // row 7
     1.0, 3.0, 4.5,                // row 8
     1.0, 3.0,                     // row 9
     1.0};                         // row 10

  Double_t TileY[38] =
    {0.5, 0.5, 0.5, 0.5, 0.5,      // row 1
     1.5, 1.5, 1.5, 1.5, 1.5, 1.5, // row 2
     2.5, 2.5, 2.5, 2.5, 2.5,      // row 3
     3.5, 3.5, 3.5, 3.5, 3.5,      // row 4
     4.5, 4.5, 4.5, 4.5,           // row 5
     5.5, 5.5, 5.5, 5.5,           // row 6
     6.5, 6.5, 6.5,                // row 7
     7.5, 7.5, 7.6,                // row 8
     8.5, 8.7,                     // row 9
     9.5};                         // row 10

  for (Int_t i=0; i<100; i++) fScintMap[i] = Map[i];
  for (Int_t i=0; i<38; i++) {
    fTileX[i] = TileX[i] * fBrickSizeX;
    fTileY[i] = TileY[i] * fBrickSizeY;
    fTileXmin[i] = fTileYmin[i] = +99999;
    fTileXmax[i] = fTileYmax[i] = -99999;
    for (Int_t j=0; j<100; j++) { // loop over bricks
      if (fScintMap[j]==i+1) { // the brick belongs to the tile
	Double_t BrickXmin = (j%10) * fBrickSizeX;
	Double_t BrickXmax = BrickXmin + fBrickSizeX;
	Double_t BrickYmin = (j/10) * fBrickSizeY;
	Double_t BrickYmax = BrickYmin + fBrickSizeY;
	if (BrickXmin < fTileXmin[i]) fTileXmin[i] = BrickXmin;
	if (BrickXmax > fTileXmax[i]) fTileXmax[i] = BrickXmax;
	if (BrickYmin < fTileYmin[i]) fTileYmin[i] = BrickYmin;
	if (BrickYmax > fTileYmax[i]) fTileYmax[i] = BrickYmax;
      }
    }
    fTileYmin[i] -= 0.5; fTileYmax[i] += 0.5; // vertical tile overlap
  }
}

Double_t NewCHODGeometry::GetBrickCentreX(Int_t BrickID) {
  Int_t ReducedBrickID = BrickID%100;
  if (ReducedBrickID>99) return 999;
  Double_t x = fBrickSizeX*(ReducedBrickID%10+0.5);
  if (BrickID/100==3 || BrickID/100==4) x = -x;
  return x;
}

Double_t NewCHODGeometry::GetBrickCentreY(Int_t BrickID) {
  Int_t ReducedBrickID = BrickID%100;
  if (ReducedBrickID>99) return 999;
  Double_t y = fBrickSizeY*(ReducedBrickID/10+0.5);
  if (BrickID/100==2 || BrickID/100==3) y = -y;
  return y;
}

Double_t NewCHODGeometry::GetTileXmin(Int_t Tile) {
  if (Tile%100>38) return 9999;
  Double_t x1 = fTileXmin[Tile%100-1];
  Double_t x2 = fTileXmax[Tile%100-1];
  Double_t x = x1;
  if (Tile/100==3 || Tile/100==4) x = -x2;
  return x;
}

Double_t NewCHODGeometry::GetTileXmax(Int_t Tile) {
  if (Tile%100>38) return 9999;
  Double_t x1 = fTileXmin[Tile%100-1];
  Double_t x2 = fTileXmax[Tile%100-1];
  Double_t x = x2;
  if (Tile/100==3 || Tile/100==4) x = -x1;
  return x;
}

Double_t NewCHODGeometry::GetTileYmin(Int_t Tile) {
  if (Tile%100>38) return 9999;
  Double_t y1 = fTileYmin[Tile%100-1];
  Double_t y2 = fTileYmax[Tile%100-1];
  Double_t y = y1;
  if (Tile/100==2 || Tile/100==3) y = -y2;
  return y;
}

Double_t NewCHODGeometry::GetTileYmax(Int_t Tile) {
  if (Tile%100>38) return 9999;
  Double_t y1 = fTileYmin[Tile%100-1];
  Double_t y2 = fTileYmax[Tile%100-1];
  Double_t y = y2;
  if (Tile/100==2 || Tile/100==3) y = -y1;
  return y;
}

Double_t NewCHODGeometry::GetTileCentreX(Int_t Tile) {
  if (Tile%100>38) return 9999;
  Double_t x = fTileX[Tile%100-1];
  if (Tile/100==3 || Tile/100==4) x = -x;
  return x;
}

Double_t NewCHODGeometry::GetTileCentreY(Int_t Tile) {
  if (Tile%100>38) return 9999;
  Double_t y = fTileY[Tile%100-1];
  if (Tile/100==2 || Tile/100==3) y = -y;
  return y;
}

Int_t NewCHODGeometry::GetTileID(TVector2 Pos) {
  if (Pos.Mod()>fOuterRadius || Pos.Mod()<fInnerRadius) return 999;
  Double_t x = Pos.X();
  Double_t y = Pos.Y(); 
  Int_t ScintMapIndex = (Int_t)(fabs(x)/fBrickSizeX) + 10*(Int_t)(fabs(y)/fBrickSizeY);
  Int_t RawTileID = fScintMap[ScintMapIndex];
  if (x>=0 && y>=0) return RawTileID + 100;
  if (x>=0 && y<0)  return RawTileID + 200;
  if (x<0  && y<0)  return RawTileID + 300;
  if (x<0  && y>=0) return RawTileID + 400;
  return 999;
}
