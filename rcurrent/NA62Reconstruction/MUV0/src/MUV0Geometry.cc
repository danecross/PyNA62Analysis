// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
// Adapted by Adam Conovaloff from NewCHODGeometry.cc
// --------------------------------------------------------------
#include "MUV0Geometry.hh"

MUV0Geometry* MUV0Geometry::fInstance = 0;

MUV0Geometry::MUV0Geometry(){
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  CreateGeometry();
}

MUV0Geometry * MUV0Geometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new MUV0Geometry(); }
  return fInstance;
}

void MUV0Geometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class

  XCenter     = 2245; // The RR should not touch RICHMirrorWindowOuterFlange (R<1510mm)
  XLength     = 1450;
  YLength     = 1450;

  ScintillatorThickness =     20;
  DetectorXPosition     =   2245;
  DetectorYPosition     =      0;
  DetectorZPosition     = 237358 + 0.5 * ScintillatorThickness;

  FrameInnerSize        =   1400;
  FrameThickness        =     20;
  CoverThickness        =      2;

  Double_t X0[9] = {-400, -400, -400, 100, 100, 100, 500, 500, 500};
  Double_t Y0[9] = {-500, -100, 400, -500, -100, 400, -500, -100, 400};
  Double_t Xsize[9] = {600, 600, 600, 400, 400, 400, 400, 400, 400};
  Double_t Ysize[9] = {400, 400, 600, 400, 400, 600, 400, 400, 600};

  for (int i=0; i<9; i++){
     TileCenterX[i] = X0[i] + DetectorXPosition;
     TileCenterY[i] = Y0[i] + DetectorYPosition;
  }

  for (int i=0; i<9; i++){
    TileXmax[i] = TileCenterX[i] + 0.5 * Xsize[i];
    TileXmin[i] = TileCenterX[i] - 0.5 * Xsize[i];
    TileYmax[i] = TileCenterY[i] + 0.5 * Ysize[i];
    TileYmin[i] = TileCenterY[i] - 0.5 * Ysize[i];
  }

}

Int_t MUV0Geometry::GetTileID (TVector2 Pos) {
  Double_t x = Pos.X();
  Double_t y = Pos.Y();

  for (int i=0; i<9; i++){
    if (x<=TileXmax[i] && x>TileXmin[i] && y<=TileYmax[i] && y>TileYmin[i])
      return i;
  }
  return 999;
}

Double_t MUV0Geometry::GetTileXmin (Int_t Tile) {
  if (Tile>8) return 9999;
  return TileXmin[Tile];
}

Double_t MUV0Geometry::GetTileXmax (Int_t Tile) {
  if (Tile>8) return 9999;
  return TileXmax[Tile];
}

Double_t MUV0Geometry::GetTileYmin (Int_t Tile) {
  if (Tile>8) return 9999;
  return TileYmin[Tile];
}

Double_t MUV0Geometry::GetTileYmax (Int_t Tile) {
  if (Tile>8) return 9999;
  return TileYmax[Tile];
}

Double_t MUV0Geometry::GetTileCenterX (Int_t Tile) {
  if (Tile>8)return 9999;
  return TileCenterX[Tile];
}

Double_t MUV0Geometry::GetTileCenterY (Int_t Tile) {
  if (Tile>8)return 9999;
  return TileCenterY[Tile];
}
