#include <iostream>
using namespace std;

#include <Rtypes.h>
#include "LAVStaticGeometry.hh"


LAVStaticGeometry* LAVStaticGeometry::fInstance = 0;

LAVStaticGeometry::LAVStaticGeometry() {

  fNLayersPerStation[0] = 5;
  fNLayersPerStation[1] = 5;
  fNLayersPerStation[2] = 5;
  fNLayersPerStation[3] = 5;
  fNLayersPerStation[4] = 5;
  fNLayersPerStation[5] = 5;
  fNLayersPerStation[6] = 5;
  fNLayersPerStation[7] = 5;
  fNLayersPerStation[8] = 4;
  fNLayersPerStation[9] = 4;
  fNLayersPerStation[10] = 4;
  fNLayersPerStation[11] = 4;

  fNBananasPerLayer[0] = 8;
  fNBananasPerLayer[1] = 8;
  fNBananasPerLayer[2] = 8;
  fNBananasPerLayer[3] = 8;
  fNBananasPerLayer[4] = 8;
  fNBananasPerLayer[5] = 12;
  fNBananasPerLayer[6] = 12;
  fNBananasPerLayer[7] = 12;
  fNBananasPerLayer[8] = 15;
  fNBananasPerLayer[9] = 15;
  fNBananasPerLayer[10] = 15;
  fNBananasPerLayer[11] = 16;

  for (Int_t iStation = 0; iStation < 12; iStation++)
    fNCompactPerStation[iStation] = 4*fNLayersPerStation[iStation]*fNBananasPerLayer[iStation];

  fStation = -1;
  fLayer = -1;
  fBanana = -1;
  fBlock = -1;

}


LAVStaticGeometry* LAVStaticGeometry::GetInstance() {

  if (fInstance == 0) {fInstance = new LAVStaticGeometry();}
  return fInstance;

}


Int_t LAVStaticGeometry::LayersPerStation() {

  if (fStation <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  return fNLayersPerStation[fStation - 1];

}


Int_t LAVStaticGeometry::LayersPerStation(Int_t Station) {

  if (Station <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  // cppcheck-suppress negativeIndex
  return fNLayersPerStation[Station - 1];

}


Int_t LAVStaticGeometry::BananasPerLayer() {

  if (fStation <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  return fNBananasPerLayer[fStation - 1];

}


Int_t LAVStaticGeometry::BananasPerLayer(Int_t Station) {

  if (Station <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  // cppcheck-suppress negativeIndex
  return fNBananasPerLayer[Station - 1];

}


Int_t LAVStaticGeometry::CompactPerStation() {

  if (fStation <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  return fNCompactPerStation[fStation - 1];

}


Int_t LAVStaticGeometry::CompactPerStation(Int_t Station) {

  if (Station <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  // cppcheck-suppress negativeIndex
  return fNCompactPerStation[Station - 1];

}


void LAVStaticGeometry::SpecifyChannel(Int_t Station, Int_t Layer, Int_t Banana, Int_t Block) {

  fStation = Station;
  fLayer = Layer;
  fBanana = Banana;
  fBlock = Block;

}


void LAVStaticGeometry::SpecifyChannel(Int_t Station, Int_t Layer, Int_t Position) {

  fStation = Station;
  fLayer = Layer;
  fBanana = Position/4;
  fBlock = Position%4;

}


void LAVStaticGeometry::SpecifyChannel(Int_t Station, Int_t Compact) {

  fStation = Station;
  fLayer = Compact/fNBananasPerLayer[Station - 1];
  fBanana =  (Compact%fNBananasPerLayer[Station - 1])/4;
  fBlock = Compact%4;

}


void LAVStaticGeometry::SpecifyChannel(Int_t BlockID) {

  fStation = BlockID/10000;
  fLayer = (BlockID%10000)/1000;
  fBanana = (BlockID%1000)/10;
  fBlock = BlockID%10;

}


void LAVStaticGeometry::SpecifyColumn(Int_t Station, Int_t Position) {

  fStation = Station;
  fLayer = -1;
  fBanana = Position/4;
  fBlock = Position%4;

}

void LAVStaticGeometry::SpecifyColumn(Int_t Column) {

  fStation = Column/100;
  fLayer = -1;
  fBanana = (Column%100)/4;
  fBlock = Column%4;

}


Int_t LAVStaticGeometry::GetStation() {

  if (fStation <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  return fStation;

}


Int_t LAVStaticGeometry::GetLayer() {

  if (fLayer < 0) cerr << "[LAVStaticGeometry] Error: Unspecified layer" << endl;
  return fLayer;

}


Int_t LAVStaticGeometry::GetBanana() {

  if (fBanana < 0) cerr << "[LAVStaticGeometry] Error: Unspecified banana" << endl;
  return fBanana;

}


Int_t LAVStaticGeometry::GetBlock() {

  if (fBlock < 0) cerr << "[LAVStaticGeometry] Error: Unspecified block" << endl;
  return fBlock;

}


Int_t LAVStaticGeometry::GetPosition() {

  if (fBanana < 0) cerr << "[LAVStaticGeometry] Error: Unspecified banana" << endl;
  if (fBlock < 0) cerr << "[LAVStaticGeometry] Error: Unspecified block" << endl;
  return 4*fBanana + fBlock;

}


Int_t LAVStaticGeometry::GetColumn() {

  if (fStation <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  if (fBanana < 0) cerr << "[LAVStaticGeometry] Error: Unspecified banana" << endl;
  if (fBlock < 0) cerr << "[LAVStaticGeometry] Error: Unspecified block" << endl;
  return 100*fStation + 4*fBanana + fBlock;

}


Int_t LAVStaticGeometry::GetCompact() {

  if (fStation <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  if (fLayer < 0) cerr << "[LAVStaticGeometry] Error: Unspecified layer" << endl;
  if (fBanana < 0) cerr << "[LAVStaticGeometry] Error: Unspecified banana" << endl;
  if (fBlock < 0) cerr << "[LAVStaticGeometry] Error: Unspecified block" << endl;
  return 4*fNBananasPerLayer[fStation - 1]*fLayer + 4*fBanana + fBlock;

}


Int_t LAVStaticGeometry::GetBlockID() {

  if (fStation <= 0) cerr << "[LAVStaticGeometry] Error: Unspecified station" << endl;
  if (fLayer < 0) cerr << "[LAVStaticGeometry] Error: Unspecified layer" << endl;
  if (fBanana < 0) cerr << "[LAVStaticGeometry] Error: Unspecified banana" << endl;
  if (fBlock < 0) cerr << "[LAVStaticGeometry] Error: Unspecified block" << endl;
  return 10000*fStation + 1000*fLayer + 10*fBanana + fBlock;

}
