// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------
#include "TLKrMicroCellHit.hh"
ClassImp(TLKrMicroCellHit)

TLKrMicroCellHit::TLKrMicroCellHit() {
    fXIndex = -1;
    fYIndex = -1;
    fZIndex = -1;
    fEnergyFraction = 0;
}

TLKrMicroCellHit::~TLKrMicroCellHit(){
}

void TLKrMicroCellHit::Clear(Option_t* /*option*/){
    fXIndex = -1;
    fYIndex = -1;
    fZIndex = -1;
    fEnergyFraction = 0;
}
