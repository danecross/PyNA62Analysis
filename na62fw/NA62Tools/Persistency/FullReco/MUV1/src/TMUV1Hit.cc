// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-11
//            Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#include "TMUV1Hit.hh"

ClassImp(TMUV1Hit)

TMUV1Hit::TMUV1Hit() : TDetectorVHit() {
}

void TMUV1Hit::Clear(Option_t * option){
  TDetectorVHit::Clear(option);
}

void TMUV1Hit :: SetChannelID (Int_t ChID){

	TDetectorVHit :: SetChannelID (ChID);

	if (ChID>50) fPlane = kHorizontalPlane;
	else fPlane = kVerticalPlane;

}
