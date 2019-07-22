// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-11
//            Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#include "TMUV2Hit.hh"

ClassImp(TMUV2Hit)

TMUV2Hit::TMUV2Hit() : TDetectorVHit() {
}


void TMUV2Hit :: SetChannelID (Int_t ChID){

	TDetectorVHit :: SetChannelID (ChID);

	if (ChID>50) fPlane = kHorizontalPlane;
	else fPlane = kVerticalPlane;
	
}

void TMUV2Hit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
}
