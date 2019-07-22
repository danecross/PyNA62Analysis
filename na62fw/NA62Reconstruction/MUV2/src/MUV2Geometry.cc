// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "MUV2Geometry.hh"
#include "TMath.h"

MUV2Geometry* MUV2Geometry::fInstance = 0;

MUV2Geometry::MUV2Geometry(){
    // Insert here all the parameters you need to define the geometry
    // waiting for reading everything from DataBase
    CreateGeometry();
    //Particle time of flight
    fTimeOfFlight = 814.8;
    
    fScintillatorPosition[1] = -1238.5;
	for (Int_t i = 2; i < 23; i++) {
		if (i < 11 || i > 13) {
			fScintillatorPosition[i] = fScintillatorPosition[i - 1] + 119;
		}
		if(i == 11 || i==13){
			fScintillatorPosition[i] = fScintillatorPosition[i - 1] + 113.5;
		}
		if (i == 12) {
			fScintillatorPosition[i] = fScintillatorPosition[i - 1] + 108;
		}
	}
}

MUV2Geometry * MUV2Geometry::GetInstance(){
    if ( fInstance == 0 ) { fInstance = new MUV2Geometry(); }
    return fInstance;
}

void MUV2Geometry::CreateGeometry(){
    // Reproduce the geometry as it is defined in the MC
    // to be able to access it by the reconstruction class
    
	fScintLengthStandard = 1300; //mm
    
}

Int_t MUV2Geometry :: GetScintillatorAt (Double_t X){
    
    Double_t Xabs = TMath::Abs(X);
    if (Xabs>fScintillatorPosition[22]+59.5) return 0;
    if (X==0) return 11;
    if (TMath::Abs(Xabs - fScintillatorPosition[12])<=54) return (X>0 ? 12 : 11);
    
    for (Int_t i=13; i<23; i++){
        if (TMath::Abs(Xabs - fScintillatorPosition[i])<=59.5) return (X>0 ? i : 23-i);
    }
    
    return -1;
}
