// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "Riostream.h"

#include "MUV2HitsCluster.hh"
#include "TMath.h"

MUV2HitsCluster::MUV2HitsCluster() : fNHits(0){
}

void MUV2HitsCluster::Clear(Option_t * /*option*/)
{
    fNHits=0;
    fPosition=TVector3();
    fHits.clear();
    
}

void MUV2HitsCluster::AddHit(TRecoMUV2Hit *Hit){

    if (fNHits==0){
       
        fTime = Hit->GetTime();
        fSigmaT = 0.;
        fCharge = Hit->GetCharge();
        fSigmaQ = 0.;
        fAmplitude = Hit->GetPeakAmplitude();
        fSigmaA = 0.;
        if (Hit->GetChannelID()%100<50) fCoordX = Hit->GetScintillatorPosition();
        else fCoordY = Hit->GetScintillatorPosition();
        fSigmaX = fSigmaY = 0;

        fPosition = Hit->GetPosition();
    }
    
    fHits.push_back(Hit);
    fNHitsSide[Hit->GetScintillatorOrientation()]++;
    fNHits++;
    
    //   std::cout << "ClusterPosition = (" << fPosition.X() << "," << fPosition.Y() << "," << fPosition.Z() << ")" << std::endl;
}


void MUV2HitsCluster::UpdateValue(){
    
    fNHitsSide[0] = fNHitsSide[1] = 0;
    fTime = 0.;
    fSigmaT = 0.;
    fCharge = 0.;
    fSigmaQ = 0.;
    fAmplitude = 0;
    fSigmaA = 0.;
    fCoordX = fCoordY = 0.;
    fSigmaX = fSigmaY = 0.;
    Double_t Qx=0, Qy=0;
    
    if (fNHits<1) return;
    
    for (int i=0; i<fNHits; i++){
        
        fTime += fHits[i]->GetTime()*fHits[i]->GetCharge();
        fCharge += fHits[i]->GetCharge();
        fAmplitude += fHits[i]->GetPeakAmplitude();
        if (fHits[i]->GetChannelID()%100<50){
            fCoordX += fHits[i]->GetScintillatorPosition()*fHits[i]->GetCharge();
            Qx += fHits[i]->GetCharge();
            fNHitsSide[0]++;
        }
        else{
            fCoordY += fHits[i]->GetScintillatorPosition()*fHits[i]->GetCharge();
            Qy += fHits[i]->GetCharge();
            fNHitsSide[1]++;
        }

    }
    
    fTime /= fCharge;
    if (fNHitsSide[0]) fCoordX /= Qx;
    else fCoordX = 0.;
    
    if (fNHitsSide[1]) fCoordY /= Qy;
    else fCoordY = 0.;
    
    fPosition[0] = fCoordX;
    fPosition[1] = fCoordY;
    
    for (int i=0; i<fNHits; i++){
        
        fSigmaT += fHits[i]->GetCharge()*(fHits[i]->GetTime() - fTime)*(fHits[i]->GetTime() - fTime);
        fSigmaQ += (fHits[i]->GetCharge() - fCharge/fNHits)*(fHits[i]->GetCharge() - fCharge/fNHits);
        fSigmaA += (fHits[i]->GetPeakAmplitude() - fAmplitude/fNHits)*(fHits[i]->GetPeakAmplitude() - fAmplitude/fNHits);
        if (fHits[i]->GetChannelID()%100<50) fSigmaX += (fHits[i]->GetScintillatorPosition() - fCoordX)*(fHits[i]->GetScintillatorPosition() - fCoordX)*fHits[i]->GetCharge();
        else fSigmaY += (fHits[i]->GetScintillatorPosition() - fCoordY)*(fHits[i]->GetScintillatorPosition() - fCoordY)*fHits[i]->GetCharge();
        
    }
    
    
    fSigmaT = TMath::Sqrt(fSigmaT/(fCharge));
    fSigmaA = TMath::Sqrt(fSigmaA/(fNHits-1));
    fSigmaQ = TMath::Sqrt(fSigmaQ/(fNHits-1));
    if (Qx>0)fSigmaX = TMath::Sqrt(fSigmaX/(Qx));
    if (Qy>0)fSigmaY = TMath::Sqrt(fSigmaY/(Qy));
    
}
