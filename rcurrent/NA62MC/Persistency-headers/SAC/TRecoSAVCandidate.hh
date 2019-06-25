// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
//
// --------------------------------------------------------------
#ifndef TRecoSAVCandidate_H
#define TRecoSAVCandidate_H

#include "TRecoVCandidate.hh"

class TRecoSAVCandidate : public TRecoVCandidate {
    
public:
    
    TRecoSAVCandidate();
    ~TRecoSAVCandidate(){};
    
    void Clear(Option_t* = "");

public:
    
    void SetTime(Double_t value){ fTime=value; }
    Double_t GetTime() { return fTime; }

    void SetEnergy (Double_t value){ fEnergy = value; }
    Double_t GetEnergy() { return fEnergy; }
  
    void SetPosition (Double_t x, Double_t y ){	fPosition.Set(x,y);	}
    void SetPosition (TVector2 value ){ fPosition = value; }
  
    Double_t GetX() {	return fPosition.X();	}
    Double_t GetY() {	return fPosition.Y();	}
    TVector2 GetPosition() { return fPosition; }
    
private:
    
    Double_t fTime;            ///< Candidate time as a mean of hit time.
  
    TVector2 fPosition;       ///< Candidate position everage mean of hit position.

    Double_t fEnergy;        ///< Candidate energy.
    
    
    ClassDef(TRecoSAVCandidate,1);
};
#endif
