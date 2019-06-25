// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TRecoMUV1Candidate_H
#define TRecoMUV1Candidate_H

#include "TRecoVCandidate.hh"

class TRecoMUV1Candidate : public TRecoVCandidate {

  public:

    TRecoMUV1Candidate();
    ~TRecoMUV1Candidate(){};

    void Clear(Option_t* = "");

    void SetTimeHorizontal(Double_t value){ fTimeX=value; }
    void SetTimeVertical(Double_t value){ fTimeY=value; }

    void SetTimeSigmaHorizontal (Double_t value){ fTimeSigmaX = value; }
    void SetTimeSigmaVertical (Double_t value){ fTimeSigmaY = value; }

    void SetShowerWidthHorizontal(Double_t value ){	fShowerWidthHorizontal=value;	}
    void SetShowerWidthVertical(Double_t value ){	fShowerWidthVertical=value;	}
    void SetShowerWidth(Double_t value ){	fShowerWidth=value;	}

    void SetCharge (Double_t value ){	fCharge=value;	}
    void SetChargeHorizontal (Double_t value){ fChargeX=value; }
    void SetChargeVertical (Double_t value){ fChargeY=value; }

    void SetEnergyHorizontal (Double_t value){ fEnergyX=value; }
    void SetEnergyVertical (Double_t value){ fEnergyY=value; }
    void SetEnergy (Double_t value){ fEnergy=value; }

    void SetSeedEnergyHorizontal(Double_t value){ fSeedEnergyX=value; }
    void SetSeedIndexHorizontal(Int_t value) { fSeedIndexX=value; }

    void SetSeedEnergyVertical(Double_t value){ fSeedEnergyY=value; }
    void SetSeedIndexVertical(Int_t value) { fSeedIndexY=value; }

    void SetInnerEnergyHorizontal(Double_t value){ fInnerEnergyX=value; }
    void SetInnerEnergyVertical(Double_t value){ fInnerEnergyY=value; }

    void SetPosition (Double_t x, Double_t y ){	fPosition.Set(x,y);	}
    void SetPosition (TVector2 value ){ fPosition = value; }

    void SetQuadrant(Int_t value ){	fQuadrant=value;	}
    void SetVerticalIndex(Int_t value){ fVerticalIndex=value;}
    void SetHorizontalIndex(Int_t value){ fHorizontalIndex=value;}
    void SetVerticalChannel(Int_t value){ fVerticalChannel=value;}
    void SetHorizontalChannel(Int_t value){ fHorizontalChannel=value;}


    Double_t GetTimeHorizontal(){ return fTimeX; }
    Double_t GetTimeSigmaHorizontal(){ return fTimeSigmaX; }
    Double_t GetTimeVertical(){ return fTimeX; }
    Double_t GetTimeSigmaVertical(){ return fTimeSigmaX; }

    Double_t GetPlaneTimeDiff(){ return (fTimeX - fTimeY);}

    Double_t GetShowerWidthHorizontal(){	return fShowerWidthHorizontal;	}
    Double_t GetShowerWidthVertical(){	return fShowerWidthVertical;	}
    Double_t GetShowerWidth(){	return fShowerWidth;	}

    Double_t GetX(){	return fPosition.X();	}
    Double_t GetY(){	return fPosition.Y();	}
    TVector2 GetPosition(){ return fPosition; }

    Double_t GetCharge () { return fCharge; }
    Double_t GetChargeHorizontal(){ return fChargeX; }
    Double_t GetChargeVertical (){ return fChargeY;}

    Double_t GetEnergyHorizontal () { return fEnergyX; }
    Double_t GetEnergyVertical () { return fEnergyY; }
    Double_t GetEnergy () { return fEnergy; }

    Double_t GetSeedEnergyHorizontal(){ return fSeedEnergyX; }
    Int_t GetSeedIndexHorizontal() { return fSeedIndexX; }

    Double_t GetSeedEnergyVertical(){ return fSeedEnergyY; }
    Int_t GetSeedIndexVertical() { return fSeedIndexY; }

    Double_t GetSeedEnergy() { return (fSeedEnergyX + fSeedEnergyY); }

    Double_t GetInnerEnergyHorizontal(){ return fInnerEnergyX; }
    Double_t GetInnerEnergyVertical(){ return fInnerEnergyY; }
    Double_t GetInnerEnergy(){ return (fInnerEnergyX + fInnerEnergyY); }

    Int_t GetQuadrant(){ return fQuadrant;};
    Int_t GetVerticalIndex(){ return fVerticalIndex;}
    Int_t GetHorizontalIndex(){ return fHorizontalIndex;}
    Int_t GetVerticalChannel(){ return fVerticalChannel;}
    Int_t GetHorizontalChannel(){ return fHorizontalChannel;}

  private:

    Double_t fTimeX, fTimeY;            ///< Time from Horizontal and Vertical planes
    Double_t fTimeSigmaX, fTimeSigmaY;  ///< Standard Deviation of the hit time in Horizontal and Vertical planes

    Double_t fShowerWidth;              ///< Standard deviation of the shower assuming a gaussian shape
    Double_t fShowerWidthHorizontal, fShowerWidthVertical;    ///< As before for Horizontal and Vertical planes

    TVector2 fPosition; ///< Center of the Cluster

    Double_t fCharge, fChargeX, fChargeY; ///< Charge collected: Total, on Horizontal and Vertical planes
    Double_t fEnergy, fEnergyX, fEnergyY; ///< Energy collected: Total, on Horizontal and Vertical planes

    Double_t fSeedEnergyX, fSeedEnergyY;  ///< Energy in the most energetic channel in Horizontal and Vertical planes
    Int_t fSeedIndexX, fSeedIndexY;       ///< Index of the hit giving the seed for Horizontal and Vertical planes

    Double_t fInnerEnergyX, fInnerEnergyY; ///< Energy collected in ±1 channel from the seed

    Int_t fQuadrant;            ///< Quadrant containing the cluster
    Int_t fVerticalIndex;       ///< Vertical Readout side 1 = Saleve, 3 = Jura
    Int_t fHorizontalIndex;     ///< Horizontal Readout side 0 = Bottom, 2 = Top
    Int_t fHorizontalChannel;   ///< Central channel on Horizontal (1 - 22, ChannelID = 100 + 50 * HorizontalIndex + HorizontalChannel)
    Int_t fVerticalChannel;     ///< Central channel on Vertical (1 - 22, ChannelID = 100 + 50 * VerticalIndex + VerticalChannel)



    ClassDef(TRecoMUV1Candidate,1);
};
#endif
