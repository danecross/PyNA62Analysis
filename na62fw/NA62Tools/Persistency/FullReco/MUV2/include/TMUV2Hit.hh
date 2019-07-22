// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-11
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#ifndef TMUV2Hit_H
#define TMUV2Hit_H

#include "TDetectorVHit.hh"

class TMUV2Hit : public TDetectorVHit {

  public:

    TMUV2Hit();
    ~TMUV2Hit(){};

    void Clear(Option_t* = "");

    Int_t GetStationID () { return 0; }

    void  SetPlane (Int_t plane) { fPlane = plane; }
    Int_t GetPlane ()	{ return fPlane; }
	

    void SetChannelID(Int_t ChID);


  public:

    Int_t	GetScintillatorID()             {  return fScintillatorID; };
    void	SetScintillatorID(Int_t value)  {  fScintillatorID = value;};
    double	GetStepLength()                 {  return fStepLength;     };
    void	SetStepLength(double value)     {  fStepLength = value;    };
    double GetPositionInScintillator()      {   return fPositionInScintillator;};
    void SetPositionInScintillator(double value) {fPositionInScintillator = value;};

  protected:

    Int_t	fScintillatorID;
	Int_t	fPlane;
	
    double	fStepLength;
    double fPositionInScintillator;

    ClassDef(TMUV2Hit,1);
};
#endif
