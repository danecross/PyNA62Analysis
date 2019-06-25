// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TRecoMUV1Hit_H
#define TRecoMUV1Hit_H

#include "TRecoVHit.hh"
#include "MUV1ChannelID.hh"

class TRecoMUV1Hit : public TRecoVHit , public MUV1ChannelID {

  public:

    TRecoMUV1Hit();
    ~TRecoMUV1Hit(){};

    void Clear(Option_t* = "");

    void DecodeChannelID(Int_t ChannelID){  fChannelID = ChannelID; MUV1ChannelID::DecodeChannelID(fChannelID); }

  public:
    void SetPeakAmplitude (Double_t Amp) { fPeakAmplitude = Amp; }
    Double_t GetPeakAmplitude () { return fPeakAmplitude; }

    void SetCharge (Double_t Q) { fCharge = Q; }
    Double_t GetCharge () { return fCharge; }

    void SetSigma (Double_t sig){ fSigma = sig; }
    Double_t GetSigma (){ return fSigma; }

    void SetTimeError(Double_t Terr) { fTimeError = Terr; }
    void SetSigmaError(Double_t Serr) { fSigmaError = Serr; }
    void SetChargeError(Double_t Qerr) { fChargeError = Qerr; }
    void SetAmplitudeError(Double_t Aerr){ fPeakError = Aerr; }

    Double_t GetTimeError () { return fTimeError; }
    Double_t GetSigmaError (){ return fSigmaError; }
    Double_t GetChargeError () {return fChargeError; }
    Double_t GetAmplitudeError () {return fPeakError; }


  private:
    Double_t fTimeError;    ///< Error on the time in ns
    Double_t fPeakAmplitude;///< Amplitude of the peak in mV
    Double_t fPeakError;    ///< Error on the amplitude
    Double_t fCharge;       ///< Charge collected in pC
    Double_t fChargeError;  ///< Error on the charge
    Double_t fSigma;        ///< Sigma of the gaussian peak in ns
    Double_t fSigmaError;   ///< Error on the sigma

    ClassDef(TRecoMUV1Hit,1);
};
#endif
