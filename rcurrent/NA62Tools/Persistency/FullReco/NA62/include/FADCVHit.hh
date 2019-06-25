// --------------------------------------------------------------
// History:
//
// Modified by Giuseppe Ruggiero 2012
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#ifndef FADCVHit_H
#define FADCVHit_H
#include "TVDigi.hh"
#include "TArrayD.h"

class FADCVHit : public TVDigi {

  public:

    FADCVHit();
    explicit FADCVHit(Int_t);
    explicit FADCVHit(TVHit* MCHit);
    ~FADCVHit(){};
    void Clear(Option_t* = "");
    virtual void UpdateReferenceTime(Double_t value){ fPeakTime -= value; };
    virtual Double_t       GetTime() { return fPeakTime; };

  public:
    inline Double_t        GetPeakEnergy()                           { return fPeakEnergy;         }
    inline void            SetPeakEnergy(Double_t val)               { fPeakEnergy = val;          }
    inline Double_t        GetPeakTime()                             { return fPeakTime;           }
    inline void            SetPeakTime(Double_t val)                 { fPeakTime = val;            }
    inline Double_t        GetADCPeakEnergy()                        { return fADCPeakEnergy;      }
    inline void            SetADCPeakEnergy(Double_t val)            { fADCPeakEnergy = val;       }
    inline Double_t        GetADCPeakTime()                          { return fADCPeakTime;        }
    inline void            SetADCPeakTime(Double_t val)              { fADCPeakTime = val;         }
    inline Int_t           GetQuality()                              { return fQuality;            }
    inline void            SetQuality(Int_t val)                     { fQuality = val;             }
    inline Int_t           GetFlags()                                { return fFlags;              }
    inline void            SetFlags(Int_t val)                       { fFlags = val;               }
    inline Int_t           GetNSamples()                             { return fNSamples;           }
    void                   AddSample(Double_t Value);
    inline Double_t *      GetAllSamples()                           { return fSamples.GetArray(); }
    inline Int_t           GetGain()                                 { return fGain;               }
    inline void            SetGain(Int_t val)                        { fGain = val;                }

  private:

    Double_t fPeakEnergy;    // Parabola peak pulse height (in GeV)
    Double_t fPeakTime;      // Parabola peak time (the time is in ns)
    Double_t fADCPeakEnergy; // Parabola peak pulse height (= PEAK + 10000*maxgain with gain = 0,1,2, or 3)
    Double_t fADCPeakTime;   // Parabola peak time (the time is in unit of samples)
    Int_t fQuality;   // Quality estimator 
    //                   0 : no energy/time estimate
    //                   bit 0 set : energy/time from fixed sampling (in header)
    //                   bit 1 set : energy/time from max sampling
    //                   bit 2 set : energy/time from 3-point parabola
    Int_t fFlags;     // flag pulse or cell 
    //                   bit  0 set : pulse BELOW zero suppression threshold
    //                   bit  1 set : pulse saturated
    //                   bit  4 set : pulse in underflow
    //                   bit  5 set : no calibration
    //                   bit  6 set : non trivial pulse (gain>0)
    //                   bit  7 set : q(iplkr+3)=max count
    //                   bit 12 set : L1 error
    TArrayD  fSamples; //[fNMaxSamples] Pulse height (if in ADC counts is given as above)

    Int_t fNSamples;
    Int_t fNMaxSamples;
    Int_t fGain;

    ClassDef(FADCVHit,1);
};
#endif
