// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoCHANTIHit_H
#define TRecoCHANTIHit_H

#include "TRecoVHit.hh"
#include "CHANTIChannelID.hh"

class TRecoCHANTIHit : public TRecoVHit, public CHANTIChannelID {

public:
  
  TRecoCHANTIHit();
  ~TRecoCHANTIHit(){};

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID();
  
public:
  
  void        SetTimeWidth(Double_t value)       { fTimeWidth = value;     };
  Double_t    GetTimeWidth()                     { return fTimeWidth;      };
  void        SetDeltaTime(Double_t value)       { fDeltaTime = value;     };
  Double_t    GetDeltaTime()                     { return fDeltaTime;      }; 
  void        SetDeltaWidth(Double_t value)      { fDeltaWidth = value;    };
  Double_t    GetDeltaWidth()                    { return fDeltaWidth;     }; 
  void        SetQualityFlag(Int_t value)        { fQualityFlag = value;   }; 
  Int_t       GetQualityFlag()                   { return fQualityFlag;    }; 

  Double_t    GetX()                             { return fX;              };
  void        SetX(Double_t value)               { fX = value;             };
  Double_t    GetY()                             { return fY;              };
  void        SetY(Double_t value)               { fY = value;             };
  Double_t    GetZ()                             { return fZ;              };
  void        SetZ(Double_t value)               { fZ = value;             };
  Int_t       GetThresholdFlag()                 {return fThresholdFlag;   };
  void        SetThresholdFlag(Int_t value)      {fThresholdFlag = value;  };
  Int_t       GetConnectorID();
  Double_t    GetXYTimeCorrection(Double_t Position);
  Int_t       GetMult()                         { return fMult;            };
  void        SetMult(Int_t value)              { fMult = value;           };


    private:

  Double_t fX;
  Double_t fY;
  Double_t fZ;
  Int_t fThresholdFlag; ///<    This is the flag that describe the quality of Reco Hit:
                        ///<    2: for physical hit with double thresholds crossed   
                        ///<    1: for physical hit with only low thresholds crossed   
                        ///<    0: for physical hit with only low threshold crossed   
  Double_t fTimeWidth; 
  Int_t fConnectorID;	///<	This is the connector index (from 1 to 18) corresponding to the Channel cable  
  Int_t fMult;		///<	This is the multilicity of the physics channel in a single event
  Double_t fDeltaTime;	///<	This is the difference between the Low and High threshold leading time
  Double_t fDeltaWidth;	///<	This is the difference between the Low and High threshold ToT
  Int_t    fQualityFlag;///<	This is the flag that describe the quality of the Reco Hit :
                        ///<    0 both leading and trealing edge(for physical hit with single and double thresholds crossed)
                        ///<    1 high threshold trailing edge missing (for physical hit with double thresholds crossed)
                        ///<    2 low threshold trailing edge missing (for physical hit with double thresholds crossed)
                        ///<    3 low and hig threshold trailing edge missing (for physical hit with double thresholds crossed)
                        ///<    4 trailing edge missing (for physical hit with single threshold crossed) 
                        ///<    5 leading edge missing (for physical hit with single threshold crossed)


  
  ClassDef(TRecoCHANTIHit,1);

};
#endif
