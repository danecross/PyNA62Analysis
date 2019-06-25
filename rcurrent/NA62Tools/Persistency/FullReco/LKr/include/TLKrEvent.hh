// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#ifndef TLKrEvent_H
#define TLKrEvent_H

#include "TDetectorVEvent.hh"

class TLKrEvent : public TDetectorVEvent {

    public:

      TLKrEvent();
      void Clear(Option_t* = "");
      // void AddSeed(Int_t );

    public:
      //  Int_t *              GetSeedIndexes()                                   { return fSeedIndexes;                  };

      //  Int_t                GetNSeeds()                                        { return fNSeeds;                       };
      //  void                 SetNSeeds(Int_t value)                             { fNSeeds = value;                      };

    private:

      Int_t fSeedIndexes[16384]; //index of the hit recognized as seed
      Int_t fNSeeds; //totalnumber of seeds

      ClassDef(TLKrEvent,1);

};
#endif
