#ifndef CHODChannel_H
#define CHODChannel_H 1

#include "TVector2.h"
#include "NA62VChannel.hh"

class TDCEvent;

class CHODChannel : public NA62VChannel{

public:

  CHODChannel(TVector2, Int_t,Int_t);
  ~CHODChannel();

private:

public:

  TVector2             GetPosition()                                      { return fPosition;                     };
  Int_t                GetPlane()                                         { return fPlane;                        };
  Int_t                GetCounter()                                       { return fCounter;                      };

private:

  TVector2 fPosition;
  Int_t fPlane;
  Int_t fCounter;

};

#endif
