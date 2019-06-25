#ifndef TGigaTrackerDigiEvent_HH_
#define TGigaTrackerDigiEvent_HH_
#include "TDCEvent.hh"

class TGigaTrackerDigiEvent : public TDCEvent
{
public:
  TGigaTrackerDigiEvent();
  explicit TGigaTrackerDigiEvent(TClass *);
  virtual ~TGigaTrackerDigiEvent();
  void SetHitNbFromPrevL0(int gtk, int chip, int half, int n);
  int GetHitNbFromPrevL0(int gtk, int chip, int half = -1);
  void Clear(Option_t* = "");

private:
  TGigaTrackerDigiEvent(const TGigaTrackerDigiEvent &);
  TGigaTrackerDigiEvent & operator=(const TGigaTrackerDigiEvent &);
  int fHitNbFromPrevL0[3][10][2];

protected:
};



#endif//~TGigaTrackerDigiEvent_HH_

