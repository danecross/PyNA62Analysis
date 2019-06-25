#ifndef TGigaTrackerSpecialTriggerEvent_HH_
#define TGigaTrackerSpecialTriggerEvent_HH_
#include "TSpecialTriggerEvent.hh"
#include "TClass.h"

class TGigaTrackerSpecialTriggerEvent: public TSpecialTriggerEvent
{
public:
  TGigaTrackerSpecialTriggerEvent();
  explicit TGigaTrackerSpecialTriggerEvent(TClass* );
  virtual ~TGigaTrackerSpecialTriggerEvent();

  UInt_t GetNHits(int gtk, int chip, int half = -1);  
  void SetNHits(int gtk, int chip, int half, UInt_t n);

  UInt_t GetFW(int gtk, int chip, int half = -1);  
  void SetFW(int gtk, int chip, int half,  UInt_t n);

  void Clear(Option_t* t = "");

private:
  TGigaTrackerSpecialTriggerEvent(const TGigaTrackerSpecialTriggerEvent &);
  TGigaTrackerSpecialTriggerEvent & operator=(const TGigaTrackerSpecialTriggerEvent &);

  UInt_t fNHits[3][10][2];
  UInt_t fFW[3][10][2];

  ClassDef(TGigaTrackerSpecialTriggerEvent,1);
};



#endif//~TGigaTrackerSpecialTriggerEvent_HH_

