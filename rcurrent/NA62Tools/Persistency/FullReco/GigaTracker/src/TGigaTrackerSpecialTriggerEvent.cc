#include "TGigaTrackerSpecialTriggerEvent.hh" 

ClassImp(TGigaTrackerSpecialTriggerEvent)
//  ==============================================  
TGigaTrackerSpecialTriggerEvent::TGigaTrackerSpecialTriggerEvent() : TSpecialTriggerEvent()
{
  Clear();
}

//  ==============================================  
void TGigaTrackerSpecialTriggerEvent::Clear(Option_t* option){
  TSpecialTriggerEvent::Clear(option);
  for (int s(0);s<3;s++){
    for (int c(0);c<10;c++){
      for (int h(0);h<2;h++){
	fNHits[s][c][h] = 0;
	fFW[s][c][h] = 0;
      }
    }
  }


}

//  ==============================================  
TGigaTrackerSpecialTriggerEvent::TGigaTrackerSpecialTriggerEvent(TClass* v) : TSpecialTriggerEvent(v)
{
  Clear();
}

//  ==============================================  
TGigaTrackerSpecialTriggerEvent::~TGigaTrackerSpecialTriggerEvent()
{
   
}


//  ==============================================  
UInt_t TGigaTrackerSpecialTriggerEvent::GetNHits(int gtk, int chip, int half){
  if(half < 0) return fNHits[gtk][chip][0]+fNHits[gtk][chip][1];
  else return fNHits[gtk][chip][half];
}

//  ==============================================  
void TGigaTrackerSpecialTriggerEvent::SetNHits(int gtk, int chip, int half, UInt_t n){
  fNHits[gtk][chip][half] = n;
  return;
}

//  ==============================================  
UInt_t TGigaTrackerSpecialTriggerEvent::GetFW(int gtk, int chip, int half){
  return fFW[gtk][chip][half];
}

//  ==============================================  
void TGigaTrackerSpecialTriggerEvent::SetFW(int gtk, int chip, int half, UInt_t n){
  fFW[gtk][chip][half] = n;
  return;
}
