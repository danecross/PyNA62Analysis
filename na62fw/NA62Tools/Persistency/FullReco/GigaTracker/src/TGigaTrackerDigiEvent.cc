#include "TGigaTrackerDigiEvent.hh" 


//  ==============================================  
// cppcheck-suppress uninitMemberVar symbolName=TGigaTrackerDigiEvent::fHitNbFromPrevL0
TGigaTrackerDigiEvent::TGigaTrackerDigiEvent()
{
  Clear(); 
}

//  ==============================================  
// cppcheck-suppress uninitMemberVar symbolName=TGigaTrackerDigiEvent::fHitNbFromPrevL0
TGigaTrackerDigiEvent::TGigaTrackerDigiEvent(TClass* HitClass) : TDCEvent(HitClass)
{
  Clear(); 
}

//  ==============================================  
TGigaTrackerDigiEvent::~TGigaTrackerDigiEvent()
{
   
}

//  ==============================================  
void TGigaTrackerDigiEvent::Clear(Option_t* option)
{
  for (int s(0);s<3;s++){
    for (int c(0);c<10;c++){
      for (int h(0);h<2;h++){
	fHitNbFromPrevL0[s][c][h] = 0;
      }
    }
  }
  TDigiVEvent::Clear(option);
  return;
}

//  ==============================================  
int TGigaTrackerDigiEvent::GetHitNbFromPrevL0(int gtk, int chip, int half){
  if(half < 0) return fHitNbFromPrevL0[gtk][chip][0]+fHitNbFromPrevL0[gtk][chip][1];
  else return fHitNbFromPrevL0[gtk][chip][half];

}

//  ==============================================  
void TGigaTrackerDigiEvent::SetHitNbFromPrevL0(int gtk, int chip, int half, int n){
  fHitNbFromPrevL0[gtk][chip][half] = n;
  return;
}


