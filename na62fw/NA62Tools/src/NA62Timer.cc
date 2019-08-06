#include "NA62Timer.hh"
#include <TRandom3.h>
#include <cstdio>

NA62Timer::NA62Timer(bool normalised = true) {
  if(normalised){
    TRandom3 localRandom; // do not mess with main random generator
    localRandom.SetSeed(12345);
    NA62TimerImp timer;
    timer.Start();
    int shots = 6500000;// 0.25s on 2.66GHz Core2 Intel CPU (Q8400) gcc 4.6.3 -02
    double sum = 0.; // to prevent optimising loop away
    while( 0 < --shots ) { sum += localRandom.Gaus() * sum ; }
    timer.Stop(true);
    double deltaT = timer.GetElapsed();
    fSpeedRatio = 250./(double)deltaT; // ratio to "standard" CPU
  }else{
    fSpeedRatio=1.;
  }
}

unsigned int NA62Timer::AddTimer(const TString &name,unsigned int indent){
  fTimers.push_back(NA62TimerImp());
  TString iName = "";
  while( indent-- ) iName += " ";
  fNames.push_back(iName+name);
  return fNames.size() - 1; // pos of just added timer
}

void NA62Timer::RemoveTimers(const TString name){
  for(unsigned int iTimer=0;iTimer<fTimers.size();iTimer++){
    if(fNames.at(iTimer).Contains(name)) {
      fTimers.erase(fTimers.begin()+iTimer);
      fNames.erase(fNames.begin()+iTimer);
      iTimer--;
    }
  }
}

void NA62Timer::PrintTimers(std::ostream &os){
  const static int bufSize = 256; // max size of output buffer
  char outputBuff[bufSize];
  if(fSpeedRatio != 1.){
    os << "**************************************************************************\n";
    snprintf(outputBuff,bufSize, // safer sprintf
            "*       CPU speed factor %7.2f to 2.66GHz Core2 Intel CPU (Q8400)      *\n",
	    fSpeedRatio);
    os << outputBuff;
    os << "**************************************************************************\n";
  }else{
    os << "**************************************************************************\n";
    os << "*                        Unnormalised system time                        *\n";
    os << "**************************************************************************\n";
  }
  // print the timer block
  snprintf(outputBuff,bufSize, // safer sprintf
	   "* %-20s * %7s * %7s * %7s * %7s * %7s *\n",
	   "Alg. name","Called","Av.(ms)","Min(ms)","Max(ms)","Tot (s)");
  os << outputBuff;
  for(unsigned int timer = 0 ; timer < fTimers.size() ; ++timer){
    PrintTimer(timer,os);
  }
  os << "**************************************************************************\n";
}

void NA62Timer::PrintTimer(unsigned int timer, std::ostream &os ){
  const static int bufSize = 256; // max size of output buffer
  char outputBuff[bufSize];
  if(fNames[timer].Contains("Total")) os << "*------------------------------------------------------------------------*\n";
  snprintf(outputBuff,bufSize, // safer sprintf
	   "* %-20s * %7i * %7.1f * %7.1f * %7.1f * %7.1f *\n"
	   , fNames[timer].Data()
	   , fTimers[timer].NumEvt()
	   , (fSpeedRatio*fTimers[timer].TotalTime()/fTimers[timer].NumEvt())
	   , (fSpeedRatio*fTimers[timer].MinTime())
	   , (fSpeedRatio*fTimers[timer].MaxTime())
	   , (fSpeedRatio*fTimers[timer].TotalTime()/1000.)); //want in seconds
  os << outputBuff;
}
