#ifndef Timer_H
#define Timer_H

#include <time.h>

#include <iostream>
#include <vector>
#include "TString.h"

class Timer{
public:
  /// constructor: use normalisation or not
  explicit Timer(bool normalised);
  
  /// destructor: remove created TimerImp
  ~Timer(){};
  
  /// add a new timer with name and indent (held by master Timer)
  unsigned int AddTimer(const TString &name,unsigned int indent);

  /// remove timers with a given name
  void RemoveTimers(const TString name);

  /// start a numbered timer
  inline void StartTimer(unsigned int timer){
    fTimers[timer].Start();
  }
  
  /// stop a numbered timer
  inline double StopTimer(unsigned int timer, bool valid=true){
    fTimers[timer].Stop(valid);
    return fTimers[timer].GetElapsed();
  }
  
  /// read last interval
  double TimerGetElapsed(unsigned int timer){
    return fTimers[timer].GetElapsed();
  }
  /// get the number of timers
  unsigned int GetNTimers(){ return fTimers.size(); }

  /// get timer name
  const TString & GetTimerName(unsigned int timer){
    return fNames[timer];
  }

  /// print timers
  void PrintTimers(std::ostream &os);

  /// print single timer
  void PrintTimer(unsigned int timer,std::ostream &os);

private: 
  // "captive" timer implementation class
  class TimerImp{
  public:
    TimerImp():
      fDeltaT(-1.),fMinTime(-1.),fMaxTime(-1.),fTotalTime(0.),fEventsNum(0) {
    }
    ~TimerImp(){}
    
    inline void Start(){
#if defined(CLOCK_MONOTONIC_RAW)
      clock_gettime(CLOCK_MONOTONIC_RAW, &fStart); // best timer using rt library (linux kernel 2.8.6 onward only)
#elif defined(CLOCK_MONOTONIC)        
      clock_gettime(CLOCK_MONOTONIC, &fStart);      // timer with lower precision
#elif defined(CLOCK_REALTIME)
      clock_gettime(CLOCK_REALTIME, &fStart); // system time, reset by ntp etc.
#else
      fStart.tv_sec = fStart.tv_nsec = 0; // no useful rt timers (Windows or OSX probably)
#endif
      fDeltaT = -1.; // invalid during runing
    }
    
    void Stop(bool valid){
      struct timespec stop;
#if defined(CLOCK_MONOTONIC_RAW)
      clock_gettime(CLOCK_MONOTONIC_RAW, &stop);  // best timer using rt library (linux kernel 2.8.6 onward only)
#elif defined(CLOCK_MONOTONIC)              
      clock_gettime(CLOCK_MONOTONIC, &stop);      // timer with lower precision
#elif defined(CLOCK_REALTIME)
      clock_gettime(CLOCK_REALTIME, &stop);       // system time, reset by ntp etc.
#else
      stop.tv_sec = stop.tv_nsec = 0; // no useful rt timers (Windows or OSX probably)
#endif
      
      fDeltaT = ((stop.tv_sec - fStart.tv_sec) * 1e3) +
        ((stop.tv_nsec - fStart.tv_nsec) * 1e-6); // scale to milliseconds from seconds and nanoseconds
      if(!valid) return; // do not include for averaging
      if( fMinTime < 0 || fDeltaT < fMinTime ) fMinTime = fDeltaT;
      if( fMaxTime < 0 || fDeltaT > fMaxTime ) fMaxTime = fDeltaT;
      ++fEventsNum;
      fTotalTime += fDeltaT;
    }

    inline double MaxTime()   const { return fMaxTime; } ///< max time interval (ms)
    inline double MinTime()   const { return fMinTime; } ///< min time interval (ms)
    inline double TotalTime() const { return fTotalTime; } ///< total time (ms)
    inline int    NumEvt()    const { return fEventsNum; }     ///< Number of intervals

    inline double GetElapsed() const { return fDeltaT; } ///< last time interval (ms)

  private:
    struct timespec fStart; ///< start of clock time
    double  fDeltaT; ///< time between last start/stop pair in ms
    double  fMinTime; ///< min time (ms)
    double  fMaxTime; ///< max time (ms)
    double  fTotalTime; ///< total time (ms)
    int   fEventsNum; ///< number of events
  }; //end of TimerImp

  std::vector<TimerImp> fTimers; ///< list of timers
  std::vector<TString> fNames; ///< alg names to match timers
  double fSpeedRatio; ///< speed ratio to "standard" compiler
};


#endif //Timer_H
