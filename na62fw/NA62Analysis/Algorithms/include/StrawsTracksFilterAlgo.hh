#ifndef StrawsTracksFilterAlgo_HH_
#define StrawsTracksFilterAlgo_HH_

#include "Algorithm.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TRecoSpectrometerEvent.hh"
#include <vector>
using namespace std;

namespace NA62Analysis
{
  class StrawsTracksFilterAlgo: public Algorithm
  {
  public:
    StrawsTracksFilterAlgo(BaseAnalysis *ba, Analyzer* ana, const string &name = "StrawsTracksFilterAlgo");
    virtual ~StrawsTracksFilterAlgo() {};
    vector<TRecoSpectrometerCandidate*> GetGoodTracks();
    bool IsGood(TRecoSpectrometerCandidate* track);
    bool IsFake(TRecoSpectrometerCandidate* track);
    void FilterGoodTrack();
    //vector<TRecoSpectrometerCandidate*> GetIsolatedTracks(TRecoSpectrometerEvent*);

  private:

    TRecoSpectrometerEvent* fSpectrometerEvent;
    
    double fChi2;
    double fChi2Fake;
    double fDeltaP;
    
    //double fIsoDeltaT;
    //double fIsoChi2;

  protected:
  };

}//~namespace IImaS

#endif//~StrawsTracksFilterAlgo_HH_

