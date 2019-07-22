#ifndef ThreePiAssociationAlgo_HH_
#define ThreePiAssociationAlgo_HH_

#include "Algorithm.hh"
#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "Particle.hh"
#include "VertexLSF.hh"
#include "BlueTubeTracker.hh"

class TRecoGigaTrackerEvent;
class TRecoCedarEvent;

namespace NA62Analysis {

  class ThreePiAssociationAlgo : public Algorithm {
  public:
    ThreePiAssociationAlgo(BaseAnalysis *ba, Analyzer* ana, const std::string &name = "ThreePiAssociationAlgo");
    virtual ~ThreePiAssociationAlgo() {};

    std::vector<Particle> Associate(std::vector<Particle> vPi);
    void Associate(std::vector<Particle>& vK3Pi, TRecoCedarEvent* cedarEvt );
    void Associate(std::vector<Particle>& vK3Pi, TRecoGigaTrackerEvent* gigatrackerEvt );

  private:

  TH1F* fHmK;
  TH1F* fHpK;
  TH1F* fHThX3Pi;
  TH1F* fHThY3Pi;
  TH1F* fHTVtx;
  TH1F* fHDtVtx;
  TH1F* fHnPi;
  TH2F* fHPRichMass;
  TH2F* fHP3PiThPi;
  TH2F* fHZChi2;

  double fChi2max;
  double fZmin;
  double fZmax;
  double fPmin;
  double fPmax;
  double fInvMmin;
  double fInvMmax;
  VertexLSF fVertexLSF;
  };

}//~namespace IImaS

#endif//~ThreePiAssociationAlgo_HH_

