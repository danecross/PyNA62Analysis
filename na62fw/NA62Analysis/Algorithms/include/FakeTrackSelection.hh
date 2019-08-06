#ifndef FAKETRACKSELECTION_HH
#define FAKETRACKSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Algorithm.hh"
#include "TRecoSpectrometerEvent.hh"

using namespace std;

class FakeTrackSelection : public Algorithm {
public:
  FakeTrackSelection(BaseAnalysis *ba, Analyzer *ana, const string &name = "FakeTrackSelection");
  virtual ~FakeTrackSelection();
  void Init();
  void Process(TRecoSpectrometerEvent*);
  void PrepareOutputs(int);
  std::vector<bool> GetAreFake(){return fFakeTracks;};
  bool has_common_hit(TRecoSpectrometerEvent*, int);
  void SetCutMinNSTRAWChambers(int n){fCutMinNSTRAWChambers = n;};
  void SetCutChi2(double c){fCutChi2 = c;};

protected:
  //output
  std::vector<bool> fFakeTracks;

  //parameters
  int fCutMinNSTRAWChambers;
  double fCutChi2;
};
#endif
