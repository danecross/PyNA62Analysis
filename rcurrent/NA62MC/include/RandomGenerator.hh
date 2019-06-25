#ifndef RandomGenerator_H
#define RandomGenerator_H 1

#include "TROOT.h"
#include "TRandom3.h"
#include "TH2D.h"
#include "TMath.h"
#include <map>


class RandomGenerator {
  public:
    virtual ~RandomGenerator();
    static RandomGenerator* GetInstance();
    void Init(unsigned int);
    void Init(TRandom3* RandDecay);
    void GetRandom2(TH2D*, Double_t&, Double_t&);  
  private:
    static RandomGenerator* fInstance;
    
  protected:
    RandomGenerator();

  public:
  TRandom3*       GetRandomDecay(){/*G4cout << "Random3Return " << GetSeedDecay() << G4endl;*/ return fRandDecay;};
    unsigned int    GetSeedDecay(){return fRandDecay->GetSeed();};
  double          GetRndmDecay(){/*G4cout << "RandomNumberReturn " << GetSeedDecay() << G4endl;*/ return fRandDecay->Rndm();};
  void            GetArrayDecay(int n,double *numbers){/*G4cout << "RandomArrayReturn " << GetSeedDecay() << G4endl;*/ fRandDecay->RndmArray(n,numbers);};
    void            WriteDecay(const char* filename){fRandDecay->WriteRandom(filename);};
    void            ReadDecay(const char* filename){fRandDecay->ReadRandom(filename);};

  private:
    TRandom3 *fRandDecay;
    std::map<TH2D*, int> mapOfHistograms;
    bool fIntegralHasBeenCalculated; 
};

#endif
