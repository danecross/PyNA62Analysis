#ifndef Particle_HH_
#define Particle_HH_
#include "ProtoParticle.hh"
#include "TVector3.h"
#include "TLorentzVector.h"
#include <vector>
#include <map>

namespace NA62Analysis
{
  using namespace std;
  class Particle
  {
  public:
    Particle();
    explicit Particle(ProtoParticle* pp);

    void SetCharge(double v){ mCharge = v;}
    void SetMass(double v){ mMass = v;}
    void SetMeasMass(double v){ mMeasMass = v;}
    void SetMeasEnergy(double v){ mMeasEnergy = v;}
    void SetTime(double v){ mTime = v;}
    void SetEndVertex(TVector3 v){ mEndVertex = v;}
    void SetProdVertex(TVector3 v){ mProdVertex = v;}
    void SetMomentum(TVector3 v){ mMomentum = v;}
    void SetPosition(TVector3 v){ mPosition = v;}
    void SetProto(ProtoParticle* p) {mProto = p;};

    double GetCharge(){ return mCharge;}
    double GetMass(){ return mMass;}
    double GetMeasMass(){ return mMeasMass;}
    double GetMeasEnergy(){ return mMeasEnergy;}
    double GetTime(){ return mTime;}
    TVector3 GetEndVertex(){ return mEndVertex;}
    TVector3 GetProdVertex(){ return mProdVertex;}
    TVector3 GetMomentum(){ return mMomentum;}
    double GetP(){ return mMomentum.Mag();}
    TVector3 GetPosition(){ return mPosition;}
    ProtoParticle* GetProto(){ return mProto ;} 

    TLorentzVector Get4Momentum();

    bool IsBasicParticle();
    void AddDaughter(Particle);
    void ClearDaughters();
    vector<Particle> GetDaughters();

    bool HasInfo(int);
    bool AddInfo(int, double);
    double GetInfo(int, double);
    unsigned int EraseInfo(int);
    map<int,double> GetExtraInfo();



    virtual ~Particle();

  private:

    int mCharge;

    double mMass;
    double mMeasMass;
    double mMeasEnergy;
    double mTime;

    TVector3 mEndVertex;
    TVector3 mProdVertex;
 
    TVector3 mMomentum;
    TVector3 mPosition;

    ProtoParticle* mProto;

    vector<Particle> mDaughters; 
    map<int,double> mExtraInfo;

  protected:
  };

}//~namespace IImaS

#endif//~Particle_HH_

