/// \class Particle
/// \Brief
/// Physics Particle. Detector oriented object from which the Particle is made can be added to the Particle as ProtoParticle.
/// Particle can also be used to describe a decaying particle and contains its daughter particles.
/// If additional informations are needed they can be added using AddInfo and GetInfo.
/// \EndBrief
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)

#include "Particle.hh" 
#include "Utils.hh"
namespace NA62Analysis
{
  //==============================================  
  Particle::Particle() :
     mCharge(0),
     mMass(0),
     mMeasMass(0),
     mMeasEnergy(0),
	 mTime(0),
     mEndVertex(0,0,0),
     mProdVertex(0,0,0),
     mMomentum(0,0,0),
     mPosition(0,0,0),
     mProto(nullptr)
  {
  }

  //==============================================  
  Particle::Particle(ProtoParticle* pp) :
     mCharge(0),
     mMass(0),
     mMeasMass(0),
     mMeasEnergy(0),
	 mTime(0),
     mEndVertex(0,0,0),
     mProdVertex(0,0,0),
     mMomentum(0,0,0),
     mPosition(0,0,0),
     mProto(pp)
  {
   
  }



  //==============================================  
  Particle::~Particle()
  {
   
  }

  //==============================================  
  void Particle::AddDaughter(Particle p){
    mDaughters.push_back(p);
  }

  //==============================================  
  vector<Particle> Particle::GetDaughters(){
    return mDaughters;
  }

  //==============================================  
  TLorentzVector Particle::Get4Momentum(){
    TLorentzVector v;
    v.SetXYZM(mMomentum.X(),mMomentum.Y(),mMomentum.Z(),mMass);
    return v;
    
  }

  //==============================================  
  bool Particle::IsBasicParticle(){
    return mDaughters.empty(); 
  }


  //==============================================  
  bool Particle::HasInfo(int key){
    return mExtraInfo.end() != mExtraInfo.find( key ) ;
  }

  //==============================================  
  bool Particle::AddInfo(int key, double info){
    return mExtraInfo.insert(std::pair<int,double>( key , info )).second ;
  }
  
  //==============================================  
  double Particle::GetInfo(int key, double default_value){
    map<int,double>::const_iterator i = mExtraInfo.find( key ) ;
    return mExtraInfo.end() == i ? default_value : i->second ;
  }

  //==============================================  
  unsigned int Particle::EraseInfo(int key){
    return mExtraInfo.erase( key ) ;
  }

  //==============================================  
  map<int,double> Particle::GetExtraInfo(){
    return mExtraInfo;
  }

}//~namespace IImaS


