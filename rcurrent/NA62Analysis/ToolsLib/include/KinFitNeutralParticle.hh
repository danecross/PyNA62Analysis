// Class for neutral particles in NA62KinFit (LKr clusters)
//	Author: Michele Corvino (corvino@na.infn.it)
//
//

#ifndef KINFITNEUTRALPARTICLE_HH
#define KINFITNEUTRALPARTICLE_HH

#include "KinFitParticle.hh"

using namespace std;

class KinFitNeutralParticle: public KinFitParticle{
	public:
	
	KinFitNeutralParticle();
	KinFitNeutralParticle(TRecoLKrCandidate* fCand, TVector3 PiKVertex); 
	KinFitNeutralParticle(TLorentzVector FourMomentum, TVector3 PiKVertex); 
	~KinFitNeutralParticle();
	void SetInitVertex(TVector3 cda);
	void SetInitMomentum(TVector3 Mom);
	void SetInitEnergy(double En);
	void SetInitEnergy(TVector3 Mom, double mass);
	void SetCovMatrix(TRecoLKrCandidate *fCand, TVector3 ChargedVertex);
	Double_t RSS(Double_t a, Double_t b, Double_t c);
	Double_t RSS(Double_t a, Double_t b);
	
	protected:
	
	private:
};
#endif

