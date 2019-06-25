/*
 * G4ParticleInterface.h
 *
 *  Created on: Aug 13, 2013
 *      Author: nlurkin
 */

#ifndef PARTICLEINTERFACE_H_
#define PARTICLEINTERFACE_H_

#include <TString.h>
#include <TDatabasePDG.h>

namespace NA62Analysis {

/// \class NA62Particle
/// \Brief
/// Proxy to TParticlePDG but returns standard NA62 Units
/// \EndBrief
///
/// \Detailed
/// Proxy to TParticlePDG but returns standard NA62 Units (masses and width in MeV)
/// \EndDetailed
class NA62Particle: TParticlePDG {
public:
	explicit NA62Particle(TParticlePDG *pdg) :
			TParticlePDG(*pdg) {
		/// \MemberDescr
		/// \param pdg : pointer to the TParticlePDG
		/// Constructor. Create proxy for the given TParticlePDG (not owned)
		/// \EndMemberDescr
	}
	NA62Particle() {
	}
	;
	virtual ~NA62Particle() {
		/// \MemberDescr
		/// Destructor
		/// \EndMemberDescr
	}
	;

	Double_t Mass() const {
		/// \MemberDescr
		/// \return Particle mass in MeV
		/// \EndMemberDescr
		return TParticlePDG::Mass() * 1000.;
	}
	Double_t Width() const {
		/// \MemberDescr
		/// \return Particle width in MeV
		/// \EndMemberDescr
		return TParticlePDG::Width() * 1000.;
	}
};

/// \class ParticleInterface
/// \Brief
/// Interface class to Root particle database
/// \EndBrief
///
/// \Detailed
/// Use it to get particle properties from ROOT.
/// \EndDetailed

class ParticleInterface {
public:
	virtual ~ParticleInterface();
	static ParticleInterface* GetParticleInterface();

	const NA62Particle FindParticle(int pdgID) const;
	const NA62Particle FindParticle(TString name) const;

	int GetParticlePDGid(TString name) const;
	TString GetParticleName(int pdgID) const;

	void PrintTable() const;
private:
	ParticleInterface();
	ParticleInterface(const ParticleInterface&); ///< Non implemented copy-constructor (prevents copy)
	ParticleInterface& operator=(const ParticleInterface&); ///< Non implemented copy-assignment operator (prevents copy)

	TDatabasePDG *fTable; ///< Pointer to TDatabasePDG instance
	static ParticleInterface *fParticleInterface; ///< static pointer to unique instance of the class
};

} /* namespace NA62Analysis */

#endif /* G4PARTICLE_H_ */
