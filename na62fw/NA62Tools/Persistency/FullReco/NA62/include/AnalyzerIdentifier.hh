/*
 * AnalyzerIdentifier.hh
 *
 *  Created on: Feb 29, 2016
 *      Author: nlurkin
 */

#ifndef INCLUDE_ANALYZERIDENTIFIER_HH_
#define INCLUDE_ANALYZERIDENTIFIER_HH_

#include <TObjString.h>
#include <TObject.h>

namespace NA62Analysis {
namespace Core {

/// \class AnalyzerIdentifier
/// \Brief
/// Class used for storage in ROOT file containing identifier for an analyzer.
/// \EndBrief
///
/// \Detailed
/// Contains currently only the name of the analyzer but can easily be extended
/// to include version number, unique identifier, ...
/// \EndDetailed
class AnalyzerIdentifier: public TObject {
public:
	AnalyzerIdentifier();
	explicit AnalyzerIdentifier(TString name);
	virtual ~AnalyzerIdentifier();

	void print() const;

	bool operator==(TString value);

	const TObjString& GetAnalyzerName() const {	return fAnalyzerName;	}
private:
	TObjString fAnalyzerName; ///< Name of the analyzer

	ClassDef(AnalyzerIdentifier,1);
};

static const AnalyzerIdentifier ANIDNone("None");
} /* namespace Core */
} /* namespace NA62Analysis */

#endif /* INCLUDE_ANALYZERIDENTIFIER_HH_ */
