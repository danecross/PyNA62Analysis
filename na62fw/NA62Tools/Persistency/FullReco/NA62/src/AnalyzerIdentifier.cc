/*
 * AnalyzerIdentifier.cc
 *
 *  Created on: Feb 29, 2016
 *      Author: nlurkin
 */

#include "AnalyzerIdentifier.hh"

#include <iostream>

ClassImp(NA62Analysis::Core::AnalyzerIdentifier)

namespace NA62Analysis {
namespace Core {

AnalyzerIdentifier::AnalyzerIdentifier() {
	/// \MemberDescr
	/// Default constructor
	/// \EndMemberDescr
}

AnalyzerIdentifier::AnalyzerIdentifier(TString name) :
		fAnalyzerName(name){
	/// \MemberDescr
	/// \param name : Name of the analyzer
	///
	/// Constructor
	/// \EndMemberDescr
}

AnalyzerIdentifier::~AnalyzerIdentifier() {
	/// \MemberDescr
	/// Default destructor
	/// \EndMemberDescr
}

void AnalyzerIdentifier::print() const {
	/// \MemberDescr
	/// Print identifier
	/// \EndMemberDescr
	std::cout << fAnalyzerName.GetString() << std::endl;
}

bool AnalyzerIdentifier::operator ==(TString value) {
	/// \MemberDescr
	/// \param value : String to compare to
	/// \return True of the string corresponds to the analyzer name
	///
	/// Comparison operator.
	/// \EndMemberDescr

	if (value.CompareTo(fAnalyzerName.GetString()) == 0)
		return true;

	return false;
}


} /* namespace Core */
} /* namespace NA62Analysis */

