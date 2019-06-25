/*
 * Verbose.cc
 *
 *  Created on: Mar 20, 2015
 *      Author: ncl
 */

#include "Verbose.hh"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>

#include "ConfigSettings.hh"

namespace NA62Analysis {

Verbose::Verbose() :
	fLocalVerbosityActive(false),
	fPrintPrefix(false),
	fLocalCoreVerbosityLevel(Verbosity::kAlways),
	fCoreVerbosityTest(Verbosity::kAlways),
	fLocalAnVerbosityLevel(Verbosity::kUserAlways),
	fAnVerbosityTest(Verbosity::kUserAlways),
	fModuleName("NA62Analysis"),
	fCurrentStream(&std::cout)
{
	/// \MemberDescr
	/// Constructor
	/// \EndMemberDescr
}

Verbose::Verbose(const std::string &name) :
	fLocalVerbosityActive(false),
	fPrintPrefix(false),
	fLocalCoreVerbosityLevel(Verbosity::kAlways),
	fCoreVerbosityTest(Verbosity::kAlways),
	fLocalAnVerbosityLevel(Verbosity::kUserAlways),
	fAnVerbosityTest(Verbosity::kUserAlways),
	fModuleName(name),
	fCurrentStream(&std::cout)
{
	/// \MemberDescr
	/// \param name : Module display name
	///
	/// Constructor with name
	/// \EndMemberDescr
}

Verbose::~Verbose() {
	/// \MemberDescr
	/// Destructor
	/// \EndMemberDescr
}

void Verbose::SetVerbosity(Verbosity::CoreVerbosityLevel vcore, Verbosity::AnalyzerVerbosityLevel van){
	/// \MemberDescr
	/// \param v : Verbosity value
	///
	/// Change local verbosity
	/// \EndMemberDescr

	std::cout << extended() << "Setting verbosity to " << (van*10+vcore) << std::endl;
	fLocalVerbosityActive = true;
	fLocalCoreVerbosityLevel = vcore;
	fLocalAnVerbosityLevel = van;
}

void Verbose::SetVerbosity(Verbosity::CoreVerbosityLevel vcore){
	/// \MemberDescr
	/// \param v : Verbosity value
	///
	/// Change local verbosity
	/// \EndMemberDescr

	std::cout << extended() << "Setting verbosity to " << (fLocalAnVerbosityLevel*10+vcore) << std::endl;
	fLocalVerbosityActive = true;
	fLocalCoreVerbosityLevel = vcore;
}

void Verbose::SetVerbosity(Verbosity::AnalyzerVerbosityLevel van){
	/// \MemberDescr
	/// \param v : Verbosity value
	///
	/// Change local verbosity
	/// \EndMemberDescr

	std::cout << extended() << "Setting verbosity to " << (van*10+fLocalCoreVerbosityLevel) << std::endl;
	fLocalVerbosityActive = true;
	fLocalAnVerbosityLevel = van;
}

void Verbose::SetGlobalVerbosity(Verbosity::CoreVerbosityLevel vcore, Verbosity::AnalyzerVerbosityLevel van){
	/// \MemberDescr
	/// \param v : Verbosity value
	///
	/// Change global verbosity
	/// \EndMemberDescr

	std::cout << always() << "Setting global verbosity to " << (van*10+vcore) << std::endl;
	fCoreVerbosityLevel = vcore;
	fAnalyzerVerbosityLevel = van;
}

const Verbose& operator<<(const Verbose &level, std::ostream& (*f)(std::ostream&)) {
	/// \MemberDescr
	/// \param level : Reference to Verbose class
	/// \param f : std::ostream manipulator (endl, flush, ...)
	///
	/// Apply manipulator to output stream if requested verbosity level is at
	/// least equal to the verbosity level.
	/// \EndMemberDescr

	if(level.CanPrint()) level.GetStream() << f;
	return level;
}

const Verbose& operator <<(std::ostream& s, const Verbose &level) {
	/// \MemberDescr
	/// \param s : Output stream
	/// \param level : Reference to Verbose class
	///
	/// Start verbose printing to the specified output stream. Print
	/// requested verbosity level and module name if requested verbosity
	/// level is at least equal to verbosity level.
	/// \EndMemberDescr

	if(Verbose::fLogToFile) level.SetStream(Verbose::fLogFileStream);
	else level.SetStream(s);

	if(level.CanPrint())
		level.GetStream() << level.GetPrefix();

	return level;
}

bool Verbose::TestLevel(Verbosity::CoreVerbosityLevel level) const {
	/// \MemberDescr
	/// \param level : Tested verbosity level
	/// \return True if level is at least equal to the verbosity level
	/// \EndMemberDescr


	if(fLocalVerbosityActive && (level<=fLocalCoreVerbosityLevel)) return true;
	else if(!fLocalVerbosityActive && (level<=fCoreVerbosityLevel)) return true;
	else return false;
}

bool Verbose::TestLevel(Verbosity::AnalyzerVerbosityLevel level) const {
	/// \MemberDescr
	/// \param level : Tested verbosity level
	/// \return True if level is at least equal to the verbosity level
	/// \EndMemberDescr


	if(fLocalVerbosityActive && (level<=fLocalAnVerbosityLevel)) return true;
	else if(!fLocalVerbosityActive && (level<=fAnalyzerVerbosityLevel)) return true;
	else return false;
}

bool Verbose::CanPrint() const {
	/// \MemberDescr
	/// \return True if the currently used verbosity level is at least
	/// equal to the verbosity level.
	/// \EndMemberDescr

	return TestLevel(fCoreVerbosityTest) || TestLevel(fAnVerbosityTest);
}

std::string Verbose::GetPrefix() const {
	/// \MemberDescr
	/// \return True if the currently used verbosity level is at least
	/// equal to the verbosity level.
	/// \EndMemberDescr

	std::stringstream ss;
	if(fPrintPrefix){
		bool symbol = false;
		bool atLeastOne = false;
		for(auto l : Configuration::ConfigSettings::global::fOutputPrefixFormat){
			if(l=='%')
				symbol = true;
			else{
				atLeastOne = true;
				if(!symbol)
					ss << l;
				else{
					switch(l){
					case 'd':
						ss << PrintDate();
						break;
					case 'l':
						ss << std::left << std::setw(6) << Verbose::GetVerbosityLevelName(fCoreVerbosityTest);
						break;
					case 'n':
						ss << std::left << std::setw(15) << fModuleName;
						break;
					default:
						break;
					}
					symbol = false;
				}
			}
		}
		if(atLeastOne)
			ss << " ";
			 fPrintPrefix = false;
	}
	return ss.str();
}

std::string Verbose::GetVerbosityLevelName(Verbosity::CoreVerbosityLevel v) {
	/// \MemberDescr
	/// \param v : Verbosity level
	/// \return String corresponding to the verbosity level
	/// \EndMemberDescr

	if(v==Verbosity::kNormal) return "NORMAL";
	else if(v==Verbosity::kExtended) return "EXTEND";
	else if(v==Verbosity::kDebug) return "DEBUG";
	else if(v==Verbosity::kTrace) return "TRACE";
	return ""; //Do not happen
}

std::string Verbose::GetVerbosityLevelName(Verbosity::AnalyzerVerbosityLevel v) {
	/// \MemberDescr
	/// \param v : Verbosity level
	/// \return String corresponding to the verbosity level
	/// \EndMemberDescr

	if(v==Verbosity::kUserNormal) return "USERN";
	else if(v==Verbosity::kUser) return "USER";
	return ""; //Do not happen
}

Verbosity::CoreVerbosityLevel Verbose::GetCoreVerbosityLevelFromInt(int v){
	return static_cast<Verbosity::CoreVerbosityLevel>(v % 10);
}

Verbosity::AnalyzerVerbosityLevel Verbose::GetAnalyzerVerbosityLevelFromInt(int v){
	return static_cast<Verbosity::AnalyzerVerbosityLevel>(v / 10);
}

void Verbose::SetLogToFile(TString fileName) {
	/// \MemberDescr
	/// \param fileName : Path to the log file
	///
	/// Request redirection of all log messages to log file
	/// \EndMemberDescr

	Verbose::fLogToFile = true;
	fLogFileStream.open(fileName.Data(), std::ofstream::out);
	std::cout << "Logging to file " << fileName << std::endl;
}

std::string Verbose::PrintDate() const {
	std::string format = Configuration::ConfigSettings::global::fDateTimeFormat;
	if(format.compare("-")==0)
		return "";
	time_t t = time(0);
	struct tm *ptm = std::localtime(&t);
	char buffer[80];
	strftime(buffer, 80, format.c_str(), ptm);
	return std::string(buffer);
}

Verbosity::CoreVerbosityLevel Verbose::fCoreVerbosityLevel = Verbosity::kAlways;
Verbosity::AnalyzerVerbosityLevel Verbose::fAnalyzerVerbosityLevel = Verbosity::kUserAlways;
bool Verbose::fLogToFile = false;
std::ofstream Verbose::fLogFileStream;

} /* namespace NA62Analysis */

