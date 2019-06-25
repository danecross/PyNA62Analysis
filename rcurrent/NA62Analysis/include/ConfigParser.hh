/*
 * ConfigParser.h
 *
 *  Created on: Jun 5, 2013
 *      Author: nlurkin
 */

#ifndef CONFIGPARSER_H_
#define CONFIGPARSER_H_

#include <map>

#include <TString.h>

#include "containers.hh"

namespace NA62Analysis {
namespace Configuration {

/// \class ConfigNamespace
/// \Brief
/// Configuration namespace.
/// \EndBrief
///
/// \Detailed
/// Configuration namespace. It contains all the parameter-value pairs
/// found in the configuration file.
/// \EndDetailed

typedef std::pair<const TString, TString> ParamPair;
class ConfigNamespace {
public:
	explicit ConfigNamespace(TString name) : fName(name){
		/// \MemberDescr
		/// \param name : Name of the namespace
		///
		/// Constructor
		/// \EndMemberDescr
	};
	ConfigNamespace(){
		/// \MemberDescr
		/// Default constructor
		/// \EndMemberDescr
	};

	bool ParamExists(TString paramName) const;
	void AddParam(TString name, TString value){
		/// \MemberDescr
		/// \param name : Parameter name
		/// \param value : Parameter value
		///
		/// Add a new parameter-value pair in the namespace
		/// \EndMemberDescr

		fParamsList[name] = value;
	}
	const std::map<TString, TString, NA62CaseIgnorecomp>& GetParams() const;
	TString GetParam(TString name) const;

	void SetValue(TString name, char &ref) const;
	void SetValue(TString name, bool &ref) const;
	void SetValue(TString name, int &ref) const;
	void SetValue(TString name, long &ref) const;
	void SetValue(TString name, float &ref) const;
	void SetValue(TString name, double &ref) const;
	void SetValue(TString name, std::string &ref) const;
	void SetValue(TString name, TString &ref) const;

	void Print() const;
private:
	TString fName; ///< Name of the namespace
	std::map<TString, TString, NA62CaseIgnorecomp> fParamsList; ///< Container for the parameter-value pairs
};

typedef std::pair<const TString, ConfigNamespace> NSPair;

/// \class ConfigParser
/// \Brief 
/// This class will parse a configuration file and environment variables and create a repository of pairs (param,value)
/// for each namespace.
/// \EndBrief 
///
/// \Detailed
/// It can parse strings passed from configuration files that looks like
/// The configuration file looks like
/// \code
/// 	[namespace1]
/// 	param1 = val1
/// 	param2 = val2
///
/// 	[namespace2]
/// 	param1 = value1
/// 	paramx = valuex
///
/// 	...
/// \endcode
/// The parameters and values are accessible in their namespace.
///
/// THe configuration file can be completed/replaced with environment variables in the format:
/// \code
///     PREFIX_NAMESPACE1_PARAM1=val1
///     PREFIX_NAMESPACE2_PARAM1=value1
/// \endcode
///
/// Be careful of the order in which the ParseFile and ParseEnv methods are called as if the same variable (in the
/// same namespace) is declared both ways, the second occurrence will overwrite the value of the first occurrence.
/// \EndDetailed

class ConfigParser {
public:
	ConfigParser();
	ConfigParser(const ConfigParser& c);
	virtual ~ConfigParser();

	virtual void ParseFile(TString const& fileName);
	virtual void ParseEnv(TString const& prefix);
	virtual void Print() const;

	bool NamespaceExists(TString ns) const;
	const ConfigNamespace& GetNamespace(TString ns) const;

protected:
	void ParseLine(TString line);

	std::map<TString, ConfigNamespace, NA62CaseIgnorecomp> fNSList;	///< Map of namespaces
	ConfigNamespace fDefault; ///< Default namespace
	TString fCurrentNS; ///< Transient. Currently processed namespace
};

} /* namespace Configuration */
} /* namespace NA62Analysis */

#endif /* CONFIGPARSER_H_ */