/*
 * ConfigParser.cpp
 *
 *  Created on: Jun 5, 2013
 *      Author: nlurkin
 */

#include "ConfigParser.hh"

#include <string>
#include <iostream>
#include <fstream>
#include <stdio.h>

#include <TString.h>
#include <TPRegexp.h>
#include <TObjArray.h>
#include <TObjString.h>

extern char **environ;

namespace NA62Analysis {
namespace Configuration {

ConfigParser::ConfigParser() {
	/// \MemberDescr
	/// Default constructor
	/// \EndMemberDescr
}

ConfigParser::ConfigParser(const ConfigParser& c):
	fNSList(c.fNSList)
{
	/// \MemberDescr
	/// \param c : Reference of the object to copy
	/// Copy constructor
	/// \EndMemberDescr
}

ConfigParser::~ConfigParser() {
	/// \MemberDescr
	/// Default destructor
	/// \EndMemberDescr
}

void ConfigParser::ParseFile(TString const& fileName){
	/// \MemberDescr
	/// \param fileName : Path to the file to be parsed
	///
	/// Parse a configuration file.
	/// \EndMemberDescr

	std::ifstream s;
	std::string line;

	if(fileName.Length()>0){
		s.open(fileName);
		if(s.is_open()){
			while(s.good()){
				getline(s, line);
				if(line[0]==';') continue;
				ParseLine(line);
			}
		}
		else{
			std::cerr << "Configuration parser: Unable to open configuration file " << fileName << std::endl;
		}

		s.close();
	}
}

void ConfigParser::ParseEnv(TString const& prefix){
	/// \MemberDescr
	/// \param prefix: prefix of the environment variables to extract
	///
	/// Parse the environment variables, searching for variables in
	/// the format PREFIX_NAMESPACE_NAME, where PREFIX is the value
	/// of the input parameter.
	/// \EndMemberDescr

	TObjArray *vals = nullptr;
	TObjArray *var_elements = nullptr;

	//Read all environment variables
	for(char **current = environ; *current; current++) {
		TString entry(*current);

		//Split the variable name and value
		vals = entry.Tokenize("=");

		if(vals->GetEntries()!=2) {
			//Not the right format (XXX=val)
			vals->Delete();
			delete vals;
			continue;
		}
		TString var = ((TObjString*)vals->At(0))->String();
		TString val = ((TObjString*)vals->At(1))->String();

		// Is it one of the variables we are looking for? Format is PREFIX_NAMESPACE_VARIABLE
		if(var.Contains(prefix)){
			var_elements = var.Tokenize("_");
			if(var_elements->GetEntries()!=3){
				var_elements->Delete();
				vals->Delete();
				delete vals;
				delete var_elements;
				continue;
			}

			TString nsName = ((TObjString*)var_elements->At(1))->String();
			TString varName = ((TObjString*)var_elements->At(2))->String();

			if(!NamespaceExists(nsName))
				fNSList.insert(NSPair(nsName, ConfigNamespace(nsName)));

			fNSList[nsName].AddParam(varName, val);

			var_elements->Delete();
			delete var_elements;
		}

		vals->Delete();
		delete vals;
	}
}

bool ConfigParser::NamespaceExists(TString ns) const {
	/// \MemberDescr
	/// \param ns : Name of the namespace
	/// \return true if the namespace is found in the config file
	///
	/// Does the specified namespace exist in the config
	/// \EndMemberDescr

	return fNSList.count(ns)>0;
}

const ConfigNamespace& ConfigParser::GetNamespace(TString ns) const {
	/// \MemberDescr
	/// \param ns : Name of the namespace
	/// \return Reference to the namespace if found. A default empty namespace else.
	///
	/// \EndMemberDescr

	auto nsRef = fNSList.find(ns);

	if(nsRef != fNSList.end()) return nsRef->second;
	else return fDefault;
}

void ConfigParser::ParseLine(TString line){
	/// \MemberDescr
	/// \param line : line to be processed
	///
	/// Read the line and process it
	/// \EndMemberDescr

	TPRegexp commExp("(.*)#.*");
	TPRegexp anExp("^\\s*\\[(.*?)\\]");
	TPRegexp paramExp("(.*?)\\s*=\\s*(.*)");
	TPRegexp plotUpdateExp("AutoUpdate = (.*)");

	TObjArray *results;
	//TObjArray *params;

	TString paramName;
	TString paramValue;
	TString tempString;

	results = commExp.MatchS(line);
	if(results->GetEntries()>=2) tempString = ((TObjString*)results->At(1))->GetString();
	else tempString = line;
	results->Delete();
	delete results;

	results = anExp.MatchS(tempString);
	if(results->GetEntries()==2){
		//New namespace section
		fCurrentNS = ((TObjString*)results->At(1))->GetString();
		fNSList.insert(NSPair(fCurrentNS, ConfigNamespace(fCurrentNS)));
	}
	results->Delete();
	delete results;

	results = paramExp.MatchS(tempString);
	if(results->GetEntries()==3){
		paramName = ((TObjString*)results->At(1))->GetString();
		fNSList[fCurrentNS].AddParam(paramName, ((TObjString*)results->At(2))->GetString());
	}
	results->Delete();
	delete results;
}

void ConfigParser::Print() const{
	/// \MemberDescr
	/// Print all the Namespaces
	/// \EndMemberDescr

	for(auto &ns : fNSList){
		ns.second.Print();
	}
}

bool ConfigNamespace::ParamExists(TString paramName) const {
	/// \MemberDescr
	/// \param paramName : Parameter whose existence is checked
	/// \return True if the parameter exists in the namespace
	/// \EndMemberDescr

	return fParamsList.count(paramName)>0;
}

const std::map<TString, TString, NA62CaseIgnorecomp>& ConfigNamespace::GetParams() const {
	/// \MemberDescr
	/// \return Map of parameter-value pairs
	/// \EndMemberDescr

	return fParamsList;
}

TString ConfigNamespace::GetParam(TString name) const {
	/// \MemberDescr
	/// \param name : Parameter name
	/// \return Value of the given parameter (as TString). If the
	/// parameter does not exist, an empty string
	/// \EndMemberDescr

	auto paramRef = fParamsList.find(name);

	if(paramRef != fParamsList.end()) return paramRef->second;
	else return "";
}

void ConfigNamespace::SetValue(TString name, char& ref) const {
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()) ref = param->second.Data()[0];
}

void ConfigNamespace::SetValue(TString name, bool& ref) const {
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()){
		if(param->second.IsDigit()) ref = param->second.Atoi();
		else if(param->second.CompareTo("true", TString::kIgnoreCase)==0) ref = true;
		else if(param->second.CompareTo("false", TString::kIgnoreCase)==0) ref = false;
	}
}

void ConfigNamespace::SetValue(TString name, int &ref) const{
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()) ref = param->second.Atoi();
}

void ConfigNamespace::SetValue(TString name, long & ref) const {
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()) ref = param->second.Atoll();
}

void ConfigNamespace::SetValue(TString name, float& ref) const {
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()) ref = param->second.Atof();
}

void ConfigNamespace::SetValue(TString name, double& ref) const {
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()) ref = param->second.Atof();
}

void ConfigNamespace::SetValue(TString name, std::string& ref) const {
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()) ref = param->second.Data();
}

void ConfigNamespace::SetValue(TString name, TString& ref) const {
	/// \MemberDescr
	/// \param name : Name of the parameter
	/// \param ref : Reference to the variable to fill with the value of the parameter.
	///
	/// Fill a variable with the value of the parameter. If the parameter does not exist
	/// the variable is unchanged.
	/// \EndMemberDescr

	auto param = fParamsList.find(name);
	if(param != fParamsList.end()) ref = param->second;
}

void ConfigNamespace::Print() const{
	/// \MemberDescr
	/// Print all the parameter-value pairs
	/// \EndMemberDescr

	std::cout << "[" << fName << "]" << std::endl;
	for(auto &param : fParamsList){
		std::cout << param.first << " = " << param.second << std::endl;
	}
}


} /* namespace Configuration */
} /* namespace NA62Analysis */
