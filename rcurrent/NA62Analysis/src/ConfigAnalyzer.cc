/*
 * AnalyzerParam.cc
 *
 *  Created on: Mar 19, 2015
 *      Author: ncl
 */

#include <ConfigAnalyzer.hh>

#include "Analyzer.hh"

namespace NA62Analysis {
namespace Configuration {

ConfigAnalyzer::ConfigAnalyzer() {
	/// \MemberDescr
	/// Default constructor
	/// \EndMemberDescr
}

ConfigAnalyzer::ConfigAnalyzer(const ConfigAnalyzer& c):
	ConfigParser(c)
{
	/// \MemberDescr
	/// \param c : Reference of the object to copy
	/// Copy constructor
	/// \EndMemberDescr
}

ConfigAnalyzer::~ConfigAnalyzer() {
	/// \MemberDescr
	/// Default destructor
	/// \EndMemberDescr
}

void ConfigAnalyzer::ApplyParams(Analyzer * const analyzer) const{
	/// \MemberDescr
	/// \param analyzer : pointer to the analyzer
	///
	/// Apply all the ParamName,ParamValue pairs to the specified analyzer
	/// \EndMemberDescr
	TString name = analyzer->GetAnalyzerName();
	if(NamespaceExists(name)){
		for(auto param : GetNamespace(name).GetParams()){
			analyzer->ApplyParam(param.first, param.second);
		}
	}
}

void ConfigAnalyzer::ParseCLI(TString params){
	/// \MemberDescr
	/// \param params : Parameter string passed in the command line
	///
	/// Parse command line interface parameter line
	/// \EndMemberDescr

	TObjArray *ans;
	TObjArray *pars, *values;
	TString paramsLine;

	TString paramValue, paramName;
	TString currentNS;

	ans = params.Tokenize("&");
	for(int i=0; i<ans->GetEntries(); i++){
		TObjArray *p = ((TObjString*)ans->At(i))->GetString().Tokenize(":");
		if(p->GetEntries()==2){
			//Got the analyzer
			currentNS = ((TObjString*)p->At(0))->GetString();
			fNSList.insert(NSPair(currentNS, ConfigNamespace(currentNS)));
			//Parse parameters now
			paramsLine = ((TObjString*)p->At(1))->GetString();
			pars = paramsLine.Tokenize(";");
			for(int j=0; j<pars->GetEntries(); j++){
				values = ((TObjString*)pars->At(j))->GetString().Tokenize("=");
				if(values->GetEntries()==2){
					paramName = ((TObjString*)values->At(0))->GetString();
					paramValue = ((TObjString*)values->At(1))->GetString();
					fNSList[currentNS].AddParam(paramName, paramValue);
				}
				else{
					std::cerr << "Configuration parser: Parameter name or value not specified " << pars->At(j)->GetName() << std::endl;
				}
				values->Delete();
				delete values;
			}
			pars->Delete();
			delete pars;
		}
		else{
			std::cerr << "Configuration parser: Parameters list not specified for analyzer " << p->At(i)->GetName() << std::endl;
		}
		p->Delete();
		delete p;
	}

	ans->Delete();
	delete ans;
}

std::map<TString, std::vector<TString> > NA62Analysis::Configuration::ConfigAnalyzer::GetFilterParams() const {
	/// \MemberDescr
	/// \return map containing the stream id and the list of analyzers requested for the stream
	///
	/// Read the filter section of the input file and extract the list of OutputStreams
	/// \EndMemberDescr
	std::map<TString, std::vector<TString> > listStreams;

	if(NamespaceExists("Filter")){
		for(auto param : GetNamespace("Filter").GetParams()){
			std::vector<TString> vAn;
			TObjArray* ans = param.second.Tokenize(" ");
			for(int i=0; i<ans->GetEntries(); i++){
				if(((TObjString*)ans->At(i))->GetString().CompareTo("*")!=0) // Means all filter analyzers
					vAn.push_back(((TObjString*)ans->At(i))->GetString());
			}
			listStreams.insert(std::pair<const TString, std::vector<TString> >(param.first, vAn));
			ans->Delete();
			delete ans;
		}
	}
	return listStreams;
}

std::vector<TString> NA62Analysis::Configuration::ConfigAnalyzer::GetStreamDisabledTrees(TString streamID) const {
	/// \MemberDescr
	/// \return vector containing the trees that should not be in the output for the given stream
	///
	/// Read the filter section of the input file and extract the list of disabled trees for the stream
	/// \EndMemberDescr
	std::vector<TString> listTrees;

	TString ns = Form("Filter_%s", streamID.Data());
	if(NamespaceExists(ns)){
		TString trees = GetNamespace(ns).GetParam("disabledtrees");
		TObjArray* ans = trees.Tokenize(" ");
		for(int i=0; i<ans->GetEntries(); i++){
			listTrees.push_back(((TObjString*)ans->At(i))->GetString());
		}
		ans->Delete();
		delete ans;
	}
	return listTrees;
}

} /* namespace Configuration */
} /* namespace NA62Analysis */

