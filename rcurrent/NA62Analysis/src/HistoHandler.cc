/*
 * HistoHandler.cc
 *
 *  Created on: 21 Jan 2014
 *      Author: ncl
 */

#include "HistoHandler.hh"

#include <iostream>

#include <TFile.h>
#include <TGraphQQ.h>
#include <TF1.h>

#include "StringTable.hh"
#include "functions.hh"
#include "ConfigSettings.hh"

namespace NA62Analysis {
namespace Core {

HistoHandler::HistoHandler():
			fUpdateRate(Configuration::ConfigSettings::global::fDefaultAutoUpdateRate)
{
	/// \MemberDescr
	/// Constructor
	/// \EndMemberDescr
}

HistoHandler::HistoHandler(const HistoHandler& c):
			fHisto(c.fHisto),
			fCanvas(c.fCanvas),
			fOutTree(c.fOutTree),
			fHistoOrder(c.fHistoOrder),
			fAutoUpdateList(c.fAutoUpdateList),
			fUpdateRate(c.fUpdateRate)
{
	/// \MemberDescr
	/// \param c : Reference of the object to copy
	///
	/// Copy constructor
	/// \EndMemberDescr
}

HistoHandler::~HistoHandler() {
	/// \MemberDescr
	/// Destructor. Delete all the histograms and canvases.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator it1;
	NA62Analysis::NA62Map<TString,CanvasOrganizer*>::type::iterator it5;

	for(it1=fHisto.begin(); it1!=fHisto.end(); ++it1){
		if(!it1->second.IsOwner())
			continue;
		plotType_e type = it1->second.GetType();
		if(type==TTH1 || type==TTH2 || type==TTH3)
			delete it1->second.GetHisto();
		else if(type==TTGraph)
			delete it1->second.GetGraph();
		else if(type==TTEfficiency)
			delete it1->second.GetEfficency();
		it1->second.Clear();
	}
	for(it5=fCanvas.begin(); it5!=fCanvas.end(); ++it5){
		delete it5->second;
	}
}

//void HistoHandler::BookHisto(TString name, TH1* const histo, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param name : Name of the histogram
//	/// \param histo : Pointer to the histogram
//	/// \param analyzerName : Name of the analyzer calling the method
//	/// \param refresh : Set the plot as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
//	///
//	/// Book a new histogram and make it available in the whole analyzer
//	/// \EndMemberDescr
//
//	fHistoOrder.push_back(name);
//	fHisto.insert(std::pair<TString,THObject>(name, THObject(histo)));
//	if(refresh) SetPlotAutoUpdate(name, analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//}
//
//void HistoHandler::BookHisto(TString name, TH2* histo, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param name : Name of the histogram
//	/// \param histo : Pointer to the histogram
//	/// \param analyzerName : Name of the analyzer calling the method
//	/// \param refresh : Set the plot as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
//	///
//	/// Book a new histogram and make it available in the whole analyzer
//	/// \EndMemberDescr
//
//	fHistoOrder.push_back(name);
//	fHisto.insert(std::pair<TString,THObject>(name, THObject(histo)));
//	if(refresh) SetPlotAutoUpdate(name, analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//}
//
//void HistoHandler::BookHisto(TString name, TH3* histo, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param name : Name of the histogram
//	/// \param histo : Pointer to the histogram
//	/// \param analyzerName : Name of the analyzer calling the method
//	/// \param refresh : Set the plot as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
//	///
//	/// Book a new histogram and make it available in the whole analyzer
//	/// \EndMemberDescr
//
//	fHistoOrder.push_back(name);
//	fHisto.insert(std::pair<TString,THObject>(name, THObject(histo)));
//	if(refresh) SetPlotAutoUpdate(name, analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//}
//
//void HistoHandler::BookHisto(TString name, TGraph* histo, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param name : Name of the histogram
//	/// \param histo : Pointer to the histogram
//	/// \param analyzerName : Name of the analyzer calling the method
//	/// \param refresh : Set the plot as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
//	///
//	/// Book a new histogram and make it available in the whole analyzer
//	/// \EndMemberDescr
//
//	fHistoOrder.push_back(name);
//	fHisto.insert(std::pair<TString,THObject>(name, THObject(histo)));
//	histo->SetNameTitle(name, name);
//	if(refresh) SetPlotAutoUpdate(name, analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//}
//
//void HistoHandler::BookHisto(TString name, TEfficiency* histo, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param name : Name of the histogram
//	/// \param histo : Pointer to the histogram
//	/// \param analyzerName : Name of the analyzer calling the method
//	/// \param refresh : Set the plot as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
//	///
//	/// Book a new histogram and make it available in the whole analyzer
//	/// \EndMemberDescr
//
//	fHistoOrder.push_back(name);
//	fHisto.insert(std::pair<TString,THObject>(name, THObject(histo)));
//	//histo->SetNameTitle(name, name); // destroys original names: EG 15/6/16
//	if(refresh) SetPlotAutoUpdate(name, analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//}

//void HistoHandler::BookHistoArray(TString baseName, TH1* histo, int number, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param baseName : Name of the histogram. The index will be appended
//	/// \param histo : Pointer to the histogram to replicate
//	/// \param analyzerName : Name of the analyzer calling the method
//	///	\param number : Number of histograms to create
//	/// \param refresh : Set the plots as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
//	///
//	/// Book an array of similar histograms and make it available in the whole analyzer.
//	/// \EndMemberDescr
//
//	TString name, title;
//	name = histo->GetName();
//	title = histo->GetTitle();
//	TH1* h;
//
//	histo->SetName(TString(name + "0").Data());
//	histo->SetTitle(TString(title + "0").Data());
//	fHistoOrder.push_back(baseName+"0");
//	fHisto.insert(std::pair<TString,THObject>(baseName + "0", histo));
//	if(refresh) SetPlotAutoUpdate(baseName + "0", analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	for(int i=1; i<number; i++){
//		h = (TH1*)histo->Clone();
//		h->SetName(TString(name + (Long_t)i).Data());
//		h->SetTitle(TString(title + (Long_t)i).Data());
//		fHistoOrder.push_back(baseName + (Long_t)i);
//		fHisto.insert(std::pair<TString,THObject>(baseName + (Long_t)i, THObject(h)));
//		if(refresh) SetPlotAutoUpdate(baseName + (Long_t)i, analyzerName);
//		if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	}
//}
//
//void HistoHandler::BookHistoArray(TString baseName, TH2* histo, int number, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param baseName : Name of the histogram. The index will be appended
//	/// \param histo : Pointer to the histogram to replicate
//	/// \param analyzerName : Name of the analyzer calling the method
//	///	\param number : Number of histograms to create
//	/// \param refresh : Set the plots as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
//	///
//	/// Book an array of similar histograms and make it available in the whole analyzer.
//	/// \EndMemberDescr
//
//	TString name, title;
//	name = histo->GetName();
//	title = histo->GetTitle();
//	TH2* h;
//
//	histo->SetName(TString(name + "0").Data());
//	histo->SetTitle(TString(title + "0").Data());
//	fHistoOrder.push_back(baseName + "0");
//	fHisto.insert(std::pair<TString,THObject>(baseName + "0", histo));
//	if(refresh) SetPlotAutoUpdate(baseName + "0", analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	for(int i=1; i<number; i++){
//		h = (TH2*)histo->Clone();
//		h->SetName(TString(name + (Long_t)i).Data());
//		h->SetTitle(TString(title + (Long_t)i).Data());
//		fHistoOrder.push_back(baseName + (Long_t)i);
//		fHisto.insert(std::pair<TString,THObject>(baseName + (Long_t)i, THObject(h)));
//		if(refresh) SetPlotAutoUpdate(baseName + (Long_t)i, analyzerName);
//		if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	}
//}
//
//void HistoHandler::BookHistoArray(TString baseName, TH3* histo, int number, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param baseName : Name of the histogram. The index will be appended
//	/// \param histo : Pointer to the histogram to replicate
//	/// \param analyzerName : Name of the analyzer calling the method
//	///	\param number : Number of histograms to create
//	/// \param refresh : Set the plots as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
//	///
//	/// Book an array of similar histograms and make it available in the whole analyzer.
//	/// \EndMemberDescr
//
//	TString name, title;
//	name = histo->GetName();
//	title = histo->GetTitle();
//	TH3* h;
//
//	histo->SetName(TString(name + "0").Data());
//	histo->SetTitle(TString(title + "0").Data());
//	fHistoOrder.push_back(baseName + "0");
//	fHisto.insert(std::pair<TString,THObject>(baseName + "0", histo));
//	if(refresh) SetPlotAutoUpdate(baseName + "0", analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	for(int i=1; i<number; i++){
//		h = (TH3*)histo->Clone();
//		h->SetName(TString(name + (Long_t)i).Data());
//		h->SetTitle(TString(title + (Long_t)i).Data());
//		fHistoOrder.push_back(baseName + (Long_t)i);
//		fHisto.insert(std::pair<TString,THObject>(baseName + (Long_t)i, THObject(h)));
//		if(refresh) SetPlotAutoUpdate(baseName + (Long_t)i, analyzerName);
//		if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	}
//}
//
//void HistoHandler::BookHistoArray(TString baseName, TGraph* histo, int number, TString analyzerName, bool refresh, TString directory){
//	/// \MemberDescr
//	/// \param baseName : Name of the histogram. The index will be appended
//	/// \param histo : Pointer to the histogram to replicate
//	/// \param analyzerName : Name of the analyzer calling the method
//	///	\param number : Number of histograms to create
//	/// \param refresh : Set the plots as AutoUpdate
//	/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
//	///
//	/// Book an array of similar histograms and make it available in the whole analyzer.
//	/// \EndMemberDescr
//
//	TString name, title;
//	name = histo->GetName();
//	title = histo->GetTitle();
//	TGraph* h;
//
//	histo->SetName(TString(name + "0").Data());
//	histo->SetTitle(TString(title + "0").Data());
//	fHistoOrder.push_back(baseName+ "0");
//	fHisto.insert(std::pair<TString,TGraph*>(baseName + "0", histo));
//	if(refresh) SetPlotAutoUpdate(baseName + "0", analyzerName);
//	if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	for(int i=1; i<number; i++){
//		h = (TGraph*)histo->Clone();
//		h->SetName(TString(name + (Long_t)i).Data());
//		h->SetTitle(TString(title + (Long_t)i).Data());
//		fHistoOrder.push_back(baseName + (Long_t)i);
//		fHisto.insert(std::pair<TString,TGraph*>(baseName + (Long_t)i, h));
//		if(refresh) SetPlotAutoUpdate(baseName + (Long_t)i, analyzerName);
//		if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
//	}
//}

void HistoHandler::FillHisto(TString name, TString x, double w){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		plotType_e type = ptr->second.GetType();
		if(type==TTH1)
			ptr->second.GetHisto()->Fill(x,w);
		else if(type==TTH2)
			std::cerr << name << " is a TH2. Cannot call with (TString,double)." << std::endl;
		else if(type==TTH3)
			std::cerr << name << " is a TH3. Cannot call with (TString,double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (TString,double)." << std::endl;
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (TString,double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, TString x, double y, double w){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		plotType_e type = ptr->second.GetType();
		if(type==TTH2)
			ptr->second.GetHisto2()->Fill(x,y,w);
		else if(type==TTH1)
			std::cerr << name << " is a TH1. Cannot call with (TString,double,double)." << std::endl;
		else if(type==TTH3)
			std::cerr << name << " is a TH3. Cannot call with (TString,double,double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (TString,double,double)." << std::endl;
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (TString,double,double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, TString x, TString y, double w){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()) {
		plotType_e type = ptr->second.GetType();
		if(type==TTH2)
			ptr->second.GetHisto2()->Fill(x,y,w);
		else if(type==TTH1)
			std::cerr << name << " is a TH1. Cannot call with (TString,TString,double)." << std::endl;
		else if(type==TTH3)
			std::cerr << name << " is a TH3. Cannot call with (TString,TString,double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (TString,TString,double)." << std::endl;
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (TString,TString,double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, TString x, TString y, TString z, double w){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param z : z
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		plotType_e type = ptr->second.GetType();
		if(type==TTH3)
			ptr->second.GetHisto3()->Fill(x,y,z,w);
		else if(type==TTH1)
			std::cerr << name << " is a TH1. Cannot call with (TString,TString,TString,double)." << std::endl;
		else if(type==TTH2)
			std::cerr << name << " is a TH3. Cannot call with (TString,TString,TString,double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (TString,TString,TString,double)." << std::endl;
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (TString,TString,TString,double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, double x){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	///
	/// Fill a previously booked histogram with a weight of 1
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		plotType_e type = ptr->second.GetType();
		if(type==TTH1)
			ptr->second.GetHisto()->Fill(x,1);
		else if(type==TTH2)
			std::cerr << name << " is a TH2. Cannot call with (double)." << std::endl;
		else if(type==TTH3)
			std::cerr << name << " is a TH3. Cannot call with (double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (double)." << std::endl;
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, double x, double y, double w){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		plotType_e type = ptr->second.GetType();
		if(type==TTH2)
			ptr->second.GetHisto2()->Fill(x,y,w);
		else if(type==TTH3)
			ptr->second.GetHisto3()->Fill(x,y,w);
		else if(type==TTH1)
			std::cerr << name << " is a TH1. Cannot call with (double,double,double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (double,double,double)." << std::endl;
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (double,double,double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, double x, double y, double z, double w){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param z : z
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()) {
		plotType_e type = ptr->second.GetType();
		if(type==TTH3)
			ptr->second.GetHisto3()->Fill(x,y,z,w);
		else if(type==TTH1)
			std::cerr << name << " is a TH1. Cannot call with (double,double,double,double)." << std::endl;
		else if(type==TTH2)
			std::cerr << name << " is a TH2. Cannot call with (double,double,double,double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (double,double,double,double)." << std::endl;
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (double,double,double,double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, double x, double y){
	/// \MemberDescr
	/// \param name : Name of the histogram
	/// \param x : abscissa
	/// \param y : ordinate
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()) {
		plotType_e type = ptr->second.GetType();
		if(type==TTH1)
			ptr->second.GetHisto()->Fill(x,y);
		else if(type==TTH2)
			ptr->second.GetHisto2()->Fill(x,y,1);
		else if(type==TTH3)
			std::cerr << name << " is a TH2. Cannot call with (double,double)." << std::endl;
		else if(type==TTGraph)
			ptr->second.GetGraph()->SetPoint(ptr->second.IncrementPoints(), x, y);
		else if(type==TTEfficiency)
			std::cerr << name << " is a TEfficiency. Cannot call with (double,double)." << std::endl;
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

void HistoHandler::FillHisto(TString name, bool b, double x, double y, double z){
	/// \MemberDescr
	/// \param name : Name of the Efficiency histogram
	/// \param b : flag whether the current event passed the selection
	/// \param x : x-value
	/// \param y : y-value (use default=0 for 1-D efficiencies)
	/// \param z : z-value (use default=0 for 2-D or 1-D efficiencies)
	///
	/// Fill a previously booked efficiency histogram
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()) {
		plotType_e type = ptr->second.GetType();
		if(type==TTH1)
			std::cerr << name << " is a TH1. Cannot call with (bool,double,double,double)." << std::endl;
		else if(type==TTH2)
			std::cerr << name << " is a TH2. Cannot call with (bool,double,double,double)." << std::endl;
		else if(type==TTH3)
			std::cerr << name << " is a TH3. Cannot call with (bool,double,double,double)." << std::endl;
		else if(type==TTGraph)
			std::cerr << name << " is a TGraph. Cannot call with (bool,double,double,double)." << std::endl;
		else if(type==TTEfficiency)
			ptr->second.GetEfficency()->Fill(b, x, y, z);
	}
	else if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " doesn't exist." << std::endl;
}

//########################################
void HistoHandler::FillHistoArray(TString baseName, int index, TString x, double w){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index, x, w);
}

void HistoHandler::FillHistoArray(TString baseName, int index, TString x, double y, double w){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index, x, y, w);
}

void HistoHandler::FillHistoArray(TString baseName, int index, TString x, TString y, double w){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index, x, y, w);
}

void HistoHandler::FillHistoArray(TString baseName, int index, TString x, TString y, TString z, double w){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param z : z
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index, x, y, z, w);
}

void HistoHandler::FillHistoArray(TString baseName, int index, double x){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	///
	/// Fill a previously booked histogram with a weight of 1
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index, x);
}

void HistoHandler::FillHistoArray(TString baseName, int index, double x, double y, double w){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index,x,y,w);
}

void HistoHandler::FillHistoArray(TString baseName, int index, double x, double y, double z, double w){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	/// \param y : ordinate
	/// \param z : z
	/// \param w : weight
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index,x,y,z,w);
}

void HistoHandler::FillHistoArray(TString baseName, int index, double x, double y){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param x : abscissa
	/// \param y : ordinate
	///
	/// Fill a previously booked histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index, x, y);
}

void HistoHandler::FillHistoArray(TString baseName, int index, bool b, double x, double y, double z){
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	/// \param b : flag whether the current event passed the selection
	/// \param x : x-value
	/// \param y : y-value (use default=0 for 1-D efficiencies)
	/// \param z : z-value (use default=0 for 2-D or 1-D efficiencies)
	///
	/// Fill a previously booked efficiency histogram
	/// \EndMemberDescr

	FillHisto(baseName + (Long_t)index, b, x, y, z);
}

void HistoHandler::PrintInitSummary() const{
	/// \MemberDescr
	///
	/// Print a list of booked histograms.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::const_iterator it;

	StringTable histoTable("List of booked histograms");

	TString sDetAcc;

	//Fill histograms table
	histoTable.AddColumn("th1", "1D Histograms");
	histoTable.AddColumn("auth1", "AU");
	histoTable.AddColumn("th2", "2D Histograms");
	histoTable.AddColumn("auth2", "AU");
	histoTable.AddColumn("th3", "3D Histograms");
	histoTable.AddColumn("auth3", "AU");
	histoTable.AddColumn("gr", "Graphs");
	histoTable.AddColumn("augr", "AU");
	histoTable.AddColumn("eff", "Efficiencies");
	histoTable.AddColumn("aueff", "AU");
	histoTable << sepr;
	for(it = fHisto.begin(); it != fHisto.end(); ++it){
		plotType_e type = it->second.GetType();
		if(type==TTH1){
			histoTable.AddValue(0, it->first);
			if(fAutoUpdateList.count(it->first)>0) histoTable.AddValue(1, "x");
			else histoTable.AddValue(1, "");
		}
		else if(type==TTH2) {
			histoTable.AddValue(2, it->first);
			if(fAutoUpdateList.count(it->first)>0) histoTable.AddValue(3, "x");
			else histoTable.AddValue(3, "");
		}
		else if(type==TTH3) {
			histoTable.AddValue(4, it->first);
			if(fAutoUpdateList.count(it->first)>0) histoTable.AddValue(5, "x");
			else histoTable.AddValue(5, "");
		}
		else if(type==TTGraph){
			histoTable.AddValue(6, it->first);
			if(fAutoUpdateList.count(it->first)>0) histoTable.AddValue(7, "x");
			else histoTable.AddValue(7, "");
		}
		else if(type==TTEfficiency){
			histoTable.AddValue(8, it->first);
			if(fAutoUpdateList.count(it->first)>0) histoTable.AddValue(9, "x");
			else histoTable.AddValue(9, "");
		}
	}

	histoTable.Print("\t");
}

void HistoHandler::ExportAllPlot(std::map<TString,TTree*> &trees, std::map<TString,void*> &branches){
	/// \MemberDescr
	/// \param trees : pointer to the list of TTrees
	/// \param branches : point to the list of branches
	///
	/// Export all booked histograms into the output file histograms trees
	/// \EndMemberDescr

	std::vector<TString>::iterator itOrder;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	for(itOrder=fHistoOrder.begin(); itOrder!=fHistoOrder.end(); ++itOrder){
		if((ptr=fHisto.find(*itOrder))!=fHisto.end()){
			plotType_e type = ptr->second.GetType();
			if(type==TTH1 || type==TTH2 || type==TTH3){
				branches[ptr->second.GetHisto()->ClassName()] = fHisto[*itOrder].GetHisto();
				trees[ptr->second.GetHisto()->ClassName()]->Fill();
			}
			else if(type==TTGraph){
				branches[ptr->second.GetGraph()->ClassName()] = fHisto[*itOrder].GetGraph();
				trees[ptr->second.GetGraph()->ClassName()]->Fill();
			}
		}
	}
}

void HistoHandler::DrawAllPlots(TString analyzerName){
	/// \MemberDescr
	/// \param analyzerName : Name of the analyzer calling the method
	///
	/// Draw all booked histograms on the screen
	/// \EndMemberDescr

	std::vector<TString>::iterator itOrder;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;
	NA62Analysis::NA62Map<TString,CanvasOrganizer*>::type::iterator ptrCanvas;
	CanvasOrganizer *c;

	for(itOrder=fHistoOrder.begin(); itOrder!=fHistoOrder.end(); ++itOrder){
		if((ptr=fHisto.find(*itOrder))!=fHisto.end()){
			if(ptr->second.IsOnCanvas()) continue;
			c = new CanvasOrganizer(TString("c_" + analyzerName + "_") + *itOrder);
			plotType_e type = ptr->second.GetType();
			if(type==TTH1)
				c->AddHisto(ptr->second.GetHisto());
			else if(type==TTH2)
				c->AddHisto(ptr->second.GetHisto2());
			else if(type==TTH3)
				c->AddHisto(ptr->second.GetHisto3());
			else if(type==TTGraph)
				c->AddHisto(ptr->second.GetGraph());
			else if(type==TTEfficiency)
				c->AddHisto(ptr->second.GetEfficency());
			c->Draw();
			fCanvas.insert(std::pair<const TString, CanvasOrganizer*>(c->GetName(), c));
		}
	}
	for(ptrCanvas=fCanvas.begin(); ptrCanvas!=fCanvas.end(); ++ptrCanvas){
		ptrCanvas->second->Draw();
	}
}

void HistoHandler::UpdatePlots(int evtNbr){
	/// \MemberDescr
	/// \param evtNbr : Current event number
	///
	/// Update all plots with refresh
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type::iterator it;

	if((evtNbr % fUpdateRate) == 0){
		for(it = fCanvas.begin(); it!=fCanvas.end(); ++it){
			it->second->Update();
		}
	}
}

void HistoHandler::SaveAllPlots(TString analyzerName, bool store_empty_plots) {
	/// \MemberDescr
	/// \param analyzerName : Name of the analyzer calling the method
	///
	/// Write all the booked histograms into the output file ordered as the booking order
	/// \EndMemberDescr

	std::vector<TString>::iterator itOrder;

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	for(itOrder=fHistoOrder.begin(); itOrder!=fHistoOrder.end(); ++itOrder){
		if((ptr=fHisto.find(*itOrder))!=fHisto.end()){
			if(ptr->second.GetDirectory() != ""){
				Mkdir(ptr->second.GetDirectory(), analyzerName);
				gFile->Cd(ptr->second.GetDirectory());
			}
			plotType_e type = ptr->second.GetType();
			if(type == TTH1) {
                if (store_empty_plots or ptr->second.GetHisto()->GetEntries() > 0) {
				    ptr->second.GetHisto()->Write();
                }
			} else if(type == TTH2) {
                if (store_empty_plots or ptr->second.GetHisto2()->GetEntries() > 0) {
				    ptr->second.GetHisto2()->Write();
                }
			} else if(type == TTH3) {
                if (store_empty_plots or ptr->second.GetHisto3()->GetEntries() > 0) {
				    ptr->second.GetHisto3()->Write();
                }
			} else if(type == TTGraph) {
                if (store_empty_plots or ptr->second.GetGraph()->GetN() > 0) {
				    ptr->second.GetGraph()->Write();
                }
			} else if(type == TTEfficiency) {
				ptr->second.GetEfficency()->Write();
            }
		}
		if (ptr->second.GetDirectory() != "") {
            gFile->Cd("/" + analyzerName);
        }
	}
}

void HistoHandler::SetUpdateInterval(int interval){
	/// \MemberDescr
	/// \param interval : Events interval at which the plots should be updated
	//
	/// Set the update interval for the plots
	/// \EndMemberDescr

	fUpdateRate = interval;
}

int HistoHandler::GetUpdateInterval() const{
	/// \MemberDescr
	/// \return Update interval for the plots
	/// \EndMemberDescr

	return fUpdateRate;
}

void HistoHandler::SetPlotAutoUpdate(TString name, TString analyzerName){
	/// \MemberDescr
	/// \param name : Name of the plot
	/// \param analyzerName : Name of the analyzer calling the method
	///
	/// Define the plot as AutoUpdate. Create the corresponding Canvas and Draw the plot
	/// \EndMemberDescr

	CanvasOrganizer *c;
	TString canvasName = TString("c_" + analyzerName + "_") + name;

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		plotType_e type = ptr->second.GetType();
		c = new CanvasOrganizer(canvasName);
		if(type==TTH1)
			c->AddHisto(ptr->second.GetHisto());
		else if(type==TTH2)
			c->AddHisto(ptr->second.GetHisto2());
		else if(type==TTH3)
			c->AddHisto(ptr->second.GetHisto3());
		else if(type==TTGraph)
			c->AddHisto(ptr->second.GetGraph());
		else if(type==TTEfficiency)
			c->AddHisto(ptr->second.GetEfficency());
		c->Draw();
	}
	else{
		if(Configuration::ConfigSettings::global::fNonExistingWarning)
			std::cerr << "Plot " << name << " does not exist. Unable to set AutoUpdate." << std::endl;
		return;
	}

	fCanvas.insert(std::pair<const TString, CanvasOrganizer*>(c->GetName(), c));
	fAutoUpdateList.insert(name);
}

double HistoHandler::compareToReferencePlot(const TH1* const hRef, const TH1* const h2, bool KS) {
	/// \MemberDescr
	/// \param hRef : Pointer to the reference plot
	/// \param h2 : Pointer to the plot to compare
	/// \param KS : If true, use Kolmogorov-Smirnov test, else use chi square test
	/// \return Probability for the histograms to be the same distribution
	///
	/// Compare similarity between two 1D histograms, returning the probability of
	///	the tested (h2) histogram following the same distribution as the reference (hRef)
	///	histogram.
	/// \EndMemberDescr

	int nBins = h2->GetNbinsX();
	double *res = new double[nBins];
	TString name = hRef->GetName();
	double probability;

	TH1* hRefCl = static_cast<TH1*>(hRef->Clone(TString(hRef->GetName()) + "Ref"));

	BookHisto(name, hRefCl);
	hRefCl->Sumw2();
	hRefCl->Scale(h2->Integral()/hRefCl->Integral());

	if(KS){
		probability = hRefCl->KolmogorovTest(h2);
	}
	else{
		BookHisto(name + "_res", new TGraph());
		TF1 *f = new TF1("f", "TMath::Gaus(x,0,1)", -10, 10);
		probability = hRefCl->Chi2Test(h2, "UUNORM P", res);

		BookHisto(name + "_QQ", new TGraphQQ(hRefCl->GetNbinsX(), res, f));
		for(int i=0;i<hRefCl->GetNbinsX();i++){
			FillHisto(name+"_res", hRefCl->GetBinCenter(i+1), res[i]);
			//FillHisto(name+"_QQ", hRef->GetBinCenter(i+1), res[i]);
		}
		fHisto[name+"_res"].GetGraph()->GetXaxis()->SetRangeUser(hRefCl->GetBinCenter(0), hRefCl->GetBinCenter(hRefCl->GetNbinsX()));
		fHisto[name+"_res"].GetGraph()->SetTitle(name + " normalised residuals");
		fHisto[name+"_QQ"].GetGraph()->SetTitle(name + " Q-Q plot of normalised residuals");
	}
	delete[] res;
	return probability;
}

TH1* HistoHandler::GetTH1(TString name) {
	/// \MemberDescr
	/// \param name : Name of the TH1 to retrieve
	/// \return Previously booked histogram with the specified name.
	///
	///	If the histogram does not exist or is not of TH1 type, print an error
	///	message and return NULL.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		return ptr->second.GetHisto();
	}
	else{
		if(Configuration::ConfigSettings::global::fNonExistingWarning)
			std::cerr << "1D Plot " << name << " does not exist." << std::endl;
		return nullptr;
	}
}

TH2* HistoHandler::GetTH2(TString name) {
	/// \MemberDescr
	/// \param name : Name of the TH2 to retrieve
	/// \return Previously booked histogram with the specified name.
	///
	///	If the histogram does not exist or is not of TH2 type, print an error
	///	message and return NULL.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		if(ptr->second.GetType()==TTH2)
			return ptr->second.GetHisto2();
	}
	if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "2D Plot " << name << " does not exist." << std::endl;
	return nullptr;
}

TH3* HistoHandler::GetTH3(TString name) {
	/// \MemberDescr
	/// \param name : Name of the TH3 to retrieve
	/// \return Previously booked histogram with the specified name.
	///
	///	If the histogram does not exist or is not of TH3 type, print an error
	///	message and return NULL.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		if(ptr->second.GetType()==TTH3)
			return ptr->second.GetHisto3();
	}
	if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "3D Plot " << name << " does not exist." << std::endl;
	return nullptr;
}

TGraph* HistoHandler::GetTGraph(TString name) {
	/// \MemberDescr
	/// \param name : Name of the TGraph to retrieve
	/// \return Previously booked graph with the specified name.
	///
	///	If the graph does not exist or is not of TGraph type, print an error
	///	message and return NULL.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		if(ptr->second.GetType()==TTGraph)
		return ptr->second.GetGraph();
	}
	if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Graph " << name << " does not exist." << std::endl;
	return nullptr;
}

TEfficiency* HistoHandler::GetTEfficiency(TString name) {
	/// \MemberDescr
	/// \param name : Name of the TGraph to retrieve
	/// \return Previously booked graph with the specified name.
	///
	///	If the efficiency does not exist or is not of TEfficiency type, print an error
	///	message and return NULL.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		if(ptr->second.GetType()==TTEfficiency)
		return ptr->second.GetEfficency();
	}
	if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Graph " << name << " does not exist." << std::endl;
	return nullptr;
}

TH1* HistoHandler::GetHisto(TString name) {
	/// \MemberDescr
	/// \param name : Name of the histogram to retrieve (TH1, TH2, TH3)
	/// \return Previously booked histogram with the specified name.
	///
	///	If the histogram does not exist, print an error message and return NULL.
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((ptr=fHisto.find(name))!=fHisto.end()){
		plotType_e type = ptr->second.GetType();
		if(type==TTH1)
			return ptr->second.GetHisto();
		else if(type==TTH2)
			return ptr->second.GetHisto2();
		else if(type==TTH3)
			return ptr->second.GetHisto3();
	}
	if(Configuration::ConfigSettings::global::fNonExistingWarning)
		std::cerr << "Histogram " << name << " does not exist." << std::endl;
	return nullptr;
}

TH1* HistoHandler::GetHistoFromArray(TString baseName, int index) {
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	///
	///	If the histogram does not exist, print an error message and return NULL.
	/// \return Pointer to histogram if found, NULL pointer else.
	/// \EndMemberDescr

	return GetHisto(baseName + (Long_t)index);
}

TGraph* HistoHandler::GetGraphFromArray(TString baseName, int index) {
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	///
	///	If the histogram does not exist, print an error message and return NULL.
	/// \return Pointer to histogram if found, NULL pointer else.
	/// \EndMemberDescr

	return GetTGraph(baseName + (Long_t)index);
}

TEfficiency* HistoHandler::GetEfficiencyFromArray(TString baseName, int index) {
	/// \MemberDescr
	/// \param baseName : Name of the histogram. The index will be appended
	///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
	///
	///	If the histogram does not exist, print an error message and return NULL.
	/// \return Pointer to histogram if found, NULL pointer else.
	/// \EndMemberDescr

	return GetTEfficiency(baseName + (Long_t)index);
}

void HistoHandler::Mkdir(TString name, TString analyzerName) const{
	/// \MemberDescr
	/// \param name: Name of the directory to create
	/// \param analyzerName : Name of the analyzer calling the method
	///
	/// Check if the directory name already exists in the analyzer subdirectory. If not create it.
	/// \EndMemberDescr

	if(gFile->GetDirectory(analyzerName + "/" + name)==NULL){
		gFile->mkdir(analyzerName + "/" + name);
	}
}

HistoHandler::IteratorTH1 HistoHandler::GetIteratorTH1() {
	/// \MemberDescr
	/// \return Iterator to the TH1
	///
	/// Create a TH1Iterator over all the TH1 stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TH1*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTH1>::type::iterator itList;

	if((itList=fTH1IteratorsList.find(""))!=fTH1IteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if((itEl=fHisto.find(*it))!=fHisto.end()) list.push_back(itEl->second.GetHisto());
	}

	itList = fTH1IteratorsList.insert(std::pair<const TString,IteratorTH1>("", IteratorTH1(list))).first;
	return itList->second;
}

HistoHandler::IteratorTH1 HistoHandler::GetIteratorTH1(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to the TH1
	///
	/// Create a TH1Iterator over all the TH1 whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TH1*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTH1>::type::iterator itList;

	if((itList=fTH1IteratorsList.find(baseName))!=fTH1IteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if(!it->BeginsWith(baseName)) continue;
		if((itEl=fHisto.find(*it))!=fHisto.end()) list.push_back(itEl->second.GetHisto());
	}

	itList = fTH1IteratorsList.insert(std::pair<const TString,IteratorTH1>(baseName, IteratorTH1(list))).first;
	return itList->second;
}

HistoHandler::IteratorTH2 HistoHandler::GetIteratorTH2() {
	/// \MemberDescr
	/// \return Iterator to the TH2
	///
	/// Create a TH2Iterator over all the TH2 stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TH2*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTH2>::type::iterator itList;

	if((itList=fTH2IteratorsList.find(""))!=fTH2IteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTH2)
				list.push_back(itEl->second.GetHisto2());
	}

	itList = fTH2IteratorsList.insert(std::pair<const TString,IteratorTH2>("", IteratorTH2(list))).first;
	return itList->second;
}

HistoHandler::IteratorTH2 HistoHandler::GetIteratorTH2(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to the TH2
	///
	/// Create a TH2Iterator over all the TH2 whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TH2*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTH2>::type::iterator itList;

	if((itList=fTH2IteratorsList.find(baseName))!=fTH2IteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if(!it->BeginsWith(baseName)) continue;
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTH2) list.push_back(itEl->second.GetHisto2());
	}

	itList = fTH2IteratorsList.insert(std::pair<const TString,IteratorTH2>(baseName, IteratorTH2(list))).first;
	return itList->second;
}

HistoHandler::IteratorTH3 HistoHandler::GetIteratorTH3() {
	/// \MemberDescr
	/// \return Iterator to the TH3
	///
	/// Create a TH3Iterator over all the TH3 stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TH3*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTH3>::type::iterator itList;

	if((itList=fTH3IteratorsList.find(""))!=fTH3IteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTH3)
				list.push_back(itEl->second.GetHisto3());
	}

	itList = fTH3IteratorsList.insert(std::pair<const TString,IteratorTH3>("", IteratorTH3(list))).first;
	return itList->second;
}

HistoHandler::IteratorTH3 HistoHandler::GetIteratorTH3(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to the TH3
	///
	/// Create a TH3Iterator over all the TH3 whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TH3*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTH3>::type::iterator itList;

	if((itList=fTH3IteratorsList.find(baseName))!=fTH3IteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if(!it->BeginsWith(baseName)) continue;
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTH3)
				list.push_back(itEl->second.GetHisto3());
	}

	itList = fTH3IteratorsList.insert(std::pair<const TString,IteratorTH3>(baseName, IteratorTH3(list))).first;
	return itList->second;
}

HistoHandler::IteratorTGraph HistoHandler::GetIteratorTGraph() {
	/// \MemberDescr
	/// \return Iterator to the TGraph
	///
	/// Create a TGraphIterator over all the TGraph stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TGraph*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTGraph>::type::iterator itList;

	if((itList=fTGraphIteratorsList.find(""))!=fTGraphIteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTGraph)
				list.push_back(itEl->second.GetGraph());
	}

	itList = fTGraphIteratorsList.insert(std::pair<const TString,IteratorTGraph>("", IteratorTGraph(list))).first;
	return itList->second;
}

HistoHandler::IteratorTGraph HistoHandler::GetIteratorTGraph(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to the TGraph
	///
	/// Create a TGraphIterator over all the TGraph whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TGraph*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTGraph>::type::iterator itList;

	if((itList=fTGraphIteratorsList.find(baseName))!=fTGraphIteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if(!it->BeginsWith(baseName)) continue;
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTGraph)
				list.push_back(itEl->second.GetGraph());
	}

	itList = fTGraphIteratorsList.insert(std::pair<const TString,IteratorTGraph>(baseName, IteratorTGraph(list))).first;
	return itList->second;
}

HistoHandler::IteratorTEfficiency HistoHandler::GetIteratorTEfficiency() {
	/// \MemberDescr
	/// \return Iterator to the TEfficiency
	///
	/// Create a TEfficiencyIterator over all the TEfficiency stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TEfficiency*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTEfficiency>::type::iterator itList;

	if((itList=fTEfficiencyIteratorsList.find(""))!=fTEfficiencyIteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTEfficiency)
				list.push_back(itEl->second.GetEfficency());
	}

	itList = fTEfficiencyIteratorsList.insert(std::pair<const TString,IteratorTEfficiency>("", IteratorTEfficiency(list))).first;
	return itList->second;
}

HistoHandler::IteratorTEfficiency HistoHandler::GetIteratorTEfficiency(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to the TGraph
	///
	/// Create a TEfficiencyIterator over all the TEfficiency whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<TString>::const_iterator it;
	std::vector<TEfficiency*> list;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorTEfficiency>::type::iterator itList;

	if((itList=fTEfficiencyIteratorsList.find(baseName))!=fTEfficiencyIteratorsList.end())
		return itList->second;

	for(it=fHistoOrder.begin(); it!=fHistoOrder.end(); ++it){
		if(!it->BeginsWith(baseName)) continue;
		if((itEl=fHisto.find(*it))!=fHisto.end())
			if(itEl->second.GetType()==TTEfficiency)
				list.push_back(itEl->second.GetEfficency());
	}

	itList = fTEfficiencyIteratorsList.insert(std::pair<const TString,IteratorTEfficiency>(baseName, IteratorTEfficiency(list))).first;
	return itList->second;
}

HistoHandler::IteratorCanvas HistoHandler::GetIteratorCanvas() {
	/// \MemberDescr
	/// \return Iterator to the CanvasOrganizer
	///
	/// Create a CanvasIterator over all the CanvasOrganizer stored in this instance of HistoHandler.
	/// \EndMemberDescr

	std::vector<CanvasOrganizer*> list;
	NA62Analysis::NA62Map<TString,CanvasOrganizer*>::type::iterator itEl;
	NA62Analysis::NA62Map<TString,IteratorCanvas>::type::iterator itList;


	if((itList=fCanvasIteratorsList.find(""))!=fCanvasIteratorsList.end()){
		if((itList->second.End() - itList->second.Begin())==(int)fCanvasOrder.size())
			return itList->second;
		else {
			fCanvasIteratorsList.erase(itList);
		}
	}

	for(auto itOrder : fCanvasOrder){
		if((itEl=fCanvas.find(itOrder))!=fCanvas.end())
			list.push_back(itEl->second);
	}

	itList = fCanvasIteratorsList.insert(std::pair<const TString,IteratorCanvas>("", IteratorCanvas(list))).first;
	return itList->second;
}

void HistoHandler::CreateCanvas(TString name, int width, int height) {
	/// \MemberDescr
	/// \param name: Name of the canvas
	/// \param width: width of the canvas (default=0=automatic)
	/// \param height: height of the canvas (default=0=automatic)
	///
	/// Create a new named canvas in the analyzer
	/// \EndMemberDescr

	CanvasOrganizer *c = new CanvasOrganizer(name);
	if(width!=0 && height!=0) c->SetSize(width, height);

	fCanvasOrder.push_back(name);
	fCanvas.insert(std::pair<const TString, CanvasOrganizer*>(c->GetName(), c));
}

bool HistoHandler::PlacePlotOnCanvas(TString histoName, TString canvasName, int row, int col) {
	/// \MemberDescr
	/// \param histoName: Name of the plot
	/// \param canvasName: Name of the canvas
	/// \param row: Row position on the canvas
	/// \param col: Column position on the canvas
	/// \return True if canvas and histograms were found
	///
	/// Add a plot to the list of Plots managed by the specified CanvasOrganizer
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type::iterator it;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	if((it=fCanvas.find(canvasName))!=fCanvas.end()){

		if((ptr=fHisto.find(histoName))!=fHisto.end()){
			plotType_e type = ptr->second.GetType();
			if(type==TTH1)
				it->second->AddHisto(ptr->second.GetHisto(), row, col);
			else if(type==TTH2)
				it->second->AddHisto(ptr->second.GetHisto2(), row, col);
			else if(type==TTH3)
				it->second->AddHisto(ptr->second.GetHisto3(), row, col);
			else if(type==TTGraph)
				it->second->AddHisto(ptr->second.GetGraph(), row, col);
			else if(type==TTEfficiency)
				it->second->AddHisto(ptr->second.GetEfficency(), row, col);
			ptr->second.SetIsOnCanvas(true);
		}
		else{
			if(Configuration::ConfigSettings::global::fNonExistingWarning)
				std::cerr << "Histogram " << histoName << " does not exist." << std::endl;
			return false;
		}
		return true;
	}
	return false;
}

bool HistoHandler::PlacePlotOnCanvas(std::initializer_list<TString> histoNames, TString canvasName, int row, int col) {
	/// \MemberDescr
	/// \param histoNames: List of plots names
	/// \param canvasName: Name of the canvas
	/// \param row: Row position on the canvas
	/// \param col: Column position on the canvas
	/// \return True if canvas and histograms were found
	///
	/// Add a list of plots to the list of Plots managed by the specified CanvasOrganizer
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type::iterator it;
	NA62Analysis::NA62Map<TString,THObject>::type::iterator ptr;

	std::vector<TH1*> vTH1;
	std::vector<TGraph*> vTGraph;
	std::vector<TEfficiency*> vTEfficiency;
	plotType_e type = TTUNKNOWN;
	plotType_e old_type = TTUNKNOWN;
	if((it=fCanvas.find(canvasName))!=fCanvas.end()){
		for(auto itNames : histoNames){
			if((ptr=fHisto.find(itNames))!=fHisto.end()){
				type = ptr->second.GetType();
				if(old_type==TTUNKNOWN)
					old_type = type;
				else if(type!=old_type){
					std::cerr
							<< "Superimposed histograms must be of the same type. "
							<< itNames
							<< " is not of the same type as the previous histogram ("
							<< type << "!=" << old_type << ")." << std::endl;
					return false;
				}
				if(type==TTH1)
					vTH1.push_back(ptr->second.GetHisto());
				else if(type==TTH2 || type==TTH3){
					std::cerr
							<< "Superimposed histograms are not allowed for TH2 and TH3."
							<< std::endl;
					return false;
				}
				else if(type==TTGraph)
					vTGraph.push_back(ptr->second.GetGraph());
				else if(type==TTEfficiency)
					vTEfficiency.push_back(ptr->second.GetEfficency());
				ptr->second.SetIsOnCanvas(true);
			}
			else{
				if(Configuration::ConfigSettings::global::fNonExistingWarning)
					std::cerr << "Histogram " << itNames << " does not exist." << std::endl;
				return false;
			}
		}
		if(type==TTH1)
			it->second->AddHisto(vTH1, row, col);
		if(type==TTGraph)
			it->second->AddHisto(vTGraph, row, col);
		if(type==TTEfficiency)
			it->second->AddHisto(vTEfficiency, row, col);
		return true;
	}
	return false;
}
bool HistoHandler::SetCanvasAutoUpdate(TString canvasName) {
	/// \MemberDescr
	/// \param canvasName: Name of the canvas
	/// \return True if canvas was found
	///
	/// Mark a canvas as AutoUpdate (will be redrawn every fAutoUpdateInterval events)
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type::iterator it;

	if((it=fCanvas.find(canvasName))!=fCanvas.end()){
		it->second->SetUpdateFrequency(fUpdateRate);
		it->second->Draw();
		return true;
	}
	return false;
}

bool HistoHandler::UpdateCanvas(TString canvasName) const {
	/// \MemberDescr
	/// \param canvasName: Name of the canvas
	/// \return True if canvas was found
	///
	/// Force the update of a canvas
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type::const_iterator it;

	if((it=fCanvas.find(canvasName))!=fCanvas.end()){
		it->second->Update();
		return true;
	}
	return false;
}

void HistoHandler::SetCanvasReference(TString canvas, TString histo, TH1* refPtr) {
	/// \MemberDescr
	/// \param canvas Name of the canvas that contains the histogram to which the reference will be added
	/// \param histo Name of the histogram to which the reference will be added
	/// \param refPtr Pointer to the reference histogram to link to histo.
	///
	/// Add a reference histogram to the specified histogram in the specified canvas
	/// \EndMemberDescr

	TH1* h = GetTH1(histo);
	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type::const_iterator it;

	if(h && ((it=fCanvas.find(canvas))!=fCanvas.end())){
		it->second->SetReference(refPtr, h);
	}
}

void HistoHandler::SetCanvasReference(TString canvas, TString histo, TGraph* refPtr) {
	/// \MemberDescr
	/// \param canvas Name of the canvas that contains the histogram to which the reference will be added
	/// \param histo Name of the histogram to which the reference will be added
	/// \param refPtr Pointer to the reference histogram to link to histo.
	///
	/// Add a reference histogram to the specified histogram in the specified canvas
	/// \EndMemberDescr

	TGraph* h = GetTGraph(histo);
	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type::const_iterator it;

	if(h && ((it=fCanvas.find(canvas))!=fCanvas.end())){
		it->second->SetReference(refPtr, h);
	}
}

bool HistoHandler::Exists(TString name) {
	/// \MemberDescr
	///	\param name: histogram name
	/// \return true if the histogram exists
	/// \EndMemberDescr
	return fHisto.count(name)>0;
}

} /* namespace Core */
} /* namespace NA62Analysis */

