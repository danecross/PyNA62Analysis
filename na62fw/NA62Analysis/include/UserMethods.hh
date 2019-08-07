/*
 * UserMethods.hh
 *
 *  Created on: 21 Jan 2014
 *      Author: ncl
 */

#ifndef USERMETHODS_HH_
#define USERMETHODS_HH_

#include <map>
#include <sstream>
#include <cassert>

#include <TString.h>
#include <TChain.h>
#include <TGraph.h>
#include <TH1I.h>
#include <TH2I.h>
#include <TH2F.h>
#include <TEfficiency.h>
#include "TDetectorVEvent.hh"
#include "TSlimRecoVEvent.hh"
#include "MCInfo.hh"
#include "RecoInfo.hh"

#include "HistoHandler.hh"
#include "FWEnums.hh"
#include "MCSimple.hh"
#include "Verbose.hh"
#include "AnalyzerIdentifier.hh"
#include "InputHandler.hh"

class EventHeader;
class Event;
class L0TPData;
class L1TPData;
class L2EBData;
class TPrimitive;
class Stream;
class BeamData;
class HLTEvent;
class L0TPSpecialTrigger;
class L1TPSpecialTrigger;
class L2EBSpecialTrigger;
class BeamSpecialTrigger;

namespace NA62Analysis {

namespace Core {
class BaseAnalysis;
class CanvasOrganizer;
} /* namespace Core */

/// \class UserMethods
/// \Brief
/// Interface class for the end user for methods whose implementation is not actually done in the analyzer class.
/// \EndBrief
///
/// \Detailed
/// This class defines a list of methods already implemented in members objects of the analyzer but that we must keep
///	in the analyzer class as well for backward compatibility and/or shorter syntax.
/// \EndDetailed
class UserMethods: public Verbose {
public:
	/// List of possible states for the output variables.
	enum OutputState {
		kOUninit, kOValid, kOInvalid
	};

	explicit UserMethods(Core::BaseAnalysis *ba);
	UserMethods(Core::BaseAnalysis *ba, const std::string &name);
	UserMethods(const UserMethods&);
	virtual ~UserMethods();

	//###### Histograms related
	//Histogram booking methods
	template<class HistoType>
	void BookHisto(TString name, HistoType* const histo, bool refresh = false,
			TString directory = "") {
		/// \MemberDescr
		/// \param name : Name of the histogram (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram
		/// \param refresh : Set the plot as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
		///
		/// Book a new histogram and make it available in the whole analyzer
		/// \EndMemberDescr

		fHisto.BookHisto(name, histo, fAnalyzerName, refresh, directory);
	}
	template<class HistoType>
	void BookHisto(const char* name, HistoType* const histo, bool refresh =
			false, TString directory = "") {
		/// \MemberDescr
		/// \param name : Name of the histogram (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram
		/// \param refresh : Set the plot as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
		///
		/// Book a new histogram and make it available in the whole analyzer
		/// \EndMemberDescr

		fHisto.BookHisto(TString(name), histo, fAnalyzerName, refresh,
				directory);
	}
	template<class HistoType>
		void BookHisto(char* name, HistoType* const histo, bool refresh =
				false, TString directory = "") {
		/// \MemberDescr
		/// \param name : Name of the histogram (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram
		/// \param refresh : Set the plot as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
		///
		/// Book a new histogram and make it available in the whole analyzer
		/// \EndMemberDescr

		fHisto.BookHisto(TString(name), histo, fAnalyzerName, refresh,
				directory);
	}
	template<class HistoType>
	void BookHisto(HistoType* const histo, bool refresh = false,
			TString directory = "") {
		/// \MemberDescr
		/// \param histo : Pointer to the histogram
		/// \param refresh : Set the plot as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
		///
		/// Book a new histogram and make it available in the whole analyzer
		/// \EndMemberDescr

		fHisto.BookHisto(histo->GetName(), histo, fAnalyzerName, refresh,
				directory);
	}

	template<class HistoType>
	void BookHistoArray(TString baseName, HistoType* const histo, int number,
			bool refresh = false, TString directory = "") {
		/// \MemberDescr
		/// \param baseName : Name of the histogram. The index will be appended. (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram to replicate
		///	\param number : Number of histograms to create
		/// \param refresh : Set the plots as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
		///
		/// Book an array of similar histograms and make it available in the whole analyzer.
		/// \EndMemberDescr

		fHisto.BookHistoArray(baseName, histo, number, fAnalyzerName, refresh,
				directory);
	}
	template<class HistoType>
		void BookHistoArray(const char* baseName, HistoType* const histo, int number,
				bool refresh = false, TString directory = "") {
		/// \MemberDescr
		/// \param baseName : Name of the histogram. The index will be appended. (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram to replicate
		///	\param number : Number of histograms to create
		/// \param refresh : Set the plots as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
		///
		/// Book an array of similar histograms and make it available in the whole analyzer.
		/// \EndMemberDescr

		fHisto.BookHistoArray(baseName, histo, number, fAnalyzerName, refresh,
				directory);
	}
	template<class HistoType>
		void BookHistoArray(char* baseName, HistoType* const histo, int number,
				bool refresh = false, TString directory = "") {
		/// \MemberDescr
		/// \param baseName : Name of the histogram. The index will be appended. (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram to replicate
		///	\param number : Number of histograms to create
		/// \param refresh : Set the plots as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
		///
		/// Book an array of similar histograms and make it available in the whole analyzer.
		/// \EndMemberDescr

		fHisto.BookHistoArray(baseName, histo, number, fAnalyzerName, refresh,
				directory);
	}
	template<class HistoType>
	void BookHistoArray(HistoType* const histo, int number,
			bool refresh = false, TString directory = "") {
		/// \MemberDescr
		/// \param histo : Pointer to the histogram to replicate
		///	\param number : Number of histograms to create
		/// \param refresh : Set the plots as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
		///
		/// Book an array of similar histograms and make it available in the whole analyzer.
		/// \EndMemberDescr

		fHisto.BookHistoArray(histo->GetName(), histo, number, fAnalyzerName,
				refresh, directory);
	}

	//Histogram filling methods
	template<typename T1>
	void FillHisto(TString name, TString x, T1 w) {
		/// \MemberDescr
		/// \param name, x, w: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, TString, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, x, static_cast<double>(w));
	}
	template<typename T1, typename T2>
	void FillHisto(TString name, TString x, T1 y, T2 w) {
		/// \MemberDescr
		/// \param name, x, y, w: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, TString, double, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, x, static_cast<double>(y),
				static_cast<double>(w));
	}
	template<typename T1>
	void FillHisto(TString name, TString x, TString y, T1 w) {
		/// \MemberDescr
		/// \param name, x, y, w: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, TString, TString, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, x, y, static_cast<double>(w));
	}
	template<typename T1>
	void FillHisto(TString name, TString x, TString y, TString z, T1 w) {
		/// \MemberDescr
		/// \param name, x, y, z, w: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, TString, TString, TString, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, x, y, z, static_cast<double>(w));
	}
	template<typename T1, typename T2>
	void FillHisto(TString name, T1 x, T2 w) {
		/// \MemberDescr
		/// \param name, x, w: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, double, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, static_cast<double>(x), static_cast<double>(w));
	}
	template<typename T1>
	void FillHisto(TString name, T1 x) {
		/// \MemberDescr
		/// \param name, x: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, static_cast<double>(x));
	}
	template<typename T1, typename T2, typename T3>
	void FillHisto(TString name, T1 x, T2 y, T3 w) {
		/// \MemberDescr
		/// \param name, x, y, w: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, double, double, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, static_cast<double>(x), static_cast<double>(y),
				static_cast<double>(w));
	}
	template<typename T1, typename T2, typename T3, typename T4>
	void FillHisto(TString name, T1 x, T2 y, T3 z, T4 w) {
		/// \MemberDescr
		/// \param name, x, y, z, w: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, double, double, double, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, static_cast<double>(x), static_cast<double>(y),
				static_cast<double>(z), static_cast<double>(w));
	}
	template<typename T1, typename T2, typename T3>
	void FillHisto(TString name, bool b, T1 x, T2 y, T3 z) {
		/// \MemberDescr
		/// \param name, b, x, y, z: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, bool, double, double, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, b, static_cast<double>(x),
				static_cast<double>(y), static_cast<double>(z));
	}
	template<typename T1, typename T2>
	void FillHisto(TString name, bool b, T1 x, T2 y) {
		/// \MemberDescr
		/// \param name, b, x, y: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, bool, double, double, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, b, static_cast<double>(x),
				static_cast<double>(y), 0);
	}
	template<typename T1>
	void FillHisto(TString name, bool b, T1 x) {
		/// \MemberDescr
		/// \param name, b, x: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHisto(TString, bool, double, double, double)
		/// \EndMemberDescr
		fHisto.FillHisto(name, b, static_cast<double>(x), 0, 0);
	}

	template<typename T1>
	void FillHistoArray(TString baseName, int index, TString x, T1 w){
		/// \MemberDescr
		/// \param baseName, index, x, w : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, TString, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, x, static_cast<double>(w));
	}
	template<typename T1, typename T2>
	void FillHistoArray(TString baseName, int index, TString x, T1 y, T2 w){
		/// \MemberDescr
		/// \param baseName, index, x, y, w : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, TString, double, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, x, static_cast<double>(y), static_cast<double>(w));
	}
	template<typename T1>
	void FillHistoArray(TString baseName, int index, TString x, TString y, T1 w){
		/// \MemberDescr
		/// \param baseName, index, x, y, w : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, TString, TString, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, x, y, static_cast<double>(w));
	}
	template<typename T1>
	void FillHistoArray(TString baseName, int index, TString x, TString y, TString z, T1 w){
		/// \MemberDescr
		/// \param baseName, index, x, y, z, w : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, TString, TString, TString, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, x, y, z, static_cast<double>(w));
	}
	template<typename T1, typename T2>
	void FillHistoArray(TString baseName, int index, T1 x, T2 w){
		/// \MemberDescr
		/// \param baseName, index, x, w : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, double, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, static_cast<double>(x), static_cast<double>(w));
	}
	template<typename T1>
	void FillHistoArray(TString baseName, int index, T1 x){
		/// \MemberDescr
		/// \param baseName, index, x: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, static_cast<double>(x));
	}
	template<typename T1, typename T2, typename T3>
	void FillHistoArray(TString baseName, int index, T1 x, T2 y, T3 w){
		/// \MemberDescr
		/// \param baseName, index, x, y, w : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, Tdouble, double, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, static_cast<double>(x), static_cast<double>(y), static_cast<double>(w));
	}
	template<typename T1, typename T2, typename T3, typename T4>
	void FillHistoArray(TString baseName, int index, T1 x, T2 y, T3 z, T4 w){
		/// \MemberDescr
		/// \param baseName, index, x, y, z, w : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, Tdouble, double, double, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, static_cast<double>(x), static_cast<double>(y), static_cast<double>(z), static_cast<double>(w));
	}
	template<typename T1, typename T2, typename T3>
	void FillHistoArray(TString baseName, int index, bool b, T1 x, T2 y, T3 z){
		/// \MemberDescr
		/// \param baseName, index, b, x, y, z : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, bool, double, double, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, b, static_cast<double>(x), static_cast<double>(y), static_cast<double>(z));
	}
	template<typename T1, typename T2>
	void FillHistoArray(TString baseName, int index, bool b, T1 x, T2 y){
		/// \MemberDescr
		/// \param baseName, index, b, x, y: params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, bool, double, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, b, static_cast<double>(x), static_cast<double>(y));
	}
	template<typename T1>
	void FillHistoArray(TString baseName, int index, bool b, T1 x){
		/// \MemberDescr
		/// \param baseName, index, b, x : params
		///
		/// See NA62Analysis::Core::HistoHandler::FillHistoArray(TString, int, bool, double)
		/// \EndMemberDescr
		fHisto.FillHistoArray(baseName, index, b, static_cast<double>(x));
	}

	Core::HistoHandler::IteratorTH1 GetIteratorTH1();
	Core::HistoHandler::IteratorTH1 GetIteratorTH1(TString baseName);
	Core::HistoHandler::IteratorTH2 GetIteratorTH2();
	Core::HistoHandler::IteratorTH2 GetIteratorTH2(TString baseName);
	Core::HistoHandler::IteratorTH3 GetIteratorTH3();
	Core::HistoHandler::IteratorTH3 GetIteratorTH3(TString baseName);
	Core::HistoHandler::IteratorTGraph GetIteratorTGraph();
	Core::HistoHandler::IteratorTGraph GetIteratorTGraph(TString baseName);
	Core::HistoHandler::IteratorTEfficiency GetIteratorTEfficiency();
	Core::HistoHandler::IteratorTEfficiency GetIteratorTEfficiency(
			TString baseName);
	Core::HistoHandler::IteratorCanvas GetIteratorCanvas();
	NA62Analysis::NA62Map<TString, Core::CanvasOrganizer*>::type GetCanvases();
	Core::HistoHandler& GetHistoHandler() { return fHisto; }

	//Alias for user to make difference between TH1 and TH2
	inline void FillHisto2(TString name, TString x, double y, double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram.
		/// \param x : abscissa
		/// \param y : ordinate
		/// \param w : weight
		///
		/// Alias to FillHisto(TString, TString, double, double)
		/// \EndMemberDescr
		FillHisto(name, x, y, w);
	}
	;
	inline void FillHisto2(TString name, TString x, TString y, double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram.
		/// \param x : abscissa
		/// \param y : ordinate
		/// \param w : weight
		///
		/// Alias to FillHisto(TString, TString, TString, double)
		/// \EndMemberDescr
		FillHisto(name, x, y, w);
	}
	;
	inline void FillHisto2(TString name, double x, double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram.
		/// \param x : abscissa
		/// \param w : weight
		///
		/// Alias to FillHisto(TString, double, double)
		/// \EndMemberDescr
		FillHisto(name, x, w);
	}
	;
	inline void FillHisto2(TString name, double x, double y, double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram.
		/// \param x : abscissa
		/// \param y : ordinate
		/// \param w : weight
		///
		/// Alias to FillHisto(TString, double, double, double)
		/// \EndMemberDescr
		FillHisto(name, x, y, w);
	}
	;

	inline void FillHisto2Array(TString name, int index, TString x, double y,
			double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram. The index will be appended
		///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
		/// \param x : abscissa
		/// \param y : ordinate
		/// \param w : weight
		///
		/// Alias to FillHistoArray(TString, int, TString, double, double)
		/// \EndMemberDescr
		FillHistoArray(name, index, x, y, w);
	}
	;
	inline void FillHisto2Array(TString name, int index, TString x, TString y,
			double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram. The index will be appended
		///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
		/// \param x : abscissa
		/// \param y : ordinate
		/// \param w : weight
		///
		/// Alias to FillHistoArray(TString, int, TString, TString, double)
		/// \EndMemberDescr
		FillHistoArray(name, index, x, y, w);
	}
	;
	inline void FillHisto2Array(TString name, int index, double x, double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram. The index will be appended
		///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
		/// \param x : abscissa
		/// \param w : weight
		///
		/// Alias to FillHistoArray(TString, int, double, double)
		/// \EndMemberDescr
		FillHistoArray(name, index, x, w);
	}
	;
	inline void FillHisto2Array(TString name, int index, double x, double y,
			double w) {
		/// \MemberDescr
		/// \param name : Name of the histogram. The index will be appended
		///	\param index : Array index of the Histogram to fill. If booked with BookHistoArray, starts at 0 to N-1.
		/// \param x : abscissa
		/// \param y : ordinate
		/// \param w : weight
		///
		/// Alias to FillHistoArray(TString, int, double, double, double)
		/// \EndMemberDescr
		FillHistoArray(name, index, x, y, w);
	}
	;

	//Methods for drawing plots on screen
	void DrawAllPlots();
	void UpdatePlots(Long64_t evtNbr);
	void SetUpdateInterval(int interval);
	int GetUpdateInterval() const;
	void CreateCanvas(TString name, int width = 0, int height = 0);
	bool PlacePlotOnCanvas(TString histoName, TString canvasName, int row = -1,
			int col = -1);
	bool PlacePlotOnCanvas(std::initializer_list<TString> histoNames, TString canvasName,
			int row = -1, int col = -1);
	bool SetCanvasAutoUpdate(TString canvasName);
	bool UpdateCanvas(TString canvasName);
	void SetCanvasReference(TString canvasName, TString histo, TH1* refPtr);
	void SetCanvasReference(TString canvasName, TString histo, TGraph* refPtr);

	//Export all histograms into output trees
	void ExportAllPlot(std::map<TString, TTree*> &trees,
			std::map<TString, void*> &branches);
	//Save all plots into output file
	virtual void SaveAllPlots();
	virtual void SaveNonEmptyPlots();

	//Request input histogram
	TH1* RequestHistogram(TString directory, TString name, bool appendOnNewFile, TString saveDirectory="");
	bool ImportAllInputHistogram(TString directory, bool appendOnNewFile, TString saveDirectory="");
	TH1* GetInputHistogram(TString directory, TString name);
	TH1* GetReferenceTH1(TString name, TString directory = "");
	TH2* GetReferenceTH2(TString name, TString directory = "");
	TGraph* GetReferenceTGraph(TString name, TString directory = "");

	//IOHandler calls
	std::vector<Core::InputHandler::keyPair> GetListOfKeys(TString directory = "");
	std::vector<TString> GetListOfDirs(TString directory = "");
	std::vector<TString> GetListOfTH1(TString directory = "");
	std::vector<TString> GetListOfTH2(TString directory = "");
	std::vector<TString> GetListOfTGraph(TString directory = "");
	std::vector<TString> GetListOfTEfficiency(TString directory = "");
	std::vector<TString> GetListOfHistos(TString directory = "");

	//###### Event fraction related
	//EventFraction methods
	void NewEventFraction(TString name);
	void AddCounterToEventFraction(TString efName, TString cName);
	void DefineSampleSizeCounter(TString efName, TString cName);
	void SetSignificantDigits(TString efName, int v);

	//Counter methods
	void BookCounter(TString cName);
	void SetCounterValue(TString cName, int v);
	void IncrementCounter(TString cName, int delta);
	void DecrementCounter(TString cName, int delta);
	void IncrementCounter(TString cName);
	void DecrementCounter(TString cName);
	int GetCounterValue(TString cName) const;

	//###### Output related
	//Methods for setting output
	void RegisterOutput(TString name, const void* const address);
	void SetOutputState(TString name, OutputState state);

	//Methods for getting output
	const void *GetOutput(TString name, OutputState &state) const;
	const void *GetOutput(TString name) const;
	template<class T>
	const T* GetOutput(TString name, OutputState &state) const {
		/// \MemberDescr
		/// \param name : name of the output
		/// \param state : is filled with the current state of the output
		/// \return an output variable and the corresponding state (template version)
		/// \EndMemberDescr

		return (T*) GetOutputVoid(name, state);
	}
	template<class T>
	const T* GetOutput(TString name) const {
		/// \MemberDescr
		/// \param name : name of the output
		/// \return an output variable and the corresponding state (template version)
		/// \EndMemberDescr

		OutputState state;
		return (T*) GetOutputVoid(name, state);
	}

	//###### Input (Event/TTree) related
	//Request new tree to analyze
	void RequestBeamData();
	void RequestL0Data();
    void RequestHLTData();
	void RequestL1Data();
	void RequestL2Data();
	void RequestL0SpecialTrigger();
	void RequestL1SpecialTrigger();
	void RequestL2SpecialTrigger();
	void RequestBeamSpecialTrigger();
	void RequestTree(TString detectorName, TDetectorVEvent *evt, TString outputStage = "");
	void RequestTree(TDetectorVEvent *evt, TString outputStage = "");
	void RequestTree(TString detectorName, TSlimRecoVEvent *evt);
	void RequestTree(TSlimRecoVEvent *evt);
	template<class T>
	void RequestTree(TString treeName, TString branchName, TString className, T* obj) {
		/// \MemberDescr
		/// \param treeName : Name of the requested TTree
		/// \param branchName : Name of the Branch to retrieve
		/// \param className : Name of the class type in this branch
		/// \param obj : Pointer to an instance of any class
		///
		/// Request a tree in the input file. If already requested before, only add the new branch.
		/// \EndMemberDescr

		if (!RequestTreeVoid(treeName, branchName, className, obj)) {
			delete obj;
		}
	}
	void RequestAllMCTrees();
	void RequestAllRecoTrees(bool RequestSpecialTriggers = true);
	void AddPrimitiveReader(TString detName, bool sorted = false);

	TChain* GetTree(TString name);
        TChain* GetReferenceTree();
	TDetectorVEvent *GetEvent(TString detName, TString outputName = "");
	TSlimRecoVEvent *GetSlimEvent(TString detName);
	template<class T>
	T *GetEvent(TString outputName = ""); ///< Dummy template for GetEvent

	Event* GetMCEvent();
	EventHeader* GetEventHeader(TString outputName = "Reco");
	L0TPData* GetL0Data();
	L1TPData* GetL1Data();
	L2EBData* GetL2Data();
	BeamData* GetBeamData();
	HLTEvent* GetHLTData();
	L0TPSpecialTrigger* GetL0SpecialTrigger();
	L1TPSpecialTrigger* GetL1SpecialTrigger();
	L2EBSpecialTrigger* GetL2SpecialTrigger();
	BeamSpecialTrigger* GetBeamSpecialTrigger();
	TPrimitive* FindMatchingPrimitive(TString detName);
	std::vector<TPrimitive> FindAllPrimitiveInMatchingWindow(TString detName);
	void SetL0MatchingWindowWidth(TString detName, float ns);
	void SetL0MatchingWindowWidth(TString detName, int timeStamp, short fineTime);

	int GetRunID();
	int GetBurstID();
	int GetBurstTime();
	TString GetRevision();
	MCInfo* GetMCInfo();
	RecoInfo* GetRecoInfo();

	bool GetWithMC() const;
	bool GetWithEventHeader();
	bool GetIsTree();
	bool GetIsHisto();
	TFile * GetCurrentFile();
	Stream* GetStreamInfo();
	bool IsFiltering();

	std::vector<Core::AnalyzerIdentifier> GetProcessingHistory() const;
	int64_t GetFilterWord() const;
	Core::AnalyzerIdentifier GetAnalyzerFromBit(int bitNumber) const;
	int GetBitFromAnalyzer(TString analyzerID) const;
	bool GetAcceptFromBit(unsigned int bitNumber) const;
	bool GetAcceptFromAnalyzer(TString analyzerID) const;
	bool IsAnalyzerInHistory(TString analyzerID) const;
	bool AnalyzerRanOnEvent(TString analyzerID) const;

	bool BadQualityMask(EventHeader *header, DetectorID detector);

	//###### Other methods
	template<class T>
	T* GetObject(TString name, TString branchName = "") {
		/// \MemberDescr
		/// \param name : Name of the TTree from which the object is read
		/// \param branchName : name of the branch to retrieve (optional)
		/// \return the pointer to the object corresponding to the given tree (template version).
		///
		/// If branchName is left empty and there is only 1 branch requested on this tree, this
		/// single branch is returned. If there is more than 1 branch requested on this tree,
		/// return the first one.
		/// If branchName is specified, try to return the specified branch.
		/// \EndMemberDescr

		return reinterpret_cast<T*>(GetObjectVoid(name, branchName));
	}
	template<typename T>
	T GetPrimitiveObject(TString name, TString branchName = "") {
		/// \MemberDescr
		/// \param name : Name of the TTree from which the object is read
		/// \param branchName : name of the branch to retrieve (optional)
		/// \return the pointer to the object corresponding to the given tree (template version).
		///
		/// If branchName is left empty and there is only 1 branch requested on this tree, this
		/// single branch is returned. If there is more than 1 branch requested on this tree,
		/// return the first one.
		/// If branchName is specified, try to return the specified branch.
		/// \EndMemberDescr

		void * data = GetObjectVoid(name, branchName);
		return *reinterpret_cast<T*>(&data);
	}

	Long64_t GetNEvents();

	void setDisableSave(bool disableSave) {
		/// \MemberDescr
		/// \param disableSave : true/false
		///
		/// Set the disable save flag
		/// \EndMemberDescr
		fDisableSave = disableSave;
	}

	template<class T>
	void ReconfigureAnalyzer(TString analyzerName, TString parameterName,
			T parameterValue) {
		/// \MemberDescr
		/// \param analyzerName : Analyzer to reconfigure
		/// \param parameterName : Parameter to change
		/// \param parameterValue : New value for the parameter
		///
		/// Reconfigure an analyzer at processing time (parameters).
		/// Template call.
		/// \EndMemberDescr

		std::stringstream ss;
		ss << parameterValue;
		TString paramStringValue(ss.str());
		CallReconfigureAnalyzer(analyzerName, parameterName, paramStringValue);
	}

	void RedrawAnalyzerOM();
private:
	UserMethods();

	const void* GetOutputVoid(TString name, OutputState &state) const;
	bool RequestTreeVoid(TString name, TString branchName, TString className, void* obj);
	void* GetObjectVoid(TString name, TString branchName);
	void CallReconfigureAnalyzer(TString analyzerName, TString parameterName,
			TString parameterValue);

protected:
	bool fDisableSave; ///< Flag to disable to possibility of saving all histograms with SaveAllPlots
	Core::HistoHandler fHisto; ///< Local instance of HistoHandler

	TString fAnalyzerName; ///< Name of the analyzer

	Core::BaseAnalysis *fParent; ///< Pointer to the BaseAnalysis instance containing the analyzer
};

} /* namespace NA62Analysis */
#endif /* USERMETHODS_HH_ */
