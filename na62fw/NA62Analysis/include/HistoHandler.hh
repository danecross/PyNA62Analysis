/*
 * HistoHandler.hh
 *
 *  Created on: 21 Jan 2014
 *      Author: ncl
 */

#ifndef HISTOHANDLER_HH_
#define HISTOHANDLER_HH_

#include <set>

#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TGraph.h>
#include <TEfficiency.h>
#include <TTree.h>
#include <TCanvas.h>

#include "containers.hh"
#include "CanvasOrganizer.hh"

namespace NA62Analysis {
namespace Core {

/// \class HistoHandler
/// \Brief
/// Class containing and handling histograms
/// \EndBrief
///
/// \Detailed
/// Implements the histogram booking and filling methods as well as the methods for drawing and exporting.
/// Contains an Iterator class to allow the user to batch access a complete subset of histogram without having
/// to request them manually one by one.
/// \EndDetailed

class HistoHandler {
	/// \class Iterator
	/// \Brief
	/// Class for traversing sets of Histograms stored in HistoHandler.
	/// \EndBrief

	template <typename PointerType>
	class Iterator {
		friend class HistoHandler;
	public:
		Iterator();
		Iterator(const Iterator<PointerType>& c);
		~ Iterator();
		Iterator<PointerType>& operator=(const Iterator<PointerType>& c);
		bool operator==(const Iterator<PointerType>& rhs) const;
		bool operator==(const typename std::vector<PointerType*>::iterator& rhs) const;
		bool operator!=(const Iterator<PointerType>& rhs) const;
		bool operator!=(const typename std::vector<PointerType*>::iterator& rhs) const;
		PointerType* operator*();
		PointerType* operator->();

		PointerType* operator++(int);
		PointerType* operator--(int);
		PointerType* operator++();
		PointerType* operator--();

		int operator-(const Iterator<PointerType>& rhs) const;
		int operator-(const typename std::vector<PointerType*>::iterator& rhs) const;

		Iterator<PointerType> operator-(int rhs) const;
		Iterator<PointerType> operator+(int rhs) const;

		Iterator<PointerType>& operator-=(int rhs);
		Iterator<PointerType>& operator+=(int rhs);

		PointerType* operator[](int rhs);

		typename std::vector<PointerType*>::iterator End();
		typename std::vector<PointerType*>::iterator Begin();
		typename std::vector<PointerType*>::iterator end();
		typename std::vector<PointerType*>::iterator begin();

		bool operator<(const Iterator<PointerType>& rhs) const;
		bool operator<(const typename std::vector<PointerType*>::iterator& rhs) const;
		bool operator<=(const Iterator<PointerType>& rhs) const;
		bool operator<=(const typename std::vector<PointerType*>::iterator& rhs) const;
		bool operator>(const Iterator<PointerType>& rhs) const;
		bool operator>(const typename std::vector<PointerType*>::iterator& rhs) const;
		bool operator>=(const Iterator<PointerType>& rhs) const;
		bool operator>=(const typename std::vector<PointerType*>::iterator& rhs) const;
	private:
		Iterator(const std::vector<PointerType*>& list);
		Iterator(const Iterator<PointerType>& c, typename std::vector<PointerType*>::iterator it);

		std::vector<PointerType*> *fList; ///< Pointer to a vector containing pointers to histograms. This vector is shared between copies of Iterator.
		int *fNInstances; ///< Pointer to the number of copies of this Iterator.
		typename std::vector<PointerType*>::iterator fIterator; ///< Internal iterator over the vector.
	};
public:
	typedef Iterator<TH1> IteratorTH1; ///< Typedef for Iterator over TH1
	typedef Iterator<TH2> IteratorTH2; ///< Typedef for Iterator over TH2
	typedef Iterator<TH3> IteratorTH3; ///< Typedef for Iterator over TH3
	typedef Iterator<TGraph> IteratorTGraph; ///< Typedef for Iterator over TGraph
	typedef Iterator<TEfficiency> IteratorTEfficiency; ///< Typedef for Iterator over TEfficiency
	typedef Iterator<CanvasOrganizer> IteratorCanvas; ///< Typedef for Iterator over TCanvas

	HistoHandler();
	HistoHandler(const HistoHandler& c);
	virtual ~HistoHandler();

	template <class HistoType>
	void BookHisto(TString name, HistoType* const histo, TString analyzerName="", bool refresh=false, TString directory="", bool ownership=true){
		/// \MemberDescr
		/// \param name : Name of the histogram. (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram
		/// \param analyzerName : Name of the analyzer calling the method
		/// \param refresh : Set the plot as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plot when calling SaveAllPlots()
		/// \param ownership: is ownership transfered to HistoHandler?
		///
		/// Book a new histogram and make it available in the whole analyzer. If the name is in the format dir1/dir2/name,
		/// dir1/dir2 is used as directory to save the histogram, the name of the histogram is changed to name, and the
		/// whole string dir1/dir2/name is used as identification key for the histogram (in FillHisto, ...)
		/// \EndMemberDescr

		if(name.Contains("/") && directory.IsNull()){
			directory = name(0, name.Last('/'));
			if(TString(histo->GetName()).Contains("/"))
				histo->SetName((TString)name(name.Last('/')+1, name.Length()-name.Last('/')));
		}
		fHistoOrder.push_back(name);
		fHisto.insert(std::pair<TString,THObject>(name, THObject(histo)));
		//histo->SetNameTitle(name, name); // destroys original names: EG 15/6/16
		if(refresh) SetPlotAutoUpdate(name, analyzerName);
		if(directory.Length()>0) fHisto[name].SetDirectory(directory.Strip(TString::kBoth, '/'));
		fHisto[name].SetHasOwnership(ownership);
	}
	template <class HistoType>
	void BookHistoArray(TString baseName, HistoType* const histo, int number, TString analyzerName="", bool refresh=false, TString directory="", bool ownership=true){
		/// \MemberDescr
		/// \param baseName : Name of the histogram. The index will be appended. (can also be in the format dir1/dir2/name)
		/// \param histo : Pointer to the histogram to replicate
		/// \param analyzerName : Name of the analyzer calling the method
		///	\param number : Number of histograms to create
		/// \param refresh : Set the plots as AutoUpdate
		/// \param directory : analyzer subdirectory to save the plots when calling SaveAllPlots()
		/// \param ownership: is ownership transfered to HistoHandler?
		///
		/// Book an array of similar histograms and make it available in the whole analyzer. If the name is in the format dir1/dir2/name,
		/// dir1/dir2 is used as directory to save the histogram, the name of the histogram is changed to name, and the
		/// whole string dir1/dir2/name is used as identification key for the histogram (in FillHisto, ...)
		/// \EndMemberDescr

		TString currName, title;
		TString hName = histo->GetName();
		title = histo->GetTitle();

		if(baseName.Contains("/") && directory.IsNull()){
			directory = (TString)baseName(0, baseName.Last('/'));
			if(TString(histo->GetName()).Contains("/"))
				hName = baseName(baseName.Last('/')+1, baseName.Length()-baseName.Last('/'));
		}

		currName = baseName + "0";
		histo->SetName(TString(hName + "0").Data());
		histo->SetTitle(TString(title + "0").Data());
		fHistoOrder.push_back(currName);
		fHisto.insert(std::pair<TString,THObject>(currName, THObject(histo)));
		if(refresh) SetPlotAutoUpdate(currName, analyzerName);
		if(directory.Length()>0) fHisto[currName].SetDirectory(directory.Strip(TString::kBoth, '/'));
		fHisto[currName].SetHasOwnership(ownership);
		for(int i=1; i<number; i++){
			HistoType* h = (HistoType*)histo->Clone();
			currName = baseName + (Long_t)i;
			h->SetName(TString(hName + (Long_t)i).Data());
			h->SetTitle(TString(title + (Long_t)i).Data());
			fHistoOrder.push_back(currName);
			fHisto.insert(std::pair<TString,THObject>(currName, THObject(h)));
			if(refresh) SetPlotAutoUpdate(currName, analyzerName);
			if(directory.Length()>0) fHisto[currName].SetDirectory(directory.Strip(TString::kBoth, '/'));
			fHisto[currName].SetHasOwnership(ownership);
		}
	}

	//Histogram filling methods
	void FillHisto(TString name, TString x, double w);
	void FillHisto(TString name, TString x, double y, double w);
	void FillHisto(TString name, TString x, TString y, double w);
	void FillHisto(TString name, TString x, TString y, TString z, double w);
	void FillHisto(TString name, double x);
	void FillHisto(TString name, double x, double y, double w);
	void FillHisto(TString name, double x, double y, double z, double w);
	void FillHisto(TString name, double x, double y);
	void FillHisto(TString name, bool b, double x, double y, double z);

	void FillHistoArray(TString baseName, int index, TString x, double w);
	void FillHistoArray(TString baseName, int index, TString x, double y, double w);
	void FillHistoArray(TString baseName, int index, TString x, TString y, double w);
	void FillHistoArray(TString baseName, int index, TString x, TString y, TString z, double w);
	void FillHistoArray(TString baseName, int index, double x);
	void FillHistoArray(TString baseName, int index, double x, double y, double w);
	void FillHistoArray(TString baseName, int index, double x, double y, double z, double w);
	void FillHistoArray(TString baseName, int index, double x, double y);
	void FillHistoArray(TString baseName, int index, bool b, double x, double y=0, double z=0);

	//Export all histograms into output trees
	void ExportAllPlot(std::map<TString,TTree*> &trees, std::map<TString,void*> &branches);

	//Methods for drawing plots on screen
	void DrawAllPlots(TString analyzerName);
	void UpdatePlots(int evtNbr);
	void SetUpdateInterval(int interval);
	int GetUpdateInterval() const;
	void CreateCanvas(TString name, int width=0, int height=0);
	bool PlacePlotOnCanvas(TString histoName, TString canvasName, int row=-1, int col=-1);
	bool PlacePlotOnCanvas(std::initializer_list<TString> histoNames, TString canvasName, int row=-1, int col=-1);
	bool UpdateCanvas(TString canvasName) const;
	void SetCanvasReference(TString canvas, TString histo, TH1* refPtr);
	void SetCanvasReference(TString canvas, TString histo, TGraph* refPtr);

	//Save all plots into output file
    void SaveAllPlots(TString analyzerName, bool store_empty_plots = true);

	void PrintInitSummary() const;
	void SetPlotAutoUpdate(TString name, TString analyzerName);
	bool SetCanvasAutoUpdate(TString canvasName);

	double compareToReferencePlot(const TH1* const hRef, const TH1* const h2, bool KS);

	bool Exists(TString name);
	TH1* GetTH1(TString name);
	TH2* GetTH2(TString name);
	TH3* GetTH3(TString name);
	TGraph* GetTGraph(TString name);
	TEfficiency* GetTEfficiency(TString name);
	TH1* GetHisto(TString name);
	TH1* GetHistoFromArray(TString baseName, int index);
	TGraph* GetGraphFromArray(TString baseName, int index);
	TEfficiency* GetEfficiencyFromArray(TString baseName, int index);

	IteratorTH1 GetIteratorTH1();
	IteratorTH1 GetIteratorTH1(TString baseName);
	IteratorTH2 GetIteratorTH2();
	IteratorTH2 GetIteratorTH2(TString baseName);
	IteratorTH3 GetIteratorTH3();
	IteratorTH3 GetIteratorTH3(TString baseName);
	IteratorTGraph GetIteratorTGraph();
	IteratorTGraph GetIteratorTGraph(TString baseName);
	IteratorTEfficiency GetIteratorTEfficiency();
	IteratorTEfficiency GetIteratorTEfficiency(TString baseName);
	IteratorCanvas GetIteratorCanvas();
	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type GetCanvases() {
		/// \MemberDescr
		/// \return Map containing the list of CanvasOrganizer for this analyzer
		/// \EndMemberDescr
		return fCanvas;
	}
private:
	void Mkdir(TString name, TString analyzerName) const;

	/// \class THObject
	/// \Brief
	/// Class containing any kind of Histogram (TH1, TH2, TH3, TGraph, TEfficiency)
	/// as an union.\n
	/// It contains all the methods required to consistently construct, get or
	/// fill the correction union member.
	/// \EndBrief
	class THObject {
		public:
			explicit THObject(): fIsOnCanvas(false), fHasOwnership(true), fNPoints(0), fHisto(), fType(TTUNKNOWN), fDirectory("")  {};
			explicit THObject(TH1* g): fIsOnCanvas(false), fHasOwnership(true), fNPoints(0), fHisto(g), fType(TTH1), fDirectory("")  {};
			explicit THObject(TH2* g): fIsOnCanvas(false), fHasOwnership(true), fNPoints(0), fHisto(g), fType(TTH2), fDirectory("")  {};
			explicit THObject(TH3* g): fIsOnCanvas(false), fHasOwnership(true), fNPoints(0), fHisto(g), fType(TTH3), fDirectory("")  {};
			explicit THObject(TGraph* g): fIsOnCanvas(false), fHasOwnership(true), fNPoints(0), fHisto(g), fType(TTGraph), fDirectory("")  {};
			explicit THObject(TEfficiency* g): fIsOnCanvas(false), fHasOwnership(true), fNPoints(0), fHisto(g), fType(TTEfficiency), fDirectory("")  {};
			~THObject(){};
			TH1* GetHisto() {return fHisto.histo;};
			TH2* GetHisto2() {return static_cast<TH2*>(fHisto.histo);}
			TH3* GetHisto3() {return static_cast<TH3*>(fHisto.histo);}
			TGraph* GetGraph() {return fHisto.graph;};
			TEfficiency* GetEfficency() {return fHisto.efficiency;};
			bool IsOnCanvas() const {return fIsOnCanvas;};
			void SetIsOnCanvas(bool b) {fIsOnCanvas = b;};
			void SetDirectory(TString d) { fDirectory = d;};
			TString GetDirectory() const { return fDirectory;};
			plotType_e GetType() const { return fType;};
			int IncrementPoints() { return ++fNPoints;};
			void Clear() { fHisto.histo = nullptr; };
			void SetHasOwnership(bool val) { fHasOwnership=val;};
			bool IsOwner() { return fHasOwnership;};
		private:
			bool fIsOnCanvas; ///< Is it on a canvas?
			bool fHasOwnership; ///< Do we have ownership?
			int fNPoints; ///< Number of points in the TGraph
			plotType_u fHisto; ///< Union containing the pointer to the object
			plotType_e fType;  ///< Enum identifying the object type in the union
			TString fDirectory; ///< Directory in which the object must be saved
	};

	//Histogram containers
	NA62Analysis::NA62Map<TString,THObject>::type fHisto; ///< Container for TH1
	NA62Analysis::NA62Map<TString, CanvasOrganizer*>::type fCanvas; ///< Container for the TCanvas
	std::vector<TString> fCanvasOrder; ///< Container for the booking order of canvases
	NA62Analysis::NA62Map<TString,TTree*>::type fOutTree; ///< Container for the output TTrees
	std::vector<TString> fHistoOrder; ///< Container for the booking order of histos
	NA62Analysis::NA62Map<TString,IteratorTH1>::type fTH1IteratorsList; ///< Container for TH1 Iterators (keep them in memory rather than building them again for efficiency reasons)
	NA62Analysis::NA62Map<TString,IteratorTH2>::type fTH2IteratorsList; ///< Container for TH2 Iterators (keep them in memory rather than building them again for efficiency reasons)
	NA62Analysis::NA62Map<TString,IteratorTH3>::type fTH3IteratorsList; ///< Container for TH3 Iterators (keep them in memory rather than building them again for efficiency reasons)
	NA62Analysis::NA62Map<TString,IteratorTGraph>::type fTGraphIteratorsList; ///< Container for TH1 Iterators (keep them in memory rather than building them again for efficiency reasons)
	NA62Analysis::NA62Map<TString,IteratorTEfficiency>::type fTEfficiencyIteratorsList; ///< Container for TEfficiency Iterators (keep them in memory rather than building them again for efficiency reasons)
	NA62Analysis::NA62Map<TString,IteratorCanvas>::type fCanvasIteratorsList; ///< Container for TCanvas Iterators (keep them in memory rather than building them again for efficiency reasons)

	std::set<TString> fAutoUpdateList; ///< List of histogram being regularly updated on screen during processing

	int fUpdateRate; ///< Event interval at which the plots should be updated
};

template <typename PointerType>
HistoHandler::Iterator<PointerType> operator+(int lhs, HistoHandler::Iterator<PointerType> rhs){
	/// \MemberDescr
	/// \param lhs : increment value
	/// \param rhs : Iterator to increment
	///
	/// Return a copy of the Iterator rhs incremented by lhs (equivalent to rhs + lhs).
	/// \EndMemberDescr

	return rhs + lhs;
}
template <typename PointerType>
HistoHandler::Iterator<PointerType> operator-(int lhs, HistoHandler::Iterator<PointerType> rhs){
	/// \MemberDescr
	/// \param lhs : decrement value
	/// \param rhs : Iterator to decrement
	///
	/// Return a copy of the Iterator rhs decremented by lhs (equivalent to rhs + lhs).
	/// \EndMemberDescr

	return rhs - lhs;
}

template<typename PointerType>
bool operator<(typename std::vector<PointerType*>::iterator lhs, const HistoHandler::Iterator<PointerType>& rhs){
	/// \MemberDescr
	/// \param lhs : Iterator to compare
	/// \param rhs : Iterator to compare
	///
	/// Compare iterators. Return true if the distance between both Iterators is smaller than 0 (lhs < rhs).
	/// \EndMemberDescr

	return rhs>lhs;
}
template<typename PointerType>
bool operator<=(typename std::vector<PointerType*>::iterator lhs, const HistoHandler::Iterator<PointerType>& rhs){
	/// \MemberDescr
	/// \param lhs : Iterator to compare
	/// \param rhs : Iterator to compare
	///
	/// Compare iterators. Return true if the distance between both Iterators is smaller or equal to 0 (lhs <= rhs).
	/// \EndMemberDescr

	return rhs>=lhs;
}
template<typename PointerType>
bool operator>(typename std::vector<PointerType*>::iterator lhs, const HistoHandler::Iterator<PointerType>& rhs){
	/// \MemberDescr
	/// \param lhs : Iterator to compare
	/// \param rhs : Iterator to compare
	///
	/// Compare iterators. Return true if the distance between both Iterators is greater than 0 (lhs > rhs).
	/// \EndMemberDescr

	return rhs<lhs;
}
template<typename PointerType>
bool operator>=(typename std::vector<PointerType*>::iterator lhs, const HistoHandler::Iterator<PointerType>& rhs){
	/// \MemberDescr
	/// \param lhs : Iterator to compare
	/// \param rhs : Iterator to compare
	///
	/// Compare iterators. Return true if the distance between both Iterators is greater or equal to 0 (lhs >= rhs).
	/// \EndMemberDescr

	return rhs<=lhs;
}

} /* namespace Core */
} /* namespace NA62Analysis */

#endif /* HISTOHANDLER_HH_ */
