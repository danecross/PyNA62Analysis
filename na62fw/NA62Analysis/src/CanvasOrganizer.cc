/*
 * CanvasOrganizer.cc
 *
 *  Created on: Jun 16, 2015
 *      Author: nlurkin
 */

#include <iostream>
#include "CanvasOrganizer.hh"
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TEfficiency.h>
#include <TList.h>
#include <cmath>
#include "Indexer.hh"
#include <algorithm>

namespace NA62Analysis {
namespace Core {

bool operator==(plotType_u lhs, TH1* rhs){ return lhs.histo==rhs; }
bool operator==(plotType_u lhs, TGraph* rhs){ return lhs.graph==rhs; }
bool operator==(plotType_u lhs, TEfficiency* rhs){ return lhs.efficiency==rhs; }
template <class T>
bool Exists(std::vector<plotType_u> hs, T n){
	return std::find(hs.begin(), hs.end(), n)!=hs.end();
}


CanvasOrganizer::CanvasOrganizer(TString name):
	fUpdateFrequency(1),
	fWidth(0),
	fHeight(0),
	fChanged(true),
	fName(name),
	fCanvas(NULL)
{
	/// \MemberDescr
	/// \param name : Name of the CanvasOrganizer
	///
	/// Default constructor.
	/// \EndMemberDescr
}

CanvasOrganizer::~CanvasOrganizer() {
	/// \MemberDescr
	/// Default destructor. Delete the canvas if exists
	/// \EndMemberDescr

	if(fCanvas) delete fCanvas;
}

void CanvasOrganizer::Draw() const {
	/// \MemberDescr
	/// Draw the canvas. If dimensions are not given (or cannot fit all histograms),
	/// width and height are computed according to internal algorithm
	/// (width/height ration must be < 1).
	/// Histograms are then placed on the canvas and it is drawn.
	/// \EndMemberDescr

	//If canvas does not exist, create it.
	if(!fCanvas) fCanvas = new TCanvas(fName, fName);
	fCanvas->Clear();
	fCanvas->Draw();

	//if canvas cannot accommodate all histo, resize it
	if((unsigned int)(fWidth*fHeight)<fHistos.size()) {
		size_t s = computeSize(fHistos.size());
		fCanvas->Divide(s.width, s.height);

	}
	else fCanvas->Divide(fWidth, fHeight);

	std::vector<plot_t> delayedPlots;
	std::set<int> busySlots;
	//Place all histos on the canvas
	for(auto it : fHistos){
		if(it.row!=-1 && it.row<fHeight && it.col!=-1 && it.col<fWidth){
			int canvasIndex = it.row*fWidth + it.col+1;
			fCanvas->cd(canvasIndex);
			busySlots.insert(canvasIndex);
			drawPlot(it);
		}
		else{
			delayedPlots.push_back(it);
		}

	}

	for(auto it : delayedPlots){
		int slot = findAvailableSlot(busySlots);
		busySlots.insert(slot);
		fCanvas->cd(slot);
		drawPlot(it);
	}
	fChanged = false;
}

void CanvasOrganizer::drawPlot(plot_t p) const{
	/// \MemberDescr
	/// \param p: Plot to draw
	///
	/// Properly draws the histogram depending on its type
	/// \EndMemberDescr

	if(p.tag == TTH1){
		TString option = "";
		for(auto itHisto : p.ptr){
			itHisto.histo->Draw(option);
			option = "SAME";
		}
		if(p.ref.histo!=nullptr){
			p.ref.histo->SetLineColor(2);
			p.ref.histo->SetLineStyle(2);
			p.ref.histo->Draw("SAME");
		}
	}
	else if(p.tag == TTH2) p.ptr[0].histo->Draw("colz");
	else if(p.tag == TTGraph){
		TString option = "A*";
		for(auto itHisto : p.ptr){
			itHisto.graph->Draw(option);
			option = "SAMEA*";
		}
		if(p.ref.graph!=nullptr){
			p.ref.graph->Draw("SAMEA*");
		}
	}
	else if(p.tag == TTEfficiency){
		TString option = "AP";
		for(auto itHisto : p.ptr){
			itHisto.efficiency->Draw(option);
			option = "SAMEAP";
		}
	}
}

void CanvasOrganizer::Update(int currentEvent) const {
	/// \MemberDescr
	/// \param currentEvent : Currently processed event number
	///
	/// Update the canvas if it exist and the current event number
	/// matches the update frequency
	/// \EndMemberDescr

	if(fCanvas) {
		if(currentEvent % fUpdateFrequency==0){
			if(fChanged) Draw();
			UpdateRef();
			fCanvas->Update();
			fCanvas->Draw();
		}
	}
}

void CanvasOrganizer::AddHisto(TH1* histoPtr, int row, int col) {
	/// \MemberDescr
	/// \param histoPtr : Pointer to added histogram
	/// \param row : Row position
	/// \param col : Column position
	///
	/// Add an histogram in the list of managed histograms.\n
	/// If the histogram at this position already exists, superimpose
	/// it (use SAME when drawing).
	/// \EndMemberDescr

	int index = GetPlotIndexAt(row,col);
	if(index==-1){
		plot_t t;
		t.ptr.push_back(histoPtr);
		t.tag = TTH1;
		t.ref.histo = nullptr;
		t.row = row;
		t.col = col;
		fHistos.push_back(t);
	}
	else
		fHistos[index].ptr.push_back(histoPtr);

	fChanged=true;
}

void CanvasOrganizer::AddHisto(std::vector<TH1*> histoPtr, int row, int col) {
	int index = GetPlotIndexAt(row,col);
	if(index==-1){
		plot_t t;
		for(auto histo : histoPtr)
			t.ptr.push_back(histo);
		t.tag = TTH1;
		t.ref.histo = nullptr;
		t.row = row;
		t.col = col;
		fHistos.push_back(t);
	}
	else
		for(auto histo : histoPtr){
			fHistos[index].ptr.push_back(histo);
	}
	fChanged=true;
}

void CanvasOrganizer::AddHisto(TH2* histoPtr, int row, int col) {
	/// \MemberDescr
	/// \param histoPtr : Pointer to added histogram
	/// \param row : Row position
	/// \param col : Column position
	///
	/// Add an histogram in the list of managed histograms.\n
	/// If the histogram at this position already exists, replace it.
	/// \EndMemberDescr

	int index = GetPlotIndexAt(row,col);
	if(index==-1){
		plot_t t;
		t.ptr.push_back(histoPtr);
		t.tag = TTH2;
		t.ref.histo = nullptr;
		t.row = row;
		t.col = col;
		fHistos.push_back(t);
	}
	else
		fHistos[index].ptr[0].histo = histoPtr;

	fChanged=true;
}

void CanvasOrganizer::AddHisto(TGraph* histoPtr, int row, int col) {
	/// \MemberDescr
	/// \param histoPtr : Pointer to added histogram
	/// \param row : Row position
	/// \param col : Column position
	///
	/// Add an histogram in the list of managed histograms.\n
	/// If the histogram at this position already exists, superimpose
	/// it (use SAME when drawing).
	/// \EndMemberDescr

	int index = GetPlotIndexAt(row,col);
	if(index==-1){
		plot_t t;
		t.ptr.push_back(histoPtr);
		t.tag = TTGraph;
		t.ref.graph = nullptr;
		t.row = row;
		t.col = col;
		fHistos.push_back(t);
	}
	else
		fHistos[index].ptr.push_back(histoPtr);

	fChanged=true;
}

void CanvasOrganizer::AddHisto(std::vector<TGraph*> histoPtr, int row, int col) {
	int index = GetPlotIndexAt(row,col);
	if(index==-1){
		plot_t t;
		for(auto histo : histoPtr)
			t.ptr.push_back(histo);
		t.tag = TTGraph;
		t.ref.graph = nullptr;
		t.row = row;
		t.col = col;
		fHistos.push_back(t);
	}
	else
		for(auto histo : histoPtr){
			fHistos[index].ptr.push_back(histo);
	}
	fChanged=true;
}

void CanvasOrganizer::AddHisto(TEfficiency* histoPtr, int row, int col) {
	/// \MemberDescr
	/// \param histoPtr : Pointer to added histogram
	/// \param row : Row position
	/// \param col : Column position
	///
	/// Add an histogram in the list of managed histograms.\n
	/// If the histogram at this position already exists, superimpose
	/// it (use SAME when drawing).
	/// \EndMemberDescr

	int index = GetPlotIndexAt(row,col);
	if(index==-1){
		plot_t t;
		t.ptr.push_back(histoPtr);
		t.tag = TTEfficiency;
		t.ref.efficiency = nullptr;
		t.row = row;
		t.col = col;
		fHistos.push_back(t);
	}
	else
		fHistos[index].ptr.push_back(histoPtr);

	fChanged=true;
}

void CanvasOrganizer::AddHisto(std::vector<TEfficiency*> histoPtr, int row, int col) {
	int index = GetPlotIndexAt(row,col);
	if(index==-1){
		plot_t t;
		for(auto histo : histoPtr)
			t.ptr.push_back(histo);
		t.tag = TTEfficiency;
		t.ref.efficiency = nullptr;
		t.row = row;
		t.col = col;
		fHistos.push_back(t);
	}
	else
		for(auto histo : histoPtr){
			fHistos[index].ptr.push_back(histo);
	}
	fChanged=true;
}

void CanvasOrganizer::SetCanvas(TCanvas* c) {
	/// \MemberDescr
	/// \param c : Pointer to a TCanvas
	///
	/// Assign a TCanvas to the CanvasOrganizer
	/// \EndMemberDescr

	fCanvas = c;
	fChanged = true;
}

CanvasOrganizer::size_t CanvasOrganizer::computeSize(int nElements) const {
	/// \MemberDescr
	/// \param nElements : Number of elements to fit on the canvas
	/// \return size_t structure with ideal dimensions
	///
	/// Computes the ideal size for the canvas to accommodate all elements.
	/// The requirement is that the ratio width/height should never be larger than 1.
	/// \EndMemberDescr

	size_t s;
	s.width = ceil(sqrt(nElements));
	s.height = ceil(nElements/(double)s.width);
	return s;
}

void CanvasOrganizer::SetReference(TH1* refPtr, TH1* histoPtr) {
	/// \MemberDescr
	/// \param refPtr Pointer to a reference histogram
	/// \param histoPtr Pointer to the histogram on which the reference should be overlaid
	///
	/// Link a reference histogram to an histogram already added to the canvas.
	/// \EndMemberDescr

	for(auto &it : fHistos){
		if(Exists(it.ptr, histoPtr)) it.ref.histo=refPtr;
	}
}

void CanvasOrganizer::SetReference(TGraph* refPtr, TGraph* histoPtr) {
	/// \MemberDescr
	/// \param refPtr Pointer to a reference histogram
	/// \param histoPtr Pointer to the histogram on which the reference should be overlaid
	///
	/// Link a reference histogram to an histogram already added to the canvas.
	/// \EndMemberDescr

	for(auto &it : fHistos){
		if(Exists(it.ptr,histoPtr)) it.ref.graph=refPtr;
	}
}

void CanvasOrganizer::UpdateRef() const {
	/// \MemberDescr
	/// Update reference histograms: scale them to the same integral as the linked histogram.
	/// \EndMemberDescr

	for(auto it : fHistos){
		if(it.ref.histo!=nullptr){
			if(it.tag==TTH1){
				it.ref.histo->Scale(it.ptr[0].histo->Integral()/it.ref.histo->Integral(), "");
			}
		}
	}
}

int CanvasOrganizer::findAvailableSlot(std::set<int> busy){
	/// \MemberDescr
	/// \param busy: Sorted set of numbers
	///
	/// \return First available number not in busy (starting from 1)
	/// \EndMemberDescr

	int i=1;
	for(auto  val : busy){
		if(val>i) return i;
		i++;
	}
	return i+1;
}

int CanvasOrganizer::GetPlotIndexAt(int row, int col) {

	if(row==-1 || col==-1)
		return -1;

    auto it = std::find_if(fHistos.begin(), fHistos.end(), [col,row] (const plot_t &p) {return p.col==col && p.row==row;});

    if(it==fHistos.end())
        return -1;

    return it-fHistos.begin();
}

} /* namespace Core */
} /* namespace NA62Analysis */

