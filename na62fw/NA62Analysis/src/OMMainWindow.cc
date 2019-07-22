/*
 * OMMainWindow.cc
 *
 *  Created on: Jun 17, 2015
 *      Author: nlurkin
 */

#include "OMMainWindow.hh"
#include <TGTab.h>
#include <TGFrame.h>

#include "OMAnalyzerWindow.hh"

namespace NA62Analysis {
namespace Core {

OMMainWindow::OMMainWindow(const TGWindow *p, UInt_t w, UInt_t h) {
	/// \MemberDescr
	/// \param p: parent element
	/// \param w: width
	/// \param h: height
	///
	/// Default constructor.
	/// Create a mainFrame and the main tab element. Add the tab element to
	/// the main frame.
	/// \EndMemberDescr

	fMainFrame = new TGMainFrame(p, w, h, kMainFrame | kVerticalFrame);
	fAnalyzerTab = new TGTab(fMainFrame);

	fMainFrame->AddFrame(fAnalyzerTab, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));
}

OMMainWindow::~OMMainWindow() {
	/// \MemberDescr
	/// Default destructor
	/// \EndMemberDescr

	for(auto analyzerTab : fListAnalyzerTab)
		delete analyzerTab.second;
	delete fAnalyzerTab;
	delete fMainFrame;
}

void OMMainWindow::Create() {
	/// \MemberDescr
	/// Create the window and display it
	/// \EndMemberDescr

	fMainFrame->MapSubwindows();
	fMainFrame->Resize();

	fMainFrame->MapWindow();
}

void OMMainWindow::AddAnalyzerTab(TString analyzerName) {
	/// \MemberDescr
	/// \param analyzerName: name of the analyzer for which the tab is added
	///
	/// Add a tab for the specified analyzer
	/// \EndMemberDescr

	OMAnalyzerWindow *window = new OMAnalyzerWindow(fAnalyzerTab->AddTab(analyzerName));

	fListAnalyzerTab.insert(std::pair<const TString, OMAnalyzerWindow*>(analyzerName, window));
}

TCanvas* OMMainWindow::AddAnalyzerCanvas(TString analyzerName,
		TString canvasName) {
	/// \MemberDescr
	/// \param analyzerName : Name of the analyzer
	/// \param canvasName : Name of the canvas
	/// \return Pointer to the newly created canvas, to be used to draw in
	/// the window.
	///
	/// Add a new tab for a canvas for the specified analyzer
	/// \EndMemberDescr

	OMAnalyzerWindow * w = fListAnalyzerTab.find(analyzerName)->second;
	return w->AddTab(canvasName);
}

void OMMainWindow::UpdateTitle(int runNumber, int burstNumber){
	/// \MemberDescr
	/// \param runNumber
	/// \param burstNumber
	///
	/// Update the title of the window with the run and burst numbers
	/// \EndMemberDescr
	fMainFrame->SetWindowName(Form("Run: %i, Burst: %i", runNumber, burstNumber));
}

bool OMMainWindow::Exists(TString anName){
	return fListAnalyzerTab.count(anName)>0;
}

bool OMMainWindow::AnalyzerCanvasExists(TString anName, TString canvasName){
	auto it = fListAnalyzerTab.find(anName);
	if(it != fListAnalyzerTab.end())
		return it->second->Exists(canvasName);
	return false;
}

} /* namespace Core */
} /* namespace NA62Analysis */

