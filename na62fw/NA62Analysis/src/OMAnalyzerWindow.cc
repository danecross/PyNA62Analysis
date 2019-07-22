/*
 * OMAnalyzerWindow.cc
 *
 *  Created on: Jun 18, 2015
 *      Author: nlurkin
 */

#include "OMAnalyzerWindow.hh"
#include <TRootEmbeddedCanvas.h>
#include <TGTab.h>

namespace NA62Analysis {
namespace Core {

OMAnalyzerWindow::OMAnalyzerWindow(TGCompositeFrame* parent):
	fParentFrame(parent)
{
	/// \MemberDescr
	/// \param parent: parent element
	///
	/// Default constructor. Creates the tab element and add it to the parent frame
	/// \EndMemberDescr

	fCanvasTab = new TGTab(fParentFrame);
	fParentFrame->AddFrame(fCanvasTab, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));
}

OMAnalyzerWindow::~OMAnalyzerWindow() {
	/// \MemberDescr
	/// Default destructor.
	/// \EndMemberDescr

	for(auto canvas : fCanvasList)
		delete canvas.second;
	delete fCanvasTab;
}

TCanvas* OMAnalyzerWindow::AddTab(TString tabName) {
	/// \MemberDescr
	/// \param tabName : Name of the tab to add
	/// \return Pointer to the newly created canvas, to be used to draw in the created
	/// window.
	///
	/// Add a tab to the analyzer window
	/// \EndMemberDescr

	TGCompositeFrame *f = fCanvasTab->AddTab(tabName);
	TStorage x;
	TRootEmbeddedCanvas *c = new TRootEmbeddedCanvas(tabName, f, gClient->GetDisplayHeight()-100, gClient->GetDisplayWidth()-100);
	fCanvasList.insert(std::pair<const TString, TGCompositeFrame*>(tabName, f));
	f->AddFrame(c, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));
	return c->GetCanvas();
}

bool OMAnalyzerWindow::Exists(TString tabName){
	return fCanvasList.count(tabName)>0;
}

} /* namespace Core */
} /* namespace NA62Analysis */
