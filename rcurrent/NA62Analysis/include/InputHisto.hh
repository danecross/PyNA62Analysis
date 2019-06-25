/*
 * IOHisto.h
 *
 *  Created on: Mar 9, 2015
 *      Author: ncl
 */

#ifndef IOHISTO_H_
#define IOHISTO_H_

#include "containers.hh"
#include "InputHandler.hh"

class TH1;
class TH2;
class TGraph;

namespace NA62Analysis {
namespace Core {

/// \class InputHisto
/// \Brief
/// Class containing and handling every Input histograms
/// \EndBrief
///
/// \Detailed
/// Inherits InputHandler and implements the Input methods for histograms\n
/// Load input files with reference histograms and give access to these histograms, update them.\n
/// \EndDetailed
class InputHisto: public InputHandler {
public:
	InputHisto();
	explicit InputHisto(const std::string &name);
	InputHisto(const InputHisto &c);
	virtual ~InputHisto();

	virtual bool OpenInput(bool specialOnly=false);
	void SetReferenceFileName(TString fileName);
	bool CheckNewFileOpened();
	virtual bool LoadEvent(Long64_t &iEvent);
	virtual bool Rewind();
	virtual bool ReloadLatest();
	bool GetWithMC() const;

	//Histogram
	TH1* GetInputHistogram(TString directory, TString name, bool append);
	TH1* GetReferenceTH1(TString name, TString directory);
	TH2* GetReferenceTH2(TString name, TString directory);
	TGraph* GetReferenceTGraph(TString name, TString directory);
	void UpdateInputHistograms();

private:
	bool checkBadFile();
	virtual Long64_t SkipBadBurst(Long64_t currentEntry, bool =false);

	bool fNewFileOpened; ///< Indicates if a new file has been opened
	bool fWithMC; ///< Do we have MC in the file?

	NA62Analysis::NA62MultiMap<TString, TH1*>::type fInputHistoAdd; ///< Container for input histograms for which we append the values of the new files
	NA62Analysis::NA62MultiMap<TString, TH1*>::type fInputHisto; ///< Container for input histograms for which we do not append the values of the new files

	TString fReferenceFileName; ///< Name of the file containing reference plots to compare with
};

} /* namespace Core */
} /* namespace NA62Analysis */

#endif /* IOHISTO_H_ */
