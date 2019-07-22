// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-03-17
//
// ---------------------------------------------------------------

#include "CollectNumbersOfTriggers.hh"
#include "Misc.hh"
#include "BaseAnalysis.hh"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class CollectNumbersOfTriggers
/// \Brief
/// Builds a table of numbers of triggers per burst
/// \EndBrief
/// \Detailed
/// Runs in the "--histo" mode and produces an output file in the following format:
/// (Run ID, burst ID, burst time, number of triggers, filename). Bad bursts are not skipped.
/// The recommended usage is as follows:
/// \code
/// ./MyApp -l <list> --histo -v0 >& log
/// \endcode
/// This will produce:
/// 1) the file NA62Analysis.skipped with the list of files that cannot be opened (if any);
/// 2) the log file with the list of files without a Reco tree;
/// 3) the file NumberOfTriggers.txt containing information for the remaining "good" files.
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

CollectNumbersOfTriggers::CollectNumbersOfTriggers(Core::BaseAnalysis *ba) :
  Analyzer(ba, "CollectNumbersOfTriggers") {
  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
}

void CollectNumbersOfTriggers::InitHist() {
  if (!GetIsHisto() || GetIsTree()) {
    cout << user_normal() << "Error: must be run in the --histo mode" << endl;
    exit(kWrongConfiguration);
  }
  fOutputStream.open("./NumberOfTriggers.txt");
  fOutputStream << "# Run ID, burst ID, burst time, number of triggers, filename" << std::endl;
}

void CollectNumbersOfTriggers::Process(int) {

  TFile* file = GetCurrentFile();
  if (!file || !file->IsOpen() || file->IsZombie()) {
    std::cout << user_normal() << "Problem opening file" << std::endl;
    return;
  }

  TString fname = file->GetName();
  if      (fname.Contains("eos"))    fname = fname.Remove(0,23);
  else if (fname.Contains("castor")) fname = fname.Remove(0,28);

  BurstID burstInfo;
  Bool_t  burstInfoSuccess = GetCurrentBurstInfo(file, burstInfo);

  if (burstInfoSuccess) {
    fOutputStream << Form("%5d %4d %10d %6d %s\n",
			  burstInfo.fRunID, burstInfo.fBurstID, burstInfo.fBurstTime,
			  burstInfo.fNEventsInTree, fname.Data());
  }
  else {
    std::cout << user_normal() << "File " <<
      fname.Data()<<" contains no Reco tree" << std::endl;
    fOutputStream << Form("%5d %4d %10d %6d %s\n", 0, 0, 0, 0, fname.Data());
  }
}

void CollectNumbersOfTriggers::EndOfJobUser() {
  fOutputStream.close();
}
