// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "SAVCoarseT0.hh"

SAVCoarseT0::SAVCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "SAV") {
  fNROMezzaninesPerFullBoard = 16; // CREAM-readout
}

void SAVCoarseT0::EndOfJobUser() {

  if (fHDigiTimeRawFine){

    Int_t NMezzanines = fHDigiTimeRawFine->GetNbinsX();
    Int_t NBins = fHDigiTimeRawFine->GetNbinsY();

    for (Int_t i=1; i<=NMezzanines; i++){

      for (Int_t j=2; j<=NBins; j++) {

        Double_t this_bin_content = fHDigiTimeRawFine->GetBinContent(i,j);
        Double_t prev_bin_content = fHDigiTimeRawFine->GetBinContent(i,j-1);
        Double_t next_bin_content = fHDigiTimeRawFine->GetBinContent(i,j+1);

        //cout <<"Mezzanine "<<i<<" bin "<<j<<" content "<<this_bin_content;

        if (2.*this_bin_content /(prev_bin_content + next_bin_content) > 1.2)
          fHDigiTimeRawFine->SetBinContent(i,j,0.5*(prev_bin_content+next_bin_content));

        //cout <<" after "<<fHDigiTimeRawFine->GetBinContent(i,j)<<endl;
      }
    }
  }
  CoarseT0Evaluation::EndOfJobUser();
}
