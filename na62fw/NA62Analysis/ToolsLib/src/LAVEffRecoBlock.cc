#include <stdlib.h>
//#include "LAVDigest.hh"
#include "LAVEffRecoBlock.hh"
#include "LAVRecoHit.hh"
#include "Persistency.hh"
#include "TDCEvent.hh"
using namespace std;


LAVEffRecoBlock::LAVEffRecoBlock(Int_t blockID) : LAVRecoBlock::LAVRecoBlock(blockID) { // BlockID without threshold identifier
  fBlockID = blockID;
  fEdgeCollection.clear();
  fEdgesAreSorted = 0;
  fNHits = 0;
} 


void LAVEffRecoBlock::CreateEffHits(){

  if (!fEdgesAreSorted) {
    cout << "---LAVRecoBlock Warning: calling CreateHits but no sorting done\n";
    LAVRecoBlock::SortEdges();
  }

  if (fNHits) {
    cout << "---LAVRecoBlock Error: Hits for this block " << fBlockID << " are already present and are " << fNHits << endl;
    return;
  }

  fOverFlow = 0;
  fOpenHit = 0;

  // Loop over hits for each threshold

  for (Int_t iTh = 0; iTh < 2; iTh++) {
    for (Int_t i=0; i<(Int_t) fEdgeCollection.size(); i++) {
      if (fEdgeCollection.at(i).Threshold == iTh) {
	
	if (fOverFlow) {
	  cout << "---LAVRecoBlock CreateHits warning: LAVCRecoHit overflow!! " << fEdgeCollection.size() << endl;
	  break;
	}
	
	if (fOpenHit) {

	  if (fEdgeCollection.at(i).Type == 0) { // Second leading at this threshold
	    LAVRecoBlock::CloseHit(); // Create isolated leading hit
	    if (LAVRecoBlock::OpenHit()) { // Open new hit
	      fRecoHits[fNHits] = new LAVRecoHit(fBlockID);
	      if (iTh == 0) {
		fRecoHits[fNHits]->SetLeadingEdgeLow(fEdgeCollection.at(i).Time);
		fRecoHits[fNHits]->SetDigiLeadingEdgeLow(fEdgeCollection.at(i).Digi);
	      } else {
		fRecoHits[fNHits]->SetLeadingEdgeHigh(fEdgeCollection.at(i).Time);
		fRecoHits[fNHits]->SetDigiLeadingEdgeHigh(fEdgeCollection.at(i).Digi);
	      }
	    }

	  } else if (fEdgeCollection.at(i).Type == 1) { // Trailing after leading - simply close hit
	    if (iTh == 0) {
	      fRecoHits[fNHits]->SetTrailingEdgeLow(fEdgeCollection.at(i).Time);
	      fRecoHits[fNHits]->SetDigiTrailingEdgeLow(fEdgeCollection.at(i).Digi);
	    } else {
	      fRecoHits[fNHits]->SetTrailingEdgeHigh(fEdgeCollection.at(i).Time);
	      fRecoHits[fNHits]->SetDigiTrailingEdgeHigh(fEdgeCollection.at(i).Digi);
	    }
	    LAVRecoBlock::CloseHit();
	  }

	} else {

	  if (fEdgeCollection.at(i).Type == 0) { // Leading edge - create new hit
	    if (LAVRecoBlock::OpenHit()) {
	      fRecoHits[fNHits] = new LAVRecoHit(fBlockID);
	      if (iTh == 0) {
		fRecoHits[fNHits]->SetLeadingEdgeLow(fEdgeCollection.at(i).Time);
		fRecoHits[fNHits]->SetDigiLeadingEdgeLow(fEdgeCollection.at(i).Digi);
	      } else {
		fRecoHits[fNHits]->SetLeadingEdgeHigh(fEdgeCollection.at(i).Time);
		fRecoHits[fNHits]->SetDigiLeadingEdgeHigh(fEdgeCollection.at(i).Digi);
	      }
	    }

	  } else if (fEdgeCollection.at(i).Type == 1) { // Trailing edge - create hit and immediately close
	    if (LAVRecoBlock::OpenHit()) {
	      fRecoHits[fNHits] = new LAVRecoHit(fBlockID);
	      if (iTh == 0) {
		fRecoHits[fNHits]->SetTrailingEdgeLow(fEdgeCollection.at(i).Time);
		fRecoHits[fNHits]->SetDigiTrailingEdgeLow(fEdgeCollection.at(i).Digi);
	      } else {
		fRecoHits[fNHits]->SetTrailingEdgeHigh(fEdgeCollection.at(i).Time);
		fRecoHits[fNHits]->SetDigiTrailingEdgeHigh(fEdgeCollection.at(i).Digi);
	      }
	      LAVRecoBlock::CloseHit();
	    }
	  }

	}

      }
    }
    if (fOpenHit) LAVRecoBlock::CloseHit();
  }

}
