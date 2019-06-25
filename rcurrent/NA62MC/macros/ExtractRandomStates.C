
#include <stdio.h>
#include <unistd.h>

void ExtractRandomStates(TString inputFile, TString na62mcSourcePath)
{
  na62mcSourcePath.Append("Persistency/lib/libNA62Persistency.so");
  gSystem->Load(na62mcSourcePath);
  TFile *oldfile = new TFile(inputFile);
  TTree *oldtree = (TTree*)oldfile->Get("Run_0");
  Event *event   = new Event();
  Rndm *rndm   = new Rndm();
  oldtree->SetBranchAddress("event",&event);
  FILE * pFile = fopen ("evlist.tmp","r");
  if (!pFile) {
   printf ("evlist.tmp could not be opened. Sorry. Please, check paths.");
   exit(0);
  }
  
  long currentEvent =  0;
  long totalevents = oldtree->GetEntries();

  TFile *newfile = new TFile("randomstate.root","recreate");
  TTree *newtree = new TTree("RandomStatesTree","rndm");
  newtree->Branch("rndm",&rndm);

  while (fscanf(pFile, "%ld", &currentEvent) != EOF) {
   if (currentEvent > (totalevents-1))
    exit(0);
   oldtree->GetEntry(currentEvent);
   rndm->StoreRandomState(event->GetRandomDecayState(), event->GetRandomBeamState(), event->GetRanecuState());
   newtree->Fill();
   event->Clear();
  }
  fclose (pFile);

  event->Clear();
  newtree->AutoSave();
  delete oldfile;
  delete newtree;
}
