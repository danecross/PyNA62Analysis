//
// Example of unification of the first and the second step of the fast simulation (input is reconstruction output tree).
//
// usage:
// root './MergeFastSimulationStep.C("plutostep1r.root","plutostep2r.root" )'
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) on 30/10/2013


int MergeFastSimulationSteps(const char * inputstep1, const char * inputstep2)
{

  TFile *oldfile1 = new TFile(inputstep1);
  TFile *oldfile2 = new TFile(inputstep2);
  
  TTree *GigaTrackerTree = (TTree*)oldfile1->Get("GigaTracker");

  TTree *EventTree = (TTree*)oldfile1->Get("mcEvent");
  TTree *LKrTree = (TTree*)oldfile2->Get("LKr");
  TTree *SpecTree = (TTree*)oldfile1->Get("Spectrometer");


  TFile *newfile = new TFile("./Merged.root","recreate");
  TTree *newtree2 = GigaTrackerTree->CloneTree();
  TTree *newtree3 = EventTree->CloneTree();
  TTree *newtree4 = SpecTree->CloneTree();
  TTree *newtree1 = LKrTree->CloneTree();

   cout << " EventTree "<< EventTree->GetEntries() << endl;
   cout << " LKrTree "<< LKrTree->GetEntries() << endl;

  
   
  if (EventTree->GetEntries() != (LKrTree->GetEntries()))
  {
   cout << " EventTree "<< EventTree->GetEntries() << endl;
   cout << " LKrTree "<< LKrTree->GetEntries() << endl;
   exit(1);
  }

  newfile->Write();
  delete newfile;
  exit();
}
