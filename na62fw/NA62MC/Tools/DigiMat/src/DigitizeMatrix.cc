// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2010-04-13
//
//  2010-04-19 Domenico Di Filippo: new independent variables definition
// --------------------------------------------------------------------

#include "DigitizeMatrix.hh"
#include <sstream>

Application::Application(int argc, char **argv){

   for (int i=0; i<argc; i++)
      args.push_back(string(argv[i]));

}

void Application::Run(){

   if (args.size()<3) {
      cout << "USAGE: " << args[0] << " CHID File1.root ... FileN.root" << endl;
      return;
   }

   stringstream ss;
   ss << args[1];
   ss >> chid;

   cout << "Preparing analysis ..." << flush;
   Prepare();
   cout << " done!" << endl;

   for (unsigned int i=2; i<args.size(); i++){
      cout << "file " << args[i] << ": " << flush;
      currentfile = new TFile(args[i].c_str(),"READ");

      if (!currentfile->IsOpen()){
         cerr << "Error opening!" << endl;
         continue;
      }

      cout << "Processing tree... " << flush;
      Process();
      cout << "Done!" << endl << flush;
      currentfile->Close();

   }

   cout << "Saving out data ..." << flush;
   Save();
   cout << endl << flush;

}

void Application::Prepare(){

   new TLAVEvent;
   new TLAVHit;
   new Event;
   new KinePart;

   LAVGeometryParameters *geopar = LAVGeometryParameters::GetInstance();

   center = geopar->GetLeadglassCenter(chid);
   zver = geopar->GetBlockZVersor(chid);
   yver = geopar->GetBlockYVersor(chid);
   xver = geopar->GetBlockXVersor(chid);

  Variable *Hit = new Variable("Hit",&phs);
  Variable *TimSum = new Variable("Tim",&timsum);
  //Variable *TimSum2 = new Variable("Tim2",&timsum2);
  Variable *Gen = new Variable("Gen",&gen);

  Header.Load(__MATRIX_FILE__);
  if (Header.GetDimensionsNumber() < 6 ){
     cerr << "ERROR! " << __MATRIX_FILE__ << " is not a regular 6-dimensional matrix file." << endl;
     return;
  }

  Int_t     DZNUM = Header.GetDimension(0)->GetBinsNumber()+1;
  Double_t* DZDIV = Header.GetDimension(0)->GetDivisions();
  Int_t     PZNUM = Header.GetDimension(1)->GetBinsNumber()+1;
  Double_t* PZDIV = Header.GetDimension(1)->GetDivisions();
  Int_t     ENNUM = Header.GetDimension(2)->GetBinsNumber()+1;
  Double_t* ENDIV = Header.GetDimension(2)->GetDivisions();
  Int_t     PRNUM = Header.GetDimension(3)->GetBinsNumber()+1;
  Double_t* PRDIV = Header.GetDimension(3)->GetDivisions();
  Int_t     DFNUM = Header.GetDimension(4)->GetBinsNumber()+1;
  Double_t* DFDIV = Header.GetDimension(4)->GetDivisions();
  Int_t     PFNUM = Header.GetDimension(5)->GetBinsNumber()+1;
  Double_t* PFDIV = Header.GetDimension(5)->GetDivisions();

  BinnedVariable *DZe = new BinnedVariable("DZe",&dz,DZNUM,DZDIV);
  BinnedVariable *PZe = new BinnedVariable("PZe",&pz,PZNUM,PZDIV);
  BinnedVariable *ENe = new BinnedVariable("ENe",&en,ENNUM,ENDIV);
  BinnedVariable *PRe = new BinnedVariable("PRe",&pr,PRNUM,PRDIV);
  BinnedVariable *DFe = new BinnedVariable("DFe",&df,DFNUM,DFDIV);
  BinnedVariable *PFe = new BinnedVariable("PFe",&pf,PFNUM,PFDIV);

  WorkMatrix.push_back(new MatrixFiller(Gen,DZe,PZe,ENe,PRe,DFe,PFe));
  WorkMatrix.push_back(new MatrixFiller(Hit,DZe,PZe,ENe,PRe,DFe,PFe));
  WorkMatrix.push_back(new MatrixFiller(TimSum,DZe,PZe,ENe,PRe,DFe,PFe));

  for (unsigned int i=0; i<WorkMatrix.size(); i++)
     if(WorkMatrix[i] != 0) WorkMatrix[i]->Prepare();

}

void Application::HitReset(){

   phs = 0;
   timsum = 0;
   timsum2 = 0;
   
}

void Application::CalculateHits(){

   phsi = lavhit->GetPhotonsNumber();
   tim = lavhit->GetPhotonsTime();
   phs += phsi;
   for (int i=0; i<phsi; i++){
      timsum += tim[i];
      timsum2 += tim[i]*tim[i];
   }
}

void Application::CalculatePrimary(){

   gen = 1; // Number of generated photons per event

//   double dr;

   CLHEP::Hep3Vector Pos = CLHEP::Hep3Vector(kinepart->GetProdPos().X(),
                                             kinepart->GetProdPos().Y(),
                                             kinepart->GetProdPos().Z());
   CLHEP::Hep3Vector Mom = CLHEP::Hep3Vector(kinepart->GetInitialMomentum().X(),
                                             kinepart->GetInitialMomentum().Y(),
                                             kinepart->GetInitialMomentum().Z());

   // Cylindric coordinates
   // z = z
   // r = Sqrt(y*y + x*x);
   // f = ATan2(y, x);

    en = Mom.mag();
    Mom = Mom.unit();
    Pos = Pos - center;
    pr = TMath::Sqrt (
       (Pos * xver) * (Pos * xver) +
       (Pos * yver) * (Pos * yver) );
    pz = Pos * zver;
    pf = TMath::ATan2(Pos * yver, Pos * xver);
//    dr = sqrt (
//       (Mom * xver) * (Mom * xver) +
//       (Mom * yver) * (Mom * yver) );
    dz = Mom * zver;
    df = TMath::ATan2(Mom * yver, Mom * xver);

}

void Application::Process(){

      TTree* lavTree = (TTree*) currentfile->Get("LAV");
      lavTree->SetBranchAddress("Hits", &aLAVEvent);

      TTree* eventTree = (TTree*) currentfile->Get("Run_0");
      eventTree->SetBranchAddress("event", &aRUNEvent);

      unsigned int nentries = lavTree->GetEntries();
      if (eventTree->GetEntries() < nentries)
         nentries = eventTree->GetEntries();

      for (unsigned int e=0; e<nentries;e++){

         lavTree->GetEntry(e);
         eventTree->GetEntry(e);

         TClonesArray *kinepartarray = aRUNEvent->GetKineParts();
         kinepart = 0;
         for (int p=0; p < kinepartarray->GetEntries(); p++){
            kinepart = (KinePart*) kinepartarray->At(p);
            if (kinepart != 0) if (kinepart->GetParentID() == 0)
               CalculatePrimary();
         }

         TClonesArray *hitarray = aLAVEvent->GetHits();
         lavhit = 0;
         HitReset();
         for (int h=0; h < hitarray->GetEntries(); h++){
            lavhit = (TLAVHit*) hitarray->At(h);
            if (lavhit != 0) if (lavhit->GetChannelID() == chid)
               CalculateHits();
         }

         for (unsigned int i=0; i<WorkMatrix.size(); i++) if(WorkMatrix[i] != 0)
            WorkMatrix[i]->Update();
      }
}

void Application::Save(){

   // SAVE MATRIX
   for (unsigned int i=0; i<WorkMatrix.size(); i++)
      if(WorkMatrix[i]!=0) WorkMatrix[i]->Save();

   // SAVE MEAN
   ofstream DataFile;
   DataFile.open("Means.txt");
   for(unsigned int i=0; i<WorkMatrix.size(); i++)
   if(WorkMatrix[i]!=0)
      DataFile << WorkMatrix[i]->GetName() << " sum: " << WorkMatrix[i]->sum << endl;
   DataFile.close();

   // SAVE RATIO MATRIX
   LAVSampleMatrix *den, *num;
   num=WorkMatrix[0]->matrix; den=WorkMatrix[0]->matrix;
   for(unsigned i=0; i< num->GetSize(); i++)
      if (den->GetArray()[i] == 0)
         num->GetArray()[i] = 0;
      else
         num->GetArray()[i] /= den->GetArray()[i];
   num->Save("LAVEff.txt");
   num=WorkMatrix[1]->matrix; den=WorkMatrix[2]->matrix;
   for(unsigned i=0; i< num->GetSize(); i++)
      if (den->GetArray()[i] == 0)
         num->GetArray()[i] = 0;
      else
         num->GetArray()[i] /= den->GetArray()[i];
   num->Save("LAVDelay.txt");

}

int main(int argc, char *argv[]){
   Application *app = new Application(argc,argv);
   app->Run();
   delete app;
   return 0;
}
