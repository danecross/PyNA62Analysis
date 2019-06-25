#include "PrintKineParts.hh"

using namespace NA62Analysis;

/// \class PrintKineParts
/// \Brief
/// A test tool to print the "true" particles generated by NA62MC
/// \EndBrief
/// \Detailed
/// Prints out the contents of the GenePart and KinePart containers and builds a few general histograms.
/// Can be run on NA62MC or NA62Reconstruction output.
/// \EndDetailed

PrintKineParts::PrintKineParts(Core::BaseAnalysis *ba) :
  Analyzer(ba, "PrintKineParts") {}

void PrintKineParts::InitHist() {
  BookHisto("hMCZVertex", new TH1F("MCZVertex", "MCZVertex;Z of beam particle endpoint [m]", 300, 0, 300));
}

void PrintKineParts::Process(Int_t iEvent) {

  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  Event *evt = GetMCEvent();
  if (evt->GetNKineParts()) {
    Double_t Zdecay = 1e-3 * evt->GetKinePart(0)->GetEndPos().Z();
    FillHisto("hMCZVertex", Zdecay);
  }

  std::cout << user_normal() << "Event " << iEvent << ": GeneParts" << std::endl;
  if(CanPrint()){
    TClonesArray &GenePartsArray = *(evt->GetGeneParts());
    GenePartsArray.Print();
  }
  std::cout << user_normal() << "Event " << iEvent << ": KineParts" << std::endl;
  if(CanPrint()){
    TClonesArray &KinePartsArray = *(evt->GetKineParts());
    KinePartsArray.Print();
  }
  std::cout << user_normal() << "------------" << std::endl;
}

void PrintKineParts::EndOfJobUser() {
  SaveAllPlots();
}