{
TGeoManager::Import("NA62.gdml");
TFile *_file0 = TFile::Open("NA62.root","RECREATE");
gGeoManager->Write();
_file0->Close();
}
