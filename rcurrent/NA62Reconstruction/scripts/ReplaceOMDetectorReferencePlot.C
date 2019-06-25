/////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                         //
// An example of use:                                                                      //
// root 'ReplaceOMDetectorReferencePlot.C("oldfile.root", "newfile.root", "GigaTracker")'  //
//                                                                                         //
// riccardo.lollini@cern.ch                                                                //
//                                                                                         //
/////////////////////////////////////////////////////////////////////////////////////////////

#include <cstdlib>

// Remove all the histograms in directory dir and in subdirectories recursively,
// while leaving the subdirectories intact.
void DeleteHistograms(TFile * f, TString dir) {
  TIter next(f->GetDirectory(dir)->GetListOfKeys());
  TKey * key;
  while ((key=(TKey*)next())) {
    if (key->IsFolder()) {
      if (f->GetDirectory(dir + "/" + key->GetTitle()))
	dir = dir + "/" + key->GetTitle();
      else
	dir = dir + "/../" + key->GetTitle();
      DeleteHistograms(f, dir);
    }
    else key->Delete();
  }
}

// Copy the content of directory dir from file fsrc to file fdst
void CopyHistograms(TFile * fdst, TFile * fsrc, TString dir) {
  TIter nexts(fsrc->GetDirectory(dir)->GetListOfKeys());
  TKey * keys;
  while ((keys=(TKey*)nexts())) {
    if (keys->IsFolder()) {
      if (fsrc->GetDirectory(dir + "/" + keys->GetTitle()))
	dir = dir + "/" + keys->GetTitle();
      else
	dir = dir + "/../" + keys->GetTitle();
      CopyHistograms(fdst, fsrc, dir);
    }
    else {
      TString paths = keys->GetMotherDir()->GetPath();
      paths.ReplaceAll(fsrc->GetName(),fdst->GetName());
      fdst->GetDirectory(paths)->cd();
      keys->ReadObj()->Write();
    }
  }
}

void ReplaceOMDetectorReferencePlot(TString fold, TString fnew, TString detector) {
  TFile * File1 = TFile::Open(fold,"update");
  TFile * File2 = TFile::Open(fnew);
  TString fDetectorDir = detector + "Monitor";

  if (!(File1->GetDirectory(fDetectorDir))) {
    std::cout << fDetectorDir << " not found in " << fold << std::endl;
    exit(EXIT_FAILURE);
  }
  if (!(File2->GetDirectory(fDetectorDir))) {
    std::cout << fDetectorDir << " not found in " << fnew << std::endl;
    exit(EXIT_FAILURE);
  }

  TIter next1(File1->GetDirectory(fDetectorDir)->GetListOfKeys());
  TIter next2(File2->GetDirectory(fDetectorDir)->GetListOfKeys());
  TKey * key1;
  TKey * key2;

  std::cout << "Deleting histograms in " << fold << ":/" << fDetectorDir << " ..." << std::endl;
  DeleteHistograms(File1, fDetectorDir);

  std::cout << "Copying histograms from " << fnew << ":/" << fDetectorDir << " to " << fold << ":/" << fDetectorDir << " ..." << std::endl;
  CopyHistograms(File1, File2, fDetectorDir);

  std::cout << "Closing files ..." << std::endl;
  File1->Close();
  File2->Close();
}
