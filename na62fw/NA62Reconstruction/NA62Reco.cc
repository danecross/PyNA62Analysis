#include <sys/stat.h>
#include <unistd.h>
#include "NA62Reconstruction.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"
#include <signal.h>
#include <fcntl.h>
#include "HLTLibController.hh"

NA62Reconstruction * NA62Reco;

void usage(char* name){
  std::cout << "Usage: "<< name << " [-h] [-b/-B #MaxFiles] [-i InputFile.dat/.root] [-l InputListFile.txt] [-n #MaxEvents] [-o OutputFile.root] [-s seed] [-c ConfigFileName.conf] [-j #JumpFirstNEvents] [-N #MaxEventsPerBurst] [-C ConditionsDirectoryPath] [-e #ExitLevel]"
    << std::endl;
}

void sighandler(int sig){
  std::cerr << std::endl << "********************************************************************************" << std::endl;
  std::cerr << "Killed with Signal " << sig << std::endl << "Closing ROOT files ..." << std::endl;

  NA62Reco->EndProcessing();

  std::cerr << "... Done" << std::endl;
  std::cerr << std::endl << "********************************************************************************" << std::endl;

  exit(128+sig);
}

int main(Int_t argc, char **argv)
{

#ifdef ONLINEHLT
  std::cout << "[NA62Reco] Is equipped with HLT Algorithms!!!" << std::endl;
#endif


  signal(SIGXCPU,sighandler);
  signal(SIGINT,sighandler);
  signal(SIGTERM,sighandler);
  signal(127,sighandler);

  extern char *optarg;
  int opt;
  TString OutputFileName("OutputFile.root");
  TString InputFileName("InputFile.root");
  TString InputListFileName("InputListFile.txt");
  TString ConfFileName("config/NA62Reconstruction.conf");
  // Available Exit Levels:
  //   0: exit if any not found or empty file is detected [for production]
  //   1: ignore not found files, exit if any empty file is detected [default]
  // >=2: ignore not found or empty files
  Int_t   ExitLevel=0;
  TString ConditionsDirPath("");
  Int_t iFile = 0, NFiles = 100000, NEvt = 100000000, JumpNEvt=0, NEvtPerFile=100000000;
  UInt_t Seed = 4357;
  struct stat filestat;
  bool InputFileIsAList = false;

  Int_t n_options_read = 0;
  Int_t nb=0, nc=0, nC=0, ne=0, ni=0, nj=0, nl=0, nn=0, nN=0, no=0, ns=0;
  while ((opt = getopt(argc, argv, "b:B:c:C:e:h:i:j:l:n:N:o:s:")) != -1) { //it must end with a :
    n_options_read++;
    switch (opt) {
      case 'b':
      case 'B':
        nb++;
        NFiles = TString(optarg).Atoi();
        break;
      case 'c':
        nc++;
        ConfFileName = TString(optarg);
        break;
      case 'C':
        nC++;
        ConditionsDirPath = TString(optarg);
        break;
      case 'e':
        ne++;
        ExitLevel = TString(optarg).Atoi();
        break;
      case 'h':
        usage(argv[0]);
        return 0;
      case 'i':
        ni++;
        InputFileName = TString(optarg);
        break;
      case 'j':
        nj++;
        JumpNEvt = TString(optarg).Atoi();
        break;
      case 'l':
        nl++;
        InputListFileName = TString(optarg);
        InputFileIsAList = true;
        //Protection against incorrect input
        if (InputListFileName.EndsWith(".root")) {
          std::cout << "Input is a *.root file and not a list" << std::endl;
          usage(argv[0]);
          exit(kWrongConfiguration);
        }
        break;
      case 'n':
        nn++;
        NEvt = TString(optarg).Atoi();
        break;
      case 'N':
        nN++;
        NEvtPerFile = TString(optarg).Atoi();
        break;
      case 'o':
        no++;
        OutputFileName = TString(optarg);
        break;
      case 's':
        ns++;
        Seed = (UInt_t)TString(optarg).Atoi();
        break;
      default:
        usage(argv[0]);
        return 0;
    }
  }

  // Sanity checks on the input
  if (!n_options_read || NEvt<=0 || NFiles<=0 || JumpNEvt<0 || NEvtPerFile<0) {
    usage(argv[0]);
    return 1;
  }
  if (nb>1 || nc>1 || nC>1 || ne>1 || ni>1 || nj>1 || nl>1 || nn>1 || nN>1 || no>1 || ns>0) {
    std::cout << "[NA62Reco] Multiple arguments of the same type are not allowed" << std::endl;
    return 1;
  }

  // Protection against incorrect input/output filenames
  if (ni && !InputFileName.Contains("/castor/") && !InputFileName.Contains("/eos/")) {
    if (stat(Form(InputFileName.Data()), &filestat)) {
      std::cout << "[NA62Reco] Input file does not exist" << std::endl;
      return 1;
    }
    if (S_ISDIR (filestat.st_mode)) {
      std::cout << "[NA62Reco] Input file is a directory" << std::endl;
      return 1;
    }
  }
  if (ni && no && InputFileName==OutputFileName) {
    std::cout << "[NA62Reco] Output file is the same as input file: a destructive call" << std::endl;
    return 1;
  }
  if (!OutputFileName.EndsWith(".root") && !stat(OutputFileName.Data(), &filestat)) {
    std::cout << " [NA62Reco] Output file exists and is not *.root: potentially a destructive call" << std::endl;
    return 1;
  }

  TObjArray InputFileNameList;
  if(InputFileIsAList && stat(Form(InputListFileName.Data()), &filestat) == 0) { // -l option used and list exists
    std::ifstream InputList(InputListFileName.Data());
    while(InputFileName.ReadLine(InputList) && iFile < NFiles){
      //if(stat(Form(InputFileName.Data()), &filestat) == 0)
      InputFileNameList.Add(new TObjString(InputFileName.Data()));
      iFile++;
    }
  }
  else if (!InputFileIsAList) { // -i option used
    InputFileNameList.Add(new TObjString(InputFileName.Data()));
  }

  if (InputFileNameList.GetEntries() == 0) {
    perror(Form("[NA62Reco] Input file list is empty or does not exist"));
    return 1;
  }

  // Init NA62ConditionsService variables
  NA62RecoManager::GetInstance()->InitNA62ConditionsService(static_cast<TObjString*>(InputFileNameList.At(0))->GetString());
  NA62ConditionsService::GetInstance()->SetExitLevel(ExitLevel);
  NA62ConditionsService::GetInstance()->SetExternalCDBDirectoryPath(ConditionsDirPath);
  NA62ConditionsService::GetInstance()->SetAdditionalDirectoryPath(Form("%s/lib-%s",std::getenv("NA62RECOSOURCE"),std::getenv("SYSTEMINSTALL"))); // for the HLT libs

#ifdef ONLINEHLT
  // Init HLT libs
  std::shared_ptr<HLTLibController> library_handler = std::make_shared<HLTLibController>(NA62ConditionsService::GetInstance()->GetFullPath("liboffline-na62-trigger-algorithms.so").Data());
  library_handler->loadObjects();
#endif

  TFile * OutputFile = TFile::Open(OutputFileName.Data(),"RECREATE");
  NA62Reco = new NA62Reconstruction(&InputFileNameList, ConfFileName, OutputFile, NEvt, NEvtPerFile, JumpNEvt, Seed);
#ifdef ONLINEHLT
  NA62Reco->SetHLTLib(library_handler);
#endif
  while (NA62Reco->NextEvent()) {
  }
#ifdef ONLINEHLT
  library_handler->closeLibrary();
#endif
  NA62Reco->EndProcessing();
}
