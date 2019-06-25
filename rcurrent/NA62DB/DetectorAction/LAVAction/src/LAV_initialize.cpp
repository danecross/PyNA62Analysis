//
//

#include <string>
#include "BaseDetectorStorage.h"

int main( int argc, char *argv[])
{   
  if (argc == 1) {
    std::cout << "Please run the program with the right password " << std::endl;
    return 0;  
  }
  //  std::string connString = "CernProduction";
  std::string connString = "OraCernProxy";
  std::string detectorName = "LAV";
  std::string userName = "LAV";
  BaseDetectorStorage * mDS = new BaseDetectorStorage(connString,detectorName,userName, coral::Update);
  std::cout << "Calling the deploy default schema " << std::endl;
  mDS->DeployDefaultSchema(true);
  delete mDS; 
  return 1;
}

