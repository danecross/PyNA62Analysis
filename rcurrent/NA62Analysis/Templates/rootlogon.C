{
  TString analysisPath = Form("%s/lib-%s", std::getenv("ANALYSISFW_PATH"), std::getenv("SYSTEMINSTALL"));
  TString persPath     = Form("%s/lib-%s/Persistency", std::getenv("NA62TOOLSSOURCE"), std::getenv("SYSTEMINSTALL"));
  TString slimpersPath = Form("%s/lib-%s/SlimPersistency", std::getenv("NA62TOOLSSOURCE"), std::getenv("SYSTEMINSTALL"));
  gSystem->AddDynamicPath(Form("lib-%s", std::getenv("SYSTEMINSTALL")));
  gSystem->AddDynamicPath(analysisPath);
  gSystem->AddDynamicPath(persPath);
  gSystem->AddDynamicPath(slimpersPath);

  //ROOT
  gSystem->Load("libCore.so");
  gSystem->Load("libPhysics.so");
  gSystem->Load("libGenVector.so");

  //Persistency
  gSystem->Load("libNA62Persistency.so");
  gSystem->Load("libCedarPersistency.so");
  gSystem->Load("libCHANTIPersistency.so");
  gSystem->Load("libCHODPersistency.so");
  gSystem->Load("libGigaTrackerPersistency.so");
  gSystem->Load("libHACPersistency.so");
  gSystem->Load("libIRCPersistency.so");
  gSystem->Load("libLAVPersistency.so");
  gSystem->Load("libLKrPersistency.so");
  gSystem->Load("libMUV0Persistency.so");
  gSystem->Load("libMUV1Persistency.so");
  gSystem->Load("libMUV2Persistency.so");
  gSystem->Load("libMUV3Persistency.so");
  gSystem->Load("libNewCHODPersistency.so");
  gSystem->Load("libRICHPersistency.so");
  gSystem->Load("libSACPersistency.so");
  gSystem->Load("libSAVPersistency.so");
  gSystem->Load("libSpectrometerPersistency.so");

  //SlimPersistency
  gSystem->Load("libNA62SlimPersistency.so");
  gSystem->Load("libCedarSlimPersistency.so");
  gSystem->Load("libCHANTISlimPersistency.so");
  gSystem->Load("libCHODSlimPersistency.so");
  gSystem->Load("libGigaTrackerSlimPersistency.so");
  gSystem->Load("libHACSlimPersistency.so");
  gSystem->Load("libIRCSlimPersistency.so");
  gSystem->Load("libLAVSlimPersistency.so");
  gSystem->Load("libLKrSlimPersistency.so");
  gSystem->Load("libMUV0SlimPersistency.so");
  gSystem->Load("libMUV1SlimPersistency.so");
  gSystem->Load("libMUV2SlimPersistency.so");
  gSystem->Load("libMUV3SlimPersistency.so");
  gSystem->Load("libNewCHODSlimPersistency.so");
  gSystem->Load("libRICHSlimPersistency.so");
  gSystem->Load("libSACSlimPersistency.so");
  gSystem->Load("libSAVSlimPersistency.so");
  gSystem->Load("libSpectrometerSlimPersistency.so");

  //NA62Analysis
  gSystem->Load("libToolsLibObjects.so");
}
