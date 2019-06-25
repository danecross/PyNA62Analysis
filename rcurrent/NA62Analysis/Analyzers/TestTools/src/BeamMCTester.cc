// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-04-08
//
// ---------------------------------------------------------------

#include "BeamMCTester.hh"
#include "TProfile.h"

using namespace NA62Analysis;

/// \class BeamMCTester
/// \Brief
/// Produce beam profiles using information at checkpoints
/// \EndBrief
/// \Detailed
/// Should be run on MC samples (either NA62MC or NA62Reconstruction output).
/// Produces the true MC distributions of x, y, dx/dz, dy/dz and their correlations in
/// six Z planes: Cedar entry (z=69200mm), Cedar exit (z=76305mm),
/// GTK entry (in front ot GTK1, z=79440mm), GTK exit (after TRIM5 and GTK3, z=102400mm),
/// FISC5 beam monitor in front of MUV3 (z=245392mm), and XWCM beam monitor (z=264325mm).
/// To be used to validate any changes to the beamline optics.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

BeamMCTester::BeamMCTester(Core::BaseAnalysis *ba) : Analyzer(ba, "BeamMCTester") {}

void BeamMCTester::InitHist() {

  BookHisto("hx_CedarEntry", new TH1F
	    ("hx_CedarEntry", "True beam x at Cedar entry (z = 69200 mm);x [mm]", 80, -40, 40));
  BookHisto("hx_CedarExit", new TH1F
	    ("hx_CedarExit", "True beam x at Cedar exit (z = 76305 mm);x [mm]", 80, -40, 40));
  BookHisto("hx_GigaTrackerEntry", new TH1F
	    ("hx_GigaTrackerEntry", "True beam x at GTK entry (z = 79440 mm);x [mm]", 80, -40, 40));
  BookHisto("hx_GigaTrackerExit", new TH1F
	    ("hx_GigaTrackerExit", "True beam x at GTK exit (z = 102400 mm);x [mm]", 80, -40, 40));
  BookHisto("hx_FISC5", new TH1F
	    ("hx_FISC5", "True beam x at FISC5 (z = 245392 mm);x [mm]", 80, -80, 80));
  BookHisto("hx_XWCM", new TH1F
	    ("hx_XWCM", "True beam x at XWCM (z = 264325 mm);x [mm]", 80, -330, -170));

  BookHisto("hy_CedarEntry", new TH1F
	    ("hy_CedarEntry", "True beam y at Cedar entry (z = 69200 mm);y [mm]", 80, -40, 40));
  BookHisto("hy_CedarExit", new TH1F
	    ("hy_CedarExit", "True beam y at Cedar exit (z = 76305 mm);y [mm]", 80, -40, 40));
  BookHisto("hy_GigaTrackerEntry", new TH1F
	    ("hy_GigaTrackerEntry", "True beam y at GTK entry (z = 79440 mm);y [mm]", 80, -40, 40));
  BookHisto("hy_GigaTrackerExit", new TH1F
	    ("hy_GigaTrackerExit", "True beam y at GTK exit (z = 102400 mm);y [mm]", 80, -40, 40));
  BookHisto("hy_FISC5", new TH1F
	    ("hy_FISC5", "True beam y at FISC5 (z = 245392 mm);y [mm]", 80, -80, 80));
  BookHisto("hy_XWCM", new TH1F
	    ("hy_XWCM", "True beam y at XWCM (z = 264325 mm);y [mm]", 80, -80, 80));

  BookHisto("hxy_CedarEntry", new TH2F
	    ("hxy_CedarEntry", "True beam (x, y) at Cedar entry (z = 69200 mm);x [mm];y [mm]",
	     80, -40, 40, 80, -40, 40));
  BookHisto("hxy_CedarExit", new TH2F
	    ("hxy_CedarExit", "True beam (x, y) at Cedar exit (z = 76305 mm);x [mm];y [mm]",
	     80, -40, 40, 80, -40, 40));
  BookHisto("hxy_GigaTrackerEntry", new TH2F
	    ("hxy_GigaTrackerEntry", "True beam (x, y) at GTK entry (z = 79440 mm);x [mm];y [mm]",
	     80, -40, 40, 80, -40, 40));
  BookHisto("hxy_GigaTrackerExit", new TH2F
	    ("hxy_GigaTrackerExit", "True beam (x, y) at GTK exit (z = 102400 mm);x [mm];y [mm]",
	     80, -40, 40, 80, -40, 40));
  BookHisto("hxy_FISC5", new TH2F
	    ("hxy_FISC5", "True beam (x, y) at FISC5 (z = 245392 mm);x [mm];y [mm]",
	     80, -80, 80, 80, -80, 80));
  BookHisto("hxy_XWCM", new TH2F
	    ("hxy_XWCM", "True beam (x, y) at XWCM (z = 264325 mm);x [mm];y [mm]",
	     80, -330, -170, 80, -80, 80));

  BookHisto("hdxdz_GigaTrackerExit", new TH1F
	    ("hdxdz_GigaTrackerExit", "True beam dx/dz at GTK exit (z = 102400 mm);dx/dz",
	     80, 0.7e-3, 1.7e-3));

  BookHisto("hdydz_GigaTrackerExit", new TH1F
	    ("hdydz_GigaTrackerExit", "True beam dy/dz at GTK exit (z = 102400 mm);dy/dz",
	     80, -0.5e-3, 0.5e-3));

  BookHisto("hxdxdz_CedarEntry", new TH2F
	    ("hxdxdz_CedarEntry", "True beam (x, dx/dz) at Cedar entry (z = 69200 mm);x [mm];dx/dz",
	     80, -40, 40, 80, -0.5e-3, 0.5e-3));
  BookHisto("hxdxdz_CedarExit", new TH2F
	    ("hxdxdz_CedarExit", "True beam (x, dx/dz) at Cedar exit (z = 76305 mm);x [mm];dx/dz",
	     80, -40, 40, 80, -0.5e-3, 0.5e-3));
  BookHisto("hxdxdz_GigaTrackerEntry", new TH2F
	    ("hxdxdz_GigaTrackerEntry", "True beam (x, dx/dz) at GTK entry (z = 79440 mm);x [mm];dx/dz",
	     80, -40, 40, 80, -0.5e-3, 0.5e-3));
  BookHisto("hxdxdz_GigaTrackerExit", new TH2F
	    ("hxdxdz_GigaTrackerExit", "True beam (x, dx/dz) at GTK exit (z = 102400 mm);x [mm];dx/dz",
	     80, -40, 40, 80, 0.7e-3, 1.7e-3));
  BookHisto("hxdxdzP_GigaTrackerExit", new TProfile
	    ("hxdxdzP_GigaTrackerExit", "True beam (x, dx/dz) at GTK exit (z = 102400 mm);x [mm];dx/dz",
	     80, -40, 40));

  BookHisto("hydydz_CedarEntry", new TH2F
	    ("hydydz_CedarEntry", "True beam (y, dy/dz) at Cedar entry (z = 69200 mm);y [mm];dy/dz",
	     80, -40, 40, 80, -0.5e-3, 0.5e-3));
  BookHisto("hydydz_CedarExit", new TH2F
	    ("hydydz_CedarExit", "True beam (y, dy/dz) at Cedar exit (z = 76305 mm);y [mm];dy/dz",
	     80, -40, 40, 80, -0.5e-3, 0.5e-3));
  BookHisto("hydydz_GigaTrackerEntry", new TH2F
	    ("hydydz_GigaTrackerEntry", "True beam (y, dy/dz) at GTK entry (z = 79440 mm);y [mm];dy/dz",
	     80, -40, 40, 80, -0.5e-3, 0.5e-3));
  BookHisto("hydydz_GigaTrackerExit", new TH2F
	    ("hydydz_GigaTrackerExit", "True beam (y, dy/dz) at GTK exit (z = 102400 mm);y [mm];dy/dz",
	     80, -40, 40, 80, -0.5e-3, 0.5e-3));
  BookHisto("hydydzP_GigaTrackerExit", new TProfile
	    ("hydydzP_GigaTrackerExit", "True beam (y, dy/dz) at GTK exit (z = 102400 mm);x [mm];dx/dz",
	     80, -40, 40));
}

void BeamMCTester::Process(int) {

  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  Event *evt = GetMCEvent();
  if (evt->GetNKineParts()) {
    TVector3 p1 = evt->GetKinePart(0)->GetPosCedarEntry();
    if (p1.Z()>1.0) {
      TLorentzVector mom = evt->GetKinePart(0)->GetMomCedarEntry();
      FillHisto("hx_CedarEntry",     p1.X());
      FillHisto("hy_CedarEntry",     p1.Y());
      FillHisto("hxy_CedarEntry",    p1.X(), p1.Y());
      FillHisto("hxdxdz_CedarEntry", p1.X(), mom.X()/mom.Z());
      FillHisto("hydydz_CedarEntry", p1.Y(), mom.Y()/mom.Z());
    }
    TVector3 p2 = evt->GetKinePart(0)->GetPosCedarExit();
    if (p2.Z()>1.0) {
      TLorentzVector mom = evt->GetKinePart(0)->GetMomCedarExit();
      FillHisto("hx_CedarExit",     p2.X());
      FillHisto("hy_CedarExit",     p2.Y());
      FillHisto("hxy_CedarExit",    p2.X(), p2.Y());
      FillHisto("hxdxdz_CedarExit", p2.X(), mom.X()/mom.Z());
      FillHisto("hydydz_CedarExit", p2.Y(), mom.Y()/mom.Z());
    }
    TVector3 p3 = evt->GetKinePart(0)->GetPosGigaTrackerEntry();
    if (p3.Z()>1.0) {
      TLorentzVector mom = evt->GetKinePart(0)->GetMomGigaTrackerEntry();
      FillHisto("hx_GigaTrackerEntry",     p3.X());
      FillHisto("hy_GigaTrackerEntry",     p3.Y());
      FillHisto("hxy_GigaTrackerEntry",    p3.X(), p3.Y());
      FillHisto("hxdxdz_GigaTrackerEntry", p3.X(), mom.X()/mom.Z());
      FillHisto("hydydz_GigaTrackerEntry", p3.Y(), mom.Y()/mom.Z());
    }
    TVector3 p4 = evt->GetKinePart(0)->GetPosGigaTrackerExit(); // entrance to the decay volume
    if (p4.Z()>1.0) {
      TLorentzVector mom = evt->GetKinePart(0)->GetMomGigaTrackerExit();
      FillHisto("hx_GigaTrackerExit",      p4.X());
      FillHisto("hy_GigaTrackerExit",      p4.Y());
      FillHisto("hxy_GigaTrackerExit",     p4.X(), p4.Y());
      FillHisto("hdxdz_GigaTrackerExit",   mom.X()/mom.Z());
      FillHisto("hdydz_GigaTrackerExit",   mom.Y()/mom.Z());
      FillHisto("hxdxdz_GigaTrackerExit",  p4.X(), mom.X()/mom.Z()); // 2D histogram
      FillHisto("hxdxdzP_GigaTrackerExit", p4.X(), mom.X()/mom.Z()); // profile histogram
      FillHisto("hydydz_GigaTrackerExit",  p4.Y(), mom.Y()/mom.Z()); // 2D histogram
      FillHisto("hydydzP_GigaTrackerExit", p4.Y(), mom.Y()/mom.Z()); // profile histogram
    }
    TVector3 p5 = evt->GetKinePart(0)->GetPosFISCEntry();
    if (p5.Z()>1.0) {
      FillHisto("hx_FISC5",  p5.X());
      FillHisto("hy_FISC5",  p5.Y());
      FillHisto("hxy_FISC5", p5.X(), p5.Y());
    }
    TVector3 p6 = evt->GetKinePart(0)->GetPosXWCMEntry();
    if (p6.Z()>1.0) {
      FillHisto("hx_XWCM",  p6.X());
      FillHisto("hy_XWCM",  p6.Y());
      FillHisto("hxy_XWCM", p6.X(), p6.Y());
    }
  }
}

void BeamMCTester::EndOfJobUser() {
  SaveAllPlots();
  std::cout << user_normal() << "Beam profile RMS widths [mm]" << std::endl;
  std::cout << user_normal() << "x at FV entrance: " << Form("%5.2f", fHisto.GetHisto("hx_GigaTrackerExit")->GetRMS()) << std::endl;
  std::cout << user_normal() << "x at FISC5      : " << Form("%5.2f", fHisto.GetHisto("hx_FISC5")->GetRMS()) << std::endl;
  std::cout << user_normal() << "x at XWCM       : " << Form("%5.2f", fHisto.GetHisto("hx_XWCM")->GetRMS()) << std::endl;
  std::cout << user_normal() << "y at FV entrance: " << Form("%5.2f", fHisto.GetHisto("hy_GigaTrackerExit")->GetRMS()) << std::endl;
  std::cout << user_normal() << "y at FISC5      : " << Form("%5.2f", fHisto.GetHisto("hy_FISC5")->GetRMS()) << std::endl;
  std::cout << user_normal() << "y at XWCM       : " << Form("%5.2f", fHisto.GetHisto("hy_XWCM")->GetRMS()) << std::endl;

  std::cout << user_normal() << "Beam RMS divergence at decay volume entrance (z=102.4m)" << std::endl;
  std::cout << user_normal() << "dx/dz [mrad]: " << Form("%5.3f", 1e3*fHisto.GetHisto("hdxdz_GigaTrackerExit")->GetRMS()) << std::endl;
  std::cout << user_normal() << "dy/dz [mrad]: " << Form("%5.3f", 1e3*fHisto.GetHisto("hdydz_GigaTrackerExit")->GetRMS()) << std::endl;
}
