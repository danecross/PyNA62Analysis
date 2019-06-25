Production release v1.1.0
=========================

NA62Tools
---------
  - Conditions Service 
  - NA62Utilities
  - Magnetic field classes + BlueTubeTracker
  - Geometry parameters classes
    - GigaTracker
  - Persistency:
    - Libraries disk storage of MC and reconstructed objects
    - Full persistency: all detectors
    - Slim persistency (~ 3x lighter): reco objects, all detectors

NA62MC
------
  - Implemented detectors:
    - Cedar, GigaTracker, CHANTI, LAV, Spectrometer, RICH, NewCHOD, CHOD, IRC, LKr, MUV0, MUV1, MUV2, MUV3, HAC, SAC
    - Geometry compatible with design parameters, 2014, 2015, 2016, 2017 and 2018 actual setups
  - Beam:
    - TURTLE for standard NA62 beam (kaons, pions, protons)
    - External generator mode (/beam/SetBeam external) e.g. for halo
    - Test particles
  - Decay generators:
    - Mostly migrated from NA48 simulation (fortran)
    - Few reimplemented in C++
  - Improvements in v0.11.3:
    - Fast simulation option /FastSimulation/FastSimulationConfigurationEnabled introduced
    - Fast MNP33 field simulation algorithm introduced
    - Beam tuning for 2017 conditions
    - Bugfix: decay generators 100-103 (4-lepton decays) producing incorrect spectra (bug NARKD-830)
  - Improvements in v1.0.0:
    - Fast simulation: /FastSimulation/FastSimulationConfigurationEnabled (boolean) renamed to /Detector/FastSimulationMode (integer); fast simulation mode #2 introduced with RICH enabled.
    - Production of biased K3pi00 and K2pig samples for topologies with merging photon clusters (/decay/twoPhotonsMaxAngle option; still to be validated).
    - Capped pion lifetime for K3pi decays introduced (/decay/piforce option).
    - Extended information in MCInfo. 
  - Bug fixes in v1.0.1:
    - incorrect timing of some detectors (bug NARKD-912,6,8);
    - fixed forced pion decay
  - Main updates in v1.0.2:
    - Datacards /decay/KPiPi0GammaIBEmin and /decay/KPiPi0GammaIBEmax generalized to /decay/RadiativePhotonMinEnergy and /decay/LeptonPhotonMinAngle; datacard /decay/LeptonPhotonMinAngle introduced; they are implemented for mode #42 (Ke3 KLOE).
    - Datacard /decay/MinTracksMomentum introduced for biasing based on total momentum of charged decay products; implemented for modes #2, #42, #43.
    - A major update of HNL decay simulation.
    - Bugs fixed:
      - all geometry conflicts in the CHANTI volume;
      - tracking warnings in the GTK volume, due to showers in magnet yokes and infinite field gradients (bug NARKD-911);
      - geometry overlaps in RICH mirrors (partial fix to the RICH geometry bugs);
      - HAC time misalignment (bug NARKD-914);
      - fine alignment of Cedar time-of-flight, which drifted in v0.11.3-v1.0.1 (bug NARKD-929, transparent for users of MCTimeAlignment pre-analyzer). 
  - Improvements in v1.0.3: 
    - Datacards /decay/RadiativePhotonMinEnergy and /decay/LeptonPhotonMinAngle implemented for mode #44 (Ke3g IB+DE).
    - A number of crucial bug fixes in exotic MC (bugs NARKD-717, NARKD-991). 
    - Metadata decommissioned
  - Improvements in v1.0.4:
    - Fixed geometry conflicts in the RICH volume
  - Improvements in v1.0.5:
    - External generator mode (/beam/SetBeam external option and the data card /beam/ExternalParticlesFile).
    - Old halo simulation technology decommissioned (/beam/Halo/InputDir and /beam/Halo/Mode data cards removed).
    - The /run/number data card removed and replaced with command-line argument to NA62MC (for future MC run-dependence)
  - Improvements in v1.0.6:
    - Improved 2017 beam tuning
    - Fixed geometry conflict in LKrColdWindow, Cedar light guides, STRAW
  - Improvements in v1.1.0:
    - K3pig decay generator added
    - K3pi with forced decay of all pions added
    - Refined GTK geometry (thicker GTK3 cooling plate)

NA62Reconstruction
------------------
  - Implemented modules:
    - Cedar, GigaTracker, CHANTI, LAV, Spectrometer, RICH, NewCHOD, CHOD, IRC, LKr, MUV0, MUV1, MUV2, MUV3, SAC, SAV
  - Compatibility:
    - Data: 2014-2018
    - MC: from this version
  - Summary of capabilities:
    - Process raw (non triggerless) data
    - Provide reconstructed hits and candidates for all the implemented modules
    - OnlineMonitor as for 2016-2018 data taking
    - Fairly tested on MC: potential residual issues in some digitization modules
    - Suitable for automatic calibration of time offsets, in combination with specific analyzers in NA62Analysis
  - Improvements in v1.0.0: 
    - GigaTracker candidate building moved to reconstruction
  - Improvements in v1.0.2: 
    - LKr jitter and fossil detection added
    - Choke trigger handling added
  - Improvements in v1.0.3: 
    - Metadata decommissioned
    - BurstTime implemented also for MC
  - Improvements in v1.0.4:
    - Automatic jitter correction (only for “clear” full jitters)
  - Improvements in v1.0.5:
    - LKr "raw energy correction" migrated from LKrClusterCorrections to NA62Reco
    - Improved LKr pedestal subtraction
    - Major update of Octane (v2)
  - Improvements in v1.0.6:
    - Fixed problem preventing NA62Reco to run on CC7 on grid
    - Correct FineTime handling for MC
    - Reproducible random numbers in digitizers 
  - Improvements in v1.1.0:
    - Sub-detector reconstruction modules can now be used in NA62Analysis
    - Major update of CHOD and LKr digitizers
    - HLT file handling migrated to ConditionsService

NA62Analysis
------------
  - Pre-analyzers:
    - Running before any analyzer
    - SpectrometerTrackCorrections, LKrClusterCorrections, GigaTrackerFineCorrections, SpectrometerRecoAnalyzer
  - Time offsets calculation:
    - For all detectors, both for coarse and fine offsets
    - Most are fully implemented and tested
  - MCTesters:
    - Test and reference plots of MC information
    - Implemented for Cedar and MUV3
  - Physics tools:
    - Vertexing
    - Particle ID
  - Monitoring tools:
    - Selection of plots from reconstructed files
  - Other tools:
    - Acceptance
    - Track extrapolation through the blue tube magnetic field
    - Track-detector geometrical association
  - Suitable for filtering:
    - Any analyzer can be set up porvide a filtering decision, to produce reconstructed files with preselected events
    - Currently 15 filters available: FilterControlTrigger FilterDimuonThreeTrackVertex FilterHNLVertexFilter FilterLKrLargePairDeposit
                                      FilterMuonElectronThreeTrackVertex FilterMuonNeutrino FilterOneTrackNotMuon FilterOneTrackPnn
                                      FilterOneTrackTwoClusters FilterPnn FilterPositronThreeTrackVertex FilterRestrictedMuon
                                      FilterRestrictedPositron FilterTwoTrackVertexWithLepton FilterRestrictedThreeTrackVertex

  - Further details: https://na62-sw.web.cern.ch/sites/na62-sw.web.cern.ch/files/doxygen/componentStatus.html
  - Improvements in 1.0.2: 
    - Additional data quality analyzers
    - Alpha/Beta computation revised
    - LKr hot cell handling improved
  - Improvements in v1.0.3: 
    - GigaTrackerEventReco preanalyser decommissioned
    - Unified bad burst format
  - Improvements in v1.0.4:
    - Dedicated IRC/SAC/SAV DQ analyser added  
  - Improvements in v1.0.6:
    - Pi0 fine calibration analysers added  

NA62DB
------
  - Implemented modules:
    - LAV
  - Development:
    - Deployment and read/write interface for subsystems
    - Ongoing development for interaction with NA62MC

Gitlab CI
---------
  - Continuous Integration tests
  - Improvements: 
    - Compilation, running and other checks (VM, conditions) added

Documentation
-------------
  - http://na62-sw.web.cern.ch/

