
from distutils.core import setup, Extension
import os

# set user_path
dir_path = os.path.dirname(os.path.realpath(__file__))
start = dir_path.find("NA62Analysis", 0, len(dir_path)-1)
user_path = dir_path[:start]
obj_path = dir_path + "/obj/"

root_path = "/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/"
root_incl = root_path + "include/"
root_lib = root_path + "lib/"

dirs_incl = [ 
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include", 
		"/cvmfs/sft.cern.ch/lcg/releases/clhep/2.4.1.0-2c56f/x86_64-centos7-gcc7-opt/lib/CLHEP-2.4.1.0/../../include -isystem ", 
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-opt/include/Geant4 -isystem",  
		"/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-ced0e/x86_64-centos7-gcc7-opt/include -isystem", 
		"/cvmfs/sft.cern.ch/lcg/releases/veccore/0.4.2-ff84f/x86_64-centos7-gcc7-opt/lib/cmake/VecCore/../../../include -isystem", 
		"/cvmfs/sft.cern.ch/lcg/releases/Vc/1.3.2-7fbe0/x86_64-centos7-gcc7-opt/include -isystem",  
		"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include -isystem", 
		"/cvmfs/sft.cern.ch/lcg/releases/VecGeom/v1.1.0-22e48/x86_64-centos7-gcc7-opt/lib/cmake/VecGeom/../../../include" 
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/include/boost",  
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/include" 
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-opt/include", 
		user_path + "NA62Tools/include",  
		user_path + "NA62Tools/Persistency/FullReco/NA62/include", 	
		user_path + "NA62Tools/Persistency/FullReco/Cedar/include", 
		user_path + "NA62Tools/Persistency/FullReco/CHANTI/include", 
		user_path + "NA62Tools/Persistency/FullReco/CHOD/include", 
		user_path + "NA62Tools/Persistency/FullReco/GigaTracker/include", 
		user_path + "NA62Tools/Persistency/FullReco/HAC/include", 
		user_path + "NA62Tools/Persistency/FullReco/IRC/include",
		user_path + "NA62Tools/Persistency/FullReco/LAV/include", 
		user_path + "NA62Tools/Persistency/FullReco/LKr/include", 
		user_path + "NA62Tools/Persistency/FullReco/MUV0/include", 
		user_path + "NA62Tools/Persistency/FullReco/MUV1/include", 
		user_path + "NA62Tools/Persistency/FullReco/MUV2/include", 
		user_path + "NA62Tools/Persistency/FullReco/MUV3/include", 
		user_path + "NA62Tools/Persistency/FullReco/NewCHOD/include", 
		user_path + "NA62Tools/Persistency/FullReco/RICH/include", 	
		user_path + "NA62Tools/Persistency/FullReco/SAC/include", 
		user_path + "NA62Tools/Persistency/FullReco/SAV/include", 
		user_path + "NA62Tools/Persistency/FullReco/Spectrometer/include", 
		user_path + "NA62Tools/Persistency/SlimReco/NA62/include", 	
		user_path + "NA62Tools/Persistency/SlimReco/Cedar/include", 
		user_path + "NA62Tools/Persistency/SlimReco/CHANTI/include", 
		user_path + "NA62Tools/Persistency/SlimReco/CHOD/include", 
		user_path + "NA62Tools/Persistency/SlimReco/GigaTracker/include", 
		user_path + "NA62Tools/Persistency/SlimReco/HAC/include", 
		user_path + "NA62Tools/Persistency/SlimReco/IRC/include", 
		user_path + "NA62Tools/Persistency/SlimReco/LAV/include", 
		user_path + "NA62Tools/Persistency/SlimReco/LKr/include", 
		user_path + "NA62Tools/Persistency/SlimReco/MUV0/include", 
		user_path + "NA62Tools/Persistency/SlimReco/MUV1/include",
		user_path + "NA62Tools/Persistency/SlimReco/MUV2/include", 
		user_path + "NA62Tools/Persistency/SlimReco/MUV3/include", 
		user_path + "NA62Tools/Persistency/SlimReco/NewCHOD/include", 
		user_path + "NA62Tools/Persistency/SlimReco/RICH/include", 
		user_path + "NA62Tools/Persistency/SlimReco/SAC/include", 	
		user_path + "NA62Tools/Persistency/SlimReco/SAV/include", 
		user_path + "NA62Tools/Persistency/SlimReco/Spectrometer/include", 
		user_path + "NA62Reconstruction/Cedar/include", 
		user_path + "NA62Reconstruction/CHANTI/include", 
		user_path + "NA62Reconstruction/CHOD/include", 
		user_path + "NA62Reconstruction/NewCHOD/include", 
		user_path + "NA62Reconstruction/GigaTracker/include", 
		user_path + "NA62Reconstruction/HAC/include", 
		user_path + "NA62Reconstruction/IRC/include", 
		user_path + "NA62Reconstruction/LAV/include", 
		user_path + "NA62Reconstruction/LKr/include", 
		user_path + "NA62Reconstruction/MUV0/include", 
		user_path + "NA62Reconstruction/MUV1/include", 
		user_path + "NA62Reconstruction/MUV2/include", 
		user_path + "NA62Reconstruction/MUV3/include", 
		user_path + "NA62Reconstruction/RICH/include", 
		user_path + "NA62Reconstruction/SAC/include", 
		user_path + "NA62Reconstruction/SAV/include", 
		user_path + "NA62Reconstruction/Spectrometer/include", 
		user_path + "NA62Reconstruction/include", 
		user_path + "NA62Reconstruction/RecoBase/include", 
		user_path + "NA62Analysis/include", 
		user_path + "NA62Analysis/ToolsLib/include", 
		user_path + "NA62Analysis/Algorithms/include", 
		user_path + "NA62Analysis/Examples/include",
		user_path + "NA62Analysis/", 
		"/usr/include/root"]

link_args=[
		user_path + "NA62Tools/lib-cc7/Persistency/libNA62Persistency-static.a",
		user_path + "NA62Tools/lib-cc7/libNA62Tools.so", 
		user_path + "NA62Analysis/lib-cc7/libAnalysisFW.so",
		user_path + "NA62Analysis/lib-cc7/libAlgorithms.so",
		user_path + "NA62Reconstruction/lib-cc7/libCedar.so", 
                user_path + "NA62Reconstruction/lib-cc7/libCHANTI.so",
                user_path + "NA62Reconstruction/lib-cc7/libCHOD.so",
                user_path + "NA62Reconstruction/lib-cc7/libHAC.so",
                user_path + "NA62Reconstruction/lib-cc7/libIRC.so",
                user_path + "NA62Reconstruction/lib-cc7/libLAV.so",
                user_path + "NA62Reconstruction/lib-cc7/libLKr.so",
                user_path + "NA62Reconstruction/lib-cc7/libMUV0.so",
                user_path + "NA62Reconstruction/lib-cc7/libMUV1.so",
                user_path + "NA62Reconstruction/lib-cc7/libMUV2.so",
                user_path + "NA62Reconstruction/lib-cc7/libMUV3.so",
                user_path + "NA62Reconstruction/lib-cc7/libNewCHOD.so",
                user_path + "NA62Reconstruction/lib-cc7/libRICH.so",
                user_path + "NA62Reconstruction/lib-cc7/libSAC.so",
                user_path + "NA62Reconstruction/lib-cc7/libSAV.so",
                user_path + "NA62Reconstruction/lib-cc7/libSpectrometer.so",
		user_path + "NA62Reconstruction/lib-cc7/libRecoBase.so",
                user_path + "NA62Reconstruction/lib-cc7/libRecoService.so",
		user_path + "NA62Reconstruction/lib-cc7/libEventDisplay.so", 
		user_path + "NA62Reconstruction/lib-cc7/libGigaTracker.so",  
		user_path + "NA62Analysis/lib-cc7/libComparator.so",
                user_path + "NA62Analysis/lib-cc7/libToolsLibObjects.so",
		user_path + "NA62Analysis/lib-cc7/libToolsLib.so",
                user_path + "NA62Analysis/lib-cc7/libTRecoLKrCandidateMC.so",    
		user_path + "NA62Tools/lib-cc7/Persistency/libLKrPersistency.so",
		user_path + "NA62Tools/lib-cc7/Persistency/libCedarPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libCHANTIPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libCHODPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libGigaTrackerPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libHACPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libIRCPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libLAVPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libMUV0Persistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libMUV1Persistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libMUV2Persistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libMUV3Persistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libNewCHODPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libRICHPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libSACPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libSAVPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libSpectrometerPersistency.so",
                user_path + "NA62Tools/lib-cc7/Persistency/libNA62Persistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libLKrSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libCedarSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libCHANTISlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libCHODSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libGigaTrackerSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libHACSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libIRCSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libLAVSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libMUV0SlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libMUV1SlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libMUV2SlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libMUV3SlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libNewCHODSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libRICHSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libSACSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libSAVSlimPersistency.so",
                user_path + "NA62Tools/lib-cc7/SlimPersistency/libSpectrometerSlimPersistency.so",
		user_path + "NA62Tools/lib-cc7/SlimPersistency/libNA62SlimPersistency.so",  
                root_lib + "libPhysics.so",
                root_lib + "libMathCore.so",
		root_lib + "libCore.so",
		root_lib + "libHist.so", 
		root_lib + "libEG.so",
                root_lib + "libGui.so", 
		root_lib + "libMLP.so", 
		root_lib + "libTMVA.so", 
		root_lib + "libEve.so", 
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/lib/libboost_program_options.so",  
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-opt/lib/libsqlite3.so", 
		'-Wl,--no-undefined', '-lm'
		]


lib_dirs = [ user_path + '/NA62Analysis/lib-cc7/']
libs = ['AnalysisFW']
extra_objs = [user_path + '/NA62Analysis/build-cc7/CMakeFiles/AnalysisFW-static.dir/src/BaseAnalysis.cc.o']

ext_compile_args = [#'-DCOMPILE_STD_AN=1', '-pthread', 
			'-std=c++17', 
			'-m64'
			]

UM_module = Extension('PyNA62Analysis.UserMethods', sources=['PyNA62Analysis/UserMethodsModule.cpp'], language='C++', 
			include_dirs=dirs_incl,
			extra_link_args=link_args,   
			extra_compile_args=ext_compile_args,
			libraries=['stdc++'],)

PyBaseAnalysis_Struct = Extension('PyNA62Analysis.PyBaseAnalysis', ['PyNA62Analysis/PyBaseAnalysisModule.cpp'], language='C++', 
				include_dirs=dirs_incl,
				library_dirs = lib_dirs,
                                extra_link_args=link_args,
                                extra_compile_args=ext_compile_args,
                                libraries=['stdc++'],)

setup(name='PyNA62Analysis',
      version='1.0',
      packages = ['PyNA62Analysis'],
      ext_modules=[UM_module, PyBaseAnalysis_Struct],
#	ext_modules=[PyBaseAnalysis_Struct],
      ) 











