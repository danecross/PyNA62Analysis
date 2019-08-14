
from distutils.core import setup, Extension
import os

# set user_path
dir_path = os.path.dirname(os.path.realpath(__file__))
user_path = dir_path + "/../../na62fw/"

print("PATH TO NA62 C++ CODE BASE: ", user_path)

#root_path = "/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/"
root_path = os.environ['ROOTSYS']
root_incl = root_path + "/include/"
root_lib = root_path + "/lib/"

dirs_incl = [root_incl, 
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-opt/include/Geant4 -isystem",  
		"/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-ced0e/x86_64-centos7-gcc7-opt/include -isystem", 
		"/cvmfs/sft.cern.ch/lcg/releases/veccore/0.4.2-ff84f/x86_64-centos7-gcc7-opt/lib/cmake/VecCore/../../../include -isystem", 
		"/cvmfs/sft.cern.ch/lcg/releases/Vc/1.3.2-7fbe0/x86_64-centos7-gcc7-opt/include -isystem",  
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
#		"/usr/include/root"
		]

link_args=['/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-slc6-gcc7-opt/lib/libboost_program_options.so', 
		'/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-slc6-gcc7-opt/lib/libboost_program_options.so.1.69.0',
#		'/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/ROOT/6.16.00/x86_64-centos7-gcc8-opt/lib/libCore.so', 
#		'/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/ROOT/6.16.00/x86_64-centos7-gcc8-opt/lib/libGraf.so', 
		'-Wl,--no-undefined', '-lm', '-lrt', '-Wall', '-Wextra']

lib_dirs = [ user_path + 'NA62Analysis/lib-slc6/', 
		user_path + 'NA62Tools/lib-slc6/Persistency/', 
		user_path + 'NA62Tools/lib-slc6/SlimPersistency/', 
		user_path + 'NA62Tools/lib-slc6/', 
		user_path + 'NA62Reconstruction/lib-slc6/', 
		root_lib, 
#		'/usr/lib64/root', 
		'/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-slc6-gcc7-opt/lib/',
		'/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-slc6-gcc7-opt/lib/', 
]
for d in lib_dirs:
	os.environ["LIBRARY_PATH"] += ":"
	os.environ["LIBRARY_PATH"] += d
	
	os.environ["PYTHONPATH"] += ":"
	os.environ["PYTHONPATH"] += d

libs = ['stdc++',  
	# NA62Analysis
	'AnalysisFW', 'Algorithms', 'Comparator', 'ToolsLibObjects', 'ToolsLib', 'TRecoLKrCandidateMC',  
	# NA62Reco
	'RecoCedar', 'RecoCHANTI', 'RecoCHOD', 'RecoHAC', 'RecoIRC', 'RecoLAV', 'RecoLKr', 
	'RecoMUV0', 'RecoMUV1', 'RecoMUV2', 'RecoMUV3', 'RecoNewCHOD', 'RecoRICH', 'RecoSAC', 
	'RecoSAV', 'RecoSpectrometer', 'RecoBase', 'RecoService', 'RecoEventDisplay', 'RecoGigaTracker',
	# NA62Tools
	'NA62ToolsMain', 
	# NA62Tools/Persistency
	'CHANTIPersistency', 'LKrPersistency', 'CHODPersistency', 'GigaTrackerPersistency', 'HACPersistency', 
	'IRCPersistency', 'LAVPersistency', 'MUV0Persistency', 'MUV1Persistency', 'MUV2Persistency', 'MUV3Persistency',  
	'NewCHODPersistency', 'RICHPersistency', 'SACPersistency', 'SAVPersistency', 'SpectrometerPersistency', 
	'CedarPersistency', 'NA62Persistency', 
	# NA62Tools/SlimPersistency
	'CHANTISlimPersistency', 'LKrSlimPersistency', 'CHODSlimPersistency', 'GigaTrackerSlimPersistency', 'HACSlimPersistency',
        'IRCSlimPersistency', 'LAVSlimPersistency', 'MUV0SlimPersistency', 'MUV1SlimPersistency', 'MUV2SlimPersistency', 'MUV3SlimPersistency',
        'NewCHODSlimPersistency', 'RICHSlimPersistency', 'SACSlimPersistency', 'SAVSlimPersistency', 'SpectrometerSlimPersistency',
        'CedarSlimPersistency', 'NA62SlimPersistency',
	# ROOT libraries
	'Core',  'Imt', 'RIO', 'Net', 'Hist', 'Graf', 'Graf3d', 'Gpad', #'ROOTDataFrame', 
	'ROOTVecOps', 'Tree', 'TreePlayer', 'Rint', 'EG', 'Gui', 'MLP', 'TMVA', 'Eve', 'Spectrum', 
	'Geom', 'Postscript', 'Matrix', 'Physics', 'MathCore', 'Thread', 'MultiProc',  
	# OTHER 
	'boost_program_options', 'boost_thread', 'sqlite3'
	]

extra_objs = [user_path + '/NA62Analysis/build-cc7/CMakeFiles/AnalysisFW-static.dir/src/BaseAnalysis.cc.o']

ext_compile_args = [ '-std=c++17', '-D_GLIBCXX_USE_CXX11_ABI=0']  

PyBaseAnalysis_Struct = Extension('PyNA62Analysis.PyBaseAnalysis', ['PyNA62Analysis/PyBaseAnalysisModule.cpp'], language='C++', 
				include_dirs=dirs_incl,
                                extra_link_args=link_args,
                                extra_compile_args=ext_compile_args,
                                libraries=libs,)

PyAnalyzer = Extension('PyNA62Analysis.PyAnalyzer', ['PyNA62Analysis/PyAnalyzer.cpp'], language='C++', 
				include_dirs=dirs_incl,
                                extra_link_args=link_args, 
                                extra_compile_args=ext_compile_args,
                                libraries=libs,)

WrapperObject = Extension('PyNA62Analysis.WrapperObject', ['PyNA62Analysis/WrapperObjectModule.cpp'], language='C++', 
				include_dirs=dirs_incl,
                                extra_link_args=link_args,
                                extra_compile_args=ext_compile_args,
                                libraries=libs,)


setup(name='PyNA62Analysis',
      version='1.0',
      packages = ['PyNA62Analysis'],
      ext_modules=[PyBaseAnalysis_Struct, PyAnalyzer, WrapperObject],
      ) 











