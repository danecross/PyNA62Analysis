
#from ROOT import TCanvas, TFile
from distutils.core import setup, Extension

#importing  C++ ROOT
import os
#os.environ['CPPFLAGS'] = '-I/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/'
#os.environ['LDFLAGS'] += '-L/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/lib'

# set user_path
dir_path = os.path.dirname(os.path.realpath(__file__))
start = dir_path.find("NA62Analysis", 0, len(dir_path)-1)
user_path = dir_path[:start]
obj_path = dir_path + "/obj/"

dirs_incl = [ #"/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/include/",
#		user_path + "NA62MC/Persistency-headers/",
#                user_path + "NA62Tools/Persistency/FullReco/NA62/include/",
#                user_path + "NA62Analysis/ToolsLib/include/",
#                user_path + "NA62Tools/Persistency/FullReco/GigaTracker/include/",
#                user_path + "NA62MC/Persistency-headers/GigaTracker/",
#                user_path + "NA62Tools/Persistency/FullReco/IRC/include/",
#                user_path + "NA62Tools/Persistency/FullReco/LAV/include/",
#                user_path + "NA62Tools/Persistency/FullReco/LKr/include/",
#                user_path + "NA62Tools/Persistency/FullReco/SAC/include/",
#                user_path + "NA62Tools/Persistency/FullReco/Spectrometer/include/",
#                user_path + "NA62MC/Persistency-headers/IRC/",
#                user_path + "NA62MC/Persistency-headers/LAV",
#                user_path + "NA62MC/Persistency-headers/LKr/",
#                user_path + "NA62MC/Persistency-headers/MUV0",
#                user_path + "NA62MC/Persistency-headers/MUV1/",
#                user_path + "NA62MC/Persistency-headers/MUV2",
#                user_path + "NA62MC/Persistency-headers/MUV3",
#                user_path + "NA62MC/Persistency-headers/NewCHOD/",
#                user_path + "NA62MC/Persistency-headers/RICH",
#                user_path + "NA62MC/Persistency-headers/SAC",
#                user_path + "NA62MC/Persistency-headers/Spectrometer/",
#                user_path + "NA62Tools/Persistency/SlimReco/NA62/include/",
#                user_path + "NA62Analysis/include/",
#                user_path + "NA62Tools/include/",
#                user_path + "NA62Tools/Persistency/FullReco/NA62/include/",
#                "/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/include/" ]
]
dirs_incl += [ 
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
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",  
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include", 	
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Cedar/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHANTI/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/CHOD/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/GigaTracker/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/HAC/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/IRC/include",
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/LAV/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/LKr/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV0/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV1/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV2/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV3/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NewCHOD/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/RICH/include", 	
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/SAC/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/SAV/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/Spectrometer/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include", 	
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/Cedar/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHANTI/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHOD/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/GigaTracker/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/HAC/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/IRC/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LAV/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/LKr/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV0/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV1/include",
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV2/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NewCHOD/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/RICH/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAC/include", 	
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAV/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/Spectrometer/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/Cedar/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/CHANTI/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/CHOD/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/NewCHOD/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/GigaTracker/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/HAC/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/IRC/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/LAV/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/LKr/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/MUV0/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/MUV1/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/MUV2/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/MUV3/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/RICH/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAC/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/Spectrometer/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/RecoBase/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Analysis/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Analysis/ToolsLib/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Analysis/Algorithms/include", 
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Analysis/Examples/include"
		"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Analysis/", 
		"/usr/include/root"]

link_args=[
		user_path + "/NA62Tools/lib-cc7/Persistency/libNA62Persistency-static.a",
		user_path + "/NA62Analysis/lib-cc7/libAnalysisFW.so", 
                "/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib/libPhysics.so",
                "/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib/libMathCore.so",
		"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib/libCore.so", 
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











