include_directories(include)
link_directories(${NA62TOOLS_LIBRARY_DIRS})
link_directories(${NA62RECO_LIBRARY_DIRS})

set(CLASSES BaseComparator
            DirComparator
            TH1Comparator
            TH2Comparator
            TGraphComparator
            TStringObjComparator
            TTreeComparator
            TBranchComparator
            UnknownComparator
            IgnoreList
   )

SANDWICH_LIST(CLASSES "src/" ".cpp")

add_library(Comparator${LIBTYPEPOSTFIX} ${LIBTYPE} ${CLASSES} ../src/TermManip.cc)
add_executable(CLs_limit CLs_limit.cpp)
add_executable(ROOTCompare ROOTCompare.cpp)
add_executable(RmObjFromRootF RmObjFromRootF.cpp)
add_executable(ROOTFileMerger ROOTFileMerger.cpp src/FixStreams.cpp)
add_executable(FixStreams FixStreams.cpp src/FixStreams.cpp)

target_link_libraries(CLs_limit ${ROOT_LIBRARIES})
target_link_libraries(ROOTCompare ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve Spectrum Gui)
target_link_libraries(ROOTCompare Comparator${LIBTYPEPOSTFIX})
target_link_libraries(RmObjFromRootF ${ROOT_LIBRARIES})
target_link_libraries(ROOTFileMerger ${NA62TOOLS_LIBS_USED} ${NA62TOOLS_LIBS_PERS_USED} ${NA62TOOLS_LIBS_SLIM_USED} ${NA62TOOLS_LIBS_USED})
target_link_libraries(ROOTFileMerger ${ROOT_LIBRARIES})
target_link_libraries(FixStreams ${NA62TOOLS_LIBS_USED} ${NA62TOOLS_LIBS_PERS_USED} ${NA62TOOLS_LIBS_SLIM_USED} ${NA62TOOLS_LIBS_USED} ${NA62RECO_LIBS_USED})
target_link_libraries(FixStreams ${ROOT_LIBRARIES} Minuit Gui Spectrum sqlite3)

add_executable(cleanFileList src/cleanFileList.cpp)
target_link_libraries(cleanFileList ${ROOT_LIBRARIES})
add_dependencies(NA62Tools ROOTCompare cleanFileList FixStreams RmObjFromRootF ROOTFileMerger)
