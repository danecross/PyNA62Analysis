file(REMOVE_RECURSE
  "libRecoBase.pdb"
  "libRecoBase.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/RecoBase.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
