file(REMOVE_RECURSE
  "libRecoService.pdb"
  "libRecoService.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/RecoService.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
