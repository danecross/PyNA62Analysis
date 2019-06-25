file(REMOVE_RECURSE
  "libRecoService-static.pdb"
  "libRecoService-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/RecoService-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
