file(REMOVE_RECURSE
  "libRecoBase-static.pdb"
  "libRecoBase-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/RecoBase-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
