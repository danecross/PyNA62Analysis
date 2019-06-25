file(REMOVE_RECURSE
  "libSAV-static.pdb"
  "libSAV-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/SAV-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
