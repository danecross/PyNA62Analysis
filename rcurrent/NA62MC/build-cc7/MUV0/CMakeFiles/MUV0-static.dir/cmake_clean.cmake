file(REMOVE_RECURSE
  "libMUV0-static.pdb"
  "libMUV0-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/MUV0-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
