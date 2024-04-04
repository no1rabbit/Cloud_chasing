#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "kmlbase" for configuration "Release"
set_property(TARGET kmlbase APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(kmlbase PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_RELEASE "/home/no1rabbit/cloud/.conda/lib/libexpat.so;/home/no1rabbit/cloud/.conda/lib/libz.so;/home/no1rabbit/cloud/.conda/lib/libminizip.so;/home/no1rabbit/cloud/.conda/lib/liburiparser.so;/home/no1rabbit/cloud/.conda/lib/libexpat.so"
  IMPORTED_LOCATION_RELEASE "/home/no1rabbit/cloud/.conda/lib/libkmlbase.so.1.3.0"
  IMPORTED_SONAME_RELEASE "libkmlbase.so.1"
  )

list(APPEND _cmake_import_check_targets kmlbase )
list(APPEND _cmake_import_check_files_for_kmlbase "/home/no1rabbit/cloud/.conda/lib/libkmlbase.so.1.3.0" )

# Import target "kmldom" for configuration "Release"
set_property(TARGET kmldom APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(kmldom PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_RELEASE "kmlbase"
  IMPORTED_LOCATION_RELEASE "/home/no1rabbit/cloud/.conda/lib/libkmldom.so.1.3.0"
  IMPORTED_SONAME_RELEASE "libkmldom.so.1"
  )

list(APPEND _cmake_import_check_targets kmldom )
list(APPEND _cmake_import_check_files_for_kmldom "/home/no1rabbit/cloud/.conda/lib/libkmldom.so.1.3.0" )

# Import target "kmlxsd" for configuration "Release"
set_property(TARGET kmlxsd APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(kmlxsd PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_RELEASE "kmlbase"
  IMPORTED_LOCATION_RELEASE "/home/no1rabbit/cloud/.conda/lib/libkmlxsd.so.1.3.0"
  IMPORTED_SONAME_RELEASE "libkmlxsd.so.1"
  )

list(APPEND _cmake_import_check_targets kmlxsd )
list(APPEND _cmake_import_check_files_for_kmlxsd "/home/no1rabbit/cloud/.conda/lib/libkmlxsd.so.1.3.0" )

# Import target "kmlengine" for configuration "Release"
set_property(TARGET kmlengine APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(kmlengine PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_RELEASE "kmlbase;kmldom"
  IMPORTED_LOCATION_RELEASE "/home/no1rabbit/cloud/.conda/lib/libkmlengine.so.1.3.0"
  IMPORTED_SONAME_RELEASE "libkmlengine.so.1"
  )

list(APPEND _cmake_import_check_targets kmlengine )
list(APPEND _cmake_import_check_files_for_kmlengine "/home/no1rabbit/cloud/.conda/lib/libkmlengine.so.1.3.0" )

# Import target "kmlconvenience" for configuration "Release"
set_property(TARGET kmlconvenience APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(kmlconvenience PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_RELEASE "kmlengine;kmldom;kmlbase"
  IMPORTED_LOCATION_RELEASE "/home/no1rabbit/cloud/.conda/lib/libkmlconvenience.so.1.3.0"
  IMPORTED_SONAME_RELEASE "libkmlconvenience.so.1"
  )

list(APPEND _cmake_import_check_targets kmlconvenience )
list(APPEND _cmake_import_check_files_for_kmlconvenience "/home/no1rabbit/cloud/.conda/lib/libkmlconvenience.so.1.3.0" )

# Import target "kmlregionator" for configuration "Release"
set_property(TARGET kmlregionator APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(kmlregionator PROPERTIES
  IMPORTED_LINK_INTERFACE_LIBRARIES_RELEASE "kmlbase;kmldom;kmlengine;kmlconvenience"
  IMPORTED_LOCATION_RELEASE "/home/no1rabbit/cloud/.conda/lib/libkmlregionator.so.1.3.0"
  IMPORTED_SONAME_RELEASE "libkmlregionator.so.1"
  )

list(APPEND _cmake_import_check_targets kmlregionator )
list(APPEND _cmake_import_check_files_for_kmlregionator "/home/no1rabbit/cloud/.conda/lib/libkmlregionator.so.1.3.0" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
