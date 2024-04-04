#----------------------------------------------------------------
# Generated CMake target import file for configuration "Debug".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "geotiff_library" for configuration "Debug"
set_property(TARGET geotiff_library APPEND PROPERTY IMPORTED_CONFIGURATIONS DEBUG)
set_target_properties(geotiff_library PROPERTIES
  IMPORTED_LINK_DEPENDENT_LIBRARIES_DEBUG "PROJ::proj"
  IMPORTED_LOCATION_DEBUG "${_IMPORT_PREFIX}/lib/libgeotiff.so.5.2.0"
  IMPORTED_SONAME_DEBUG "libgeotiff.so.5"
  )

list(APPEND _cmake_import_check_targets geotiff_library )
list(APPEND _cmake_import_check_files_for_geotiff_library "${_IMPORT_PREFIX}/lib/libgeotiff.so.5.2.0" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
