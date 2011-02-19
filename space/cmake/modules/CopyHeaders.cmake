macro(copy_headers root prefix)
  file(GLOB_RECURSE headers "${root}*.hpp")
  foreach(header ${headers})
    file(RELATIVE_PATH relPath ${root} ${header})
    get_filename_component(dirName ${relPath} PATH)
    install(FILES ${header}
      DESTINATION "include/${prefix}/${dirName}")
  endforeach(header headers)
endmacro(copy_headers)