cmake_minimum_required(VERSION 3.17)
project(pvec)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=address")
add_executable(test test.cpp)
target_link_libraries(test ${CONAN_LIBS})
set_target_properties(test PROPERTIES
  CXX_STANDARD 20
  CXX_STANDARD_REQUIRED YES)
