cmake_minimum_required(VERSION 3.12.0)
project(web-solutions VERSION 0.1.0)

include (CTest)
enable_testing()

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
include(CPack)

###############
# Google Test #
###############

# Download and unpack googletest at configure time
configure_file(gtest.CMakeLists.txt googletest-download/CMakeLists.txt)
execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
  RESULT_VARIABLE result
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download )
if(result)
  message(FATAL_ERROR "CMake step for googletest failed: ${result}")
endif()
execute_process(COMMAND ${CMAKE_COMMAND} --build .
  RESULT_VARIABLE result
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/googletest-download )
if(result)
  message(FATAL_ERROR "Build step for googletest failed: ${result}")
endif()

# Prevent overriding the parent project's compiler/linker
# settings on Windows
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# Add googletest directly to our build. This defines
# the gtest and gtest_main targets.
add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/googletest-src
                 ${CMAKE_CURRENT_BINARY_DIR}/googletest-build
                 EXCLUDE_FROM_ALL)

# The gtest/gtest_main targets carry header search path
# dependencies automatically when using CMake 2.8.11 or
# later. Otherwise we have to add them here ourselves.
include_directories("${gtest_SOURCE_DIR}/include")

function(set_compile_options TARGET_NAME)
if(MSVC)
  target_compile_options(${TARGET_NAME} PRIVATE /W3 /WX)
else ()
  target_compile_options(${TARGET_NAME} PRIVATE -Wall -Wextra -pedantic -Wconversion)
endif(MSVC)
endfunction()


function(make_test TARGET_NAME SRC_FILES) 

add_executable(${TARGET_NAME} ${SRC_FILES})
target_link_libraries(${TARGET_NAME} gtest_main)
add_test(NAME "${TARGET_NAME}-test" COMMAND ${TARGET_NAME})
set_compile_options(${TARGET_NAME})
endfunction()

add_subdirectory(codingame)