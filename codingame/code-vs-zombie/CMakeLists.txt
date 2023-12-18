add_executable(code-vs-zombie code_vs_zombie.cpp)
set_compile_options(code-vs-zombie)
set(INCLUDE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(code-vs-zombie PRIVATE ${INCLUDE_DIRECTORY})

set(TEST_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/)
set(TEST_FILES 
    basic.cpp)

list(TRANSFORM TEST_FILES PREPEND ${TEST_DIRECTORY})

make_test(cvsz "${TEST_FILES}")
target_include_directories(cvsz PRIVATE ${INCLUDE_DIRECTORY}) 