add_executable(ghost-in-the-cell ghost_in_the_cell.cpp)
set_compile_options(ghost-in-the-cell)
set_target_properties(ghost-in-the-cell PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON CXX_EXTENSIONS NO)

set(TEST_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/)
set(TEST_FILES 
    gitc_test.cpp 
    graph.cpp)

list(TRANSFORM TEST_FILES PREPEND ${TEST_DIRECTORY})

make_test(gitc "${TEST_FILES}")
target_include_directories(gitc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}) 