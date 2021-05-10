add_executable(ghost-in-the-cell ghost_in_the_cell.cpp)
set_common_include_directory(ghost-in-the-cell)
set_compile_options(ghost-in-the-cell)
set_target_properties(ghost-in-the-cell PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON CXX_EXTENSIONS NO)

set(TEST_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests/)
set(TEST_FILES 
    gitc_test.cpp 
    graph.cpp)

list(TRANSFORM TEST_FILES PREPEND ${TEST_DIRECTORY})

make_test(gitc "${TEST_FILES}")
set_common_include_directory(gitc)
target_include_directories(gitc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}) 


set(SIMU_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/simulator/)
set(SIMU_SRC_FILES simulator.cpp)
list(TRANSFORM SIMU_SRC_FILES PREPEND ${SIMU_SRC_DIR})
add_executable(gitc-referee "${SIMU_SRC_FILES}")
set_common_include_directory(gitc-referee)
set_compile_options(gitc-referee)
target_include_directories(gitc-referee PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${SIMU_SRC_DIR})