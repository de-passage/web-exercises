set(COMMON_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/include)

function (set_common_include_directory TARGET_NAME)
target_include_directories(${TARGET_NAME} AFTER PUBLIC ${COMMON_INCLUDES})
endfunction()

add_subdirectory(traffic-lights)
add_subdirectory(ghost-in-the-cell)
add_subdirectory(tic-tac-toe-ultimate)
add_subdirectory(code-vs-zombie)
add_subdirectory(golf-winamax)
add_subdirectory(binary-search)
add_subdirectory(spring-2021)