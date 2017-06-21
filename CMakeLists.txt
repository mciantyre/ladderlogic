cmake_minimum_required(VERSION 2.8.11)
project(ladderrt CXX)
add_definitions(-std=c++11 -Wall -Werror)

# Build library
include_directories(${PROJECT_NAME}/include)
file(GLOB SOURCES "${PROJECT_NAME}/src/*.cpp")
add_library(${PROJECT_NAME} STATIC ${SOURCES})

# Download and unpack googletest at configure time
configure_file(CMakeLists.txt.in googletest-download/CMakeLists.txt)
execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
  RESULT_VARIABLE result
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/googletest-download )
if(result)
  message(FATAL_ERROR "CMake step for googletest failed: ${result}")
endif()
execute_process(COMMAND ${CMAKE_COMMAND} --build .
  RESULT_VARIABLE result
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/googletest-download )
if(result)
  message(FATAL_ERROR "Build step for googletest failed: ${result}")
endif()

# Prevent overriding the parent project's compiler/linker
# settings on Windows
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# Add googletest directly to our build. This defines
# the gtest and gtest_main targets.
add_subdirectory(${CMAKE_BINARY_DIR}/googletest-src
                 ${CMAKE_BINARY_DIR}/googletest-build)

# Build the tests
include_directories(${PROJECT_NAME}/test/include)
file(GLOB TESTS "${PROJECT_NAME}/test/src/*.cpp")
add_executable(tests ${TESTS})
target_link_libraries(tests ${PROJECT_NAME} gtest gmock)
