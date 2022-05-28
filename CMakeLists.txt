cmake_minimum_required(VERSION 3.16.3)

project(regularc)

if(NOT DEFINED ENV{LLVM_CONFIG})
    find_program(LLVM_CONFIG "llvm-config")
else()
    find_program(LLVM_CONFIG $ENV{LLVM_CONFIG})
endif()

execute_process(COMMAND ${LLVM_CONFIG} --libs all
                OUTPUT_VARIABLE llvm_libraries)

execute_process(COMMAND ${LLVM_CONFIG} --includedir
                OUTPUT_VARIABLE llvm_includes)

execute_process(COMMAND ${LLVM_CONFIG} --libdir
                OUTPUT_VARIABLE llvm_libdir)

add_executable(regularc llvm/llvmgen.cpp oniguruma/handler.c oniguruma/onigurumaparse.cpp)

find_package(range-v3 CONFIG REQUIRED)

find_package(oniguruma CONFIG REQUIRED)

if(llvm_libraries STREQUAL "")
    message(WARNING "Missing valid LLVM_CONFIG")
endif()

string(STRIP ${llvm_libraries} llvm_libraries)

string(STRIP ${llvm_includes} llvm_includes)

string(STRIP ${llvm_libdir} llvm_libdir)

target_link_directories(regularc PRIVATE ${llvm_libdir})

target_link_libraries(regularc PRIVATE ${llvm_libraries} -rdynamic onig -lpthread range-v3 range-v3-meta range-v3::meta range-v3-concepts -lz -lcurses -ldl)

target_compile_options(regularc PRIVATE -rdynamic -Ofast -g)

set(CMAKE_C_FLAGS -Wno-error=implicit-function-declaration)

target_include_directories(regularc PRIVATE ${PERL_INCLUDE_PATH} ${llvm_includes} src)

set_property(TARGET regularc PROPERTY CXX_STANDARD 23)

set_property(TARGET regularc PROPERTY C_STANDARD 90)

set(CMAKE_CXX_VISIBILITY_PRESET default)
