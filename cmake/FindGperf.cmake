cmake_minimum_required(VERSION 3.25)

find_program(Gperf_EXECUTABLE NAMES gperf)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Gperf
    FOUND_VAR Gperf_FOUND
    REQUIRED_VARS Gperf_EXECUTABLE
)
