set(UNIT_TEST_SRC
    unit_test_named_tuple.cpp
    unit_test_tagged_bitset.cpp
    unit_test_tagged_array.cpp
)

add_executable(unit_tests)
target_sources(unit_tests PRIVATE ${UNIT_TEST_SRC})
target_link_libraries(unit_tests PRIVATE named Catch2::Catch2WithMain)

enable_testing()

add_test(NAME unit_tests
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND unit_tests)
