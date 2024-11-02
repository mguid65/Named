from utility import *

import unittest


class TestCompilationSanityChecks(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not check_compiler_exists("g++"):
            print("Failed to find compiler")
            exit(1)

        cls.compiler = Compiler("g++", "-I../../include", "-std=c++20")

    def test_tuple_element_empty_tuple(self):
        self.assertTrue(self.compiler.compile_fails("tests/tuple_element_empty_tuple.cpp"))

    def test_tuple_element_out_of_range(self):
        self.assertTrue(self.compiler.compile_fails("tests/tuple_element_out_of_range.cpp"))

    def test_different_size_comparison(self):
        self.assertTrue(self.compiler.compile_fails("tests/different_size_comparison.cpp"))

    def test_non_unique_keys(self):
        self.assertTrue(self.compiler.compile_fails("tests/non_unique_keys.cpp"))

    def test_same_size_comparison(self):
        self.assertTrue(self.compiler.compiles("tests/same_size_comparison.cpp"))

    @classmethod
    def tearDownClass(cls):
        cls.compiler.cleanup()


if __name__ == '__main__':
    unittest.main()
