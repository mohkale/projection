;; -*- lexical-binding: t -*-

(require 'projection-test-utils)

(describe "Project type Pytest"
  (+projection-test-setup)

  (before-each
    (+projection-setup-project '(("foo_test.py" . "import pytest

def test_foo():
    pass

def test_bar():
    pass")
                                 ("param_test.py" . "import pytest
@pytest.mark.parametrize(\"arg\", [(1), (2), (3)])
def test_parmeterised(arg):
    pass")
                                 ("class_test.py" . "import unittest
class ClassTest(unittest.TestCase):
    def test_foo(self):
        pass

    def test_bar(self):
        pass
"))))
  (describe "Multi compile"
    (before-all (require 'projection-multi-pytest))

    (it "Can determine pytest tasks"
      (let ((targets (mapcar #'car (projection-multi-pytest-targets))))
        (expect targets :to-have-same-items-as
                '("pytest-file:foo_test.py"
                  "pytest-test:test_foo"
                  "pytest-test:test_bar"

                  "pytest-file:class_test.py"
                  "pytest-class:ClassTest"
                  "pytest-test:ClassTest#test_bar"
                  "pytest-test:ClassTest#test_foo"

                  "pytest-file:param_test.py"
                  "pytest-test:test_parmeterised"
                  "pytest-test:test_parmeterised[1]"
                  "pytest-test:test_parmeterised[2]"
                  "pytest-test:test_parmeterised[3]"))))))
