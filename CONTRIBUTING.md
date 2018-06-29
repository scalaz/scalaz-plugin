# `partest`

We use `partest` for integration testing. `partest` is the same tool used
to integration test the scala compiler, and its
[sources](https://github.com/scala/scala/tree/2.13.x/src/partest/scala/tools/partest)
can be found in the `scala/scala` repository. 

## Overview:
- All tests go in the `test/files/<KIND>` directory, for some kind.
- The kinds are:
  - `pos`: the file(s) must compile
  - `neg`: the file(s) must _not_ compile
  - `run`: the file(s) must compile and run
- Individual tests can be either:
  - a `.scala` file, which will be the only source compiled and/or run
  - a directory, in which case all files in the directory will be 
    compiled and/or run together
- For `neg` and `run` tests, a same-named `.check` file specifies the 
  output which the test must produce.
- When adding a new test case, name it appropriately:
  - If it is a regression test for an issue on GitHub, name it `t1234`,
    where `1234` is the GitHub issue number
  - If it is a new feature, give it a descriptive but terse name

## In `sbt`:
Our `sbt` has a `partest` command that can be used to run partest. It
recognizes most of the common partest options:
- `partest`: run all test cases
- `partest test/files/<KIND>/<test>`: run one test case
- `partest <KIND>`: run all test cases of kind `KIND`
- `partest --grep <SEARCH>`: run all test cases matching `SEARCH`
- `partest --update-check ...`: rather than failing when the output does
  not match the `.check` file, modify the `.check` file to match the 
  actual output
- `partest --verbose`: print the output of the test cases to the terminal
