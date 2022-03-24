# Unit Testing
The unit tests use [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit)
	to build.
See [example.pf](example.pf) for a simple example and their documentation for
	more indepth examples.

# Prerequisites
 - pFUnit
 - CMake, version 3.14+ has been used successfully.

# CMake Setup
To turn on the unit tests add the following flags during CMake configuration.
```
      -DUNIT_TESTS:BOOL=1 \
      -DCMAKE_PREFIX_PATH=/glade/u/home/soren/local/pfunit/4.2.4-gnu/PFUNIT-4.2
```


# Creating Unit Tests
Create a new `.pf` module file. Add a new entry to `CMakeLists.txt` using
	`add_pfunit_ctest`, making sure that the module name matches the file
	name.
Also, make sure that `add_pfunit_ctest` has a `LINK_LIBRARIES` entry or the
	needs files may not be populated correctly.


## `.pf` Fortran Formatting in Editor
 - Emacs: add `(add-to-list 'auto-mode-alist '("\\.pf\\'" . f90-mode))` to `~/.emacs`
