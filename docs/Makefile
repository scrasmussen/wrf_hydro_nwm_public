SOURCEDIR = userguide/
BUILDDIR = _build/html

# List all source files to track dependencies
SOURCE_FILES = $(shell find . -name '*.rst' -o -name '*.rest' -o -name '*.py' -o -name '*.css')

all: build

build: $(BUILDDIR)/index.html

$(BUILDDIR)/index.html: $(SOURCE_FILES)
	@echo "Building readthedocs documentation"
	sphinx-build -b html $(SOURCEDIR) $(BUILDDIR)

open: build
	open $(BUILDDIR)/index.html

clean:
	rm -rf _build
