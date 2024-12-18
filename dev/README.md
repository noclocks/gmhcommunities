# Package Development Folder (`dev/`)

> [!NOTE]
> This folder is for development purposes only. It is not included in the final package.

The `dev/` folder contains files and directories dedicated to internal package development.

## Folder Structure

## Package Scripts (`pkg_`)

All scripts prefixed with `pkg_` are dedicated to package development:

- `pkg_init.R`: Initializes the package, setting up directories, dependencies, and authors.
- `pkg_devt.R`: Adds new R scripts, Shiny modules, and development utilities.
- `pkg_tests.R`: Sets up and manages unit tests, testthat, and covr integration.
- `pkg_docs.R`: Handles vignettes, README generation, and pkgdown configuration.
- `pkg_data.R`: Prepares raw data for use within the package.
- `pkg_check.R`: Runs `R CMD check` and related validations.
- `pkg_build.R`: Builds and packages the project.
- `pkg_install.R`: Installs the package locally or remotely.
  
Keeping the development scripts separate avoids confusion and ensures each script remains short and focused to its intended purpose.

## Supplementary Files and Folders

- `dev/scripts`: Additional scripts for development tasks.
- `dev/R`: Additional R scripts for development tasks.
- `dev/sandbox`: A playground for testing code snippets and exploring ideas.
- `dev/docs`: Additional documentation for the package.

## Orchestrating Development

Leverage a `Makefile` or an R-based orchestration script to run the development scripts in sequence when they are needed.

This approach ensures consistency and automates the package development workflow.

Example:

```Makefile
# Makefile

all: init devt docs test check build install

init:
	Rscript dev/pkg_init.R
	
devt:
	Rscript dev/pkg_devt.R
	
docs:
	Rscript dev/pkg_docs.R
	
test:
	Rscript dev/pkg_tests.R
	
check:
	Rscript dev/pkg_check.R
	
build:
	Rscript dev/pkg_build.R

install:
  Rscript dev/pkg_install.R
```

