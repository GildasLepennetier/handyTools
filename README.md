
# handyTools

This R package contains a collection of useful functions.

# installation

## using remote

	remotes::install_github("GildasLepennetier/handyTools")

## clone locally the repository

	git clone https://github.com/GildasLepennetier/handyTools.git
	devtools::install_local("~/Github/handyTools")

# check package - developer
	
	Sys.setenv("_R_CHECK_SYSTEM_CLOCK_" = "0")
	devtools::document()
	devtools::check()
	devtools::build()

