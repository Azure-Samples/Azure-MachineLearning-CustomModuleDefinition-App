> **NOTE** This content is no longer maintained. Visit the [Azure Machine Learning Notebook](https://github.com/Azure/MachineLearningNotebooks) project for sample Jupyter notebooks for ML and deep learning with Azure Machine Learning.

Author: Antonia Miruna Oprescu
Setup instructions:

To run this app locally, you need R and RStudio.
From R or RStudio, install and load the following packages:

	install.packages(c("shiny", "XML"))
	library(shiny)

You can start the app from the directory containing 
the ui.R and server.R files with:

	runApp(".")

Alternatively, you can start the app from a different
directory with:

	runApp(appDir)

Or you can avoid downloading the app files to your 
local machine by starting the app directly from GitHub with:

	runGitHub("Azure/Azure-MachineLearning-CustomModuleDefinition-App")

If you are running on Windows, you must make sure you have
zip capabilities enabled. The easiest way to ensure this is
to install RTools from

	https://cran.r-project.org/bin/windows/Rtools/

and then uncomment line 3 from server.R .

