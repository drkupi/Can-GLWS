# Can-GLWS

This repository contains the code for the [Canadian Great Lakes Weather Data Service for SWAT (Can-GLWS) application](https://www.uoguelph.ca/watershed/glws/).

Can-GLWS is a Data as a Service (DaaS) platform that allows application users to download SWAT-ready (https://swat.tamu.edu) climate data (historical, climate change scenarios and weather statistics) of a pre-specified region within the Canadian Great Lakes watersheds. The aim of this application is to remove the redundancy associated with SWAT-model weather data preparation. Service / app users simply need to provide / delineate their geographical area of interest (within the Canadian Great Lakes watersheds), and they can subsequently download their desired SWAT-model-ready weather inputs (historical, climate change-related or weather statistics). The area is selected using either i) a map-based interactive polygon drawing tool or ii) user input of a shape file. 

## Technical Information
Can-GLWS is developed using the [R Shiny](https://shiny.rstudio.com/) web framework. Important information regarding the application code is as follows:

* *app.R* - Contains the R code for the entire application. One of the required packages, i.e., geoshaper, is not available on CRAN. Please refer to https://github.com/RedOakStrategic/geoshaper for installing geoshaper.
* *GL_SWAT_all_WGN.csv* - Contains a statistical dataset prepared for dissemination using this application.
* *Dockerfile* - Can-GLWS is hosted as a dockerzied application on a server at University of Guelph. The Dockerfile is provided here as well.

Can-GLWS is currently deployed on a [UoG Sever](https://www.uoguelph.ca/watershed/glws/). The large historical and climate change datasets (in SWAT-ready format) being shared with the application are hosted on the server.   

## Authors
Dr. Narayan Shrestha, Dr. Taimoor Akhtar, Uttam Ghimire, Dr. Prasad Daggupati, Dr. Ramesh Rudra, Dr. Pradeep Goel and Dr. Rituraj Shukla, UNIVERSITY OF GUELPH & MINISTRY OF ENVIRONMENT CANADA.

The shiny application is developed by Dr. Taimoor Akhtar.

Please Contact Us at: pdaggupa@uoguelph.ca for any questions.

## Citing Us
If you use our data, application or codes, please cite this repository and its associated DOI.

