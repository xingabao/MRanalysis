# Load Packages
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(esquisse)))
suppressMessages(suppressWarnings(library(bs4Dash)))
suppressMessages(suppressWarnings(library(fresh)))
suppressMessages(suppressWarnings(library(shinyjs)))
suppressMessages(suppressWarnings(library(colourpicker)))
suppressMessages(suppressWarnings(library(shinyWidgets)))
suppressMessages(suppressWarnings(library(shinyvalidate)))
suppressMessages(suppressWarnings(library(shinymeta)))
suppressMessages(suppressWarnings(library(shinyalert)))
suppressMessages(suppressWarnings(library(shinyjqui)))
suppressMessages(suppressWarnings(library(spsComps)))
suppressMessages(suppressWarnings(library(formatR)))
suppressMessages(suppressWarnings(library(shinycssloaders)))
suppressMessages(suppressWarnings(library(formattable)))
suppressMessages(suppressWarnings(library(datamods)))
suppressMessages(suppressWarnings(library(shinydashboard)))
suppressMessages(suppressWarnings(library(DT)))

suppressMessages(suppressWarnings(library(ggtext)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(ggthemes)))


# Load Scripts
source('src/utils.R')
source('src/palette.R')
source('src/defaults.R')

# MR HOME
MR.HOME <- 'https://mranalysis.cn/'

# Set Env
options(shiny.maxRequestSize = 800*1024^2)
options(formatR.width = 300)
Sys.setenv(CLIPR_ALLOW = TRUE)
Sys.setenv(LANGUAGE = "en")