#########################
# This is the user-interface definition of a Shiny web application for Gypsy
##########################

# load relevant libraries ------------
rm(list = ls())
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  
  library(shiny)
  library(shinythemes)
  library(shinysky)
  
  library(reshape2)
  library(rgdal)
  library(ithir)
  library(raster)
  library(rasterVis)
  library(RColorBrewer)
  library(viridis)
  library(ggplot2)
  library(ggrepel)
  library(gridExtra)
  library(rgeos)
  
  library(ggmap)
  library(geoR)
  library(rnaturalearth)
  library(legendMap)
})

# source relevant functions --------------

spath <- file.path('www/scripts', '0001Functions.R')
source(spath)





ui <- fluidPage(             
  theme = shinytheme("cerulean"),
  titlePanel("Gypsy: a discounted cash flow analysis for the application of gypsum to sodic soils"),
  # footer = includeHTML("scripts/footer.html"),
  windowTitle = 'Gypsy',
  navbarPage(title = 'Gypsy',
             # tab panel 1 - Home ---------
             tabPanel("Home",
                      p('Note that for this testing, yield and economic analysis for wheat was dummied from the sugarcane, and is for illustration only.', 'We are conducting a meta-analysis on effects of sodicity amelioration on yield, which will hopefully inform the yield and economic analysis for wheat and be incorporated into Gypsy.' ,'For field-based analysis, currently only areas within the GRDC northern grain-cropping region (NGR) will return a result, because we only have ESP maps for the NGR as a part of my (Chloe) PhD.', 'A next step might be perform ESP mapping for Australia, or at least for the sugar and grains cropping regions, to enable wider application.', 'The www/data folder contains shapefiles for three fields within NGR for testing the field-based analysis. The sugarcane shapefile is located near Sarina, Qld. The two farm shapefiles were located in southern Queensland and in northern NSW',style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                      includeHTML("www/scripts/footer.html")
                      ),
             
             # tab panel 2 - Profile-based analysis ------
             tabPanel('Profile-based',
                      profileAnalysis()),
             
             #tab panel 3 - field-based analysis ---------
             tabPanel('Field-based',
                      fieldAnalysis()),
             
             #tab panel 4 - About ------------
             tabPanel('About',
                      # includeHTML("scripts/footer.html")
                      )
  )
)
