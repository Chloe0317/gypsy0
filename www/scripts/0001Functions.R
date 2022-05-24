
#############
#functions
##############
suppressPackageStartupMessages({
  
  # libraries for data manipulation
  library(tidyverse)
  library(dplyr)
  library(reshape2)
  # libraries for displaying nice table
  # library(xtable) # for latex data presentation if needed
  library(kableExtra)
  
  library(rmarkdown)
  
  # libraries for the shiny app and themes
  library(shiny)
  library(shinythemes)
  # library(shinysky)
  
  # libraries for raster/gis data manipulation
  library(rgdal)
  library(ithir)
  library(raster)
  library(rasterVis)
  library(rgeos)
  library(geoR)
  
  # libraries for plotting graphes
  library(RColorBrewer)
  library(viridis)
  library(ggplot2)
  library(ggrepel)
  library(gridExtra)
  library(rnaturalearth)
  library(legendMap)
  library(ggmap)
  
  
})
# General setup ---------------
# this contains variables that need to be loaded into the app only once before evaluation
districts <- c('Mareeba','Herbert', 'Burdekin', 'Proserpine', 'CentralQld', 'SQld')
crops <- c('Sugarcane', 'Wheat')


depthsNames <- c('0-5 cm', '5-15 cm', '15-30 cm', 
                 '30-60 cm', '60-100 cm', '100-200 cm')

#depths is the thickness of each depth interval
depths <- c(5,10,15,30,40,100)
#depth.all is  the threshold of each depth interval
depth.all <- c(0,5,15,30,60,100,200)


###variableRaster-based----------

subImage = function(rr, aoi.reproj) {
  ############################################
  #function to subset raster using shapefile#
  ############################################
  
  
  rr.e  <-  crop(rr, aoi.reproj)
  dummy  <- setValues(rr.e, NA)
  shp.r  <-  rasterize(aoi.reproj, dummy)
  out <- mask(rr.e, mask = shp.r)
  
  
}

calGreq.r <- function(factorF, BD, espc, CEC, depths){
  # function to calculate gypsum requirement for raster Mg/ha
  # res(rr) = 0.747162 ha/pixel
  gReq <- 0.086 * factorF * depths * BD * CEC * espc*0.747162
  
  
}

# calculate additional yield using yield factor (district-based)
# yf is the increased yield in t/ha for every 1 cmolc/kg drop in ExNa
# time factor assuming half of benefits of gypsum application realized in 1st year 
# with full benefits realized in subsequent years of the crop cycle

calcYldadd_f <- function(
  # crop,
  district, exNa.d, yr){
  if (district == 'Burdekin' | district == 'Mareeba'){
    yf <- 14.8
  }else{
    yf <- 10.6
  }
  # return additional yield in Mg per pixel
  yldAdd <- yf * exNa.d *(yr-0.2)/yr*0.747162
  
}




######### profile-based-------------


cal_l <- function(CECu, ESPu, ECsoil_u, district){
  #function to estimate cec, esp and ec_soil in 25-50 cm from values in 0-25 cm
  #based on district
  if (district == 'Mareeba'){
    cec <- 0.69*CECu + 1.972
    esp <- 2.34*ESPu + 3.48
    ec <- 1.99*ECsoil_u - 0.03
  }
  if (district == 'Herbert'){
    cec <- 1.387*CECu + 1.861
    esp <- 0.849*ESPu + 7.595
    ec <- 1.33*ECsoil_u + 0.04
  }
  if (district == 'Burdekin'){
    cec <- 0.732*CECu + 6.658
    esp <- 1.319*ESPu + 4.153
    ec <- 1.12*ECsoil_u + 0.1
  }
  if (district == 'Proserpine'){
    cec <- 1.209*CECu + 1.73
    esp <- 0.96*ESPu + 4.504
    ec <- 0.89*ECsoil_u + 0.02
  }
  if (district == 'CentralQld'){
    cec <- 1.209*CECu + 1.73
    esp <- 1.226*ESPu + 3.062
    ec <- 1.02*ECsoil_u + 0.05
  }
  if (district == 'SQld'){
    cec <- 0.719*CECu + 2.355
    esp <- ESPu + 4
    ec <- 0.51*ECsoil_u + 0.08
  }
  out <- list('CECl'= cec, 'ESPl' = esp, 'ECsoil_l'=ec)
  return(out)
}

calCl_l <- function(CLu, ECsoil_u, ECsoil_l){
  cl.l <- CLu*ECsoil_l/ECsoil_u
  cl.l
}

corr_esp <- function(CEC, ESP, Cl){
  #if EC > 0.3, soil is saline, adjust ESP #should be surface soil EC
  # THIS CHECK NEED A FLAG
  cec <- CEC-(Cl/355)
  
  # only correct for salinity if the result is positive 
  if (cec > 0){
    esp <- (((ESP*CEC/100)-(Cl/355))*100)/(CEC-(Cl/355))
  }else{
    cec <- CEC 
    esp <- ESP 
  }
  out <- list('CEC' = cec, 'ESP' = esp)
  return(out)
}


cal_frate <- function(gRate){
  #frate is a rate factor that accounts for 
  #possible losses of gypsum in surface runoff and 
  #negative salinity effects at high application rates
  if (gRate <= 10){
    frate <- 1
  }
  if (10 < gRate & gRate<30){
    frate <- (10+0.9*(gRate-10))/gRate
  }
  if (gRate>=30){
    frate <- (28+0.8*(gRate-30))/gRate
  }
  frate
}

calRatepure <- function(gRate, qualG){
  rPure <- gRate*cal_frate(gRate)*qualG/18.6
  rPure
}

objFun <- function(rPure, gRate, qualG){
  rPure1 <- calRatepure(gRate, qualG)
  dif <- rPure-rPure1
  return(abs(dif))
}


calGrate <- function(rPure, qualG){
  # calculate the gRate needed to ameliorate ESP
  gRate <- (rPure*18.6)/qualG
  return(gRate)
}






# calculate gypsum used either to reduce initESP to minESP or 
# total applied depending on gypsum requirement

calGreq <- function(ESPmin, ESPinit, BD, CEC, depth = 0.25,factorF){
  
  
  #calculate gypsum requirement
  if (ESPmin <= ESPinit){
    gReq <- 0.086 * factorF * depth * BD * CEC * (ESPinit - ESPmin)
  }else{
    gReq <- 0
  }
  return(gReq)
}


calGused_z <- function(gReq, rPure){
  #function to calculate gypsum used in upper depths (0-25 cm)
  gUsed <- vector(mode = 'numeric', length = length(gReq))
  diff <- rPure - gReq
  idx <- which(diff>0)
  gUsed[idx] <- gReq[idx]
  gUsed[-idx] <- rPure[-idx]
  return(gUsed)
  
}

calGused <- function(gReq, rPure){
  #function to calculate gypsum used in upper depths (0-25 cm)
  
  if (gReq >= rPure) {
    gUsed <- rPure
  }else{
    gUsed <- gReq
  }
  gUsed
}



calESPf <- function(gUsed, f, ESPinit, BD, CEC, depth = 0.25){
  
  ESP_f <- ESPinit - (gUsed/(0.086*f*depth*BD*CEC))
  if (ESP_f>3){
    return(ESP_f)
  }else{
    ESP_f <- 3
    return(ESP_f)
  }
  
}



calExNa <- function(ESPu, ESPl, CECu, CECl){
  #calculate depth weighted exchangeable sodium in cmolc/kg
  exNa <- (0.619 * ESPu * CECu + 0.381 * ESPl * CECl)/100
  exNa
}

# calculate additional yield using yield factor (district-based)
# yf is the increased yield in t/ha for every 1 cmolc/kg drop in ExNa
# time factor assuming half of benefits of gypsum application realized in 1st year 
# with full benefits realized in subsequent years 

calcYldadd <- function(
  # crop, 
  district, ExNa.init, ExNa.fin, yr){
  # if (crop == 'Sugarcane'){
  #   if (district == 'Burdekin' | district == 'Mareeba'){
  #     yf <- 14.8
  #   }else{
  #     yf <- 10.6
  #   }
  #   yldAdd <- yf * (ExNa.init - ExNa.fin)*(yr-0.2)/yr
  # }
  
  if (district == 'Burdekin' | district == 'Mareeba'){
    yf <- 14.8
  }else{
    yf <- 10.6
  }
  yldAdd <- yf * (ExNa.init - ExNa.fin)*(yr-0.2)/yr
  return(yldAdd)
  
}

calcGtotal <- function(gRequ, gReql, qualG){
  gTotal <- 18.6 * (gRequ+gReql)/qualG
  gTotal
}

optimZoneWrap <- function(gypZ1, gyp.t, df1, nzone, ds, df2, yr, qG, espmin, pricez, dRatez, costg){
  # gypZ1 is the allocated gypsum application in each zone
  # we'd like to optimize the allocation of gypZ1 in individual zones to maximize additional yield and minimize spending
  if (sum(gypZ1)>gyp.t){
    return(0)
  }else{
    y <- optimZone(gypZ1, df1, nzone, ds, df2, yr, qG, espmin, pricez, dRatez, costg)
    return(y)
  }
  
}

optimZone <- function(gypZ1, df1, nzone, ds, df2, yr, qG, espmin, pricez, dRatez, costg){
  
  # this gypsum is not pure gypsum, calculate pure gypsum applied to each zone
  
  # get per ha application
  gypZ <- gypZ1/unique(df2$Area)
  
  new <- cbind(gypZ, qG)
  gypZ <- apply(new, MARGIN = 1, FUN = function(x){
    xx <- as.numeric(x)
    y <- calRatepure(xx[1], xx[2])
    return(y)
  })
  
  # calculate gypsum used in each zone-------
  # split into upper depths and lower depths
  dfu <- df1[df1$UpperDepth == 0,]
  dfl <- df1[df1$UpperDepth != 0,]
  # make sure the zone is sorted same as df2
  dfu <- dfu[order(dfu$ZoneName),]
  dfl <- dfl[order(dfl$ZoneName),]
  
  uu <- cbind(dfu$gypReqP/dfu$Area, gypZ)
  gUsed.u <- apply(uu, 1, function(x){
    xx <- as.numeric(x)
    y <- calGused(xx[1],xx[2])
    return(y)
  })
  # check if surplus, goes to lowerdepth
  gLeft <- gypZ-gUsed.u
  ll <- cbind(dfl$gypReqP/dfu$Area, gLeft)
  gUsed.l <- apply(ll, 1, function(x){
    xx <- as.numeric(x)
    y <- calGused(xx[1],xx[2])
    return(y)
  })
  
  
  #calculate final expected ESP
  # get final ESP for upper and lower depths
  df3 <- rbind.data.frame(dfu,dfl)
  df3$gUsed <- NA
  
  df3$gUsed<- c(gUsed.u, gUsed.l)
  df3$fFactor <- 1.3
  nn <- df3[c('gUsed','fFactor', 'ESP','BD', 'CEC', 'UpperDepth','LowerDepth')]
  espf <- apply(nn, 1, function(x){
    xx <- as.numeric(x)
    dd <- (xx[7]-xx[6])/100
    y <- calESPf(xx[1],xx[2],xx[3],xx[4],xx[5],dd)
    return(y)
  })
  df3$ESPf <- espf
  
  
  # calculate final exchangeable sodium
  new <- cbind(df3$ESPf[df3$UpperDepth == 0], df3$ESPf[df3$UpperDepth != 0], df3$CEC[df3$UpperDepth == 0],df3$CEC[df3$UpperDepth != 0])
  na.f <- apply(new, MARGIN = 1, FUN = function(x) {
    xx <- as.numeric(x)
    y <- calExNa(xx[1],xx[2],xx[3],xx[4])
    return(y)
  })
  
  # calculate init ExNa
  exNa.init <- calExNa(dfu$ESP, dfl$ESP, dfu$CEC, dfl$CEC)
  # get total yield improvement for this field
  new2 <- cbind(exNa.init, na.f, df2$Area)
  yld.t <- apply(new2, MARGIN = 1, FUN = function(x){
    xx <- as.numeric(x)
    y <- calcYldadd(ds, xx[1],xx[2], yr)*xx[3]
  })
  # calculate the additional income
  incomez <- sum(yld.t)*pricez
  
  
  
  # calculate the net benefit over the crop cycle
  # gypZ1 <- round(gypZ1, digits = 2)
  netBen <- yr*incomez-costg*(sum(gypZ1))-dRatez*costg*(sum(gypZ1))*0.01*yr
  # netBen <- round(netBen, digit = 2)
  return(netBen)
}

optimZone1 <- function(gypZ1, df1, nzone, ds, df2, yr, qG, espmin, pricez, dRatez, costg){
  # same as optimZone but detailed output
  # this gypsum is not pure gypsum, calculate pure gypsum applied to each zone
  
  # get per ha application
  gypZ <- gypZ1/unique(df2$Area)
  
  new <- cbind(gypZ, qG)
  gypZ <- apply(new, MARGIN = 1, FUN = function(x){
    xx <- as.numeric(x)
    y <- calRatepure(xx[1], xx[2])
    return(y)
  })
  
  # calculate gypsum used in each zone-------
  # split into upper depths and lower depths
  dfu <- df1[df1$UpperDepth == 0,]
  dfl <- df1[df1$UpperDepth != 0,]
  # make sure the zone is sorted same as df2
  dfu <- dfu[order(dfu$ZoneName),]
  dfl <- dfl[order(dfl$ZoneName),]
  
  uu <- cbind(dfu$gypReqP/dfu$Area, gypZ)
  gUsed.u <- apply(uu, 1, function(x){
    xx <- as.numeric(x)
    y <- calGused(xx[1],xx[2])
    return(y)
  })
  # check if surplus, goes to lowerdepth
  gLeft <- gypZ-gUsed.u
  ll <- cbind(dfl$gypReqP/dfu$Area, gLeft)
  gUsed.l <- apply(ll, 1, function(x){
    xx <- as.numeric(x)
    y <- calGused(xx[1],xx[2])
    return(y)
  })
  
  
  #calculate final expected ESP
  # get final ESP for upper and lower depths
  df3 <- rbind.data.frame(dfu,dfl)
  df3$gUsed <- NA
  
  df3$gUsed<- c(gUsed.u, gUsed.l)
  df3$fFactor <- 1.3
  nn <- df3[c('gUsed','fFactor', 'ESP','BD', 'CEC', 'UpperDepth','LowerDepth')]
  espf <- apply(nn, 1, function(x){
    xx <- as.numeric(x)
    dd <- (xx[7]-xx[6])/100
    y <- calESPf(xx[1],xx[2],xx[3],xx[4],xx[5],dd)
    return(y)
  })
  df3$ESPf <- espf
  
  
  # calculate final exchangeable sodium
  new <- cbind(df3$ESPf[df3$UpperDepth == 0], df3$ESPf[df3$UpperDepth != 0], df3$CEC[df3$UpperDepth == 0],df3$CEC[df3$UpperDepth != 0])
  na.f <- apply(new, MARGIN = 1, FUN = function(x) {
    xx <- as.numeric(x)
    y <- calExNa(xx[1],xx[2],xx[3],xx[4])
    return(y)
  })
  
  # calculate init ExNa
  exNa.init <- calExNa(dfu$ESP, dfl$ESP, dfu$CEC, dfl$CEC)
  # get total yield improvement for this field
  new2 <- cbind(exNa.init, na.f, df2$Area)
  yld.t <- apply(new2, MARGIN = 1, FUN = function(x){
    xx <- as.numeric(x)
    y <- calcYldadd(ds, xx[1],xx[2], yr)*xx[3]
  })
  # calculate the additional income
  incomez <- sum(yld.t)*pricez
  
  
  
  # calculate the net benefit over the crop cycle
  # gypZ1 <- round(gypZ1, digits = 2)
  netBen <- yr*incomez-costg*(sum(gypZ1))-dRatez*costg*(sum(gypZ1))*0.01*yr
  # netBen <- round(netBen, digit = 2)
  addInc <- yld.t*pricez
  
  
  df <- data.frame('ZoneName' = df2$ZoneName, 'ZoneArea' = df2$Area, 'GypsumRate' = round(gypZ1/df2$Area, digits = 2), 'GypsumTotal' = round(gypZ1, digits = 2),  'AdditionalYield' = yld.t, 'AdditionalIncome' = addInc)
  out <- list('dfa' = df, 'dfb'= df3,'incAdd' = incomez, 'cost' = costg*(sum(gypZ1)),'netBen' = netBen)
  return(out)
  
}

#####-----------
# UI related funtions
############



# define original gypsy input into a sidebar panel-------
gypsyO_side <- sidebarLayout(
  sidebarPanel(
    
    #specify paddock name
    textInput(inputId = 'fieldName', label = strong('Paddock Name'), 
              value = "", width = NULL, placeholder = NULL),
    
    # select crop 
    # when we have more crops, build a df so that crops are limited by locations
    selectInput(inputId = "cropType", label = strong("Crop"),
                choices = crops,
                selected = "Sugarcane"),
    
    # Select districts to use
    selectInput(inputId = "location", label = strong("District"),
                choices = districts,
                selected = "Burdekin"),
    
    # specify key variables
    numericInput('qualG', label = strong('Gypsum quality (% Sulfur)'),
                 value = 14, min = 1, max = 18.6),
    numericInput('costg', label = strong('Cost of gypsum spread ($/t)'),
                 value = 110, min = 10, max = 300),
    
    numericInput('price', label = strong('Price of crop ($/t)'),
                 value = 30, min = 10, max = 150),
    numericInput('disRate', label = strong('Discount rate (% p.a.)'),
                 value = 7, min = 0, max = 30),
    HTML('NB: Nine gypsum rates based on the maximum gypsum rate will be plotted'),
    numericInput('MaxRate', label = strong('Maximum gypsum rate for output graph (t/ha)'),
                 value = 30, min = 10, max = 80),
    HTML('NB: Cash flow predictions over more than 4 years should be treated very cautiously'),
    numericInput('yr', label = strong('Time Period (Years)'),
                 value = 3, min = 0, max = 10),
    #key soil inputs
    HTML("Key soil properties (0-20 cm)"),
    numericInput('CECu', label = strong('CEC (cmolc/kg)'),
                 value = 10, min = 0.01, max = 60),
    
    numericInput('ESPinit_u', label = strong('ESP (%)'),
                 value = 10, min = 0, max = 100),
    numericInput('ECsoil_u', label = strong('EC 1:5 soil water (dS/m)'),
                 value = 0.05, min = 0, max = 5),
    numericInput('Cl_u', label = strong('Chloride (mg/kg)'),
                 value = 0.05, min = 0, max = 5),
    
    checkboxInput(inputId = "lowDepth", label = strong("Key soil properties (20-50 cm) known"), value = FALSE),
    # Display only if the lower depth soil properties are checked
    conditionalPanel(condition = "input.lowDepth == true",
                     #key soil inputs
                     HTML("Key soil properties (20-50 cm)"),
                     numericInput('CECl', label = strong('CEC (cmolc/kg)'),
                                  value = 10, min = 0.01, max = 60),
                     numericInput('ESPinit_l', label = strong('ESP (%)'),
                                  value = 10, min = 0, max = 100),
                     numericInput('ECsoil_l', label = strong('EC 1:5 soil water (dS/m)'),
                                  value = 0.05, min = 0, max = 5),
                     numericInput('Cl_l', label = strong('Chloride (mg/kg)'),
                                  value = 0.05, min = 0, max = 5),
                     
    ),
    
    
    
    # Select whether to add advanced inputs
    checkboxInput(inputId = "advInput", label = strong("Advanced inputs"), value = FALSE),
    
    # Display only if the advanced inputs box is checked
    conditionalPanel(condition = "input.advInput == true",
                     HTML("Gypsy uses typical values for the parameters below. 
                                          However, these may be changed if such information is available"),
                     #key soil inputs
                     HTML("Additional soil properties (0-20 cm)"),
                     numericInput('BDu', label = strong('Bulk density (t/m3)'),
                                  value = 1.3, min = 0.8, max = 2),
                     numericInput('ESPmin_u', label = strong('minimum ESP (%) desired'),
                                  value = 3, min = 0, max = 10),
                     numericInput('Fu', label = strong('F factor'),
                                  value = 1.3, min = 1.1, max = 1.3),
                     #key soil inputs
                     HTML("Additional soil properties (20-50 cm)"),
                     numericInput('BDl', label = strong('Bulk density (t/m3)'),
                                  value = 1.4, min = 1, max = 2.5),
                     numericInput('ESPmin_l', label = strong('minimum ESP (%) desired'),
                                  value = 3, min = 0, max = 10),
                     numericInput('Fl', label = strong('F factor'),
                                  value = 1.3, min = 1.1, max = 1.3)
                     
                     
    ),
    actionButton('calc', 'Calculate')),
  #output: cash flow analysis
  
  #maybe use tab panel
  mainPanel(
    hr(),
    htmlOutput('gTotal'),
    br(),
    # tableOutput('cashFlow'), 
    htmlOutput('cashFlow'),
    br(),
    plotOutput(outputId = 'netBenefit'),
    br(),
    #add download button
    downloadButton('downloadP','Download', class = 'btn-block'),
    HTML('NB: applying more than 10 t/ha is not recommended, even if estimated net benefit is positive.')
  )
)

########define field-based gypsy layout----------

gypsyF_side <- sidebarLayout(
  sidebarPanel(
    # because of  the nature of shp files, uploading just the .shp won't work
    # fileInput('shp','Define field boundary', buttonLabel = 'Upload'),
    
    #specify paddock name
    textInput(inputId = 'fieldName_f', label = strong('Paddock Name'), value = "", width = NULL,
              placeholder = NULL),
    
    fileInput(inputId = "shp",
              label = "Define field boundary. Choose shapefile",buttonLabel = 'Upload',
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    
    selectInput(inputId = "cropType_f", label = strong("Crop"),
                choices = crops,
                selected = "Sugarcane"),
    # Select districts to use
    selectInput(inputId = "location_f", label = strong("District"),
                choices = districts,
                selected = "Burdekin"),
    
    #specify desired ESP
    numericInput('ESPmin_f', label = strong('minimum ESP (%) desired'),
                 value = 6, min = 0, max = 10),
    
    selectInput('depth', 'Soil depths to include (cm)',
                choices = c(5, 15, 30, 60, 100, 200), selected = 60),
    # gypsum quality and cost
    numericInput('qualG_f', label = strong('Gypsum quality (% Sulfur)'),
                 value = 14, min = 10, max = 18.6),
    numericInput('costg_f', label = strong('Cost of gypsum spread ($/t)'),
                 value = 110, min = 40, max = 300),
    
    numericInput('price_f', label = strong('Price of crop ($/t)'),
                 value = 30, min = 10, max = 150),
    numericInput('disRate_f', label = strong('Discount rate (% p.a.)'),
                 value = 7, min = 0, max = 30),
    HTML('NB: Cash flow predictions over more than 4 years should be treated very cautiously'),
    numericInput('yr_f', label = strong('Time Period (Years)'),
                 value = 3, min = 0, max = 10),
    
    #add an action button so that the app can start variable-rate calc
    actionButton('calcF', 'Calculate')
  ),
  mainPanel(
    hr(),
    plotOutput(outputId = 'ESP'),
    br(),
    plotOutput(outputId = 'gypReq'),
    br(),
    plotOutput(outputId = 'gypTotal.v'),
    br(),
    plotOutput(outputId = 'gypTotal.f'),
    br(),
    plotOutput(outputId = 'gypCost'),
    br(),
    plotOutput(outputId = 'yldAdd'),
    br(),
    plotOutput(outputId = 'incAdd'),
    br(),
    plotOutput(outputId = 'netBen'),
    #add download button
    downloadButton('downloadF','Download Report', class = 'btn-block'),
    downloadButton('downloadRaster','Download maps as .tiff raster', class = 'btn-block'),
    HTML('NB: applying more than 10 t/ha is not recommended, even if estimated net benefit is positive.')
  )
)


#####Profile-based analysis-----------

profileAnalysis <- function(){
  tagList(
    div(class = 'container', 
        h1('Profile-based', class = 'title fit-h1'),
        p('The profile-based analysis of this new online version of Gypsy performs the same analysis offered by the original Gypsy software.',style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        #add the input and output
        gypsyO_side
    )
  )
}

######field-based analysis--------------

fieldAnalysis <- function(){
  tagList(
    div(class = 'container',
        h1('Field-based', class = 'title fit-h1'),
        p('The field-based analysis of this new online version of Gypsy performs variable-rate calculation of gypsum requirement based on a boundary file of a field.','Upload an ESRI shapefile to allow the delineation of field boundaries. Note that a shapefile consists of different files with extensions .shp, .dbf, .shx, .prj. Select all the relevant files then click upload. Select the desired soil depths of calculation from the drop-down menu. Click calculate.',style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        gypsyF_side
    )
  )
}

#########zonal-based analysis---------------

gypsyZ_side <- sidebarLayout(
  sidebarPanel(
    # because of  the nature of shp files, uploading just the .shp won't work
    # fileInput('shp','Define field boundary', buttonLabel = 'Upload'),
    
    #specify paddock name
    textInput(inputId = 'fieldName_z', label = strong('Paddock Name'), value = "", width = NULL,
              placeholder = NULL),
    
    # fileInput(inputId = "shp_z",
    #           label = "Define field boundary. Choose shapefile",buttonLabel = 'Upload',
    #           multiple = TRUE,
    #           accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    
    fileInput(inputId = "csv",
              label = "Define zones within a field",buttonLabel = 'Upload',
              multiple = T,
              accept = c('.csv','.txt')),
    
    selectInput(inputId = "cropType_z", label = strong("Crop"),
                choices = crops,
                selected = "Sugarcane"),
    # Select districts to use
    selectInput(inputId = "location_z", label = strong("District"),
                choices = districts,
                selected = "Burdekin"),
    #specify desired ESP
    numericInput('Budget', label = strong('Total budget to spend across the field ($)'), value = 10000),
    #specify desired ESP
    numericInput('ESPmin_z', label = strong('minimum ESP (%) desired'),
                 value = 3, min = 0, max = 10),
    # gypsum quality and cost
    numericInput('qualG_z', label = strong('Gypsum quality (% Sulfur)'),
                 value = 14, min = 10, max = 18.6),
    numericInput('costg_z', label = strong('Cost of gypsum spread ($/t)'),
                 value = 110, min = 40, max = 300),
    
    numericInput('price_z', label = strong('Price of crop ($/t)'),
                 value = 30, min = 10, max = 150),
    numericInput('disRate_z', label = strong('Discount rate (% p.a.)'),
                 value = 7, min = 0, max = 30),
    HTML('NB: Cash flow predictions over more than 4 years should be treated very cautiously'),
    numericInput('yr_z', label = strong('Time Period (Years)'),
                 value = 3, min = 0, max = 10),
    
    #add an action button so that the app can start variable-rate calc
    actionButton('calcZ', 'Calculate')
  ),
  mainPanel(
    hr(),
    htmlOutput('incTotal'),
    br(),
    htmlOutput('costZ'),
    br(),
    htmlOutput('NetBenTotal'),
    br(),
    htmlOutput('gTotal_z'),
    br(),
    # tableOutput('Table'), 
    htmlOutput('TableZ'),
    br(),
    htmlOutput('dfTable'),
    br(),
    #add download button
    downloadButton('downloadZ','Download Report', class = 'btn-block'),
    HTML('NB: As application of gypsum below 0.25 t/ha or above 10 t/ha may not be economical, the gypsum application rates for this zonal analysis are capped at 10 t/ha. Similarly, if the recomendation is adjusted so that no gypsum is applied when optimization returns a gypsum rate below 0.25 t/ha. ')
  )
)


zonalAnalysis <- function(){
  tagList(
    div(class = 'container',
        h1('Zonal-based', class = 'title fit-h1'),
        
        # p() used previously will leave blank spaces next to super-scripted m3, need to pass html code instead
        HTML(paste0('<p style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px">The zonal-based analysis of this new online version of Gypsy performs gypsum recommendation based on zonal data. Key in the budget available for the field in question.','Upload a .csv file with the following headings in this order: ZoneName, Area (ha), UpperDepth (cm), LowerDepth (cm), EC (dS/m), CEC (cmol(+)/kg), ExNa (cmol(+)/kg), ESP (%), Cl (mg/kg), BD (t/m',tags$sup('3'), '). The gypsum application recommended for each zone will be capped at 10 t/ha and is based on maximizing net-benefit of the whole field.</p>')),
        gypsyZ_side
    )
  )
}

# a tab that serves the same purpose of the calculations tab in gypsy original

convEC <- function(x, u.in, u.out){
  #function to convert EC between different units
  
  # for interchangeable/same units
  if (u.in == u.out){
    y <- x
  }
  
  if ((u.in == 'mS/cm' & u.out == 'dS/m')|(u.in == 'dS/m' & u.out == 'mS/cm')){
    y <- x
  }
  
  if ((u.in == 'ppm' & u.out == 'mg/L')|(u.in == 'mg/L' & u.out == 'ppm')){
    y <- x
  }
  
  
  
  # for the units that need conversion 
  if (u.in == 'muS/cm'){
    if (u.out == 'dS/m' | u.out == 'mS/cm'){
      y <- x/1000
    }
    if (u.out == 'mg/L' | u.out == 'ppm'){
      y <- x/1000/0.0017
    }
    if (u.out == 'grains/gallon'){
      y <- x/1000/0.022
    }
  }
  
  if (u.in == 'mS/cm' | u.in == 'dS/m'){
    if (u.out == 'muS/cm'){
      y <- x*1000
    }
    
    if (u.out == 'mg/L' | u.out == 'ppm'){
      y <- x/0.0017
    }
    if (u.out == 'grains/gallon'){
      y <- x/0.022
    }
  }
  
  if (u.in == 'mg/L' | u.in == 'ppm'){
    if (u.out == 'muS/cm'){
      y <- x * 0.0017 * 1000
    }
    
    if (u.out == 'mS/cm' | u.out == 'dS/m'){
      y <- x * 0.0017
    }
    if (u.out == 'grains/gallon'){
      y <- x*0.0017/0.022
    }
  }
  
  if (u.in == 'grains/gallon'){
    if (u.out == 'mS/cm' | u.out == 'dS/m'){
      y <- x * 0.022
    }
    if (u.out == 'muS/cm'){
      y <- x * 0.022*1000
    }
    if (u.out == 'mg/L' | u.out == 'ppm'){
      y <- x*0.022/0.0017
    }
    
  }
  return(y)
  
  
}

convECm <- function(vv, mm.in, mm.out, tt){
  #vv - value, mm - method, tt - texture
  if (mm.in == mm.out){
    yy <- vv
  }
  
  if (mm.in == '1:5 Soil Water' & mm.out == 'Saturated Paste'){
    if (tt == 'Sand'){
      yy <- vv*15
    }
    if (tt == 'Sandy loam'){
      yy <- vv*13
    }
    if (tt == 'Loam'){
      yy <- vv*11
    }
    if (tt == 'Clay loam'){
      yy <- vv*9
    }
    if (tt == 'Medium clay'){
      yy <- vv*8
    }
    if (tt == 'Heavy clay'){
      yy <- vv*6
    }
  }
  if (mm.in == 'Saturated Paste' & mm.out == '1:5 Soil Water'){
    if (tt == 'Sand'){
      yy <- vv/15
    }
    if (tt == 'Sandy loam'){
      yy <- vv/13
    }
    if (tt == 'Loam'){
      yy <- vv/11
    }
    if (tt == 'Clay loam'){
      yy <- vv/9
    }
    if (tt == 'Medium clay'){
      yy <- vv/8
    }
    if (tt == 'Heavy clay'){
      yy <- vv/6
    }
  }
  return(yy)
}

espCalc <- function(ca, mg, na, kk, exH){
  cec <- ca+mg+na+kk+exH
  esp <- na/cec * 100
  out <- c('CEC' = cec, 'ESP' = esp)
  return(out)
}


convTab <- function(){
  tagList(
    div(class = 'container',
        h1('EC Converter', class = 'title fit-h1'),
        h2('Converting between EC units', class = 'title fit-h2'),
        HTML('<p style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px">Electrical conductivity (EC) is used to measure the salinity of soil extracts or water. Conversion between the EC units (&#956;S/cm, mS/cm, dS/m) and the concentration units (mg/L, ppm, grains/gallon) are approximate.</p>'),
        br(),
        
        # insert dynamic conversion function (reactive)
        h3('Input', class = 'title fit-h3'),
        numericInput('ec.i', value = 1, label = NULL),
        selectInput('ec.u', choices = c("\u03BCS/cm"="muS/cm",'mS/cm', 'dS/m', 'mg/L', 'ppm','grains/gallon'), selected = 'mS/cm', label = 'Input Unit'),
        
        selectInput('ec.uo', choices = c("\u03BCS/cm"="muS/cm",'mS/cm', 'dS/m', 'mg/L', 'ppm','grains/gallon'), selected = 'dS/m', label = 'Output Unit'),
        h3('Output', class = 'title fit-h3'),
        textOutput('ec.o', inline = T),
        br(),
        
        h2('Converting between EC(1:5) and EC(s.e.)', class = 'title fit-h2'),
        p('Soil salinity is determined by measuring the EC of a 1:5 soil water extract (EC1:5) or a saturated paste extract (ECs.e.). It is possible to convert between the two if the soil texture is known.', style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        # insert conversion
        h3('Input', class = 'title fit-h3'),
        numericInput('ec.im', value = 0.3, label = 'dS/m'),
        selectInput('ec.mi', choices = c('1:5 Soil Water', 'Saturated Paste'), 
                    selected = '1:5 Soil Water', label = 'Input Method'),
        selectInput('txtrS', choices = c('Sand','Sandy loam','Loam', 'Clay loam', 'Medium clay', 'Heavy clay'), label = 'Soil Texture', selected = 'Sand'),
        selectInput('ec.mo', choices = c('1:5 Soil Water', 'Saturated Paste'), 
                    selected = 'Saturated Paste', 
                    label = 'Output Method'),
        
        h3('Output', class = 'title fit-h3'),
        textOutput('ec.om', inline = T),
        br(),
        
        hr(), # insert a horizontal line
        h1('Calculating ESP and CEC', class = 'title fit-h2'),
        h2('based on exchangeable cations and acidity', class = 'title fit-h2'),
        # &#37 represent %
        HTML('<p style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px">Units: me &#37;, meq/100g and cmol(+)/kg are numerically equal</p>'),
        # insert calculation
        numericInput('exca', value = 1, label = 'Exchangeable calcium (Ca), cmol(+)/kg'),
        numericInput('exmg', value = 1, label = 'Exchangeable magnesium (Mg), cmol(+)/kg'),
        numericInput('exk', value = 1, label = 'Exchangeable potassium (K), cmol(+)/kg'),
        numericInput('exna', value = 1, label = 'Exchangeable sodium (Na), cmol(+)/kg'),
        numericInput('exAcid', value = 0, label = 'Exchangeable acidity, cmol(+)/kg (Only relevant if pH < 6, otherwise specify 0'),
        HTML('<h3 class="title fit-h3">Exchangeable sodium percentage (ESP) (&#37;)</h3>'),
        textOutput('esp.o', inline = T),
        h3('Cation exchange capacity (CEC) (cmol(+)/kg)', class = 'title fit-h3'),
        textOutput('cec.o', inline = T),
        br(),
        br(),
        br()
        
        
    )
  )
}

# a tab for irrigation water usage

irrTab <- function(){
  tagList(
    div(class = 'container',
        h1('Irrigation: Conjunctive use', class = 'title fit-h1'),
        p('Achieving a desired electrical conductivity (EC) in a mix', style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        # insert functions
        numericInput('ecmain', value = 0.2, label = 'EC of main supply (dS/m)'),
        numericInput('ecsup', value = 1, label = 'EC of supplementary supply (dS/m)'),
        numericInput('ec.d', value = 0.7, label = 'Desired EC (dS/m)'),
        numericInput('flowRate.m', value = 0, label = 'Flow rate of main supply (L/second)'),
        textOutput('flowRate.s'),
        textOutput('msRatio'),
        br(),
        
        
        p('Concentration of any component (e.g. residual alkali) in the mix', style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        # insert
        numericInput('conc.m', value = 0, label = 'Concentration in main supply'),
        numericInput('conc.s', value = 0, label = 'Concentration in supplementary supply'),
        textOutput('conc.mix'),
        br(),
        
        p('SAR of mix', style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        # insert
        numericInput('sar.m', value = 0, label = 'SAR of main supply'),
        numericInput('sar.s', value = 0, label = 'SAR of supplementary supply'),
        textOutput('sar.mix'),
        br(),
        hr(),
        h1('Irrigation: dissolvenator', class = 'title fit-h1'),
        p('Proportion of water to pass through dissolvenator', style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        numericInput('ecIrrig', value = 0.2, label = 'EC of irrigation water (dS/m)'),
        numericInput('ecDesired', value = 0.7, label = 'Desired EC (dS/m)'),
        textOutput('percWater'),
        p('SAR of the mix', style="text-align:justify;color:white;background-color:#97BF0D;padding:15px;border-radius:10px"),
        numericInput('sarIrrig', value = 1, label = 'SAR of irrigation water'),
        textOutput('sar.mixd'),
        br(),
        br(),
        br()
        
    )
  )
}
