
#############
#functions
##############

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
  cec <- CEC-(Cl/355)
  esp <- ((ESP*CEC/100)-(Cl/355)*100)/(CEC-(Cl/355))
  out <- list('CEC' = cec, 'ESP' = esp)
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


calGreq <- function(ESPmin, ESPinit, BD, factorF, CEC){
  #accommodate raster option
  if (class(ESPinit) == 'RasterLayer'){
    maxInit <- maxValue(ESPinit)
  }else{
    maxInit <- ESPinit
  }
  
  #calculate gypsum requirement 
  if (ESPmin <= maxInit){
    gReq <- 0.086 * factorF * 0.25 * BD * CEC * (ESPinit - ESPmin)
  }else{
    gReq <- 0
  }
  return(gReq)
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



calESPf <- function(gUsed, f, ESPinit, BD, CEC){
  ESP_f <- ESPinit - (gUsed/(0.086*f*0.25*BD*CEC))
  ESP_f
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

}

calcGtotal <- function(gRequ, gReql, qualG){
  gTotal <- 18.6 * (gRequ+gReql)/qualG
  gTotal
}



#####-----------
# UI related funtions
############



# define original gypsy input into a sidebar panel-------
gypsyO_side <- sidebarLayout(
  sidebarPanel(
    
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
    numericInput('qualG', label = strong('Gypsum quality (%)'),
                 value = 14, min = 1, max = 18.6),
    numericInput('costg', label = strong('Cost of gypsum spread ($/Mg)'),
                 value = 90, min = 10, max = 300),
    
    numericInput('price', label = strong('Price of crop ($/Mg)'),
                 value = 30, min = 10, max = 150),
    numericInput('disRate', label = strong('Discount rate (% p.a.)'),
                 value = 7, min = 0, max = 30),
    numericInput('MaxRate', label = strong('Maximum gypsum rate (Mg/ha)'),
                 value = 30, min = 10, max = 80),
    numericInput('yr', label = strong('Time Period (Years)'),
                 value = 4, min = 0, max = 10),
    #key soil inputs
    HTML("Key soil properties (0-25 cm)"),
    numericInput('CECu', label = strong('CEC (cmolc/kg)'),
                 value = 10, min = 0.01, max = 60),
    numericInput('ESPinit_u', label = strong('ESP (%)'),
                 value = 10, min = 0, max = 100),
    numericInput('ECsoil_u', label = strong('EC 1:5 soil water (dS/m)'),
                 value = 0.05, min = 0, max = 5),
    
    checkboxInput(inputId = "lowDepth", label = strong("Key soil properties (25-50 cm) known"), value = FALSE),
    # Display only if the lower depth soil properties are checked
    conditionalPanel(condition = "input.lowDepth == true",
                     #key soil inputs
                     HTML("Key soil properties (25-50 cm)"),
                     numericInput('CECl', label = strong('CEC (cmolc/kg)'),
                                  value = 10, min = 0.01, max = 60),
                     numericInput('ESPinit_l', label = strong('ESP (%)'),
                                  value = 10, min = 0, max = 100),
                     numericInput('ECsoil_l', label = strong('EC 1:5 soil water (dS/m)'),
                                  value = 0.05, min = 0, max = 5)
                     
    ),
    
    
    
    # Select whether to add advanced inputs
    checkboxInput(inputId = "advInput", label = strong("Advanced inputs"), value = FALSE),
    
    # Display only if the advanced inputs box is checked
    conditionalPanel(condition = "input.advInput == true",
                     HTML("Gypsy uses typical values for the parameters below. 
                                          However, these may be changed if such information is available"),
                     #key soil inputs
                     HTML("Additional soil properties (0-25 cm)"),
                     numericInput('BDu', label = strong('Bulk density (Mg/m3)'),
                                  value = 1.3, min = 0.8, max = 2),
                     numericInput('ESPmin_u', label = strong('minimum ESP (%) desired'),
                                  value = 3, min = 0, max = 10),
                     numericInput('Fu', label = strong('F factor'),
                                  value = 1.3, min = 1.1, max = 1.3),
                     #key soil inputs
                     HTML("Additional soil properties (25-50 cm)"),
                     numericInput('BDl', label = strong('Bulk density (Mg/m3)'),
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
    tableOutput('cashFlow'), 
    br(),
    plotOutput(outputId = 'netBenefit'),
    br(),
    #add download button
    downloadButton('downloadP','Download', class = 'btn-block'),
    HTML('NB: applying more than 10 Mg/ha is not recommended, even if estimated net benefit is positive.')
    
  )
)

########define field-based gypsy layout----------

gypsyF_side <- sidebarLayout(
  sidebarPanel(
    # because of  the nature of shp files, uploading just the .shp won't work
    # fileInput('shp','Define field boundary', buttonLabel = 'Upload'),
    
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
    numericInput('qualG_f', label = strong('Gypsum quality (%)'),
                 value = 14, min = 10, max = 18.6),
    numericInput('costg_f', label = strong('Cost of gypsum spread ($/Mg)'),
                 value = 90, min = 40, max = 150),
    
    numericInput('price_f', label = strong('Price of crop ($/Mg)'),
                 value = 30, min = 10, max = 50),
    numericInput('disRate_f', label = strong('Discount rate (% p.a.)'),
                 value = 7, min = 0, max = 30),
    numericInput('yr_f', label = strong('Time Period (Years)'),
                 value = 4, min = 0, max = 10),
    
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
    downloadButton('downloadF','Download', class = 'btn-block'),
    HTML('NB: applying more than 10 Mg/ha is not recommended, even if estimated net benefit is positive.')
  )
)


#####Profile-based analysis-----------

profileAnalysis <- function(){
  tagList(
    div(class = 'container', 
        h1('Profile-based', class = 'title fit-h1'),
        p('The profile-based analysis of this new online version of Gypsy performs the same analysis offered by the original Gypsy software.',style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
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
        p('The field-based analysis of this new online version of Gypsy performs variable-rate calculation of gypsum requirement based on a boundary file of a field.','Upload an ESRI shapefile to allow the delineation of field boundaries. Note that a shapefile consists of different files with extensions .shp, .dbf, .shx, .prj. Select all the relevant files then click upload. Select the desired soil depths of calculation from the drop-down menu. Click calculate.',style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
        gypsyF_side
    )
  )
}

