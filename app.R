#########################
# This is the user-interface definition of a Shiny web application for Gypsy
##########################

# load relevant libraries ------------
rm(list = ls())
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  
  library(kableExtra)
  
  library(shiny)
  library(shinythemes)
  # library(shinysky)
  
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


#define th ui -----------

ui <- fluidPage(             
  theme = shinytheme("cerulean"),
  titlePanel("Gypsy: a discounted cash flow analysis for the application of gypsum to sodic soils"),
  # footer = includeHTML("www/scripts/footer.html"),
  windowTitle = 'Gypsy',
  navbarPage(title = 'Gypsy',
             # tab panel 1 - Home ---------
             tabPanel("Home",
                      p('Note that for this testing, yield and economic analysis for wheat was dummied from the sugarcane, and is for illustration only.', 'We are conducting a meta-analysis on effects of sodicity amelioration on yield, which will hopefully inform the yield and economic analysis for wheat and be incorporated into Gypsy.' ,"For field-based analysis, currently only areas within the GRDC northern grain-cropping region (NGR) will return a result, because we only have ESP maps for the NGR as a part of my (Chloe's) PhD. This means that it won't work for most sugar-growing regions apart from a small area near Sarina, Queensland.", 'A next step might be perform ESP mapping for Australia, or at least for the sugar and grains cropping regions, to enable wider application.', style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px")
                      # includeHTML("www/scripts/footer.html")
             ),
             
             # tab panel 2 - Profile-based analysis ------
             tabPanel('Profile-based',
                      profileAnalysis()),
             
             #tab panel 3 - field-based analysis ---------
             tabPanel('Field-based',
                      fieldAnalysis()),
             # 
             #tab panel 4 - zonal-based analysis ---------
             tabPanel('Zonal-based',
                      zonalAnalysis()),
             
             #tab panel 5 - About ------------
             tabPanel('About',
                      # includeHTML("www/scripts/footer.html")
             )
  )
)

########define the server logic ---------

# load the relevant data
fesp <- list.files('www/data/ESP/', full.names = T)
esp.r <- stack(fesp)
cec.r <- stack('www/data/CEC/CEC_NGR.tif')



server <- function(input, output, session){
  

  
  
  ###### profile-based analysis ---------------
  ## reactive values 
  out_list <- eventReactive(input$calc, {

    
    # the following calculations are gypsum specific but not crop specific ---------
    # based on the max rate
    # generate 9 application rates same as the previous gypsy version
    gypRate <- input$MaxRate * seq(0, 1, 0.125)
    
    
    # calculate rates of pure gypsum applied based on the 9 application rates
    qualG <- input$qualG
    ratePure <- sapply(gypRate, FUN = function(gRate){
      rPure <- gRate*cal_frate(gRate)*qualG/18.6
      rPure})
    
    # check the EC of soil and adjust ESP 
    req(input$ECsoil_u)
    if (input$ECsoil_u > 0.3) {
      showNotification("In soils with EC(1:5) > 0.3 dS/m, standard laboratory tests overestimates CEC and ESP, Gypsy can correct these values using the chloride value. If you do not have a chloride value for the 20-50 cm layer, this will be estimated based on your soil attributes values in the 0-20 cm layer.", type = 'warning', duration = 20)
      
    }
    
    
    
    if (input$ECsoil_u > 0.3 & input$lowDepth == T){
      # adjust ESP and CEC for both upper and lower
      soil.u <- corr_esp(input$CECu, input$ESPinit_u, input$Cl_u)
      soil.l <- corr_esp(input$CECl, input$ESPinit_l, input$Cl_l)
      soilV.l <- list('CECl'= soil.l$CEC, 'ESPl' = soil.l$ESP, 'ECsoil_l'=input$ECsoil_l)
    }
    
    if (input$ECsoil_u > 0.3 & input$lowDepth == F){
      # adjust ESP and CEC for both upper and lower
      soil.u <- corr_esp(input$CECu, input$ESPinit_u, input$Cl_u)
      # extrapolation of soil attribute values from upper depths 
      # this is sugarcane plantation district-based
      soilV.l <- cal_l(soil.u$CEC, soil.u$ESP, input$ECsoil_u, input$location)
      
    }
    if (input$ECsoil_u <= 0.3 & input$lowDepth == F){
      # extrapolation of soil attribute values from upper depths 
      # this is sugarcane plantation district-based
      soil.u <- list('CEC'= input$CECu, 'ESP' = input$ESPinit_u)
      soilV.l <- cal_l(input$CECu, input$ESPinit_u, input$ECsoil_u, input$location)
    }
    
    if (input$ECsoil_u <= 0.3 & input$lowDepth == T){
      
      # use the input 
      soil.u <- list('CEC'= input$CECu, 'ESP' = input$ESPinit_u)
      soilV.l <- list('CECl'= input$CECl, 'ESPl' = input$ESPinit_l, 'ECsoil_l'=input$ECsoil_l)
    }
    # output adjusted values for further analysis
    # calculate gypsum requirement for each soil depth layer
    gRequ <- calGreq(ESPmin = input$ESPmin_u, ESPinit = soil.u$ESP,
                     BD = input$BDu, factorF =input$Fu, CEC= soil.u$CEC)
    gReql <- calGreq(ESPmin = input$ESPmin_l, ESPinit = soilV.l$ESPl,
                     BD = input$BDl, factorF =input$Fl, CEC= soilV.l$CECl)
    # calculate total gypsum requirement for amelioration
    gTotal <- calcGtotal(gRequ, gReql, qualG)
    
    # calculate gypsum used in the 1st depth layer of soil
    usedG_u <- sapply(ratePure, function(x){
      y <- calGused(rPure = x, gReq = gRequ)
      y})
    # calculated gypsum left after ameliorating upper depth
    rPurel <- ratePure - usedG_u
    # the gypsum used in lower depths is affected by the amount of gypsum entering that layer
    usedG_l <- sapply(rPurel, function(x){
      y <- calGused(rPure = x, gReq = gReql)
      y
    })
    # calculate surplus gypsum (gypsum leached below 50 cm)
    surG <- (ratePure - usedG_u - usedG_l)*18.6/qualG
    #calculate final expected ESP
    espFin_u <- calESPf(usedG_u,input$Fu, soil.u$ESP, input$BDu, soil.u$CEC)
    espFin_l <- calESPf(usedG_l,input$Fl, soilV.l$ESPl, input$BDl, soilV.l$CECl)
    
    # these calculations are currently sugarcane-specific --------
    
    # yield effects of sodicity were analysed based on depth-weighted exNa
    # calculate exchangeable sodium in cmolc/kg
    exNa_init <- calExNa(soil.u$ESP, soilV.l$ESPl, soil.u$CEC, soilV.l$CECl)
    exNa_fin <- calExNa(espFin_u, espFin_l, soil.u$CEC, soilV.l$CECl)
    
    # calculate yield improvement
    # the calcYldadd function needs to be updated with 
    # wheat yield improvement on sodic soils 
    yldAdd <- calcYldadd(
      # input$cropType, 
      input$location, exNa_init, exNa_fin, input$yr)
    
    # calculate additional annual income
    income.add <- yldAdd * input$price
    
    # calculate cost
    cost.add <- input$costg * gypRate
    
    # calculate net benefit over the specified crop cycle
    ben.net <- input$yr*income.add-cost.add-input$disRate*cost.add*0.01*input$yr
    
    
    #export results in a data.table-------------
    #create a df with rows corresponding to: gypRate, ESPfin_u, ESPfin_l, surG, cost, yldAdd, income.add, netBen
    df <- rbind.data.frame(gypRate, espFin_u, espFin_l, surG, cost.add, yldAdd, income.add, ben.net) %>%
      round(digits = 2)
    var <- c('Gypsum application rates (t/ha)', 'Final ESP (0-20 cm)', 'Final ESP (20-50 cm)','Surplus Gypsum (t/ha)',
             'Gypsum cost, spread (&#36 /ha)', 'Additional yield p.a. (t/ha)', 'Additional income p.a. (&#36 /ha)',
             'Net benefit over time period (&#36 /ha)')
    df <- cbind.data.frame(var, df)
    colnames(df) <- NULL
    out <- list(
      'df'= df,
      'gTotal' = gTotal,
      'gypRate' = gypRate,
      'ben.net' = ben.net)
    return(out)
  })
  
  
  ## output specification for profile based analysis -----------
  # output$cashFlow <- renderTable({
  #   req(out_list())
  #   out_list()$df
  #   
  # })
  
  # try kable instead
  output$cashFlow <- renderText({
    req(out_list())
    kable(out_list()$df, escape = F)%>%
      column_spec(1, bold = T) %>%
      kable_styling(
        font_size = 15,
        bootstrap_options = c("striped", "hover", "condensed","responsive")
      )
    
  })
  
  output$netBenefit <- renderPlot({
    # Plot net benefit graph
    # 
    # The expression is wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should be automatically 
    #     re-executed when inputs change
    #  2) Its output type is a plot
    req(out_list())
    
    plot(out_list()$gypRate, out_list()$ben.net, type = 'l',
         #add diff gyp rates to the plot as vertical lines,
         xlab = 'Gypsum rate (t/ha)', ylab = 'Estimated net benefit over time period ($/ha)')
    #grey out rate larger than 10 (??? need to establish the sensible value)
    abline(v = 10, col = 'red')
    abline(h = 0, col = 'navy')
  })
  
  
  #report total gypsum required
  output$gTotal <- renderText({
    req(out_list())
    paste0('Total gypsum required to reach non-limiting ESP (t/ha): <b>', round(out_list()$gTotal, digits = 2), '</b>')})
  
  ##### field-based analysis ----------
  ## reactive values
  

  
  varRate <- eventReactive(input$calcF, {
    showNotification("Depending on the size of your paddock, this may take a few minutes to load", type = 'message', duration = 20)
    
    req(input$shp)
    # ###############################
    # # field-scale analysis
    #
    shpdf <- input$shp
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    
    aoi <- shapefile(paste(tempdirname,
                           shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                           sep = "/"))
    # aoi <- shapefile(file.choose())
    #ensure the projection is consistent
    # check the projection and do suitable transformation if needed
    if(proj4string(esp.r) != proj4string(aoi)){
      aoi.reproj <- spTransform(aoi, CRS = esp.r@crs)
    }else{
      aoi.reproj <- aoi
    }
    
    
    
    # extract data from API soil maps (need both ESP and CEC)
    # currently using local soil maps stored in www folder
    # only covers the NGR 
    # crop and perform analysis
    esp.a <- subImage(esp.r, aoi.reproj)
    # select layer based on input soil depths
    idx.d <- grep(input$depth, c(5,15,30,60,100,200))
    esp.a <- subset(esp.a, subset = 1:idx.d)
    names(esp.a) <- depthsNames[1:idx.d]
    
    esp.rr <- esp.a #make a copy to manipulate
    cec.rr <- subImage(cec.r, aoi.reproj)
    cec.rr <- subset(cec.rr, subset = 1:idx.d)
    # calculate gypsum requirement
    # set values smaller than ESPmin to ESPmin
    esp.rr[esp.rr < input$ESPmin_f] <- input$ESPmin_f
    # get the esp diff that need to be ameliorated (ESPinit - ESPmin)
    espc <- esp.rr - input$ESPmin_f
    
    gr.s <- stack() # empty raster stack to host gypsum requirement maps
    for (i in (1:idx.d)){
      # calculate gypsum requirement for each soil depth layer
      # for depth layers less than 30 cm, use bulk density of 1.3
      if (i < 4){
        gr.i <- calGreq.r(1.3, 1.3, raster(espc, layer = i), raster(cec.rr, layer = i), depths = depths[i]/100)
      }else{
        # otherise use bd of 1.4
        gr.i <- calGreq.r(1.3, 1.4, raster(espc, layer = i), raster(cec.rr, layer = i), depths = depths[i]/100)
      }
      gr.s <- stack(gr.s,gr.i)
    }
    #name the layers properly for plotting
    names(gr.s) <- depthsNames[1:idx.d]
    # get total gypsum requirement for selected depths
    gr.all <- sum(gr.s)
    
    # (resolution appox. 90, suitable for variable rate)
    # do we want zonal management? zonal maps?
    # to identify regions that require
    # lower application = less than rule-of-thumb rate 2.5 Mg/ha
    # between 2.5Mg/ha to 10 Mg/ha (manageable)
    # above 10 (recommanding no remediative actions taken and reduce other fertiliser input)
    # (using indicator variable based on gypsum requirement, low = 1, med = 2, high = 3)
    
    # use the gypsum quality to back calculate the actual gypsum requirement
    # calculate the gypsum required for the input gypsum quality to reach pure gypsum amelioration assuming no loss
    gR <- calGrate(gr.all, input$qualG_f) 
    # use optim function to back calculate gypsum requirement assuming surface and runoff loss
    # optim cannot be applied directly to raster, hence extract raster values then calculate
    aa <- cbind(extract(gR, 1:ncell(gR)), extract(gr.all, 1:ncell(gr.all))) %>%
      data.frame()
    aa$gq <- input$qualG_f
    # construct a dataframe containing the gypsum requirement, pure gypsum requirement and the gypsum quality
    colnames(aa) <- c('gr','gp','gq')
    nn <- na.omit(aa) # optim does not handle NA values
    bb <- apply(nn, MARGIN = 1, FUN = function(x){
      xx <- as.numeric(x)
      gRate <- xx[1]
      rP <- xx[2]
      qG <- xx[3]
      new <- optim(gRate, objFun, rPure = rP, qualG = qG, method = 'Brent', lower = 0, upper = 300)
      return(new$par)
      
    })
    cc <- aa$gr
    idx <- which(!is.na(aa$gr))
    cc[idx] <- bb
    #output final variable rate gypsum requirement as a raster
    gR.f <- setValues(gR, cc)
    
    # use cost of spread to calculate variable cost for the field
    gR.c <- input$costg_f * gR.f
    
    
    # these calculations are sugarcane specific-----------
    
    # calculate change in exchangeable sodium in cmolc/kg 
    Na.d <- espc*cec.rr/100
    
    # with a depth weighting of root distribution from Nelson and  Ham (2000)
    # the best way is to harmonise the change in ExNa to the depths in Nelson and Ham (2000) then apply weighting
    df <- extract(Na.d, 1:ncell(Na.d))
    id <- 1:ncell(Na.d)
    df.na <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(df.na)<- c('ID', 'dU', 'dL', 'ExNa')
    for (i in (1:idx.d)){
      aa <- data.frame('ID' = id,'dU'= rep(depth.all[i], length(id)),'dL'= rep(depth.all[i+1], length(id)), 'ExNa'= df[,i])
      
      df.na <- rbind.data.frame(df.na, aa)
      
    }
    # use ea_spline to calculate new weights at globalsoilmap specifications
    wts <- data.frame('ID' = 1, 'dU' = seq(0, 75, 12.5)[-7], 'dL'= seq(12.5, 75, 12.5), 'weights' = c(0.408, 0.208, 0.144, 0.098, 0.092, 0.05))
    wts.fit <- ithir::ea_spline(wts, var.name = 'weights', d = t(c(0,5,15,30,60,75)))$harmonised[,2:6]
    wts.fit <- wts.fit/sum(wts.fit)
    # extract the weight to the user-selected depth layer
    if (ncol(wts.fit)<idx.d){
      # assume no root after 75 cm
      wts.fit <- c(wts.fit,0)
    }else{
      wts.fit <- wts.fit[1:idx.d]
    }
    
    # apply the weighting to individual depth layer
    na.w <- apply(wts.fit, MARGIN = 2, FUN = function(x){
      idx <- grep(x, as.numeric(wts.fit))
      rr <- subset(Na.d, subset = idx)
      out <- x*rr
      out
    })
    na.r <- stack(unlist(na.w))
    na.r <- calc(na.r, fun = sum)
    
    # calculate yield improvement
    yldAdd <- calcYldadd_f(input$location_f, na.r, input$yr_f)
    # calculate additional annual income
    income.add <- yldAdd * input$price_f
    
    # calculate net benefit over the specified crop cycle
    ben.net <- input$yr_f*income.add-gR.c-input$disRate_f*gR.c*0.01*input$yr_f
    
    
    # return ESP maps of the field,  gypsum requirement maps---------
    out <- list('aoi' = aoi.reproj,
                'esp' = esp.a,
                'gr_byLayer' = gr.s,
                'grAll' = gr.all, # pure gypsum requirement
                'gRf' = gR.f,
                'gRcost' = gR.c,
                'yldadd' = yldAdd,
                'incAdd' = income.add,
                'netBen' = ben.net
                
    )
    return(out)
    
  })
  ## output 
  # currently the render won't display the shapefile 
  # probably because the latticeExtra package evaluate shapefile expressions differently 
  
  # create a reactivevalue that stores the outputs
  outs <- reactiveValues(p = NULL, 
                         p1 = NULL,
                         p2 = NULL,
                         p3 = NULL,
                         p4 = NULL,
                         p5 = NULL,
                         p6 = NULL,
                         p7 = NULL)
  
  output$ESP <- renderPlot({
    req(varRate())
    # render the extracted ESP maps as an output
    # plotting using levelplot
    # aoi <- varRate()$aoi
    outs$p <- levelplot(varRate()$esp,
                        col.regions = viridis,
                        main = 'Exchangeable Sodium Percentage (%)',
                        xlab = 'Easting (m)',
                        ylab = 'Northing (m)',
                        names.attr = depthsNames[1:nlayers(varRate()$esp)])
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p)
  })
  output$gypReq <- renderPlot({
    req(varRate())
    # aoi <- varRate()$aoi
    # render the gypsum requirement by layer maps as an output
    outs$p1 <- levelplot(varRate()$gr_byLayer,
                         col.regions = viridis,
                         main = 'Pure gypsum requirement for individual soil depth layers (t)',
                         xlab = 'Easting (m)',
                         ylab = 'Northing (m)',
                         names.attr = depthsNames[1:nlayers(varRate()$gr_byLayer)])
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p1)
  })
  output$gypTotal.v <- renderPlot({
    req(varRate())
    # aoi <- varRate()$aoi
    # render the total gypsum requirement as an output
    outs$p2 <- levelplot(varRate()$grAll,
                         margin = F,
                         col.regions = viridis,
                         main = 'Total pure gypsum requirement \nfor the field(s) (t)',
                         xlab = 'Easting (m)',
                         ylab = 'Northing (m)'
    )
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p2)
  })
  
  output$gypTotal.f <- renderPlot({
    req(varRate())
    # aoi <- varRate()$aoi
    # render the total gypsum requirement as an output
    outs$p3 <- levelplot(varRate()$gRf,
                         margin = F, 
                         col.regions = viridis,
                         main = 'Total gypsum requirement based on \nthe gypsum quality specified for the field(s) (t)',
                         xlab = 'Easting (m)',
                         ylab = 'Northing (m)'
    )
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p3)
  })
  
  output$gypCost <- renderPlot({
    req(varRate())
    # aoi <- varRate()$aoi
    # render the total gypsum requirement as an output
    outs$p4 <- levelplot(varRate()$gRcost,
                         margin = F, 
                         col.regions = viridis,
                         main = 'Total cost of gypsum application for the field(s) ($)',
                         xlab = 'Easting (m)',
                         ylab = 'Northing (m)'
    )
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p4)
  })
  
  
  output$yldAdd <- renderPlot({
    req(varRate())
    # aoi <- varRate()$aoi
    
    outs$p5 <- levelplot(varRate()$yldadd,
                         margin = F, 
                         col.regions = viridis,
                         main = 'Predicted yield increase (t p.a.)',
                         xlab = 'Easting (m)',
                         ylab = 'Northing (m)'
    )
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p5)
  })
  
  output$incAdd <- renderPlot({
    req(varRate())
    # aoi <- varRate()$aoi
    
    outs$p6 <- levelplot(varRate()$incAdd,
                         margin = F, 
                         col.regions = viridis,
                         main = 'Additional annual income ($ p.a.)',
                         xlab = 'Easting (m)',
                         ylab = 'Northing (m)'
    )
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p6)
  })
  
  output$netBen <- renderPlot({
    req(varRate())
    # aoi <- varRate()$aoi
    
    outs$p7 <- levelplot(varRate()$netBen,
                         margin = F, 
                         col.regions = viridis,
                         main = 'The net benefit over the specified periods ($)',
                         xlab = 'Easting (m)',
                         ylab = 'Northing (m)'
    )
    # +latticeExtra::layer(sp.polygons(aoi))
    return(outs$p7)
  })
  
  #################
  # Zonal Analysis -------------
  
  zRate <- eventReactive(input$calcZ, {
    # if shapefile is enabled
    # req(input$shp_z)
    # shpdf <- input$shp_z
    # # Name of the temporary directory where files are uploaded
    # tempdirname <- dirname(shpdf$datapath[1])
    # 
    # # Rename files
    # for (i in 1:nrow(shpdf)) {
    #   file.rename(
    #     shpdf$datapath[i],
    #     paste0(tempdirname, "/", shpdf$name[i])
    #   )
    # }
    # 
    # 
    # aoi <- shapefile(paste(tempdirname,
    #                        shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
    #                        sep = "/"))
    
    
    # read in the csv uploaded
    req(input$csv)
    inFile <- input$csv
    df <- read.csv(inFile$datapath, header = TRUE,sep = ',')
    
    # df <- read.csv('Data/PLarsenZone.csv',sep = ',')
    # replicate df for manipulation
    df1 <- df
    # make sure df1 colnames are what we want
    colnames(df1) <- c('ZoneName', 'Area', 'UpperDepth', 'LowerDepth', 'EC', 'CEC', 'ExNa', 'ESP', 'Cl', 'BD')

    # check EC and adjust CEC and ESP 
    if (any(df1$EC>0.3)){
      showNotification("In soils with EC(1:5) > 0.3 dS/m, standard laboratory tests overestimates CEC and ESP, Gypsy can correct these values using the chloride value.", type = 'warning', duration = 20)
      
      #get indices of EC larger than 0.3 dS/m and adjust ESP and CEC
      idx <- which(df1$EC>0.3)
      df.t <- df1[idx,]
      df.t <- df.t[c('CEC','ESP', 'Cl')]
      #correct ESP and CEC
      tmp <- apply(df.t, 1, function(x){
        xx <- as.numeric(x)
        y <- corr_esp(xx[1],xx[2],xx[3])
        return(y)
      })
      tmp <- data.frame(simplify2array(tmp))
      df1$CEC[idx] <- tmp[1,]
      df1$ESP[idx] <- tmp[2,]
    }


    # total gypsum can be bought by the budget
    #
    # gyp.total <- 10000/90
    gyp.total <- input$Budget/input$costg_z
    
    # check if total gypsum can be bought exceeds the gypsum requirement
    # calculate pure gypsum requirement for each zone--------------
    # aa <- cbind.data.frame(rep(3, nrow(df1)),df1[c('ESP','BD', 'CEC', 'UpperDepth', 'LowerDepth')], 1.3)
    aa <- cbind.data.frame(rep(input$ESPmin_z, nrow(df1)),df1[c('ESP','BD', 'CEC', 'UpperDepth', 'LowerDepth')], 1.3)
    PureGpHa <- apply(aa,1, function(x){
      xx <- as.numeric(x)
      dd <- (xx[6]-xx[5])/100
      # y is the pure gypsum required 
      y <- calGreq(xx[1], xx[2], xx[3],xx[4],dd,1.3)
      return(y)
    })
    # qG <- 14
    qG <- input$qualG_z
    
    # back calculate total gypsum requirement based on % sulfur
    gR <- calGrate(PureGpHa, qG)
    #for each zone, calculate gypsum required------------
    # use optim function to back calculate gypsum requirement assuming surface and runoff loss
    nn <- cbind.data.frame(gR, PureGpHa, rep(qG, nrow(df)))
    # bb is the gypsum requirement rate taken into account of runoff, %sulfur 
    bb <- apply(nn, MARGIN = 1, FUN = function(x){
      xx <- as.numeric(x)
      new <- optim(xx[1], objFun, rPure = xx[2], qualG = xx[3], method = 'Brent', lower = 0, upper = 300)
      return(new$par)
      
    })
    # zonal pure gypsum requirement for each depth in each zone---
    PureGReq <- PureGpHa * df1$Area
    df1$gR <- bb*df1$Area
    df1$gypReqP <- PureGReq
    #summarize by zone
    # depth-weight initial exchangeable Na from df
    # this weighting assumes 0-25, 25-50, need to update later 
    
    # need to implement weighing based on depth interval same as the field-analysis
    df1$ExNa[df1$UpperDepth == 0] <- df1$ExNa[df1$UpperDepth == 0]*0.619
    df1$ExNa[df1$UpperDepth != 0] <- df1$ExNa[df1$UpperDepth != 0]*0.381
    
    df2 <- df1 %>%
      group_by(ZoneName) %>%
      # gypReq is the  total gypsum required for each zone, gypReqP is the total pure gypsum required for each zone
      summarise(ExNa = sum(ExNa), gypReq = sum(gR), gypReqP = sum(gypReqP), Area = mean(Area))
    
    # cap the gypsum requirement at 10 t/ha
    grc.t <- 10*df2$Area # this is the amount of gypsum for each zone if applied at 10 t/ha
    # get the zone that requires more than 10t/ha
    diff.a <- df2$gypReq - grc.t
    idx <- which(diff.a > 0)
    grcapped <- df2$gypReq
    grcapped[idx] <- grc.t[idx] # cap at 10 t/ha
    
    pgr.c <- df2$gypReqP
    gPureCapped <- calRatepure(10, qG)*df2$Area
    pgr.c[idx] <- gPureCapped[idx]
    # get number of zones
    nzone <- length(unique(df1$ZoneName))
    
    # gypZ1 is the zonal gypsum application based on gypsum requirement and capped at 10 t/ha
    gypZ1 <-grcapped
    # if the budget is over the total capped gypsum requirement, start with total capped gypsum requirement
    if (gyp.total > sum(gypZ1)){
      gyp.total <- sum(gypZ1)
    }else{
      # scale zonal application to that is available by budget
      rr <- gypZ1/sum(gypZ1)
      gypZ1 <- gyp.total*rr
    }
    
    #initialize optimZone with gypZ1*0.8 so that the function won't fail when try to extend
    stillRemoving <- T
    if (stillRemoving == T){

      netBen.t <- optim(gypZ1*0.8, optimZoneWrap, gyp.t=gyp.total, df1 = df1, nzone = nzone, ds = input$location_z, 
                        df2 = df2, yr = input$yr, qG = qG, espmin = input$ESPmin_z, pricez = input$price_z, 
                        dRatez = input$disRate_z, costg = input$costg_z, control = list(fnscale = -1))
      if (netBen.t$value <= 0){
        stillRemoving <- T
        out <- optimZone1(gypZ1 = netBen.t$par, df1 = df1, nzone = nzone, ds = input$location_z, 
                          df2 = df2, yr = input$yr, qG = qG,
                          espmin = input$ESPmin_z, pricez = input$price_z, dRatez = input$disRate_z, costg = input$costg_z)
        gypZ1 <- out$dfa$GypsumRate
        
      }else{
        stillRemoving <- F
      }
    }

    
    #get output
    out <- optimZone1(gypZ1 = netBen.t$par, df1 = df1, nzone = nzone, ds = input$location_z, 
                      df2 = df2, yr = input$yr, qG = qG,
                      espmin = input$ESPmin_z, pricez = input$price_z, dRatez = input$disRate_z, costg = input$costg_z)
    
    return(out)
    
    
  })
  
  ## output specification for zonal based analysis -----------
  # output$Table <- renderTable({
  #   req(zRate())
  #   zRate()$dfa
  #   
  # })
  
  
  #report total additional income projected
  output$incTotal <- renderText({
    req(zRate())
    paste0('Projected additional income p.a. ($) for the field: <b>', round(zRate()$incAdd, digits = 2), '</b>')})
  
  output$costZ <- renderText({
    req(zRate())
    paste0('Cost of gypsum application based on recommendation ($) for the field: <b>', round(zRate()$cost, digits = 2), '</b>')})
  
  #report total net benefit
  output$NetBenTotal <- renderText({
    req(zRate())
    paste0('Projected net benefit over time period ($) for the field: <b>', round(zRate()$netBen, digits = 2), '</b>')})
  
  # try kable instead
  output$TableZ <- renderText({
    req(zRate())
    kable(zRate()$dfa, escape = F, col.names = c('Zone Name', 'Zone Area (ha)',
                                                 'Recommended Gypsum Application Rate (t/ha)',
                                                 'Total Gypsum Recommended to Apply (t)',
                                                 'Projected Yield Increase (t)',
                                                 'Predicted Additional Income (&#36 )'))%>% #replace column name with sensible display
      column_spec(1, bold = T) %>%
      kable_styling(
        font_size = 15,
        bootstrap_options = c("striped", "hover", "condensed","responsive")
      )
    
  })
  
  output$dfTable <- renderText({
    req(zRate())
    df <- zRate()$dfb[c('ZoneName','gR','ESP', 'ESPf')] 
    row.names(df) <- NULL
    df[,2:4] <- round(df[,2:4], digits = 2)
    kable(df, escape = F, col.names = c('Zone Name', 
                                        'Estimated Gypsum Requirement for Each Depth for the Zone Area(t)',
                                        'Initial ESP for Each depth',
                                        'Final ESP based on Maximizing Net Benefit')) %>%
      column_spec(1, bold = T) %>%
      kable_styling(
        font_size = 15,
        bootstrap_options = c("striped", "hover", "condensed","responsive"))
  })
  
  
  # download --------------------
  # allow the user to download a report based on analysis type
  
  # profile-based ------------
  # create temp file location to store the pdf
  
  frmd.a <- tempfile('report', tmpdir = tempdir(), fileext = '.Rmd')  
  output$downloadP <- downloadHandler(
    
    #specify a filename to save, need to be a reactive expression (i.e. a function)
    filename = function(){
      tempfile(paste0(input$fieldName, 'Report'), tmpdir = tempdir(), fileext = '.html')
    },
    # knitr provides fancy reporting however, can be very slow for pdf, output html instead
    content = function(file){
      # Copy the template r markdown file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy('reportP.Rmd', frmd.a, overwrite = T)
      rmarkdown::render(frmd.a, output_file = file)
    }
  )
  
  
  
  
  
  # field-based -------------
  # create temp file location to store the pdf
  
  frmd.b <- tempfile('report', tmpdir = tempdir(), fileext = '.Rmd')
  
  
  output$downloadF <- downloadHandler(
    
    #specify a filename to save, need to be a reactive expression (i.e. a function)
    filename = function(){
      tempfile(paste0(input$fieldName_f, 'Report'), tmpdir = tempdir(), fileext = '.html')},
    # content = function(file){
    #   # compile a report (currently the display is managed through specifying the size of pdf)
    #   # need knitr to compile report
    #   pdf(file, onefile = T, width = 7, height = 35)
    #   grid.arrange(outs$p, outs$p1, outs$p2, outs$p3, outs$p4, ncol = 1)
    #   dev.off()
    # }
    
    # knitr provides fancy reporting however, can be very slow
    content = function(file){
      # Copy the template r markdown file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      file.copy('reportF.Rmd', frmd.b, overwrite = T)
      rmarkdown::render(frmd.b, output_file = file)
    }
  )
  
  # specify a filename to save, need to be a reactive expression (i.e. a function)
  # downloadRaster
  
  # create a folder in tmpdir and write output in that folder
  
  
  output$downloadRaster <- downloadHandler(
    
    #specify a filename to save, need to be a reactive expression (i.e. a function)
    filename = function(){
      tempfile(paste0(input$fieldName_f, '_raster'), tmpdir = tempdir(), fileext = '.tif')},
    content = function(file){
      # do this only if download button is clicked
      rr.out <- stack(varRate()$gR.f, varRate()$gRcost, varRate()$yldadd, varRate()$incAdd, varRate()$netBen)
      writeRaster(rr.out, file, options = 'INTERLEAVE=BAND', overwrite = T)
    }
  )
  
  
  # This code will be run after the client has disconnected to remove the report generated
  session$onSessionEnded(function() {
    fpathP <-  tempfile(paste0(input$fieldName, 'Report'), tmpdir = tempdir(), fileext = '.html')
    fpathF <-  tempfile(paste0(input$fieldName_f, 'Report'), tmpdir = tempdir(), fileext = '.html')
    fpathR <- tempfile(paste0(input$fieldName_f, '_raster'), tmpdir = tempdir(), fileext = '.tif')
    unlink(c(fpathP, frmd.a, fpathF, fpathR, frmd.b), recursive = TRUE)
  })
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
