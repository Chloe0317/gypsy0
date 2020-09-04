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
    
    # if no input of lower depth soil data available, calculate based on upper soil data
    if(input$lowDepth == F){
      # extrapolation of soil attribute values from upper depths 
      # this is sugarcane plantation district-based
      soilV.l <- cal_l(input$CECu, input$ESPinit_u, input$ECsoil_u, input$location)
    # otherwise use the input  
    }else{
      soilV.l <- list('CECl'= input$CECl, 'ESPl' = input$ESPinit_l, 'ECsoil_l'=input$ECsoil_l)
    }
    # calculate rates of pure gypsum applied based on the 9 application rates
    qualG <- input$qualG
    ratePure <- sapply(gypRate, FUN = function(gRate){
      rPure <- gRate*cal_frate(gRate)*qualG/18.6
      rPure})
    # calculate gypsum requirement for each soil depth layer
    gRequ <- calGreq(ESPmin = input$ESPmin_u, ESPinit = input$ESPinit_u,
                     BD = input$BDu, factorF =input$Fu, CEC= input$CECu)
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
    espFin_u <- calESPf(usedG_u,input$Fu, input$ESPinit_u, input$BDu, input$CECu)
    espFin_l <- calESPf(usedG_l,input$Fl, soilV.l$ESPl, input$BDl, soilV.l$CECl)
    
    # these calculations are currently sugarcane-specific --------
    
    # yield effects of sodicity were analysed based on depth-weighted exNa
    # calculate exchangeable sodium in cmolc/kg
    exNa_init <- calExNa(input$ESPinit_u, soilV.l$ESPl, input$CECu, soilV.l$CECl)
    exNa_fin <- calExNa(espFin_u, espFin_l, input$CECu, soilV.l$CECl)
    
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
    var <- c('Gypsum application rates', 'Final ESP (0-25 cm)', 'Final ESP (25-50 cm)','Surplus Gypsum (Mg/ha',
             'Gypsum cost, spread ($/ha)', 'Additional yield p.a. (Mg/ha)', 'Additional income p.a. ($/ha)',
             'Net benefit over time period ($/ha)')
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
  output$cashFlow <- renderTable({
    req(out_list())
    out_list()$df
    
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
         xlab = 'Gypsum rate (Mg/ha)', ylab = 'Estimated net benefit over time period ($/ha)')
    #grey out rate larger than 10 (??? need to establish the sensible value)
    abline(v = 10, col = 'red')
  })
  
  #report total gypsum required
  output$gTotal <- renderText({
    req(out_list())
    paste0('Total gypsum required to reach non-limiting ESP (Mg/ha): <b>', round(out_list()$gTotal, digits = 2), '</b>')})
  
  ##### field-based analysis ----------
  ## reative values
  varRate <- eventReactive(input$calcF, {
    
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
                    main = 'Pure gypsum requirement for individual soil depth layers (Mg)',
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
                    main = 'Total pure gypsum requirement \nfor the field(s) (Mg)',
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
                    main = 'Total gypsum requirement based on \nthe gypsum quality specified for the field(s) (Mg)',
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
                         main = 'Predicted yield increase (Mg p.a.)',
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
  

  
  
  # download --------------------
  # allow the user to download a report based on analysis type
  
  # profile-based ------------
  # create temp file location to store the pdf
  fpathP <- tempfile('report', tmpdir = tempdir(), fileext = '.html')
  frmd.a <- tempfile('report', tmpdir = tempdir(), fileext = '.Rmd')
  
  output$downloadP <- downloadHandler(
    
    #specify a filename to save, need to be a reactive expression (i.e. a function)
    filename = function(){fpathP},
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
  fpathF <- tempfile('report', tmpdir = tempdir(), fileext = '.html')
  frmd.b <- tempfile('report', tmpdir = tempdir(), fileext = '.Rmd')
  output$downloadF <- downloadHandler(
    
    #specify a filename to save, need to be a reactive expression (i.e. a function)
    filename = function(){fpathF},
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
  
  # This code will be run after the client has disconnected to remove the report generated
  session$onSessionEnded(function() {
    unlink(c(fpathP, frmd.a, fpathF, frmd.b), recursive = TRUE)
  })
  
  

  
  
  
}