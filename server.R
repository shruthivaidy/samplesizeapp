# use server function to assemble inputs into outputs
# build objects to display with render*() function
# render functions create type of output you need (many in Shiny Library)

shinyServer(function(input, output, session) {
################# COMPARISON OF MEANS ####################  
  trial_mean <- reactive({
    power.t.test(n = NULL, delta = input$delta, sd = input$sd, 
                 sig.level = input$siglevel, power = input$power, 
                 type = input$type, alternative = input$alt)
    })
  
  output$samplesize_mean <- renderText({ 
    ceiling(trial_mean()$n)
  })
  output$powerplot_mean <- renderPlot({
    powers <- seq(from = 0.05, to = 0.95, by = 0.05)
    fcn <- function(x, ...) { ceiling(power.t.test(power = x, ...)$n) }
    trialsample_mean <- sapply(powers, fcn, delta = input$delta, 
                               sd = input$sd, sig.level = input$siglevel, 
                               type = input$type, alternative = input$alt)
    
    plot(trialsample_mean,powers,xlab="Sample size",
         xlim = c(ceiling(trialsample_mean[1]),ceiling(trialsample_mean[19])),
         ylab="Expected power (%)",ylim=c(0,1),type="b",col="darkorange",
         lwd=5,axes=FALSE, 
         main = "Plot of Power against Sample Size")
    axis(1,at=trialsample_mean)
    axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c(0,10,20,30,40,50,60,70,80,90,100))
  })
  
  
  
##### with ancova ######  
  trial_mean_an <- reactive({
    power.t.test(n = NULL, delta = input$delta_an, sd = input$sd_an, 
                 type = input$type_an,sig.level = input$siglevel_an, 
                 power = input$power_an,alternative = input$alt_an)
    })
  
  output$samplesizean_mean <- renderText({ 
    
    ceiling(trial_mean_an()$n)
  })
  output$samplesizean_mean_ancova <- renderText({ 
    ceiling((trial_mean_an()$n) * (1-((as.numeric(input$r2))^2)))
  })
  
  output$powerplotan_mean <- renderPlot({
    powers <- seq(from = 0.05, to = 0.95, by = 0.05)
    fcn <- function(x, ...) { ceiling(power.t.test(power = x, ...)$n) }
    trialsamplean_mean <- sapply(powers, fcn, delta = input$delta_an, 
                                 sd = input$sd_an, sig.level = input$siglevel_an, 
                                 type = input$type_an, alternative = input$alt_an)
    plot(trialsamplean_mean,powers,
         xlim=c(trialsamplean_mean[1],trialsamplean_mean[19]),
         xlab="Sample size",
         ylab="Expected power (%)",ylim=c(0,1),type="b",col="darkorange",
         lwd=5,axes=FALSE, 
         main = "Plot of Power against Sample Size")
    lines((trialsamplean_mean)*(1-((as.numeric(input$r2))^2)), powers, col = "turquoise", lwd = 5, type = "b")
    axis(1,at=trialsamplean_mean)
    axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=c(0,10,20,30,40,50,60,70,80,90,100))
    legend(x="right",lwd=5,bty="n",legend=c("t-test sample size","ANCOVA sample size"),col=c("darkorange","turquoise"))
  })
  
  
  
  
#################### CLUSTER RANDOMIZED TRIALS ######################  
####### Means #############  
  observeEvent(input$icc_crtm_txt,{
    if(input$icc_crtm_txt != input$icc_crtm_slider)
    {
      updateSliderInput(
        session = session,
        inputId = 'icc_crtm_slider',
        value = input$icc_crtm_txt
      ) # updateSliderInput
    }#if
  })
  
  observeEvent(input$icc_crtm_slider,{
    if(input$icc_crtm_txt != input$icc_crtm_slider)
    {
      updateNumericInput(
        session = session,
        inputId = 'icc_crtm_txt',
        value = input$icc_crtm_slider
      ) # updateTextInput
    }#if
  })
  
  de_crtm <- reactive({
    (((input$clus_crtm - 1) * input$icc_crtm_txt) + 1)
  })
  
  trial_mean_crt <- reactive({
    power.t.test(n = NULL, delta = input$delta_crt,sd = input$sd_crt,
                 type = input$type_crt, sig.level = input$siglevel_crt, 
                 power = input$power_crt,alternative = input$alt_crt)
    })
  
  output$samplesize_mean_crt <- renderText({ 
    ceiling((trial_mean_crt()$n) * de_crtm())
    })
  output$de_crt <- renderText({ 
    de_crtm()
    })
  
  output$num_clus_crt <- renderText({ 
    round(((trial_mean_crt()$n) * de_crtm())/(input$clus_crtm),digits = 2)
  })
  output$powerplot_mean_crt <- renderPlot({ 
    powers <- seq(from = 0.05, to = 0.95, by = 0.05)
    fcn <- function(x, ...) { ceiling(power.t.test(power = x, ...)$n) }
    trialsample_mean <- sapply(powers, fcn, delta = input$delta_crt, 
                               sd = input$sd_crt, 
                               sig.level = input$siglevel_crt, 
                               type = input$type_crt, 
                               alternative = input$alt_crt)
    trialsample_crt <- (trialsample_mean) * de_crtm()
    plot(trialsample_crt,powers,
         xlim=c(ceiling(trialsample_crt[1]),ceiling(trialsample_crt[19])),
         xlab="Sample size",ylab="Expected power (%)",ylim=c(0,1),type="b",
         col="darkorange",lwd=5,axes=FALSE, 
         main = "Plot of Power against Sample Size")
    lines(trialsample_mean, powers, col = "turquoise", lwd = 5, type = "b")
    axis(1,at=trialsample_crt)
    axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
         labels=c(0,10,20,30,40,50,60,70,80,90,100))
    legend(x="right",lwd=5,bty="n",legend=c("cluster sample size",
                                            "t-test sample size"),
           col=c("darkorange","turquoise"))
    })

###### Proportions #########    
  observeEvent(input$icc_crtp_txt,{
    if(as.numeric(input$icc_crtp_txt) != input$icc_crtp_slider)
    {
      updateSliderInput(
        session = session,
        inputId = 'icc_crtp_slider',
        value = input$icc_crtp_txt
      ) # updateSliderInput
    }#if
  })
  
  observeEvent(input$icc_crtp_slider,{
    if(as.numeric(input$icc_crtp_txt) != input$icc_crtp_slider)
    {
      updateNumericInput(
        session = session,
        inputId = 'icc_crtp_txt',
        value = input$icc_crtp_slider
      ) # updateTextInput
    }#if
  })
  
  
  observeEvent(input$control_crtp_txt,{
    if(as.numeric(input$control_crtp_txt) != input$control_crtp_slider)
    {
      updateSliderInput(
        session = session,
        inputId = 'control_crtp_slider',
        value = input$control_crtp_txt
      ) # updateSliderInput
    }#if
  })
  
  observeEvent(input$control_crtp_slider,{
    if(as.numeric(input$control_crtp_txt) != input$control_crtp_slider)
    {
      updateNumericInput(
        session = session,
        inputId = 'control_crtp_txt',
        value = input$control_crtp_slider
      ) # updateTextInput
    }#if
  })
  
  output$ui_prop_crt <- renderUI({
    if (is.null(input$prop_type_crt))
      return()
    
    switch(input$prop_type_crt,
           "risk difference" = sliderInput(inputId = "intervention_crt_txt",
                                           label = "Enter probability in intervention",
                                           value = 0.25, min = 0, max = 1,
                                           step = 0.005),
           "odds ratio" = numericInput(inputId = "or_crt",label = "Enter odds ratio",
                                       value = 1.5, min = 0),
           "risk ratio" = sliderInput(inputId = "rr_crt",
                                      label = "Choose risk ratio",
                                      value = 0.775, min = 0, max = 1, 
                                      step = 0.005),
           "relative risk reduction" = sliderInput(inputId = "rrr_crt",
                                                   label = "Choose relative risk reduction",
                                                   value = 0.775, min = 0, 
                                                   max = 1, step = 0.005)
    )
    
  })
  
  rv_crtp <- reactiveValues(prop_crt = 0.25, control_crtp = 0.75)
  
  observe({
    updateSliderInput(session, "intervention_crt_txt",value=lapply(reactiveValuesToList(input), unclass)$intervention_crt_txt, min = 0 )
    updateNumericInput(session, "or_crt", value=lapply(reactiveValuesToList(input), unclass)$or_crt, min = 0 )
    updateSliderInput(session, "rr_crt", value=lapply(reactiveValuesToList(input), unclass)$rr_crt, min = 0)
    updateSliderInput(session, "rrr_crt", value=lapply(reactiveValuesToList(input), unclass)$rrr_crt, min = 0)
  })
  
  observeEvent(input$control_crtp_txt, {
    rv_crtp$control_crtp <- input$control_crtp_txt
  })
  observeEvent(input$intervention_crt_txt, {
    rv_crtp$prop_crt <- input$intervention_crt_txt
  })
  observeEvent(input$or_crt, { 
      rv_crtp$prop_crt <- ((input$or_crt*rv_crtp$control_crtp)/(1 - rv_crtp$control_crtp))/(1 + ((input$or_crt*rv_crtp$control_crtp)/(1 - rv_crtp$control_crtp)))
  })
  observeEvent(input$rr_crt, { 
      rv_crtp$prop_crt <- input$rr_crt*rv_crtp$control_crtp
  })
  observeEvent(input$rrr_crt, { 
      rv_crtp$prop_crt <- (1 - input$rrr_crt)*rv_crtp$control_crtp
  })
  
  output$type_crtp <- renderText({
    rv_crtp$prop_crt
  })
  
  
  de_crtp <- reactive({
    ((input$clus_p - 1)* (input$icc_crtp_txt) ) + 1
  })
  
  trial_prop_crt <- reactive({
    power.prop.test(n = NULL, p1 = rv_crtp$control_crtp,p2 = rv_crtp$prop_crt, 
                    sig.level = input$siglevel_crtp,power = input$power_crtp,
                    alternative = input$alt_crtp)
  })
  
  output$samplesize_prop_crt <- renderText({ 
    ceiling((trial_prop_crt()$n) * de())
  })
  output$de_crtp <- renderText({ 
    de_crtp()
  })
  
  output$num_clus_crtp <- renderText({ 
    round(((trial_prop_crt()$n) * de_crtp())/(input$clus_p),digits = 2)
  })
  output$powerplot_prop_crt <- renderPlot({ 
    powers <- seq(from = 0.05, to = 0.95, by = 0.05)
    fcn <- function(x, ...) { ceiling(power.prop.test(power = x, ...)$n) }
    trialsample_prop <- sapply(powers, fcn, p1 = rv_crtp$control_crtp,
                               p2 = rv_crtp$prop_crt, 
                               sig.level = input$siglevel_crtp,
                               alternative = input$alt_crtp)
    trialsample_crt <- (trialsample_prop) * de_crtp()
    plot(trialsample_crt,powers,
         xlim=c(ceiling(trialsample_crt[1]),ceiling(trialsample_crt[19])),
         xlab="Sample size",ylab="Expected power (%)",ylim=c(0,1),type="b",
         col="darkorange",lwd=5,axes=FALSE, 
         main = "Plot of Power against Sample Size")
    lines(trialsample_prop, powers, col = "turquoise", lwd = 5, type = "b")
    axis(1,at=trialsample_crt)
    axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
         labels=c(0,10,20,30,40,50,60,70,80,90,100))
    legend(x="right",lwd=5,bty="n",legend=c("cluster sample size",
                                            "t-test sample size"),
           col=c("darkorange","turquoise"))
  })
  
################# STEPPED-WEDGE CRT #################    
##### Means #############  
  observeEvent(input$icc_sw_txt,{
    if(as.numeric(input$icc_sw_txt) != input$icc_sw_slider)
    {
      updateSliderInput(
        session = session,
        inputId = 'icc_sw_slider',
        value = input$icc_sw_txt
      ) # updateSliderInput
    }#if
  })
  
  observeEvent(input$icc_sw_slider,{
    if(as.numeric(input$icc_sw_txt) != input$icc_sw_slider)
    {
      updateNumericInput(
        session = session,
        inputId = 'icc_sw_txt',
        value = input$icc_sw_slider
      ) # updateTextInput
    }#if
  })
  
  de <- reactive({
    ((input$clus_sw - 1)* (input$icc_sw_txt) ) + 1
  })
  
  de_sw_mean <- reactive({
    
    DE.woert(outcome = "cont",input=list(delta = input$delta_sw,sd = input$sd_sw),
             K=input$clus_sw,J=input$num_steps,B=input$num_base,T=input$num_t,
             rho=input$icc_sw_txt,sig.level=input$siglevel_sw,power=input$power_sw)
  })
  
  trial_mean_sw <- reactive({
    power.t.test(n = NULL, delta = input$delta_sw,sd = input$sd_sw,
                 type = input$type_sw, sig.level = input$siglevel_sw, 
                 power = input$power_sw,alternative = input$alt_sw)
  })
  
  output$samplesize_sw <- renderText({ 
    
    ceiling((de_sw_mean()$DE.woert * trial_mean_sw()$n))
  })
  output$de_swm <- renderText({ 
    de_sw_mean()$DE.woert
  })
  output$num_clus_sw <- renderText({ 
    de_sw_mean()$n.cls.swt
  })
  output$powerplot_mean_sw <- renderPlot({ 
    powers <- seq(from = 0.05, to = 0.95, by = 0.05)
    fcn <- function(x, ...) { ceiling(power.t.test(power = x, ...)$n) }
    trialsample_mean <- sapply(powers, fcn, delta = input$delta_sw, 
                               sd = input$sd_sw, sig.level = input$siglevel_sw, 
                               type = input$type_sw, alternative = input$alt_sw)
    trialsample_sw <- ((trialsample_mean) * (de_sw_mean()$DE.woert))
    plot(trialsample_sw,powers,
         xlim=c(ceiling(trialsample_sw[1]),ceiling(trialsample_sw[19])),
         xlab="Sample Size",ylab="Expected Power (%)",ylim=c(0,1),type="b",
         col="turquoise",lwd=5,axes=FALSE, 
         main = "Plot of Power against Sample Size")
    lines(((trialsample_mean) * de()), powers, col = "darkorange", lwd = 5, 
          type = "b")
    axis(1,at=trialsample_sw)
    axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
         labels=c(0,10,20,30,40,50,60,70,80,90,100))
    legend(x="right",lwd=5,bty="n",legend=c("SW cluster sample size",
                                            "parallel cluster sample size"),
           col=c("turquoise","darkorange"))
  })
  output$iccplot_mean_sw <- renderPlot({ 
    icc_list <- seq(from = 0, to = 0.5, by = 0.05)
    de_sw_list <- (DE.woert(outcome = "cont",input=list(delta = input$delta_sw,sd = input$sd_sw),
                            K=input$clus_sw,J=input$num_steps,B=input$num_base,T=input$num_t,
                            rho=icc_list,sig.level=input$siglevel_sw,power=input$power_sw)$DE.woert)
    de_list <- (1 + ((input$clus_sw - 1)*icc_list))
    trialsamp_mean <- power.t.test(n = NULL, 
                                   delta = input$delta_sw, 
                                   sd = input$sd_sw, 
                                   sig.level = input$siglevel_sw, power = input$power, 
                                   type = input$type_sw, 
                                   alternative = input$alt_sw)$n
    plot(icc_list,(de_sw_list) * (trialsamp_mean),xlim=c(0,0.5),
         xlab="Intracluster Correlation Coefficient (ICC)",ylab="Sample Size",ylim=c(0,600),type="b",
         col="turquoise",lwd=5,axes=FALSE, 
         main = "Plot of Sample Size against ICC")
    lines(icc_list,(de_list) * (trialsamp_mean), col = "darkorange", lwd = 5, 
          type = "b")
    axis(1,at=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5))
    axis(2,at=c(0,50,100,150,200,250,300,350,400,450,500,550,600))
    legend(x="topleft",lwd=5,bty="n",legend=c("SW cluster sample size",
                                            "parallel cluster sample size"),
           col=c("turquoise","darkorange"))
  })
    
####### Proportions #########
  observeEvent(input$icc_swp_txt,{
    if(as.numeric(input$icc_swp_txt) != input$icc_swp_slider)
    {
      updateSliderInput(
        session = session,
        inputId = 'icc_swp_slider',
        value = input$icc_swp_txt
      ) # updateSliderInput
    }#if
  })
  
  observeEvent(input$icc_swp_slider,{
    if(as.numeric(input$icc_swp_txt) != input$icc_swp_slider)
    {
      updateNumericInput(
        session = session,
        inputId = 'icc_swp_txt',
        value = input$icc_swp_slider
      ) # updateTextInput
    }#if
  })
  
  observeEvent(input$control_swp_txt,{
    if(as.numeric(input$control_swp_txt) != input$control_swp_slider)
    {
      updateSliderInput(
        session = session,
        inputId = 'control_swp_slider',
        value = input$control_swp_txt
      ) # updateSliderInput
    }#if
  })
  
  observeEvent(input$control_swp_slider,{
    if(as.numeric(input$control_swp_txt) != input$control_swp_slider)
    {
      updateNumericInput(
        session = session,
        inputId = 'control_swp_txt',
        value = input$control_swp_slider
      ) # updateTextInput
    }#if
  })
  
  output$ui_prop_sw <- renderUI({
    if (is.null(input$prop_type_sw))
      return()
    
    switch(input$prop_type_sw,
           "risk difference" = sliderInput(inputId = "intervention_swp_txt",
                                            label = "Enter probability in intervention",
                                            value = 0.25, min = 0, max = 1, 
                                           step = 0.005),
           "odds ratio" = numericInput(inputId = "or_swp",
                                       label = "Enter odds ratio",
                                       value = 1.5, min = 0),
           "risk ratio" = sliderInput(inputId = "rr_swp",
                                      label = "Choose risk ratio",
                                      value = 0.775, min = 0, max = 1, 
                                      step = 0.005),
           "relative risk reduction" = sliderInput(inputId = "rrr_swp",
                                                   label = "Choose relative risk reduction",
                                                   value = 0.775, min = 0, 
                                                   max = 1, step = 0.005)
    )
    
  })
  
  rv_swp <- reactiveValues(prop_swp = 0.25,or_sw = 1.5, control_swp = 0.75)
  
  observe({
    updateSliderInput(session, "intervention_swp_txt",value=lapply(reactiveValuesToList(input), unclass)$intervention_swp_txt, min = 0 )
    updateNumericInput(session, "or_swp", value=lapply(reactiveValuesToList(input), unclass)$or_swp )
    updateSliderInput(session, "rr_swp", value=lapply(reactiveValuesToList(input), unclass)$rr, min = 0)
    updateSliderInput(session, "rrr_swp", value=lapply(reactiveValuesToList(input), unclass)$rrr_swp, min = 0)
  })
  
  observeEvent(input$control_swp_txt, {
    rv_swp$control_swp <- input$control_swp_txt
  })
  
  observeEvent(input$intervention_swp_txt, {
    rv_swp$prop_swp <- input$intervention_swp_txt
  })
  observeEvent(input$intervention_swp_txt, {
    rv_swp$or_sw <- (input$intervention_swp_txt/(1- input$intervention_swp_txt))/(rv_swp$control_swp/(1- rv_swp$control_swp))
  })
  observeEvent(input$or_swp, { 
      rv_swp$prop_swp <- ((input$or_swp*rv_swp$control_swp)/(1 - rv_swp$control_swp))/(1 + ((input$or_swp*rv_swp$control_swp)/(1 - rv_swp$control_swp)))
  })
  observeEvent(input$or_swp, { 
    rv_swp$or_sw <- input$or_swp
  })
  observeEvent(input$rr_swp, { 
    rv_swp$prop_swp <- input$rr_swp*rv_swp$control_swp
  })
  observeEvent(input$rr_swp, { 
    rv_swp$or_sw <- ((input$rr_swp*rv_swp$control_swp)/(1 - (input$rr_swp*rv_swp$control_swp)))/((rv_swp$control_swp)/(1 - (rv_swp$control_swp)))
  })
  observeEvent(input$rrr_swp, { 
    rv_swp$prop_swp <- (1 - input$rrr_swp)*rv_swp$control_swp
  })
  observeEvent(input$rrr_swp, { 
    rv_swp$or_sw <- (((1 - input$rrr_swp)*rv_swp$control_swp)/(1 - ((1 - input$rrr_swp)*rv_swp$control_swp)))/((rv_swp$control_swp)/(1 - (rv_swp$control_swp)))
  })
  
  de_cr <- reactive({
    ((input$clus_swp - 1)* (input$icc_swp_txt) ) + 1
  })
  
  de_sw <- reactive({
    DE.woert(outcome = "bin",input=list(p1 = rv_swp$control_swp,OR = rv_swp$or_sw),
             K=input$clus_swp,J=input$num_steps,B=input$num_base,T=input$num_t,
             rho=input$icc_swp_txt,sig.level=input$siglevel_swp,power=input$power_swp)
  })
  
  trial_prop_sw <- reactive({
    power.prop.test(n = NULL, p1 = rv_swp$control_swp,p2 = rv_swp$prop_swp, 
                    sig.level = input$siglevel_swp,power = input$power_swp,
                    alternative = input$alt_swp)
  })
  
  output$samplesize_swp <- renderText({ 
    
    ceiling((de_sw()$DE.woert * trial_prop_sw()$n))
  })
  output$type_swp <- renderText({ 
    rv_swp$prop_swp
  })
  
  output$or_swp <- renderText({
    rv_swp$or_sw
  })
  
  output$de_swp <- renderText({ 
    de_sw()$DE.woert
  })
  output$num_clus_swp <- renderText({ 
    de_sw()$n.cls.swt
  })
  output$powerplot_prop_sw <- renderPlot({ 
    powers <- seq(from = 0.05, to = 0.95, by = 0.05)
    fcn <- function(x, ...) { ceiling(power.prop.test(power = x, ...)$n) }
    trialsample_mean <- sapply(powers, fcn, p1 = rv_swp$control_swp,
                               p2 = rv_swp$prop_swp, 
                               sig.level = input$siglevel_swp,
                               alternative = input$alt_swp)
    trialsample_sw <- (trialsample_mean) * (de_sw()$DE.woert)
    plot(trialsample_sw,powers,
         xlim=c(ceiling(trialsample_sw[1]),ceiling(trialsample_sw[19])),
         xlab="Sample Size",ylab="Expected Power (%)",ylim=c(0,1),type="b",
         col="turquoise",lwd=5,axes=FALSE, 
         main = "Plot of Power against Sample Size")
    lines(((trialsample_mean) * de_cr()), powers, col = "darkorange", lwd = 5, 
          type = "b")
    axis(1,at=trialsample_sw)
    axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
         labels=c(0,10,20,30,40,50,60,70,80,90,100))
    legend(x="right",lwd=5,bty="n",legend=c("SW cluster sample size",
                                            "parallel cluster sample size"),
           col=c("turquoise","darkorange"))
  })
  output$iccplot_prop_sw <- renderPlot({ 
    icc_list <- seq(from = 0, to = 0.5, by = 0.05)
    de_sw_list <- (DE.woert(outcome = "bin",input=list(p1 = rv_swp$control_swp,OR = rv_swp$or_sw),
                            K=input$clus_swp,J=input$num_steps,B=input$num_base,T=input$num_t,
                            rho=icc_list,sig.level=input$siglevel_swp,power=input$power_swp)$DE.woert)
    de_list <- (1 + ((input$clus_swp - 1)*icc_list))
    trialsamp_mean <- power.prop.test(n = NULL, 
                                      p1 = rv_swp$control_swp,
                                      p2 = rv_swp$prop_swp, 
                                      sig.level = input$siglevel_swp,
                                      power = input$power_swp,
                                      alternative = input$alt_swp)$n
    plot(icc_list,(de_sw_list) * (trialsamp_mean),xlim=c(0,0.5),
         xlab="Intracluster Correlation Coefficient (ICC)",ylab="Sample Size",ylim=c(0,600),type="b",
         col="turquoise",lwd=5,axes=FALSE, 
         main = "Plot of Sample Size against ICC")
    lines(icc_list,(de_list) * (trialsamp_mean), col = "darkorange", lwd = 5, 
          type = "b")
    axis(1,at=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5))
    axis(2,at=c(0,50,100,150,200,250,300,350,400,450,500,550,600))
    legend(x="topleft",lwd=5,bty="n",legend=c("SW cluster sample size",
                                              "parallel cluster sample size"),
           col=c("turquoise","darkorange"))
  })  
  
    
  
################# COMPARISON OF PROPORTIONS CODE ###################
          
  output$ui_prop <- renderUI({
    if (is.null(input$prop_type))
      return()
    
    switch(input$prop_type,
           "risk difference" = sliderInput(inputId = "intervention",
                                           label = "Enter probability in intervention",
                                           value = 0.255, min = 0, max = 1,
                                           step = 0.005),
           "odds ratio" = numericInput(inputId = "or",
                                       label = "Enter odds ratio",
                                       value = 1.5, min = 0),
           "risk ratio" = sliderInput(inputId = "rr",
                                      label = "Choose risk ratio",
                                      value = 0.775, min = 0, max = 1, 
                                      step = 0.005),
           "relative risk reduction" =  sliderInput(inputId = "rrr",
                                                    label = "Choose relative risk reduction",
                                                    value = 0.775, min = 0, 
                                                    max = 1, step = 0.005)
    )
    
  })

  observeEvent(input$control_txt,{
    if(as.numeric(input$control_txt) != input$control_slider)
    {
      updateSliderInput(
        session = session,
        inputId = 'control_slider',
        value = input$control_txt
      ) # updateSliderInput
    }#if
  })
  
  observeEvent(input$control_slider,{
    if(as.numeric(input$control_txt) != input$control_slider)
    {
      updateNumericInput(
        session = session,
        inputId = 'control_txt',
        value = input$control_slider
      ) # updateTextInput
    }#if
  })
  
  rv <- reactiveValues(prop = 0.25,control = 0.75)
  
  observe({
    updateSliderInput(session, "intervention",value=lapply(reactiveValuesToList(input), unclass)$intervention, min = 0 )
    updateNumericInput(session, "or", value=lapply(reactiveValuesToList(input), unclass)$or )
    updateSliderInput(session, "rr", value=lapply(reactiveValuesToList(input), unclass)$rr, min = 0)
    updateSliderInput(session, "rrr", value=lapply(reactiveValuesToList(input), unclass)$rrr, min = 0)
  })
  
  observeEvent(input$control_txt, {
    rv$control <- input$control_txt
  })

  observeEvent(input$intervention, {
    rv$prop <- input$intervention
  })
  observeEvent(input$or, { 
    observe({
      rv$prop <- ((input$or*rv$control)/(1 - rv$control))/(1 + ((input$or*rv$control)/(1 - rv$control)))
    })
  })
  observeEvent(input$rr, { 
    observe({
      rv$prop <- input$rr*rv$control
    })
  })
  observeEvent(input$rrr, { 
    observe({
      rv$prop <- (1 - input$rrr)*rv$control
    }) 
  })
  
  output$type_prop <- renderText({
    rv$prop
  })
  
  trial_prop <- reactive({
    power.prop.test(n = NULL, p1 = rv$control,p2 = rv$prop, 
                    sig.level = input$siglevel_prop,power = input$power_prop,
                    alternative = input$alt_prop)
    })
  
  output$samplesize_prop <- renderText({
    ceiling(trial_prop()$n)
  })
  output$powerplot_prop <- renderPlot({
    powers <- seq(from = 0.05, to = 0.95, by = 0.05)
    fcn <- function(x, ...) { ceiling(power.prop.test(power = x, ...)$n) }
    trialmean_prop <- sapply(powers,fcn, p1 = rv$control, 
                             p2 = rv$prop, 
                             sig.level = input$siglevel,
                             alternative = input$alt)
    plot(trialmean_prop,powers,xlim=c(trialmean_prop[1],trialmean_prop[19]),
         xlab="Sample size",
         ylab="Expected power (%)",ylim=c(0,1),type="b",col="darkorange",
         lwd=5,axes=FALSE, 
         main = "Plot of Power against Sample Size")
    axis(1,at=trialmean_prop)
    axis(2,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
         labels=c(0,10,20,30,40,50,60,70,80,90,100))
    })
  
  
  
  
  
  
  
###############SURVIVAL ANALYSIS CODE###############  
  
  
   output$ui_surv <- renderUI({
     if (is.null(input$surv_type))
       return()
     
     switch(input$surv_type,
            "control hazard rate" = numericInput(inputId = "lambdaC",
                                                 label = "Enter event hazard rate for control group",
                                                 value = 0.05, min = 0),
            "median survival" = numericInput(inputId = "surv",
                                             label = "Enter median survival time for the control group",
                                             value = 6, min = 0)
            )
     })
   
   lambda <- reactiveVal(0.05)
   
   
   observe({
     updateNumericInput(session, "lambdaC",value=lapply(reactiveValuesToList(input), unclass)$lambdaC )
     updateNumericInput(session, "surv", value=lapply(reactiveValuesToList(input), unclass)$surv ) 
     })
   
   observeEvent(input$lambdaC, {
     lambda(input$lambdaC)
     })
   observeEvent(input$surv, { 
     lambda((-log(0.5))/input$surv)
     })
   
   output$lambdaC_surv <- renderText({
     lambda()
     })
   
   trial_surv <- reactive({
     nSurv(alpha = input$alpha, sided = as.numeric(input$sided),
           beta=(1-as.numeric(input$power_surv)),lambdaC = lambda(),
           hr = input$hr, R= input$R,T=input$T,minfup = input$minfup)
     })
   
   output$samplesize_surv <- renderText({
     
     ceiling((trial_surv()$n)*(0.5))
   })
   
   output$tot_sample_surv <- renderText({

     ceiling(trial_surv()$n)
   })
   
   output$events_surv <- renderText({
     
     ceiling(trial_surv()$d)
   })
   
 })

#shinyApp(ui = ui, server = server) 