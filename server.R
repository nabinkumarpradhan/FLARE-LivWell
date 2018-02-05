

library(shiny)
library(ggplot2)
library(plm)
library(ggthemes)

options(scipen=999)

#Demography variables
########################################################################################################
livwell_demo$MHAGE<-factor(livwell_demo$MHAGE,
                           levels = c("< 25yrs",
                                      "26 to 50yrs","51 to 75yrs","> 75yrs"),
                           ordered=TRUE)
livwell_demo$DRATIO<-factor(livwell_demo$DRATIO,
                            levels = c(" < 25%",
                                       "26% to 50%","> 50%"),
                            ordered=TRUE)
livwell_demo$GRATIO<-factor(livwell_demo$GRATIO,
                            levels = c(" < 25%",
                                       "26% to 50%","> 50%"),
                            ordered=TRUE)

livwell_demo$MMIGRN<-factor(livwell_demo$MMIGRN,
                            levels = c("Yes",
                                       "No"),
                            ordered=TRUE)

livwell_demo$MHOCC<-factor(livwell_demo$MHOCC,
                           levels = c("Agriculture and Livestock",
                                      "Laborer and Others","Professional","Students"),
                           ordered=TRUE)


#Income variables
########################################################################################################



#Education variables
########################################################################################################

livwell_education$EDNR<-factor(livwell_education$EDNR,
                               levels = c(" < 25%",
                                          "26% to 50%",
                                          "> 50%"),
                               ordered=TRUE)



#Health variables
########################################################################################################

livwell_health$HWMISS<-factor(livwell_health$HWMISS,
                              levels = c("None",
                                         "< 1month",
                                         "1 to 2 months",
                                         "2 to 3 months","> 3months"),
                              ordered=TRUE)
livwell_health$HFSAT<-factor(livwell_health$HFSAT,
                             levels = c("All satisfied",
                                        "< 1 month",
                                        "2 to 3 months",
                                        "> 3 months"),
                             ordered=TRUE)


#Asset variables
########################################################################################################

livwell_asset$AELEC<-factor(livwell_asset$AELEC,
                            levels = c("Yes",
                                       "No"),ordered=TRUE)

livwell_asset$CTLET<-factor(livwell_asset$CTLET,
                            levels = c("No Toilet",
                                       "Pit latrine","Flush toilet","Other"),ordered=TRUE)

livwell_asset$WAT<-factor(livwell_asset$WAT,
                          levels = c("Ground",
                                     "Surface","Piped","Vendor","Other"),ordered=TRUE)

#Wellbeing variables
########################################################################################################

livwell_wellbeing$WHPY_Code<-factor(livwell_wellbeing$WHPY_Code,
                                    levels = c("Very happy",
                                               "Somewhat happy",
                                               "Neither happy or unhappy",
                                               "Somewhat unhappy","Very unhappy","Not sure",
                                               "Data not available"),
                                    ordered=TRUE)

## Demography
demopredname = c(
  "%HH headed by sex",
  "%HH headed by age group",
  "%HH dependent ratio",
  "%HH gender ratio (Female : Male)",
  "%HH migrated",
  "%HH by number of income sources")
varvaldemo <- data.frame(mapdemo = c(3, 4:8), demopredname)

## Income
incomepredname = c(
  "%HH reported income from sales of crop",
  "%HH reported income from sales of livestock",
  "%HH reported income from On-farm wage",
  "%HH reported income from Off-farm wage",
  "%HH reported income from Off-farm business",
  "%HH reported income from cash remittances",
  "%HH reported income from rent")
varvalincome <- data.frame(mapincome = c(3, 4:9), incomepredname)


## Well being
wellbeingpredname = c("HH% Reported Happiness")

varvalwellbeing <- data.frame(mapwellbeing = c(3), wellbeingpredname)


## Education
edupredname = c("%HH by child school attendance",
                "%HH members with 5+yrs educational achievement")
varvaledu <- data.frame(mapedu = c(3,4), edupredname)


## Health
healthpredname = c("%HH lost work days owing to illness(last 12 months)",
                   "%HH lost life (last 12 months)",
                   "%HH not been able to satisfy food need(last 12 months)")
varvalhealth <- data.frame(maphealth = c(3,4:5), healthpredname)


## Asset
assetpredname = c("HH% with electricity ",
                  "HH% with toilet facility",
                  "HH% by main water sources")
varvalasset <- data.frame(mapasset = c(3,4:5), assetpredname)



## BIVARIATE
bipredname = c("Annual income ",
               "Annual expenditure")
varvalbi <- data.frame(mapbi = c(3,4), bipredname)



## Crosstab

#Demography variables
########################################################################################################
livwell_crtb$MHAGE<-factor(livwell_crtb$MHAGE,
                           levels = c("< 25yrs",
                                      "26 to 50yrs","51 to 75yrs","> 75yrs"),
                           ordered=TRUE)
livwell_crtb$DRATIO<-factor(livwell_crtb$DRATIO,
                            levels = c(" < 25%",
                                       "26% to 50%","> 50%"),
                            ordered=TRUE)
livwell_crtb$GRATIO<-factor(livwell_crtb$GRATIO,
                            levels = c(" < 25%",
                                       "26% to 50%","> 50%"),
                            ordered=TRUE)

livwell_crtb$MMIGRN<-factor(livwell_crtb$MMIGRN,
                            levels = c("Yes",
                                       "No"),
                            ordered=TRUE)

livwell_crtb$EDNR<-factor(livwell_crtb$EDNR,
                          levels = c(" < 25%",
                                     "26% to 50%",
                                     "> 50%"),
                          ordered=TRUE)

livwell_crtb$HWMISS<-factor(livwell_crtb$HWMISS,
                            levels = c("None",
                                       "< 1month",
                                       "1 to 2 months",
                                       "2 to 3 months","> 3months"),
                            ordered=TRUE)
livwell_crtb$HFSAT<-factor(livwell_crtb$HFSAT,
                           levels = c("All satisfied",
                                      "< 1 month",
                                      "2 to 3 months",
                                      "> 3 months"),
                           ordered=TRUE)


livwell_crtb$WHPY_Code<-factor(livwell_crtb$WHPY_Code,
                               levels = c("Very happy",
                                          "Somewhat happy",
                                          "Neither happy or unhappy",
                                          "Somewhat unhappy","Very unhappy","Not sure",
                                          "Data not available"),
                               ordered=TRUE)



crtbpredname = c(
  "%HH headed by sex",
  "%HH headed by age",
  "%HH dependent ratio",
  "%HH gender ratio",
  "%HH migrated",
  "%HH number of income sources",
  "%HH reported income from sales of crop",
  "%HH reported income from sales of livestock",
  "%HH reported income from On-farm wage",
  "%HH reported income from Off-farm wage",
  "%HH reported income from Off-farm business",
  "%HH reported income from cash remittances",
  "%HH reported income from rent",
  "%HH by child school attendance",
  "%HH members with 5+yrs educational achievement",
  "%HH lost work days owing to illness(last 12 months)",
  "%HH lost life (last 12 months)",
  "%HH not been able to satisfy food need(last 12 months)","HH% with electricity ",
  "HH% with toilet facility",
  "HH% by main water sources"
  
  
)
varvalcrtb <- data.frame(mapcrtb = c(3, 4:8,10:16,17:18,19:21,22:24), crtbpredname)

## Crosstab
crtbpredname1 = c(
  "%HH headed by sex",
  "%HH headed by age",
  "%HH dependent ratio",
  "%HH gender ratio",
  "%HH migrated",
  "%HH number of income sources",
  "%HH reported income from sales of crop",
  "%HH reported income from sales of livestock",
  "%HH reported income from On-farm wage",
  "%HH reported income from Off-farm wage",
  "%HH reported income from Off-farm business",
  "%HH reported income from cash remittances",
  "%HH reported income from rent",
  "%HH by child school attendance",
  "%HH members with 5+yrs educational achievement",
  "%HH lost work days owing to illness(last 12 months)",
  "%HH lost life (last 12 months)",
  "%HH not been able to satisfy food need(last 12 months)",
  "HH% with electricity ",
  "HH% with toilet facility",
  "HH% by main water sources"
  
  
)
varvalcrtb1 <- data.frame(mapcrtb1 = c(3,4:8,10:16,17:18,19:21,22:24), crtbpredname1)



shinyServer(function(input, output) {
  
  output$demo_predictors = renderUI({
    selectizeInput(
      "demo_predictors",
      label = "VARIABLES",
      choices = demopredname,
      selected = demopredname[1:4],
      multiple = TRUE,
      options = list(maxItems = 4)
    )
  })
  output$income_predictors = renderUI({
    selectizeInput(
      "income_predictors",
      label = "VARIABLES",
      choices = incomepredname,
      selected = incomepredname[1:4],
      multiple = TRUE,
      options = list(maxItems = 4)
    )
  })
  
  output$wellbeing_predictors = renderUI({
    selectizeInput(
      "wellbeing_predictors",
      label = "VARIABLES",
      choices = wellbeingpredname,
      selected = wellbeingpredname[1],
      multiple = TRUE,
      options = list(maxItems = 1)
    )
  }) 
  output$edu_predictors = renderUI({
    selectizeInput(
      "edu_predictors",
      label = "VARIABLES",
      choices = edupredname,
      selected = edupredname[1:2],
      multiple = TRUE,
      options = list(maxItems = 2)
    )
  })
  
  output$health_predictors = renderUI({
    selectizeInput(
      "health_predictors",
      label = "VARIABLES",
      choices = healthpredname,
      selected = healthpredname[1:2],
      multiple = TRUE,
      options = list(maxItems = 2)
    )
  })
  
  output$asset_predictors = renderUI({
    selectizeInput(
      "asset_predictors",
      label = "VARIABLES",
      choices = assetpredname,
      selected = assetpredname[1:3],
      multiple = TRUE,
      options = list(maxItems = 4)
    )
  })
  
  
  
  
  output$crtb_predictors = renderUI({
    selectizeInput(
      "crtb_predictors",
      label = "CROSS TABULATION",
      choices = list(
        "DEMOGRAPHIC" = c(
          crtbpredname[1:6]),
        "INCOME" = c(crtbpredname[7:13]),
        "EDUCATION" = c(crtbpredname[14:15]),
        "HEALTH" = c(crtbpredname[16:18]),
        "ASSETS" = c(
          crtbpredname[19:21]),
        "WELL-BEING" = c(crtbpredname[22:22])
        
      ),
      selected = crtbpredname[2],
      multiple = TRUE,
      options = list(maxItems = 1)
    )
  })
  
  
  output$crtb_predictors1 = renderUI({
    selectizeInput(
      "crtb_predictors1",
      label = "",
      choices = list(
        "DEMOGRAPHIC" = c(
          crtbpredname1[1:6]),
        "INCOME" = c(crtbpredname1[7:13]),
        "EDUCATION" = c(crtbpredname1[14:15]),
        "HEALTH" = c(
          crtbpredname1[16:18]),
        "ASSETS" = c(crtbpredname1[19:21]),
        "WELL-BEING" = c(crtbpredname1[22])
      ),
      selected = crtbpredname1[1],
      multiple = TRUE,
      options = list(maxItems = 1)
    )
  })
  
  
  
  output$ttest_predictors = renderUI({
    selectizeInput(
      "ttest_predictors",
      label = "VARIABLES",
      choices = bipredname,
      selected = bipredname[1:2],
      multiple = TRUE,
      options = list(maxItems = 2)
    )
  })
  
  output$ttest_predictors1 = renderUI({
    selectizeInput(
      "ttest_predictors1",
      label = "VARIABLES",
      choices = bipredname1,
      selected = bipredname1[1],
      multiple = TRUE,
      options = list(maxItems = 1)
    )
  })
  
  
  
  ############################################################################################
  #***************************UNIVARIATE*************************************
  #*******************************************************************************
  
  #Filter Demo
  filtered_uni <- reactive({
    
    
    subset(livwell_demo,(if(input$country == 
                            "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                            {CID %in%  c(input$country)}))
  })
  
  outlist <- NULL
  output$plotdemo <- renderPlot({
    for (i in seq(length(input$demo_predictors)))
    {
      t <-
        filtered_uni()[, varvaldemo$mapdemo[which(demopredname == input$demo_predictors[i])]]
      p <-
        eval(substitute(
          
          ggplot(filtered_uni(), aes(x = t,group=Year)) +
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")  +
            geom_text(aes(label = scales::percent(..prop..),
                          y= ..prop..), stat= "count", vjust = 1)+
            facet_grid(~Year)+theme(axis.text.x=element_blank())+theme(legend.position = "none")+
            labs(
              title = input$demo_predictors[i],
              y = "",
              x = ""
            )+
            theme(axis.text.x = element_text(face="bold",
                                             angle = 0, hjust = .5,vjust = .5))+
            scale_y_continuous(labels = scales::percent)+
            theme(plot.title = element_text(lineheight=2, face="bold"))+
            theme(plot.background=element_rect(fill = "transparent",colour = NA))+
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            theme(strip.background = element_rect(colour = "black", fill = "light green"))+
            theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
        ))
      
      print(input$demo_predictors[i])
      
      outlist[[i]] <- p
      
    }
    
    n <- length(outlist)
    
    nCol <- 2
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
    
  })
  
  
  ############################################# 
  
  #Filter income
  filtered_income <- reactive({
    
    
    subset(livwell_income,(if(input$country1 == 
                              "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                              {CID %in%  c(input$country1)}))
  })
  outlist <- NULL
  output$plotincome <- renderPlot({
    
    for (i in seq(length(input$income_predictors)))
    {
      t1 <-
        filtered_income()[, varvalincome$mapincome[which(incomepredname == input$income_predictors[i])]]
      p1 <-
        eval(substitute(
          ggplot(filtered_income(), aes(x = t1,group=Year)) +
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")  +
            geom_text(aes(label = scales::percent(..prop..),
                          y= ..prop..), stat= "count", vjust = 1)+
            facet_grid(~Year)+theme(axis.text.x=element_blank())+theme(legend.position = "none")+
            labs(
              title = input$income_predictors[i],
              y = "",
              x = ""
            )+
            theme(axis.text.x = element_text(face="bold",
                                             angle = 0, hjust = .5,vjust = .5))+
            scale_y_continuous(labels = scales::percent)+
            theme(plot.title = element_text(lineheight=2, face="bold"))+
            theme(plot.background=element_rect(fill = "transparent",colour = NA))+
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            theme(strip.background = element_rect(colour = "black", fill = "light green"))+
            theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
          
        ))
      
      print(input$income_predictors[i])
      
      outlist[[i]] <- p1
      
    }
    
    n <- length(outlist)
    
    nCol <- 2
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
  })
  
  
  
  ################################################################################################  
  #Filter wellbeing
  
  filtered_wellbeing <- reactive({
    
    
    subset(livwell_wellbeing,(if(input$country2 == 
                                 "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                                 {CID %in%  c(input$country2)}))
  })
  outlist <- NULL
  output$plotwellbeing <- renderPlot({
    
    for (i in seq(length(input$wellbeing_predictors)))
    {
      t2 <-
        filtered_wellbeing()[, varvalwellbeing$mapwellbeing[which(wellbeingpredname == input$wellbeing_predictors[i])]]
      p2 <-
        eval(substitute(
          
          ggplot(filtered_wellbeing(), aes(x = t2,group=Year)) +
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")  +
            geom_text(aes(label = scales::percent(..prop..),
                          y= ..prop..), stat= "count", vjust = 1)+
            facet_grid(~Year)+theme(axis.text.x=element_blank())+theme(legend.position = "none")+
            labs(
              title = input$wellbeing_predictors[i],
              y = "",
              x = ""
            )+
            theme(axis.text.x = element_text(face="bold",
                                             angle = 90, hjust = .5,vjust = .5))+
            scale_y_continuous(labels = scales::percent)+
            theme(plot.title = element_text(lineheight=2, face="bold"))+
            theme(plot.background=element_rect(fill = "transparent",colour = NA))+
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            theme(strip.background = element_rect(colour = "black", fill = "light green"))+
            theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
          
        ))
      
      print(input$wellbeing_predictors[i])
      
      outlist[[i]] <- p2
      
    }
    
    n <- length(outlist)
    
    nCol <- 1
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
  })
  
  
  
  ################################################################################################  
  #Filter education
  
  filtered_edu <- reactive({
    
    
    subset(livwell_education,(if(input$country3 == 
                                 "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                                 {CID %in%  c(input$country3)}))
  })
  outlist <- NULL
  output$plotedu <- renderPlot({
    
    for (i in seq(length(input$edu_predictors)))
    {
      t3 <-
        filtered_edu()[, varvaledu$mapedu[which(edupredname == input$edu_predictors[i])]]
      p3 <-
        eval(substitute(
          
          ggplot(filtered_edu(), aes(x = t3,group=Year)) +
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")  +
            geom_text(aes(label = scales::percent(..prop..),
                          y= ..prop..), stat= "count", vjust = 1)+
            facet_grid(~Year)+theme(axis.text.x=element_blank())+theme(legend.position = "none")+
            labs(
              title = input$edu_predictors[i],
              y = "",
              x = ""
            )+
            theme(axis.text.x = element_text(face="bold",
                                             angle = 0, hjust = .5,vjust = .5))+
            scale_y_continuous(labels = scales::percent)+
            theme(plot.title = element_text(lineheight=2, face="bold"))+
            theme(plot.background=element_rect(fill = "transparent",colour = NA))+
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            theme(strip.background = element_rect(colour = "black", fill = "light green"))+
            theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
          
        ))
      
      print(input$edu_predictors[i])
      
      outlist[[i]] <- p3
      
    }
    
    n <- length(outlist)
    
    nCol <- 2
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
  })
  
  
  ################################################################################################  
  #Filter Health
  
  filtered_health <- reactive({
    
    
    subset(livwell_health,(if(input$country4 == 
                              "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                              {CID %in%  c(input$country4)}))
  })
  outlist <- NULL
  output$plothealth <- renderPlot({
    
    for (i in seq(length(input$health_predictors)))
    {
      t4 <-
        filtered_health()[, varvalhealth$maphealth[which(healthpredname == input$health_predictors[i])]]
      p4 <-
        eval(substitute(
          
          ggplot(filtered_health(), aes(x = t4,group=Year)) +
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")  +
            geom_text(aes(label = scales::percent(..prop..),
                          y= ..prop..), stat= "count", vjust = 1)+
            facet_grid(~Year)+theme(axis.text.x=element_blank())+theme(legend.position = "none")+
            labs(
              title = input$health_predictors[i],
              y = "",
              x = ""
            )+
            theme(axis.text.x = element_text(face="bold",
                                             angle = 90, hjust = .5,vjust = .5))+
            scale_y_continuous(labels = scales::percent)+
            theme(plot.title = element_text(lineheight=2, face="bold"))+
            theme(plot.background=element_rect(fill = "transparent",colour = NA))+
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            theme(strip.background = element_rect(colour = "black", fill = "light green"))+
            theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
          
        ))
      
      print(input$health_predictors[i])
      
      outlist[[i]] <- p4
      
    }
    
    n <- length(outlist)
    
    nCol <- 2
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
  })
  
  ################################################################################################  
  #Filter Asset
  
  filtered_asset <- reactive({
    
    
    subset(livwell_asset,(if(input$country5 == 
                             "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                             {CID %in%  c(input$country5)}))
  })
  outlist <- NULL
  output$plotasset <- renderPlot({
    
    for (i in seq(length(input$asset_predictors)))
    {
      t5 <-
        filtered_asset()[, varvalasset$mapasset[which(assetpredname == input$asset_predictors[i])]]
      
      #print(t5)
      
      p5 <-
        eval(substitute(
          ggplot(filtered_asset(), aes(x = t5,group=Year)) +
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count")  +
            geom_text(aes(label = scales::percent(..prop..),
                          y= ..prop..), stat= "count", vjust = 1)+
            facet_grid(~Year)+theme(axis.text.x=element_blank())+theme(legend.position = "none")+
            labs(
              title = input$asset_predictors[i],
              y = "",
              x = ""
            )+
            theme(axis.text.x = element_text(face="bold",
                                             angle = 0, hjust = .5,vjust = .5))+
            scale_y_continuous(labels = scales::percent)+
            theme(plot.title = element_text(lineheight=2, face="bold"))+
            theme(plot.background=element_rect(fill = "transparent",colour = NA))+
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            theme(strip.background = element_rect(colour = "black", fill = "light green"))+
            theme(strip.text.x = element_text(colour = "black", face = "bold",size = 10))
          
        ))
      
      print(input$asset_predictors[i])
      
      outlist[[i]] <- p5
      
    }
    
    n <- length(outlist)
    
    nCol <- 2
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
  })
  
  
  #***************************UNIVARIATE END*************************************
  
  #***************************CROSS TABLE*************************************
  
  filtered_crb <- reactive({
    
    
    subset(livwell_crtb,(if(input$countrycr == 
                            "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                            {CID %in%  c(input$countrycr)}))
  }) 
  
  
  outlist <- NULL
  
  output$plotbi1 <- renderPlot({
    
    gtitol=paste( "Comparision of",input$crtb_predictors,"&",input$crtb_predictors1)
    
    
    
    for (i in seq(length(input$crtb_predictors)))
    {
      t6 <-
        filtered_crb()[, varvalcrtb$mapcrtb[which(crtbpredname == input$crtb_predictors[i])]]
      
      t7 <-
        filtered_crb()[, varvalcrtb1$mapcrtb1[which(crtbpredname1 == input$crtb_predictors1[i])]]
      
      p6 <-
        eval(substitute(
          
          ggplot(filtered_crb(),aes(x = factor(t6), fill = factor(t7)))+
            
            geom_bar(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])))+
            geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
                          label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)
                                                  [..PANEL..])), stat = "count",position = position_stack(vjust = .5))+
            facet_grid(~Year)+scale_y_continuous(labels=scales::percent)+
            theme_economist()+
            theme(legend.title=element_blank())+
            labs(
              title = gtitol,
              y = "",
              x = input$crtb_predictors[i])+
            theme(legend.position = "right")
          
          
          
          #############################
          # ggplot(filtered_crb(),aes(x = factor(t6), group = factor(t7),
          # y = (..count..)))+
          # geom_bar(aes(y = ..count.., fill = factor(t7))) +
          # stat_count(geom = "text",aes(label =(..count..)),
          # position = position_stack(vjust = .5))+facet_grid(~Year)+
          #   theme_economist()+
          #      theme(legend.title=element_blank())+
          #      labs(
          #        title = gtitol,
          #        y = "",
          #       x = input$crtb_predictors[i])+
          #     theme(legend.position = "right")
          
          #############################
          
          # ggplot(filtered_crb(), aes(t6),y = (..count..)/sum(..count..))+
          #   geom_bar(aes(fill=as.factor(t7)), position="fill",stat="count")+facet_grid(~Year)+
          #   scale_y_continuous(labels=scales::percent)+theme_economist()+
          #    theme(legend.title=element_blank())+
          #    labs(
          #      title = gtitol,
          #      y = "",
          #     x = input$crtb_predictors[i])+
          #   theme(legend.position = "right")
          #   +theme(plot.title = element_text(lineheight=2, face="bold",size = 14))+
          #   theme(axis.text.x = element_text(face="bold",
          #                                    angle = 0, hjust = .5,vjust = .5,size = 12))+
          #   theme(plot.background=element_rect(fill = "transparent",colour = NA))+
          #   theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
          #   theme(strip.background = element_rect(colour = "black", fill = "light green"))+
          #   theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
          
          
          
        ))
      
      print(input$crtb_predictors[i])
      
      outlist[[i]] <- p6
      
    }
    
    n <- length(outlist)
    
    nCol <- 1
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
    
  })
  
  #***************************OUTCOME ANALYSIS*************************************
  
  filtered_bi <- reactive({
    subset(livwell_incomeexp,(if(input$countryttst == 
                                 "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                                 {CID %in%  c(input$countryttst)}))
  })
  
  outlist = NULL
  output$plot_test1 <- renderPlot({
    
    for (i in seq(length(input$ttest_predictors)))
    {
      
      t8 <- filtered_bi()[, varvalbi$mapbi[which(bipredname == input$ttest_predictors[i])]]
      
      pbi <-
        ggplot(filtered_bi(), aes(t8, fill = factor(Year))) +
        geom_density(position = "stack")+
        theme(legend.title=element_blank(),legend.position = "top")+
        labs(
          title = input$ttest_predictors[i],
          y = "",
          x = ""
        )+
        theme(plot.title = element_text(lineheight=2, face="bold",size = 14))+
        theme(axis.text.x = element_text(face="bold",
                                         angle = 0, hjust = .5,vjust = .5,size = 12))+
        theme(plot.background=element_rect(fill = "transparent",colour = NA))+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
        theme(strip.background = element_rect(colour = "black", fill = "light green"))+
        theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))+
        geom_vline(xintercept = median(t8), color = "red",linetype = "dashed",size=1)
      
      print(input$ttest_predictors[i])
      
      outlist[[i]] <- pbi
      
    }
    
    n <- length(outlist)
    
    nCol <- 2
    do.call(gridExtra::grid.arrange, c(outlist, ncol = nCol))
    
  })
  
  ####################################################################
  
  filtered_bi1 <- reactive({
    subset(livwell_incomeexp,(if(input$countryttst == 
                                 "Select country"){CID %in% c("Ethiopia","Tanzania")}else
                                 {CID %in%  c(input$countryttst)}))
  })
  
  filtered_err <- reactive({
    error_plot6 <- Rmisc::summarySE(filtered_bi1(),measurevar ='ECINC',groupvars = 'Year')
    
  })
  
  output$plot_test2 <- renderPlot({
    
    ggplot2::ggplot(filtered_err(), aes(x=as.factor(Year),y=ECINC,fill=factor(Year)))+geom_bar(
      
      stat="identity",position=position_dodge())+geom_text(aes(label=round(ECINC,digits = 2)))+
      geom_errorbar(aes(ymin=ECINC-ci, ymax=ECINC+ci),
                    width=.2,                   
                    position=position_dodge(.9))+theme(legend.position="none")+
      labs(
        title = "Average annual cash income",
        y = "Average annual cash income",
        x = ""
      )+
      theme(plot.title = element_text(lineheight=2, face="bold",size = 14))+
      theme(axis.text.x = element_text(face="bold",
                                       angle = 0, hjust = .5,vjust = .5,size = 12))+
      theme(plot.background=element_rect(fill = "transparent",colour = NA))+
      theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
      theme(strip.background = element_rect(colour = "black", fill = "light green"))+
      theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
  })
  
  #######################################################################################
  
  filtered_err1 <- reactive({
    error_plot7 <- Rmisc::summarySE(filtered_bi1(),measurevar ='EEXP',groupvars = 'Year')
    
  })
  
  
  output$plot_test3 <- renderPlot({
    
    
    ggplot2::ggplot(filtered_err1(), aes(x=as.factor(Year),y=EEXP,fill=factor(Year)))+geom_bar(
      
      stat="identity",position=position_dodge())+geom_text(aes(label=round(EEXP,digits = 2)))+
      geom_errorbar(aes(ymin=EEXP-ci, ymax=EEXP+ci),
                    width=.2,                   
                    position=position_dodge(.9))+theme(legend.position="none")+
      labs(
        title = "Average annual cash expenditure",
        y = "Average annual cash income",
        x = ""
      )+
      theme(plot.title = element_text(lineheight=2, face="bold",size = 14))+
      theme(axis.text.x = element_text(face="bold",
                                       angle = 0, hjust = .5,vjust = .5,size = 12))+
      theme(plot.background=element_rect(fill = "transparent",colour = NA))+
      theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
      theme(strip.background = element_rect(colour = "black", fill = "light green"))+
      theme(strip.text.x = element_text(colour = "black", face = "bold",size = 12))
    
    
  })
  
  #***********************END****************************************************************  
  
  
})
