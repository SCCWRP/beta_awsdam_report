# Arid SouthWest Method Development
# Shiny Application for pdf Report Generation
# Created by: Heili Lowman
# Created on: September 30, 2020
# Last edited: November 11, 2020

# Setup -------------------------------------------------------------------

# Load necessary packages
library(tidyverse)
library(randomForest)
library(shiny)
library(shinythemes)
library(ggforce)
library(imager)
library(here)
library(sf)
library(mapview)

# Load dataset for random forest model
load("FinalRF.Rdata")

# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

# User Interface ----------------------------------------------------------

shinyApp(
  ui = fluidPage( theme = "classic",
    
    # App title
     navbarPage("Beta Streamflow Duration Assessment Method for the Arid West",
       position = c("fixed-top")),
    
    # Adding padding due to pinning title at top
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    
    # Adding images
    titlePanel(tags$img(src="Slide1.jpeg", height="80%", width="80%")),
    
    br(), # line break
      
      # Output: set of 2 tabs
      tabsetPanel(type = "tabs",
        position = c("fixed-top"),

    tabPanel(h4("Background Information"),
      br(),
      column(width = 6,
        p("This is a draft tool to calculate the interim Streamflow Duration Assessment Method (SDAM) developed for the Arid West region. Do not use for regulatory purposes without prior consulting with the EPA product delivery team. Contact Raphael Mazor (raphaelm@sccwrp.org) with questions."))),
      
    tabPanel(h4("Report Generation"),
      br(),
      
    # General Info ------------------------------------------------------------
      
    h3("General Site Information"), # Adds section header
    br(), # line break
    
    textInput("project", label = h5("Project name or number:"), value = "Enter text..."), # text input box
    textInput("code", label = h5("Site code or identifier:"), value = "Enter text..."), # text input box
    textInput("assessor", label = h5("Assessor(s):"), value = "Enter text..."), # text input box
    textInput("waterway", label = h5("Waterway name:"), value = "Enter text..."), # text input box
    textInput("date", label = h5("Visit date:"), value = "Enter text..."), # text input box
    
    radioButtons(inputId = "radio_weather", label = "Current weather conditions (check one).", choices = list("Storm/heavy rain" = 0, "Steady rain" = 1, "Intermittent rain" = 2, "Snowing" = 3, "Cloudy" = 4, "Clear/Sunny" = 5), selected = 5), # radio buttons
    textInput("weather", label = h5("Notes on current or recent weather conditions:"), value = "Enter text..."), # text input box
    
    h4("Coordinates at downstream end:"), # Adds section header
    textInput("lat", label = h5("Lat (N, decimal degrees):"), value = "Enter text..."), # text input box
    textInput("lon", label = h5("Lon (W, decimal degrees):"), value = "Enter text..."), # text input box
    textInput("datum", label = h5("Datum:"), value = "Enter text..."), # text input box
    
    radioButtons("radio_use", label = h5("Surrounding land-use within 100 m (check one)."), choices = list("Urban/industrial/residential" = 0, "Agricultural" = 1, "Developed open-space" = 2, "Forested" = 3, "Other natural" = 4, "Other" = 5), selected = 5), # radio buttons
    textInput("boundary", label = h5("Describe reach boundaries:"), value = "Enter text..."), # text input box
    
    textInput("channel", label = h5("Mean channel width (m)"), value = "Enter text..."), # text input box
    textInput("actreach", label = h5("Reach length (m)"), value = "Enter text..."), # text input box
    
    radioButtons(inputId = "radio_situation", label = "Disturbed or difficult conditions (check all that apply).", choices = list("Recent flood or debris flow" = 0, "Stream modifications (e.g., channelization)" = 1, "Diversions" = 2, "Discharges" = 3, "Drought" = 4, "Vegetation removal/limitations" = 5, "Other (explain in notes)" = 6, "None" = 7), selected = 7), # radion buttons
    textInput("situation", label = h5("Site Disturbance/Difficulties"), value = "Enter text..."), # text input box
    
    h4("Observed hydrology:"), # Adds section header
    textInput("surfflow", label = h5("Percent of Reach with Surface Flows"), value = "Enter text..."), # text input box
    textInput("subflow", label = h5("Percent of Reach with Surface and Sub-Surface Flows"), value = "Enter text..."), # text input box
    textInput("pool", label = h5("Number of Isolated Pools"), value = "Enter text..."), # text input box
    textInput("hydro_comments", label = h5("Comments on observed hydrology:"), value = "Enter text..."), # text input box
    
# Site Photos -------------------------------------------------------------
    
    hr(), # adds divider
    h3("Site Photos:"), # Adds section header
    br(), # line break
    fileInput("tld", label = HTML("Site Photo - Top of Reach Looking Downstream<br />Upload photo file here.")), # file input box
    fileInput("mlu", label = HTML("Site Photo - Middle of Reach Looking Upstream<br />Upload photo file here.")), # file input box
    fileInput("mld", label = HTML("Site Photo - Middle of Reach Looking Downstream<br />Upload photo file here.")), # file input box
    fileInput("blu", label = HTML("Site Photo - Bottom of Reach Looking Upstream<br />Upload photo file here.")), # file input box
    
    h3("Site Sketch:"), # Adds section header
    fileInput("sketch", label = HTML("Site Sketch<br />Upload photo file here.")), # file input box

# Hydrophytes -------------------------------------------------------------

    hr(), # adds divider
    
    h3("Hydrophytic Plants"), # Adds section header
    tags$div(
      "Field form instructions: Record up to 5 hydrophytic plant species (FACW or OBL in the Arid West regional wetland plant list) within the assessment area: within the channel or up to one half-channel width. Explain in notes if species has an odd distribution (e.g., covers less than 2% of assessment area, long-lived species solely represented by seedlings, or long-lived species solely represented by specimens in decline), or if there is uncertainty about the identification. Enter photo ID, or check if photo is taken."), # subtext
    br(), # line break
    
    #textInput("checklist", label = h5("Hydrophytic Vegetation Checklist Used"), value = "Enter text..."), # text input box
    # hydrophyte input for report
    #textInput("hydro", label = h5("Hydrophytes (number of species) - exact number observed"), value = "Enter text..."), # text input box
    # hydrophyte input for determination
    radioButtons(inputId = "radio_hydro", label = "How many hydrophytic species are found in or near the channel? - select one of the below options", choices = list("None" = 0, "1 or 2 species" = 0.5, "3 or more species" = 1), selected = 0), # radio buttons
    br(), # line break
    fileInput("hyd1", label = HTML("Hydrophyte Photo #1<br />Upload photo file here.")), # file input box
    textInput("hyd1_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    fileInput("hyd2", label = HTML("Hydrophyte Photo #2<br />Upload photo file here.")), # file input box
    textInput("hyd2_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    fileInput("hyd3", label = HTML("Hydrophyte Photo #3<br />Upload photo file here.")), # file input box
    textInput("hyd3_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    fileInput("hyd4", label = HTML("Hydrophyte Photo #4<br />Upload photo file here.")), # file input box
    textInput("hyd4_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    textInput("hydnotes", label = h5("Notes on hydrophytic vegetation:"), value = "Enter text..."), # text input box
    
# Invertebrates -----------------------------------------------------------

    hr(), # adds divider
    
    h3("Aquatic Invertebrates"), # Adds section header
    tags$div("Field form instructions: Do not count mosquito larvae."), # subtext
    br(), # line break
    
    # inverts input for report
    # textInput("abundance", label = h5("How many aquatic invertebrates are found? - exact number of individuals observed"), value = "Enter text..."), # text input box
    # inverts input for determination
    radioButtons(inputId = "radio_bmi", label = "How many aquatic invertebrates are quantified in a 15-minute search? (number of individuals observed) - select one of the below options", choices = list("None" = 0, "1 to 19" = 0.5, "20 or more" = 1), selected = 0), # radion buttons
    # EPT input for report
    # textInput("ept", label = h5("Are EPT (Ephemeroptera, Plecoptera, and Trichoptera) present?"), value = "Enter text..."), # text input box
    # EPT input for determination
    radioButtons(inputId = "radio_ept", label = "Is there evidence of aquatic stages of EPT (Ephemeroptera, Plecoptera, and Trichoptera)? - select one of the below options", choices = list("Yes" = 1, "No" = 0), selected = 0), # radio buttons
    #textInput("invtaxa", label = h5("Invertebrate Taxa Observed"), value = "Enter text..."), # text input box
    fileInput("inv1", label = HTML("Invertebrate Photo #1<br />Upload photo file here.")), # file input box
    fileInput("inv2", label = HTML("Invertebrate Photo #2<br />Upload photo file here.")), # file input box
    textInput("invnotes", label = h5("Notes on aquatic invertebrates:"), value = "Enter text..."), # text input box

# Algae -------------------------------------------------------------------

    hr(), # adds divider
    
    h3("Algae Cover"), # Adds section header
    tags$div(
      "Field form instructions: Estimate cover of live or dead algae in the streambed (local growth only)."), # subtext
    br(), # line break
    
    # algae input for report
    # textInput("algae", label = h5("Cover of Live or Dead Algal Mats in Streambed"), value = "Enter text..."), # text input box
    # algae input for determination
    radioButtons(inputId = "radio_algae", label = "Are algae found on the streambed? - select one of the below options", choices = list("Not detected" = 0, "Yes, <10% cover" = 1, "Yes, >10% cover" = 2), selected = 0),
    fileInput("alg1", label = HTML("Algae Photo #1<br />Upload photo file here.")), # file input box
    textInput("algnotes", label = h5("Notes on algae cover:"), value = "Enter text..."), # text input box

# Single Indicators -------------------------------------------------------

    hr(), # adds divider
    
    h3("Single Indicators"), # Adds section header
    tags$div("Field form instructions: Single indicators result in classification of 'At least intermittent', in the absence of other information."), # subtext
    br(), # line break
    
    radioButtons("fish", label = "Fish (other than mosquitofish)", choices = list("Yes" = 1, "No" = 0), selected = 0), # radio button
    fileInput("fish1", label = HTML("Fish Photo #1<br />Upload photo file here.")), # file input box
    
    # radioButtons("alg_si", label = "Algae cover > 10%", choices = list("Yes" = 1, "No" = 0), selected = 0), # radio button
    # fileInput("alg_si1", label = HTML("Algae Photo #1<br />Upload photo file here.")), # file input box
    
    #textInput("fishnotes", label = h5("General Notes about Fish"), value = "Enter text..."), # text input box
    #textInput("amph", label = h5("Aquatic Amphibians Observed"), value = "Enter text..."), # text input box
    #textInput("snake", label = h5("Aquatic Snakes Observed"), value = "Enter text..."), # text input box
    
# Supplemental Info -------------------------------------------------------
    
    hr(), # adds divider
    h3("Supplemental Information"), # Adds section header
    tags$div("Field form instructions: e.g., aquatic life stages of amphibians, snakes, or turtles; iron-oxidizing bacteria and fungi; hydric soils, etc."), # subtext
    br(), # line break
    textInput("other_ind", label = h5("Supplemental information"), value = "Enter text..."), # text input box
    
    fileInput("add1", label = HTML("Additional Photo #1<br />Upload photo file here.")), # file input box
    textInput("add_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    fileInput("add2", label = HTML("Additional Photo #2<br />Upload photo file here.")), # file input box
    textInput("add_cap2", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    textInput("add_notes", label = h5("Additional Notes about the Assessment"), value = "Enter text..."), # text input box
    
    downloadButton("report", "Generate report.")
  ),
        
        # 3rd tab
        
        tabPanel(h4("Additional Resources"),
          br(),
          column(width = 6,
            p("For additional information, please refer to the field manual and protocol developed for the Arid West Streamflow Duration Assessment Method. Field forms and additional materials can be found at INSERT HYPERLINK HERE.")))
        )),
  
# Server ------------------------------------------------------------------

  server = function(input, output) {
    
    # Need to process figures separately.
    # Site photos
    fig1 <- reactive({gsub("\\\\", "/", input$blu$datapath)})
    fig2 <- reactive({gsub("\\\\", "/", input$mld$datapath)})
    fig3 <- reactive({gsub("\\\\", "/", input$mlu$datapath)})
    fig4 <- reactive({gsub("\\\\", "/", input$tld$datapath)})
    fig5 <- reactive({gsub("\\\\", "/", input$sketch$datapath)})
    # Hydrophyte photos
    fig6 <- reactive({gsub("\\\\", "/", input$hyd1$datapath)})
    fig7 <- reactive({gsub("\\\\", "/", input$hyd2$datapath)})
    fig8 <- reactive({gsub("\\\\", "/", input$hyd3$datapath)})
    fig9 <- reactive({gsub("\\\\", "/", input$hyd4$datapath)})
    # Invertebrate photos
    fig10 <- reactive({gsub("\\\\", "/", input$inv1$datapath)})
    fig11 <- reactive({gsub("\\\\", "/", input$inv2$datapath)})
    # Algae photos
    fig12 <- reactive({gsub("\\\\", "/", input$alg1$datapath)})
    # Fish photos
    fig13 <- reactive({gsub("\\\\", "/", input$fish1$datapath)})
    # Additional photos
    fig14 <- reactive({gsub("\\\\", "/", input$add1$datapath)})
    fig15 <- reactive({gsub("\\\\", "/", input$add2$datapath)})
    #fig16 <- reactive({gsub("\\\\", "/", input$alg_si1$datapath)})
    
    # Code for stream classification determination (using random forest model results)
    predict_flowduration <- reactive({
      
      # Convert all measure inputs to numerical values for use in our function.
      hydrophytes <- as.numeric(input$radio_hydro)
      EPT <- as.numeric(input$radio_ept)
      BMI <- as.numeric(input$radio_bmi)
      livedeadalg <- as.numeric(ifelse(input$radio_algae == 0, 0, 1)) # model results only take the binary yes/no, 1/0
      SIalg <- as.numeric(ifelse(input$radio_algae == 0, 0,
        ifelse(input$radio_algae == 1, 1, 2))) # need to account for two "yes" options in single indicators
      SIfish <- as.numeric(input$fish)
      
      #assemble test data that will be input by the user
      test.df<-data.frame(hydrophytes_3pa=hydrophytes,
        EPT_pa=EPT,
        BMI_20=BMI,
        livedeadalg_pa=livedeadalg)
      
      #generates predictions based on random forest model outputs (frf_2) and used inputs (test.df) and flips from wide to long (1 column, 3 rows)
      xdf <- predict(frf_2, newdata=test.df, type="prob") %>%
        as.data.frame()
      
      # updated final determination of the greatest probability/classification, 2/3rds cutoff
      mincut <-.667
      
      # create new column with "at least intermittent" category designation
      xdf$pALI <- xdf$I + xdf$P
      
      # final determination
      if(EPT==1 & BMI==0)
        print("WARNING! Illogical invertebrate inputs.")
      else
        case_when(xdf$P>mincut~"Perennial",
          xdf$I>mincut~"Intermittent",
          xdf$E>mincut & SIfish==1 & SIalg==2 ~"At Least Intermittent",
          xdf$E>mincut~"Ephemeral",
          xdf$pALI>mincut~"At Least Intermittent",
          SIfish==1 & SIalg==2 ~ "At Least Intermittent",
          T~"Need more information")
    })
    
    #If prediction is NMI or E, but there are single indicators present, prediction becomes "ALI".
    
    # Based on indicator inputs, this will choose which table to display.
    
    predict_figure <- reactive({
      
      # Using similar code from above to convert inputs to numerical values.
      hydro <- as.numeric(input$radio_hydro)
      BMI <- as.numeric(input$radio_bmi)
      EPT <- as.numeric(input$radio_ept)
      SIalg <- as.numeric(ifelse(input$radio_algae == 0, 0,
        ifelse(input$radio_algae == 1, 1, 2))) 
      SIfish <- as.numeric(input$fish)
      
      # Going down list of 31 possible iterations.
      
      case_when(hydro == 0 & BMI == 0 & EPT == 0 & SIalg == 0 & SIfish == 0 ~ 1,
        hydro == 0 & BMI == 0 & EPT == 0 & SIalg == 0 & SIfish == 1 ~ 2,
        hydro == 0 & BMI == 0 & EPT == 0 & SIalg == 1 & SIfish == 0 ~ 3,
        hydro == 0 & BMI == 0 & EPT == 0 & SIalg == 1 & SIfish == 1 ~ 4,
        hydro == 0 & BMI == 0 & EPT == 0 & SIalg == 2 ~ 4,
        hydro == 0 & BMI == 0.5 & EPT == 0 & SIalg == 0 & SIfish == 0 ~ 5,
        hydro == 0 & BMI == 0.5 & EPT == 0 & SIalg == 0 & SIfish == 1 ~ 6,
        hydro == 0 & BMI == 0.5 & EPT == 0 & SIalg == 1 & SIfish == 0 ~ 7,
        hydro == 0 & BMI == 0.5 & EPT == 0 & SIalg == 1 & SIfish == 1 ~ 8,
        hydro == 0 & BMI == 0.5 & EPT == 0 & SIalg == 2 ~ 8,
        hydro == 0 & BMI == 0.5 & EPT == 1 ~ 9,
        hydro == 0 & BMI == 1 & EPT == 0 & SIalg == 0 & SIfish == 0 ~ 10,
        hydro == 0 & BMI == 1 & EPT == 0 & SIalg == 0 & SIfish == 1 ~ 11,
        hydro == 0 & BMI == 1 & EPT == 0 & SIalg == 1 & SIfish == 0 ~ 12,
        hydro == 0 & BMI == 1 & EPT == 0 & SIalg == 1 & SIfish == 1 ~ 13,
        hydro == 0 & BMI == 1 & EPT == 0 & SIalg == 2 ~ 13,
        hydro == 0 & BMI == 1 & EPT == 1 ~ 14,
        hydro == 0.5 & BMI == 0 & EPT == 0 & SIalg == 0 & SIfish == 0 ~ 15,
        hydro == 0.5 & BMI == 0 & EPT == 0 & SIalg == 0 & SIfish == 1 ~ 16,
        hydro == 0.5 & BMI == 0 & EPT == 0 & SIalg > 0 ~ 17,
        hydro == 0.5 & BMI == 0.5 & EPT == 0 & SIalg == 0 ~ 18,
        hydro == 0.5 & BMI == 0.5 & EPT == 0 & SIalg > 0 ~ 19,
        hydro == 0.5 & BMI == 0.5 & EPT == 1 ~ 20,
        hydro == 0.5 & BMI == 1 & EPT == 0 & SIalg == 0 ~ 21,
        hydro == 0.5 & BMI == 1 & EPT == 0 & SIalg > 0 ~ 22,
        hydro == 0.5 & BMI == 1 & EPT == 1 & SIalg == 0 ~ 23,
        hydro == 0.5 & BMI == 1 & EPT == 1 & SIalg > 0 ~ 24,
        hydro == 1 & BMI == 0 & EPT == 0 & SIalg == 0 & SIfish == 0 ~ 25,
        hydro == 1 & BMI == 0 & EPT == 0 & SIalg == 0 & SIfish == 1 ~ 26,
        hydro == 1 & BMI == 0 & EPT == 0 & SIalg > 0 ~ 27,
        hydro == 1 & BMI == 0.5 & EPT == 0 ~ 28,
        hydro == 1 & BMI == 0.5 & EPT == 1 ~ 29,
        hydro == 1 & BMI == 1 & EPT == 0 ~ 30,
        hydro == 1 & BMI == 1 & EPT == 1 ~ 31)
      
    })
    
    
    output$report <- downloadHandler(
      filename = "AWSDAM_report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          a = input$project,
          b = input$assessor,
          c = input$code,
          d = input$waterway,
          e = input$date,
          f = input$boundary,
          g = as.numeric(input$lat),
          h = as.numeric(input$lon),
          i = input$datum,
          j = input$weather,
          k = input$situation,
          l = ifelse(input$radio_use == 0, "Urban/industrial/residential",
            ifelse(input$radio_use == 1, "Agricultural",
              ifelse(input$radio_use == 2, "Developed open-space",
                ifelse(input$radio_use == 3, "Forested",
                  ifelse(input$radio_use == 4, "Other natural", "Other"))))),
          m = input$surfflow,
          n = input$subflow,
          o = input$pool,
          p = input$channel,
          r = input$actreach,
          s = fig1(),
          t = fig2(),
          u = fig3(),
          v = fig4(),
          w = fig5(),
          #x = input$checklist,
          aa = fig6(),
          ab = input$hyd1_cap,
          ac = fig7(),
          ad = input$hyd2_cap,
          ae = fig8(),
          af = input$hyd3_cap,
          ag = fig9(),
          ah = input$hyd4_cap,
          ai = ifelse(input$radio_bmi == 0, "None", 
            ifelse(input$radio_bmi == 0.5, "1 to 19", "20+")),
          aj = ifelse(input$radio_ept == 0, "No", "Yes"),
          #ak = input$invtaxa,
          al = fig10(),
          am = fig11(),
          an = input$invnotes,
          ao = ifelse(input$radio_algae == 0, "Not detected",
            ifelse(input$radio_algae == 1, "Yes, <10% cover", "Yes >10% cover")),
          ap = fig12(),
          aq = ifelse(input$fish == 0, "No", "Yes"),
          ar = fig13(),
          as = input$amph,
          at = input$snake,
          av = ifelse(input$radio_hydro == 0, "0 species", 
            ifelse(input$radio_hydro == 0.5, "1 - 2 species", "3+ species")),
          ba = fig14(),
          bb = input$algnotes,
          bc = input$hydnotes,
          bd = input$fishnotes,
          be = input$add_cap,
          bf = input$other_ind,
          bg = input$add_notes,
          bh = fig15(),
          bi = input$add_cap2,
          #bj = fig16(),
          #bk = ifelse(input$alg_si == 0, "No", "Yes"),
          bm = case_when(input$radio_weather == 0~"Storm/heavy rain",
            input$radio_weather == 1~"Steady rain",
            input$radio_weather == 2~"Intermittent rain",
            input$radio_weather == 3~"Snowing",
            input$radio_weather == 4~"Cloudy",
            input$radio_weather == 5~"Clear/Sunny"),
          bn = case_when(input$radio_situation == 0~"Recent flood or debris flow",
            input$radio_situation == 1~"Stream modifications (e.g., channelization)",
            input$radio_situation == 2~"Diversions",
            input$radio_situation == 3~"Discharges",
            input$radio_situation == 4~"Drought",
            input$radio_situation == 5~"Vegetation removal/limitations",
            input$radio_situation == 6~"Other (explain in notes)",
            input$radio_situation == 7~"None"),
          bo = input$hydro_comments,
          #fm = field_map(),
          rf = predict_flowduration(),
          tbl = predict_figure())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  }
  
)

# End of R Shiny app script.