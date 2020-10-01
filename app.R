# Arid SouthWest Method Development
# Shiny Application for pdf Report Generation
# Created by: Heili Lowman
# Created on: September 30, 2020

# Setup -------------------------------------------------------------------

# Load necessary packages
library(tidyverse)
library(randomForest)
library(shiny)
library(shinythemes)
library(ggforce)

# Load dataset for random forest model
load("FinalRF.Rdata")

# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

# User Interface ----------------------------------------------------------

shinyApp(
  ui = fluidPage(
    
    # App title
    titlePanel("Field Form for Arid West Streamflow Duration Assessment Method"),
    
    # Title panel subtext
    tags$div(
      "This is a draft tool to calculate the interim Streamflow Duration Assessment Method (SDAM) developed for the Arid West region. Do not use for regulatory purposes without prior consulting with the EPA product delivery team. Contact Raphael Mazor (raphaelm@sccwrp.org) with questions."),
    
    textInput("project", label = h5("Project Name or Number"), value = "Enter text..."), # text input box
    textInput("assessor", label = h5("Assessor(s)"), value = "Enter text..."), # text input box
    textInput("code", label = h5("Site Code or Identifier:"), value = "Enter text..."), # text input box
    textInput("waterway", label = h5("Waterway Name"), value = "Enter text..."), # text input box
    textInput("date", label = h5("Visit Date"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    textInput("boundary", label = h5("Reach Boundaries"), value = "Enter text..."), # text input box
    textInput("lat", label = h5("Latitude at downstream end (d.ddddd N)"), value = "Enter text..."), # text input box
    textInput("lon", label = h5("Longitude at downstream end (d.ddddd E)"), value = "Enter text..."), # text input box
    textInput("datum", label = h5("Datum"), value = "Enter text..."), # text input box
    textInput("weather", label = h5("Weather"), value = "Enter text..."), # text input box
    textInput("situation", label = h5("Site Disturbance/Difficulties"), value = "Enter text..."), # text input box
    textInput("use", label = h5("Surrounding Land Use"), value = "Enter text..."), # text input box
    textInput("surfflow", label = h5("Percent of Reach with Surface Flows"), value = "Enter text..."), # text input box
    textInput("subflow", label = h5("Percent of Reach with Surface and Sub-Surface Flows"), value = "Enter text..."), # text input box
    textInput("pool", label = h5("Number of Isolated Pools"), value = "Enter text..."), # text input box
    textInput("channel", label = h5("Mean Channel Width (m)"), value = "Enter text..."), # text input box
    textInput("actreach", label = h5("Reach Length (m)"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    fileInput("blu", label = h5("Site Photo - Bottom Looking Up")), # file input box
    fileInput("mld", label = h5("Site Photo - Middle Looking Down")), # file input box
    fileInput("mlu", label = h5("Site Photo - Middle Looking Up")), # file input box
    fileInput("tld", label = h5("Site Photo - Top Looking Down")), # file input box
    fileInput("sketch", label = h5("Site Sketch")), # file input box
    
    hr(), # adds divider
    
    textInput("checklist", label = h5("Checklist Used"), value = "Enter text..."), # text input box
    # hydrophyte input for report
    textInput("hydro", label = h5("Hydrophytes (number of species) - exact number observed"), value = "Enter text..."), # text input box
    # hydrophyte input for determination
    radioButtons(inputId = "radio_hydro", label = "Hydrophytes (number of species) - select one of the below options", choices = list("None" = 0, "1 or 2 species" = 0.5, "3 or more species" = 1), selected = 0), # radio buttons
    fileInput("hyd1", label = h5("Hydrophyte Photo #1")), # file input box
    textInput("hyd1_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    fileInput("hyd2", label = h5("Hydrophyte Photo #2")), # file input box
    textInput("hyd2_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    fileInput("hyd3", label = h5("Hydrophyte Photo #3")), # file input box
    textInput("hyd3_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    fileInput("hyd4", label = h5("Hydrophyte Photo #4")), # file input box
    textInput("hyd4_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    textInput("algnotes", label = h5("General Notes about Hydrophytic Vegetation"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    # inverts input for report
    textInput("abundance", label = h5("Aquatic Invertebrates Abundance (number of individuals) - exact number observed"), value = "Enter text..."), # text input box
    # inverts input for determination
    radioButtons(inputId = "radio_bmi", label = "Aquatic Invertebrates Abundance (number of individuals) - select one of the below options", choices = list("None" = 0, "1 to 19" = 0.5, "20 or more" = 1), selected = 0), # radion buttons
    # EPT input for report
    textInput("ept", label = h5("Mayflies, stoneflies, or caddisflies (EPT), live or dead - exact number observed"), value = "Enter text..."), # text input box
    # EPT input for determination
    radioButtons(inputId = "radio_ept", label = "Mayflies, stoneflies, or caddisflies (EPT), live or dead - select one of the below options", choices = list("Absent" = 0, "Present" = 1), selected = 0), # radio buttons
    textInput("invtaxa", label = h5("Invertebrate Taxa Observed"), value = "Enter text..."), # text input box
    fileInput("inv1", label = h5("Invertebrate Photo #1")), # file input box
    fileInput("inv2", label = h5("Invertebrate Photo #2")), # file input box
    textInput("invnotes", label = h5("General Notes about Aquatic Invertebrates"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    # algae input for report
    textInput("algae", label = h5("Algal mats (live or dead) - exact number observed"), value = "Enter text..."), # text input box
    # algae input for determination
    # Algal measure input
    radioButtons(inputId = "radio_algae", label = "Algal mats (live or dead) - select one of the below options", choices = list("Absent" = 0, "Present" = 1), selected = 0),
    fileInput("alg1", label = h5("Algae Photo #1")), # file input box
    textInput("algnotes", label = h5("General Notes about Algae"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    textInput("fish", label = h5("Fish Observed"), value = "Enter text..."), # text input box
    fileInput("fish1", label = h5("Fish Photo #1")), # file input box
    textInput("fishnotes", label = h5("General Notes about Fish"), value = "Enter text..."), # text input box
    textInput("amph", label = h5("Aquatic Amphibians Observed"), value = "Enter text..."), # text input box
    textInput("snake", label = h5("Aquatic Snakes Observed"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    textInput("other_ind", label = h5("Other Indicators"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    fileInput("add1", label = h5("Additional Photo #1")), # file input box
    textInput("add_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    textInput("add_notes", label = h5("Additional Notes about the Assessment"), value = "Enter text..."), # text input box
    
    downloadButton("report", "Generate report.")
  ),
  

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
    
    # Code for stream classification determination (using random forest model results)
    predict_flowduration <- reactive({
      # Convert all measure inputs to numerical values for use in our function.
      hydrophytes <- as.numeric(input$radio_hydro)
      EPT <- as.numeric(input$radio_ept)
      BMI <- as.numeric(input$radio_bmi)
      livedeadalg <- as.numeric(input$radio_algae)
      
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
          xdf$E>mincut~"Ephemeral",
          xdf$pALI>mincut~"At Least Intermittent",
          T~"Need more information")
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
        params <- list(a = input$project,
          b = input$assessor,
          c = input$code,
          d = input$waterway,
          e = input$date,
          f = input$boundary,
          g = input$lat,
          h = input$lon,
          i = input$datum,
          j = input$weather,
          k = input$situation,
          l = input$use,
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
          x = input$checklist,
          aa = fig6(),
          ab = input$hyd1_cap,
          ac = fig7(),
          ad = input$hyd2_cap,
          ae = fig8(),
          af = input$hyd3_cap,
          ag = fig9(),
          ah = input$hyd4_cap,
          ai = input$abundance,
          aj = input$ept,
          ak = input$invtaxa,
          al = fig10(),
          am = fig11(),
          an = input$invnotes,
          ao = input$algae,
          ap = fig12(),
          aq = input$fish,
          ar = fig13(),
          as = input$amph,
          at = input$snake,
          av = input$hydro,
          ba = fig14(),
          bb = input$algnotes,
          bc = input$hydnotes,
          bd = input$fishnotes,
          be = input$add_cap,
          bf = input$other_ind,
          bg = input$add_notes,
          rf = predict_flowduration())
        
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