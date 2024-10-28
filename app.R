# Arid SouthWest Method Development
# Shiny Application for pdf Report Generation
# Created by: Heili Lowman
# Created on: September 30, 2020
# Last edited: November 25, 2020

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
library(leaflet)
library(htmlwidgets)
library(webshot)
library(knitr)

# Increase file upload size limit
options(shiny.maxRequestSize = 100 * 1024^2)  # Set to 100 MB


# Load dataset for random forest model
load("FinalRF.Rdata")

# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

# User Interface ----------------------------------------------------------

shinyApp(
  ui = fluidPage( theme = "classic",
    # tags$head(
    # tags$style(HTML("
    #   .nav-tabs {
    #     position: fixed;
    #     top: 0;
    #     z-index: 100; 
    #     width: 100%;
    #     /* background-color: #ffffff;*/ /* You can adjust the color as needed */
    #   }
    # 
    #   .tab-content {
    #     padding-top: 50px; /* This creates space at the top so the content doesn't hide behind the fixed tabs. Adjust as needed. */
    #   }
    #   "))
    # ),
                  
    # App title
    # update to 1.1 on Nov 9, 2023
     navbarPage(HTML("Beta Streamflow Duration Assessment Method for the Arid West: Online Report Generating Tool <a href=https://github.com/SCCWRP/beta_awsdam_report target=_blank>Version 1.1</a>"),
       position = c("fixed-top")),
    
    # Adding padding due to pinning title at top
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    
    # Adding images
    titlePanel(tags$img(src="Slide1.jpeg", height="80%", width="80%")),
    
    br(), # line break
      
      # Output: set of 2 tabs
      tabsetPanel(type = "tabs",

    tabPanel(h4("Background Information"),
      br(),
      column(width = 6,
        HTML("<p>This is a draft tool to calculate the Beta Streamflow Duration Assessment Method (SDAM) developed for the Arid West region. Do not use for regulatory purposes without prior consulting with the EPA product delivery team. For more information, consult the <a href='https://www.epa.gov/streamflow-duration-assessment'>Environmental Protection Agency's Streamflow Duration Assessment Methods homepage</a>.<p>"),
        br(),
        HTML("<p>Streams may exhibit a diverse range of hydrologic regimes that strongly influence physical, chemical and biological characteristics of streams and their adjacent riparian areas. Such hydrologic information supports many management decisions. One important aspect of hydrologic regime is streamflow duration—the length of time that a stream supports sustained surface flow. However, requisite hydrologic data to determine flow duration is unavailable at most reaches nationwide. Although maps, hydrologic models, and other data resources exist (e.g., the <a href='https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con'>National Hydrography Dataset</a>), they may exclude small headwater streams, and limitations on accuracy and spatial or temporal resolution may reduce their utility for many management applications. Therefore, there is a need for rapid, field-based methods to determine flow duration class at the reach scale in the absence of long-term hydrologic data (e.g., <a href='https://www.mdpi.com/2073-4441/12/9/2545'>Fritz et al., 2020</a>).</p>"),
        br(),
        p("For the purposes of the method presented here, stream reaches are classified into three types based on increasing streamflow duration:"),
        HTML("<p>- <em>Ephemeral</em> reaches flow only in direct response to precipitation. Water typically flows only during and shortly after large precipitation events, the streambed is always above the water table, and stormwater runoff is the primary source of water.<p>"),
        HTML("<p>- <em>Intermittent</em> reaches are channels that contain water for only part of the year, typically during the wet season, where the streambed may be below the water table and/or where the snowmelt from surrounding uplands provides sustained flow. The flow may vary greatly with stormwater runoff.<p>"),
        HTML("<p>- <em>Perennial</em> reaches contain water continuously during a year of normal rainfall, often with the streambed located below the water table for most of the year. Groundwater supplies the baseflow for perennial reaches, but flow is also supplemented by stormwater runoff or snowmelt.<p>"),
        br(),
        img(src="SDAMS.png", height="80%", width="80%"),
        br(),
        HTML("<p>This online reporting tool allows application of the Beta Streamflow Duration Assessment Method for the Arid West (SDAM AW), the EPA’s standard method for the Arid West region outside the Pacific Northwest (which is covered by method described in <a href='https://www.epa.gov/sites/production/files/2016-01/documents/streamflow_duration_assessment_method_pacific_northwest_2015.pdf'>Nadeau 2015</a>). The SDAM AW is based on the presence of biological indicators that are known to respond to gradients of streamflow duration. The first four indicators are evaluated together to assign a preliminary flow duration class to a reach, whereas the fifth indicator consists of <em>single indicators</em> whose presence determines that a reach is <em>at least intermittent</em>, either supporting or superseding the preliminary classification determined from the other four indicators. An <em>at least intermittent classification</em> indicates that the reach is not ephemeral, but the biological indicator data does not determine if the reach is perennial or intermittent with high confidence.<p>"),
        br(),
        p("(1) How many hydrophytic plant species (up to five) are growing in the channel, or within one half-channel width of the channel?"),
        p("(2) How many aquatic macroinvertebrates are quantified during a 15-minute search?"),
        p("(3) Is there evidence of aquatic stages of Ephemeroptera, Plecoptera, or Trichoptera (EPT)? "),
        p("(4) Are algae found on the streambed?"),
        p("(5) Are single indicators (i.e., the presence of fish or >10% algal cover) of intermittent or perennial streamflow duration observed?"),
        br(),
        HTML("<p><strong>Note:</strong> Prior to version 1.1 released on November 16, 2023,  the beta SDAM for the AW incorrectly applied the “At least intermittent” and “Need more information” classifications to streams where other classifications should have been applied. If your data resulted in a classification of “At least intermittent” or “Need more information” using version 1.0 of the model please check your classification using version 1.1. Classifications of perennial, intermittent, and ephemeral from version 1.0 are unaffected.</p>"),
        HTML("<p>This Beta method will be updated as more data are collected. For further information about streamflow duration assessment methods, refer to the <a href='https://www.epa.gov/streamflow-duration-assessment'>EPA website</a>.<p>"),
        br(),
        HTML("<p>For additional support with this website, please contact Dr. Raphael Mazor (raphaelm@sccwrp.org) at the Southern California Coastal Water Research Project.</p>")
      
        )
      ),

    tabPanel(h4("Enter Data"),
      br(),
      # Disclaimer -------
      div(HTML('<p style="color:#e32431;"><em>This is an analysis tool and does not store data. After 60 minutes the tool will timeout and all data will have to be re-entered.</em></p>') ),
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
    
    checkboxGroupInput("check_use", label = h5("Surrounding land-use within 100 m (check one or two)."), choices = list("Urban/industrial/residential" = 0, "Agricultural" = 1, "Developed open-space" = 2, "Forested" = 3, "Other natural" = 4, "Other" = 5), selected = 5), # radio buttons
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
      "Field form instructions: Record up to 5 hydrophytic plant species (FACW or OBL in the Arid West regional wetland plant list) within the assessment area: within the channel or up to one half-channel width. Explain in figure caption if species has an odd distribution (e.g., covers less than 2% of assessment area, long-lived species solely represented by seedlings, or long-lived species solely represented by specimens in decline), or if there is uncertainty about the identification. Enter photo ID, or check if photo is taken."), # subtext
    br(), # line break

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
    textInput("hydnotes", label = h5("Notes on hydrophytic vegetation (please be sure to denote if there was no vegetation at all observed within the reach):"), value = "Enter text..."), # text input box
    
# Invertebrates -----------------------------------------------------------

    hr(), # adds divider
    
    h3("Aquatic Invertebrates"), # Adds section header
    tags$div("Field form instructions: Do not count mosquito larvae."), # subtext
    br(), # line break
    
    # inverts input for determination
    radioButtons(inputId = "radio_bmi", label = "How many aquatic invertebrates are quantified in a 15-minute search? (number of individuals observed) - select one of the below options", choices = list("None" = 0, "1 to 19" = 0.5, "20 or more" = 1), selected = 0), # radion buttons
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
    
    # algae input for determination
    checkboxInput("algae_checkbox", label = "Check if all observed algae appear to be deposited from an upstream source.", value = FALSE),  
    radioButtons(inputId = "radio_algae", label = "Are algae found on the streambed? - select one of the below options", choices = list("Not detected" = 0, "Yes, ≤10% cover" = 1, "Yes, >10% cover" = 2), selected = 0),
    fileInput("alg1", label = HTML("Algae Photo #1<br />Upload photo file here.")), # file input box
    textInput("algnotes", label = h5("Notes on algae cover:"), value = "Enter text..."), # text input box

# Single Indicators -------------------------------------------------------

    hr(), # adds divider
    
    h3("Fish"), # Adds section header
    
    br(), # line break
    
    radioButtons("fish", label = "Fish", choices = list("Yes (species other than mosquitofish observed)" = 2, "Yes (strictly mosquitofish observed)" = 1, "No" = 0), selected = 0), # radio button
      
    fileInput("fish1", label = HTML("Fish Photo #1<br />Upload photo file here.")), # file input box
    
# Supplemental Info -------------------------------------------------------
    
    hr(), # adds divider
    h3("Supplemental Information"), # Adds section header
    tags$div("Field form instructions: If observed, note the presence of the aquatic life stages of amphibians, snakes, or turtles; iron-oxidizing bacteria and fungi; hydric soils; aquatic invertebrates that prefer perennial streams etc."), # subtext
    br(), # line break
    textInput("other_ind", label = h5("Supplemental information"), value = "Enter text..."), # text input box
    
    fileInput("add1", label = HTML("Additional Photo #1<br />Upload photo file here.")), # file input box
    textInput("add_cap", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    fileInput("add2", label = HTML("Additional Photo #2<br />Upload photo file here.")), # file input box
    textInput("add_cap2", label = h5("Figure Caption"), value = "Enter text..."), # text input box
    br(), # line break
    textInput("add_notes", label = h5("Additional Notes about the Assessment"), value = "Enter text..."), # text input box
    
    downloadButton("report", "Generate report")
  ),
        
        # 3rd tab
        
        tabPanel(h4("Additional Resources"),
          br(),
          column(width = 12,
          column(width = 6,
            HTML("<p>For additional information, please refer to the user manual developed for the Beta Streamflow Duration Assessment Method for the Arid West. Information about this method can be found on <a href='https://www.epa.gov/streamflow-duration-assessment/beta-streamflow-duration-assessment-method-arid-west'>the Environmental Protection Agency's Streamflow Duration Assessment Methods for the Arid West homepage</a>.</p>")
        )),
          br(),
          column(width = 12,
               column(width = 6,
                      HTML("<p>The user manual, training material, and other resources may be accessed from the <a href ='https://betasdamaw-sccwrp.hub.arcgis.com'> SDAM AW Training Website</a>.</p>")
               )),
          br(),
          column (
            width =12,
            column(width = 6,
               HTML("<p>R code used to develop this application is available here: <a href =\"https://github.com/SCCWRP/beta_awsdam_report\"> https://github.com/SCCWRP/beta_awsdam_report. </a></p>"
               )      
            )
          ),
          br(),
          column(width = 12,
          column(width = 6,
            HTML("<p>For additional support with this website, please contact Dr. Raphael Mazor (raphaelm@sccwrp.org) at the Southern California Coastal Water Research Project.</p>")
          ),

          
          ))
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
    

    
    #If prediction is NMI, LTP or E, but there are single indicators present, prediction becomes "ALI".
    
    # Based on indicator inputs, this will choose which table to display.
    predict_figure <- reactive({
      
      # Using similar code from above to convert inputs to numerical values.
      hydro <- as.numeric(input$radio_hydro)
      BMI <- as.numeric(input$radio_bmi)
      EPT <- as.numeric(input$radio_ept)
      AlgPA <- case_when(
        input$algae_checkbox == TRUE ~ 0, # Use checkbox to override
        input$radio_algae < 1 ~ as.numeric(0),
        input$radio_algae >= 1 ~ as.numeric(1),
        TRUE ~ NA_real_  # default case if needed
      )

      SIalg <- case_when(
        input$algae_checkbox == TRUE ~ 0, # Use checkbox to override
        input$radio_algae < 2 ~ as.numeric(0),
        input$radio_algae == 2 ~ as.numeric(1),
        TRUE ~ NA_real_  # default case if needed
      )

      SIfish <- as.numeric(ifelse(input$fish == 2, 1, 0))

      SI_Present <- max(SIalg, SIfish, na.rm = TRUE)
      
      # Going down list of possible iterations.
      # Create a data frame using the provided data
      possible.outcomes <- read.csv('data/outcomes.csv')

      print("possible.outcomes")
      print(possible.outcomes)
      print("hydro")
      print(hydro)
      print("BMI")
      print(BMI)
      print("EPT")
      print(EPT)
      print("SI_Present")
      print(SI_Present)

      classdf <- possible.outcomes %>% 
        filter(
          hydrophytes_3pa == hydro,
          BMI_20 == BMI,
          EPT_pa == EPT,
          livedeadalg_pa == AlgPA,
          SI_Present == SI_Present
        ) 
      
      print("classdf$FinalClassification[1]")
      print(classdf$FinalClassification[1])

      # function will return this
      classdf$FinalClassification[1]

    })
    
    
    output$report <- downloadHandler(
      filename = "AWSDAM_report.pdf",
      content = function(file) {
        
        # Show modal with loader
        showModal(modalDialog(
          title = "Processing",
          "Generating report",
          easyClose = FALSE,
          footer = NULL
        ))

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
          l = case_when(input$check_use == 0 ~ "Urban/industrial/residential",
                        input$check_use == 1 ~ "Agricultural",
                        input$check_use == 2 ~ "Developed open-space",
                        input$check_use == 3 ~ "Forested",
                        input$check_use == 4 ~ "Other natural", 
                        input$check_use == 5 ~ "Other"),
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
          aa = fig6(),
          ab = input$hyd1_cap,
          ac = fig7(),
          ad = input$hyd2_cap,
          ae = fig8(),
          af = input$hyd3_cap,
          ag = fig9(),
          ah = input$hyd4_cap,
          ai = case_when(input$radio_bmi == 0 ~ "None", 
                         input$radio_bmi == 0.5 ~ "1 to 19",
                         input$radio_bmi == 1 ~ "20+"),
          aj = ifelse(input$radio_ept == 0, "No", "Yes"),
          ak = input$algae_checkbox,
          al = fig10(),
          am = fig11(),
          an = input$invnotes,
          ao = case_when(input$radio_algae == 0 ~ "Not detected",
                         input$radio_algae == 1 ~ "No, <10% cover",
                         input$radio_algae == 2 ~ "Yes >10% cover"),
          ap = fig12(),
          aq = case_when(input$fish == 0 ~ "No fish observed",
                      input$fish == 1 ~ "No, only mosquito fish observed",
                      input$fish == 2 ~ "Yes, fish other than mosquitofish observed"),
          ar = fig13(),
          as = input$amph,
          at = input$snake,
          av = case_when(input$radio_hydro == 0 ~ "0 species",
                         input$radio_hydro == 0.5 ~ "1 - 2 species",
                         input$radio_hydro == 1 ~ "3+ species"),
          ba = fig14(),
          bb = input$algnotes,
          bc = input$hydnotes,
          bd = input$fishnotes,
          be = input$add_cap,
          bf = input$other_ind,
          bg = input$add_notes,
          bh = fig15(),
          bi = input$add_cap2,
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
          #rf = predict_flowduration(),
          classification = predict_figure()#,
          #fig_map = fig_map()
          )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        out <- rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )

        # remove the modal
        removeModal()

        # return the rendered knitted R Markdown file
        out

      }
    )
  }
  
)

# End of R Shiny app script.
