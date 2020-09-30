# Arid SouthWest Method Development
# Shiny Application for pdf Report Generation
# Created by: Heili Lowman
# Created on: September 30, 2020


# Setup -------------------------------------------------------------------

# Create Shiny app. Anything in the sections below (user interface & server) should be the reactive/interactive parts of the shiny application.

# User Interface ----------------------------------------------------------

shinyApp(
  ui = fluidPage(
    
    # App title
    titlePanel("Field form for the Arid West Streamflow Duration Assessment Method"),
    
    # Title panel subtext
    tags$div(
      "This is a draft tool to calculate the interim Streamflow Duration Assessment Method (SDAM) developed for the Arid West region. Do not use for regulatory purposes without prior consulting with the EPA product delivery team. Contact Raphael Mazor (raphaelm@sccwrp.org) with questions."),
    
    textInput("project", label = h5("Project Name or Number"), value = "Enter text..."), # text input box
    textInput("assessor", label = h5("Assessor(s)"), value = "Enter text..."), # text input box
    textInput("address", label = h5("Site Address"), value = "Enter text..."), # text input box
    textInput("waterway", label = h5("Waterway Name"), value = "Enter text..."), # text input box
    textInput("date", label = h5("Assessment Date"), value = "Enter text..."), # text input box
    
    hr(), # adds divider
    
    downloadButton("report", "Generate report")
  ),
  

# Server ------------------------------------------------------------------

  server = function(input, output) {
    output$report <- downloadHandler(
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(a = input$project,
          b = input$assessor,
          c = input$address,
          d = input$waterway,
          e = input$date)
        
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