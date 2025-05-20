library(shiny)
library(bslib)
library(chromote)

ui <- page_sidebar(
  title = "Website Screenshot Tool",
  sidebar = sidebar(
    textInput("url", "Website URL", value = "https://www.example.com"),
    numericInput("width", "Screenshot Width (pixels)", value = 1024, min = 320, max = 1920),
    numericInput("height", "Screenshot Height (pixels)", value = 768, min = 240, max = 1080),
    actionButton("capture", "Capture Screenshot", class = "btn-primary"),
    hr(),
    helpText("Enter a URL and click 'Capture Screenshot' to take a screenshot of the website.")
  ),
  card(
    card_header("Screenshot Preview"),
    uiOutput("screenshot_ui")
  )
)

server <- function(input, output, session) {
  # Initialize Chrome when the app starts
  chrome <- reactiveVal(Chrome$new())
  
  # Create a reactive for the screenshot data
  screenshot_data <- reactiveVal(NULL)
  
  # When the capture button is clicked
  observeEvent(input$capture, {
    # Show a progress notification
    withProgress(message = "Capturing screenshot...", {
      tryCatch({
        # Get a new tab
        tab <- chrome()$new_session()
        
        # Navigate to the URL
        tab$Page$navigate(input$url)
        tab$wait_for_load_event("load")
        
        # Set viewport size
        tab$Emulation$setDeviceMetricsOverride(
          width = input$width,
          height = input$height,
          deviceScaleFactor = 1,
          mobile = FALSE
        )
        
        # Wait a bit for the page to render completely
        Sys.sleep(1)
        
        # Take the screenshot
        img_data <- tab$Page$captureScreenshot()
        screenshot_data(img_data)
        
        # Close the tab
        tab$close()
      }, error = function(e) {
        showNotification(
          paste("Error:", e$message),
          type = "error"
        )
      })
    })
  })
  
  # Render the screenshot
  output$screenshot_ui <- renderUI({
    if (is.null(screenshot_data())) {
      return(div(
        class = "text-center p-5",
        p("No screenshot captured yet. Enter a URL and click the 'Capture Screenshot' button.")
      ))
    } else {
      # Convert the binary data to a data URI
      img_src <- sprintf("data:image/png;base64,%s", screenshot_data())
      
      # Return the image tag
      return(img(
        src = img_src,
        style = "max-width: 100%; border: 1px solid #ddd; margin-top: 10px;"
      ))
    }
  })
  
  # Clean up when the session ends
  onSessionEnded(function() {
    chrome()$close()
  })
}

shinyApp(ui, server)
