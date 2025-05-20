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
  # Initialize Chrome when the app starts - store the actual Chrome instance
  # directly rather than in a reactive value to avoid reactive context issues
  chrome_instance <- Chrome$new()
  
  # Create a reactive for the screenshot data
  screenshot_data <- reactiveVal(NULL)
  
  # When the capture button is clicked
  observeEvent(input$capture, {
    # Show a progress notification
    withProgress(message = "Capturing screenshot...", {
      tryCatch({
        # Get a new tab
        tab <- chrome_instance$new_session()
        
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
        
        # Take the screenshot - the result is a list with a data element containing base64 data
        screenshot_result <- tab$Page$captureScreenshot()
        
        # Extract just the base64 data from the result
        if (is.list(screenshot_result) && !is.null(screenshot_result$data)) {
          screenshot_data(screenshot_result$data)
        } else {
          showNotification("Failed to get screenshot data", type = "error")
        }
        
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
    screenshot <- screenshot_data()
    
    if (is.null(screenshot)) {
      return(div(
        class = "text-center p-5",
        p("No screenshot captured yet. Enter a URL and click the 'Capture Screenshot' button.")
      ))
    } else {
      # The data is already base64-encoded, so we can use it directly
      img_src <- sprintf("data:image/png;base64,%s", screenshot)
      
      # Return the image tag
      return(img(
        src = img_src,
        style = "max-width: 100%; border: 1px solid #ddd; margin-top: 10px;"
      ))
    }
  })
  
  # Clean up when the session ends - use the direct chrome_instance variable
  session$onSessionEnded(function() {
    if (!is.null(chrome_instance)) {
      try(chrome_instance$close(), silent = TRUE)
    }
  })
}

shinyApp(ui, server)
