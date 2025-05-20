library(shiny)
library(bslib)
library(chromote)
library(base64enc)

# Check if Chrome is available
chrome_available <- tryCatch({
  chromote::find_chrome() != ""
}, error = function(e) {
  FALSE
})

startup_message <- if (chrome_available) {
  "Chrome detected on system"
} else {
  "Chrome not detected on system. This app requires Chrome/Chromium to be installed."
}

ui <- page_sidebar(
  title = "Website Screenshot Tool",
  sidebar = sidebar(
    textInput("url", "Website URL", value = "https://www.posit.co"),
    numericInput("width", "Screenshot Width (pixels)", value = 1024, min = 320, max = 1920),
    numericInput("height", "Screenshot Height (pixels)", value = 768, min = 240, max = 1080),
    actionButton("capture", "Capture Screenshot", class = "btn-primary"),
    hr(),
    # Add download button (initially disabled)
    downloadButton("downloadScreenshot", "Download Screenshot", 
                   style = "margin-top: 10px; width: 100%;"),
    hr(),
    tags$div(
      style = "color: red; margin-top: 20px;",
      textOutput("chrome_status")
    ),
    helpText("Enter a URL and click 'Capture Screenshot' to take a screenshot of the website.")
  ),
  card(
    card_header("Screenshot Preview"),
    uiOutput("screenshot_ui"),
    card_footer(
      verbatimTextOutput("debug_output")
    )
  )
)

server <- function(input, output, session) {
  # Create reactive values for app state
  state <- reactiveValues(
    chrome_initialized = FALSE,
    screenshot_data = NULL,
    debug_info = startup_message,
    chrome = NULL  # Store Chrome object directly in reactiveValues
  )
  
  # Output Chrome status
  output$chrome_status <- renderText({
    if (!chrome_available) {
      return("⚠️ Chrome/Chromium not detected - app will not function")
    } else if (!state$chrome_initialized) {
      return("⚠️ Chrome instance not initialized")
    } else {
      return("✓ Chrome initialized successfully")
    }
  })
  
  # Initialize Chrome at startup with detailed error handling
  observe({
    if (chrome_available) {
      tryCatch({
        # Find Chrome executable
        chrome_path <- chromote::find_chrome()
        state$debug_info <- paste("Found Chrome at:", chrome_path)
        
        # Initialize a global chromote session instead of a Chrome instance
        # This approach can be more stable in some environments
        session_obj <- ChromoteSession$new()
        state$chrome <- session_obj
        state$chrome_initialized <- TRUE
        
        state$debug_info <- paste("Chrome initialized successfully using path:", chrome_path)
      }, error = function(e) {
        state$debug_info <- paste("Chrome initialization failed:", e$message)
      })
    } else {
      state$debug_info <- "Chrome not found on system. Screenshots cannot be taken."
    }
  })
  
  # When the capture button is clicked
  observeEvent(input$capture, {
    # Update debug info
    state$debug_info <- "Starting screenshot capture process"
    
    # Check if Chrome is initialized
    if (!state$chrome_initialized || is.null(state$chrome)) {
      msg <- "Chrome is not initialized. Please check your installation."
      showNotification(msg, type = "error")
      state$debug_info <- msg
      return()
    }
    
    # Show a progress notification
    withProgress(message = "Capturing screenshot...", {
      tryCatch({
        # Get the Chrome session
        chrome_session <- state$chrome
        
        # Print information about the chrome_session object to debug
        state$debug_info <- paste("Chrome session class:", paste(class(chrome_session), collapse=", "))
        
        # Navigate to the URL
        state$debug_info <- paste("Navigating to URL:", input$url)
        chrome_session$Page$navigate(input$url)
        chrome_session$Page$loadEventFired()
        
        # Set viewport size
        state$debug_info <- paste("Setting viewport size:", input$width, "x", input$height)
        chrome_session$Emulation$setDeviceMetricsOverride(
          width = input$width,
          height = input$height,
          deviceScaleFactor = 1,
          mobile = FALSE
        )
        
        # Wait for page to render
        state$debug_info <- "Waiting for page to render..."
        Sys.sleep(2)
        
        # Take the screenshot
        state$debug_info <- "Taking screenshot"
        
        # Use captureScreenshot with proper format parameter
        result <- chrome_session$Page$captureScreenshot()
        
        # Print information about the result to help debug
        state$debug_info <- paste("Result class:", paste(class(result), collapse=", "))
        
        # Handle the screenshot data
        if (is.list(result) && "data" %in% names(result)) {
          state$screenshot_data <- result$data
          state$debug_info <- "Successfully captured screenshot (from result$data)"
        } else {
          # Inspect result structure more deeply
          state$debug_info <- paste(
            "Screenshot captured. Result structure:",
            paste(names(result), collapse=", ")
          )
          
          # Try different ways to extract the data
          if (is.list(result) && "result" %in% names(result) && is.list(result$result) && "data" %in% names(result$result)) {
            state$screenshot_data <- result$result$data
            state$debug_info <- "Successfully captured screenshot (from result$result$data)"
          } else if (is.character(result)) {
            state$screenshot_data <- result
            state$debug_info <- "Successfully captured screenshot (direct character string)"
          } else {
            state$debug_info <- paste(
              "Screenshot captured but format is unexpected. Result type:", 
              class(result)[1]
            )
          }
        }
        
        # Enable download button if screenshot was captured
        if (!is.null(state$screenshot_data)) {
          shinyjs::enable("downloadScreenshot")
        }
        
      }, error = function(e) {
        msg <- paste("Error during screenshot capture:", e$message)
        showNotification(msg, type = "error")
        state$debug_info <- paste(msg, "\nStack trace:", paste(capture.output(traceback()), collapse="\n"))
      })
    })
  })
  
  # Provide download functionality for the screenshot
  output$downloadScreenshot <- downloadHandler(
    filename = function() {
      # Create a filename based on the URL and timestamp
      url_part <- gsub("[^a-zA-Z0-9]", "_", input$url)
      url_part <- substr(url_part, 1, 30)  # Limit the length
      paste0(url_part, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      # Decode the base64 data and write to file
      if (!is.null(state$screenshot_data)) {
        tryCatch({
          # Convert base64 to binary
          binary_data <- base64decode(state$screenshot_data)
          # Write to file
          writeBin(binary_data, file)
          state$debug_info <- paste("Screenshot saved to:", file)
        }, error = function(e) {
          state$debug_info <- paste("Error saving screenshot:", e$message)
        })
      }
    }
  )
  
  # Render the screenshot
  output$screenshot_ui <- renderUI({
    screenshot <- state$screenshot_data
    
    if (is.null(screenshot)) {
      # Update UI state
      shinyjs::disable("downloadScreenshot")
      
      return(div(
        class = "text-center p-5",
        p("No screenshot captured yet. Enter a URL and click the 'Capture Screenshot' button.")
      ))
    } else {
      # Enable download button
      shinyjs::enable("downloadScreenshot")
      
      # Try to ensure it's a proper string
      if (is.raw(screenshot)) {
        screenshot <- rawToChar(screenshot)
      }
      
      if (!is.character(screenshot)) {
        return(div(
          class = "text-center p-5",
          p("Screenshot data is in an unexpected format. Check debug output.")
        ))
      }
      
      # The data is already base64-encoded, so we can use it directly
      img_src <- sprintf("data:image/png;base64,%s", screenshot)
      
      # Return the image tag
      return(img(
        src = img_src,
        style = "max-width: 100%; border: 1px solid #ddd; margin-top: 10px;"
      ))
    }
  })
  
  # Display debug information
  output$debug_output <- renderText({
    return(state$debug_info)
  })
  
  # Clean up when the session ends
  session$onSessionEnded(function() {
    if (!is.null(state$chrome)) {
      try({
        state$chrome$close()
      }, silent = TRUE)
    }
  })
}

shinyApp(ui, server)
