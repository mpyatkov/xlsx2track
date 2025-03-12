library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)


file_validation <- function(file_path) {
  
  output_list <- list(data = FALSE, header = FALSE, status = FALSE, error_message <- "")
  
  params <- tryCatch({
    read_xlsx(file_path, range = cell_rows(1:2), sheet = 1)
  }, error = function(e) {
    output_list$status <- FALSE
    output_list$error_message <- paste0("Error reading the HEADER from the file: ", e$message)
    return(output_list)
  })
  

  data <- tryCatch({
    read_xlsx(file_path, skip = 3, sheet = 1)
  }, error = function(e) {
    output_list$status <- FALSE
    output_list$error_message <- paste0("Error reading the DATA from the file: ", e$message)
    return(output_list)
  })
 
  missing_columns_header <- setdiff(c("name", "description"), colnames(params))
  missing_columns_data <- setdiff(c("chr", "start", "end", "region_id", "strand", "color"), colnames(data))
  
  if (length(missing_columns_header) > 0) {
    output_list$status <- FALSE
    output_list$error_message <- paste0("Missing HEADER columns: ", paste(missing_columns_header, collapse = ", "))
    return(output_list)
  }
  
  if (length(missing_columns_data) > 0) {
    output_list$status <- FALSE
    output_list$error_message <- paste0("Missing DATA columns: ", paste(missing_columns_data, collapse = ", "))
    return(output_list)
  }
 
  params <- params %>% as.list()
  header <- str_glue('track name=\"{params$name}\" description=\"{params$description}\" visibility=3 itemRgb=On')

  data <- data %>% 
    mutate(color = str_replace_all(color,"_",",")) %>% 
    mutate(score = 1000, thickStart = start, thickEnd = end) %>% 
    relocate(score, .after = region_id) %>% 
    relocate(all_of(c("thickStart", "thickEnd")), .before= color)
  
  output_list$header <- header
  output_list$data <- data
  output_list$status <- TRUE
  output_list$error_message <- "IAMOK"
  
  
  output_list
}



# Define UI (Updated)
ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),  # Add a theme for improved appearance
  titlePanel("Convert XLSX files to BED track files that can be loaded into the UCSC Genome Browser"),
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),  # Add a header for the instructions section
      helpText(
        "Upload one or more XLSX files for conversion. Each file must follow format and instructions for UCSC browser upload in example.xlsx file:",
        # tags$ul(
        #   tags$li(strong("'chr'")),
        #   tags$li(strong("'start'")),
        #   tags$li(strong("'end'"))
        # ),
        # "Additional columns will be ignored."
      ),
      fileInput("fileInput", "Select XLSX Files",
                multiple = TRUE,
                accept = c(".xlsx")),
      actionButton("process", "Convert to UCSC track", class = "btn btn-primary"),  # Styled button
      tags$hr(),  # Horizontal line for separation
      h5("Example File"),
      tags$a(href = "example.xlsx", "Download Example XLSX File", target = "_blank"),  # Link to example file
      width = 4  # Adjust sidebar width for better layout
    ),
    mainPanel(
      h3("Download Results"),
      uiOutput("downloadUI"),
      tags$hr(),  # Horizontal line for separation
      h5("Note: Ensure your files follow the specified format for successful conversion."),
      width = 8  # Adjust main panel width
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  processed_files <- reactiveVal(NULL)
  
  observeEvent(input$process, {
    req(input$fileInput)
    
    files <- input$fileInput$datapath
    filenames <- input$fileInput$name
    output_dir <- tempdir()
    
    validation_failed <- FALSE

    output_files <- files |>
      set_names(filenames) |>  # Create named list for better debugging
      imap(function(file, filename) {
        tryCatch({
          
          validated_data <- file_validation(file) 
          if (isFALSE(validated_data$status)) {
            validation_failed <<- TRUE
            stop(validated_data$error_message)
          } 
          
          # Save as BED file using readr::write_delim
          bed_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(filename), ".bed"))
          
          # Write HEADER
          write_lines(validated_data$header, bed_file)
          
          # Write DATA
          write_tsv(validated_data$data, file = bed_file, append = T, col_names = F)
          bed_file
        }, error = function(e) {
          showNotification(paste("Error processing", filename, ":", e$message), type = "error")
          NULL
        })
      }) |> 
      compact() %>%   # Remove NULL entries for failed files
      list_c()
    
    if (validation_failed || length(output_files) == 0) {
      processed_files(NULL)
      return()
    }

    # Zip all valid BED files
    zip_file <- file.path(output_dir, "output_files.zip")
    zip(zip_file, output_files, flags = '-jr9X')
    processed_files(zip_file)
    
  })
  
  output$downloadUI <- renderUI({
    req(processed_files())
    downloadLink("downloadData", "Download ZIP Archive of BED Files")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "converted_bed_files.zip"
    },
    content = function(file) {
      file.copy(processed_files(), file)
    }
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)
