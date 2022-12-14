# Function for module UI
ref_normalization_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(12, selectInput(ns("norm_method"), "Between Plate normalization methods:",
                           choices = c("median", "max", "mean"))),
    column(12, actionButton(ns("normalize"), "Start Normalization"))
  )
}


# Function for module server logic
ref_normalization <- function(input, output, session, values) {
  
  # normalization 
  observeEvent(input$normalize,{
    req(input$ref_idf_str)
    req(input$norm_method)
    
    temp <- bdg_norm_multi(bridge.str =  strsplit(input$ref_idf_str, split = ",")%>%unlist(), 
                           data.ls = values$upload_data, between.plate.method = input$norm_method)
    
    values$normed_data <- temp
    values$norm_method <- input$norm_method
    shinyCatch(message("Created Normed datasets."))
  })
}


