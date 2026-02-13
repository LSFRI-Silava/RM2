library(shiny)
library(sf)

workdir <- "Set your workdir here C:/..."
setwd(workdir)

if (exists("lvmmap")) {
  message("LVM map exists!")
} else {
  lvmmap <- st_read(paste0(workdir, "LVM_NOGABALI/LVM_NOGABALI_Shape.shp"))
}

getCompartment <- function(fullmap, kvapg, kv, nog) {
  if (kvapg == "" || kv == "" || nog == "") {
    return(FALSE)
  }
  lvmmap_blockdistr <- fullmap[fullmap$BLOCKDISTR %in% c(kvapg), ]
  lvmmap_block <- lvmmap_blockdistr[lvmmap_blockdistr$BLOCKNUMBE %in% c(kv), ]
  lvmmap_comp <- lvmmap_block[lvmmap_block$COMPARTMEN %in% c(nog), ]
  resultsdir <- paste(kvapg, kv, nog, sep = "-")
  st_write(lvmmap_comp, resultsdir, driver = "ESRI Shapefile", append = FALSE)
  return(resultsdir)
}

source("forvarder.R")

ui <- fluidPage(
  textInput("numblockdistr", "Kvartāla apgabals (piem: 610)", value = "610"),
  textInput("numblock", "Kvartāla numurs (piem: 33)", value = "33"),
  textInput("numcomp", "Nogabala numurs (piem: 8 vai 14,21,3)", value = ""),
  sliderInput("rut_depth_threshold", "Risu dziļuma slieksnis (m):", min = 0.1, max = 1, value = 0.2, step = 0.1),
  sliderInput("risu_bufers", "TK platums (m):", min = 3.5, max = 10, value = 0.2, step = 0.1),
  submitButton("Aprēķins"),
  
  fluidRow(
    column(6,
           textOutput("text"),
           htmlOutput("text1"),
           htmlOutput("text2")
    ),
    column(6,
           plotOutput("plot"),
           imageOutput("image"),
           imageOutput("image2")
    )
  )
)

server <- function(input, output) {
  output$text <- renderText({
    resultsdir <- getCompartment(lvmmap, input$numblockdistr, input$numblock, input$numcomp)
    if (resultsdir != FALSE) {
      rises <- forvardera_rises(workdir, resultsdir, input$rut_depth_threshold, input$risu_bufers)
      paste0("Rezultatu direktorija: ", workdir, resultsdir)
    } else {
      paste0("Nav ievaditi ieejas parametri")
    }
  })
  
  output$text1 <- renderUI({
    resultsdir <- getCompartment(lvmmap, input$numblockdistr, input$numblock, input$numcomp)
    if (resultsdir != FALSE) {
      rises_info <- file.path(workdir, resultsdir, "output.txt")
      if (file.exists(rises_info)) {
        rises_content <- readLines(rises_info, encoding = "UTF-8")
        return(HTML(paste(rises_content, collapse = "<br>")))
      } else {
        return("Kļūda: 'output.txt' nav atrasts.")
      }
    }
  })
  
  output$text2 <- renderUI({
    resultsdir <- getCompartment(lvmmap, input$numblockdistr, input$numblock, input$numcomp)
    if (resultsdir != FALSE) {
      rises_info <- file.path(workdir, resultsdir, "output_tk.txt")
      if (file.exists(rises_info)) {
        rises_content <- readLines(rises_info, encoding = "UTF-8")
        return(HTML(paste(rises_content, collapse = "<br>")))
      } else {
        return("Kļūda: 'output_tk.txt' nav atrasts.")
      }
    }
  })
  
  output$image <- renderImage({
    resultsdir <- getCompartment(lvmmap, input$numblockdistr, input$numblock, input$numcomp)
    if (!isFALSE(resultsdir)) {
      rises_path <- file.path(workdir, resultsdir, "rises_plot.png")
      if (file.exists(rises_path)) {
        list(src = rises_path, contentType = "image/png", width = "100%")
      }
    }
  }, deleteFile = FALSE)
  
  output$image2 <- renderImage({
    resultsdir <- getCompartment(lvmmap, input$numblockdistr, input$numblock, input$numcomp)
    if (!isFALSE(resultsdir)) {
      rises_path <- file.path(workdir, resultsdir, "rises_plot_tk.png")
      print(paste("Checking path for image2:", rises_path)) # Diagnostika
      if (file.exists(rises_path)) {
        print("File exists for image2") # Diagnostika
        list(src = rises_path, contentType = "image/png", width = "100%")
      } else {
        print("File does not exist for image2") # Diagnostika
        NULL
      }
    } else {
      print("resultsdir is FALSE") # Diagnostika
      NULL
    }
  }, deleteFile = FALSE)
  
  
  outputOptions(output, 'image', suspendWhenHidden = FALSE)
  outputOptions(output, 'image2', suspendWhenHidden = FALSE)
}

shinyApp(ui, server)

