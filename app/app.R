# Load libraries ----------------------------------------------------------
library(shiny)
# library(shinyjs)
library(climatestripes)
library(colourpicker)


# UI ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("TESA Carbon - Carbon footprint estimator for meetings and courses"),
    sidebarLayout(
        sidebarPanel(
            helpText("You can either use a default dataset or upload a .csv file."),
            selectInput("defaultData", label="Select a default dataset", choices = c("Use custom data"="none", "Hadley CRUT4 SST"="sst", "St Margaret’s Bay, NS, Canada"="stmargaretsbay")),
            fileInput("uploadedCSV", "Choose a CSV file for upload (maximum 5MB)", multiple = FALSE, accept = c("test/csv", "text/comma-separated-values", "text/plain", ".csv")),
            uiOutput("upload.timevector"),
            uiOutput("upload.temperature.vector"),
            uiOutput("upload.nan"),
            numericInput("num.colors", "If you want to use a custom color scheme, please specify how many colours you would like in the spectrum (leave at 0 for default colours)", min=0, max=10, step=1, value=0),
            checkboxInput("trendline", "Superimpose the data and a trend line?"),
            textInput("title", "Plot title"),
            # checkboxInput("months", "Divide annual data into months?"),
            actionButton("createplot", "Create climate stripe plot")
        ),
        mainPanel(
            uiOutput("custom.colors"),
            plotOutput("tempStripes")
        )
    )
)

server <- function(input, output, session){
    # dataset selection
    dataset <<- reactive({
        if(input$defaultData != "none"){
            get(input$defaultData)
        } else {
            read.csv(input$uploadedCSV$datapath, header=TRUE,sep=",")
        }
    })

    data.cols <<- colnames(dataset)
    observeEvent(input$defaultData, {
        if(input$defaultData == "none"){
            output$upload.timevector <- renderUI({
                selectInput("timevector", label="Select column containing time data", choices=data.cols)
            })
            output$upload.temperature.vector <- renderUI({
                selectInput("tempvector", label="Select column containing temperature data", choices=data.cols)
            })
            output$upload.nan <- renderUI({
                textInput("nan", label="What value indicates 'nan' or missing data? Leave blank for no value.", value="Enter NAN value")
            })
        } else {
            output$upload.timevector <- renderUI({})
            output$upload.temperature.vector <- renderUI({})
            output$upload.nan <- renderUI({})
        }
    })

    observeEvent(input$num.colors, {
        if(input$num.colors > 0){
            output$custom.colors <- renderUI({
                lapply(1:input$num.colors, function(val) {
                    fluidRow(
                        column(3, 
                            colourInput(paste0("col_", val), paste0("Select color ", val), "black")
                    ))
                })
            })
        } else {
            output$custom.colors <- renderUI({})
        }
    })

    observeEvent(input$createplot, {
        if(input$defaultData == "none"){
            time.vector <<- dataset()[,input$timevector]
            temp.vector <<- dataset()[,input$tempvector]
            temp.vector[temp.vector == input$nan] <- NA
        } else if(input$defaultData == "sst"){
            time.vector <<- dataset()$year
            temp.vector <<- dataset()$median
        } else if(input$defaultData == "stmargaretsbay"){
            time.vector <<- dataset()$YEAR
            temp.vector <<- dataset()$metANN
            temp.vector[temp.vector==999.9] <- NA
        }
        if(input$num.colors != 0){
            colour.vec <- vector(length=input$num.colors)
            for(i in 1:length(input$num.colors)){
                colour.vec[i] <- eval(parse(text=paste0("input$col_",i)))
            }
        } else {
            colour.vec <- c("navyblue", "lightblue", "red", "darkred")
        }
        if(input$trendline==FALSE){
            output$tempStripes <- renderPlot({
                climate.col.stripes.f(
                    time.vector=time.vector, temperature.vector=temp.vector, colour.vec=colour.vec, 
                    title=input$title, 
                    legend=T,
                    text.col.legend="yellow"
                )
            })
        } else {
            output$tempStripes <- renderPlot({
                climate.col.stripes.f(
                    time.vector=time.vector, temperature.vector=temp.vector, colour.vec=colour.vec, 
                    title=input$title, 
                    legend=F,
                    text.col.legend="yellow"
                )
                superimpose.data.f(time.vector=time.vector, temperature.vector=temp.vector, data.colour="yellow", spline=T, spline.colour="white",lwd=4)
            })
        }
    })
}

shiny::shinyApp(ui = ui, server = server)