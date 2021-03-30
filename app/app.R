# Load libraries ----------------------------------------------------------
library(shiny)
library(shinyjs)
library(climatestripes)
library(colourpicker)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Climate stripes - color gradients of temperature change"),
    sidebarLayout(
        sidebarPanel(
            shinyjs::useShinyjs(),
            helpText("You can either use a default dataset or upload a .csv file."),
            selectInput("defaultData", label="Select a default dataset", choices = c("Use custom data"="none", "Hadley CRUT4 SST"="sst", "St Margaret’s Bay, NS, Canada"="stmargaretsbay")),
            fileInput("uploadedCSV", "Choose a CSV file for upload (maximum 5MB)", multiple = FALSE, accept = c("test/csv", "text/comma-separated-values", "text/plain", ".csv")),
            uiOutput("upload.timevector"),
            uiOutput("upload.temperature.vector"),
            uiOutput("upload.month.vector"),
            uiOutput("upload.nan"),
            numericInput("num.colors", "If you want to use a custom color scheme, please specify how many colours you would like in the spectrum (leave at 0 for default colours)", min=0, max=10, step=1, value=0),
            checkboxInput("trendline", "Superimpose the data and a trend line?"),
            textInput("title", "Plot title"),
            checkboxInput("months", "Divide annual data into months?"),
            actionButton("createplot", "Create climate stripe plot")
        ),
        mainPanel(
            uiOutput("custom.colors"),
            plotOutput("tempStripes"),
            p("Temperature legend"),
            plotOutput("tempLegend", width="25%") #, height="100px")
        )
    )
)

server <- function(input, output, session){
    uploadedFile <<- FALSE
    # dataset selection
    dataset <<- reactive({
        if(input$defaultData != "none" && input$defaultData != ""){
            get(input$defaultData)
        } else {
            read.csv(input$uploadedCSV$datapath, header=TRUE,sep=",")
        }
    })

    observeEvent(input$defaultData, {
        if(input$defaultData == "sst"){
            shinyjs::disable("months")
        } else {
            shinyjs::enable("months")
        }
    })

    observeEvent(input$months, {
        if(input$months==TRUE){
            shinyjs::disable("trendline")
        } else {
            shinyjs::enable("trendline")
        }
    })

    observeEvent(input$uploadedCSV, {
        if(input$defaultData == "none" && !is.null(input$uploadedCSV)){
            output$upload.timevector <- renderUI({
                selectInput("timevector", label="Select column containing time data", choices=colnames(dataset()))
            })
            output$upload.temperature.vector <- renderUI({
                selectInput("tempvector", label="Select column containing temperature data", choices=colnames(dataset()))
            })
            output$upload.month.vector <- renderUI({
                selectInput("monthvector", label="Select column containing months", choices=colnames(dataset()))
            })
            output$upload.nan <- renderUI({
                textInput("nan", label="What value indicates 'nan' or missing data? (Leave blank if there are no 'NAN' characters in the data)")
            })
        } else {
            output$upload.timevector <- renderUI({})
            output$upload.temperature.vector <- renderUI({})
            output$upload.month.vector <- renderUI({})
            output$upload.nan <- renderUI({})
        }
    })

    observeEvent(input$num.colors, {
        if(input$num.colors > 0){
            output$custom.colors <- renderUI({
                fluidRow(
                    p(paste0("Color 1 represents the minimum temperature, the final colour (", input$num.colors, ") represents maximum temperature")),
                    lapply(1:input$num.colors, function(val) {
                            column(3, 
                                colourInput(paste0("col_", val), paste0("Select color ", val)) #, "white")
                        )
                    })
                )
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
            colour.vec <<- vector(length=input$num.colors)
            for(i in 1:input$num.colors){
                colour.vec[i] <- eval(parse(text=paste0("input$col_",i)))
                # print(length(colour.vec))
                # print("New colour:")
                # print(colour.vec[i])
            }
        } else {
            colour.vec <<- c("navyblue", "lightblue", "red", "darkred")
        }
        if(input$trendline==FALSE && input$months==FALSE){
            output$tempStripes <- renderPlot({
                climate.col.stripes.f(
                    time.vector=time.vector, temperature.vector=temp.vector, colour.vec=colour.vec, 
                    title=input$title, 
                    legend=F,
                    text.col.legend="yellow"
                )
            })
            output$tempLegend <- renderPlot({
                tempvec <- matrix(seq(from=round(min(temp.vector,na.rm=T),1), to=round(max(temp.vector, na.rm=T), 1), length.out=length(colour.vec))) # input$num.colors 
                image(1, tempvec, t(seq_along(tempvec)), col=colour.vec, axes=FALSE, xlab="", ylab="")
                axis(4)
#                colour.gradient.legend.f(
#                    xleft=0, ybottom=0, xright=1, ytop=3, colour.vec=colour.vec, ncolours=10, labels=T, 
#                    var.min.label=round(min(temp.vector,na.rm=T),1), var.max.label=round(max(temp.vector,na.rm=T),1),
#                    text.col.legend="yellow")
            })
        } else if(input$trendline==TRUE && input$months==FALSE){
            output$tempStripes <- renderPlot({
                climate.col.stripes.f(
                    time.vector=time.vector, temperature.vector=temp.vector, colour.vec=colour.vec, 
                    title=input$title, 
                    legend=F,
                    text.col.legend="yellow"
                )
                superimpose.data.f(time.vector=time.vector, temperature.vector=temp.vector, data.colour="yellow", spline=T, spline.colour="white",lwd=4)
                
            })
            output$tempLegend <- renderPlot({
                tempvec <- matrix(seq(from=round(min(temp.vector,na.rm=T),1), to=round(max(temp.vector, na.rm=T), 1), length.out=length(colour.vec))) # input$num.colors 
                image(1, tempvec, t(seq_along(tempvec)), col=colour.vec, axes=FALSE, xlab="", ylab="")
                axis(4)
#                colour.gradient.legend.f(
#                    xleft=0, ybottom=0, xright=1, ytop=3, colour.vec=colour.vec, ncolours=10, labels=T, 
#                    var.min.label=round(min(temp.vector,na.rm=T),1), var.max.label=round(max(temp.vector,na.rm=T),1),
#                    text.col.legend="yellow")
            })
        } else if(input$months == TRUE){
            if(input$defaultData=="stmargaretsbay"){
                months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
                monthcols <- match(months, names(stmargaretsbay))
                output$tempStripes <- renderPlot({
                    par(mfcol=c(6,2),mar=c(.2,.1,.5,.1))
                    for (i in monthcols){
                      temperature.vector <- dataset()[,i]
                      temperature.vector[temperature.vector==999.9] <- NA
                      climate.col.stripes.f(time.vector= time.vector,temperature.vector, colour.vec=colour.vec,title=months[i-1], time.scale=F, legend=F)
                    }
                })
                output$tempLegend <- renderPlot({
                    tempvec <- matrix(seq(from=round(min(temp.vector,na.rm=T),1), to=round(max(temp.vector, na.rm=T), 1), length.out=length(colour.vec))) # input$num.colors 
                    image(1, tempvec, t(seq_along(tempvec)), col=colour.vec, axes=FALSE, xlab="", ylab="")
                    axis(4)
            })
            } else {
                months <- unique(dataset()[,input$monthvector])
                output$tempStripes <- renderPlot({
                    par(mfcol=c(6,2),mar=c(.2,.1,.5,.1))
                    for (i in 1:length(months)){
                        subdata <<- dataset()[which(dataset()[,input$monthvector]==months[i]),]
                        print(subdata)
                        temperature.vector <- subdata[,input$tempvector]
                        temperature.vector[temperature.vector==input$nan] <- NA
                        time.vector <- subdata[,input$timevector]
                        climate.col.stripes.f(time.vector= time.vector, temperature.vector, colour.vec=colour.vec, title=months[i], time.scale=F)
                    }
                })
                output$tempLegend <- renderPlot({
                    tempvec <- matrix(seq(from=round(min(temp.vector,na.rm=T),1), to=round(max(temp.vector, na.rm=T), 1), length.out=length(colour.vec))) # input$num.colors 
                    image(1, tempvec, t(seq_along(tempvec)), col=colour.vec, axes=FALSE, xlab="", ylab="")
                    axis(4)
#                colour.gradient.legend.f(
#                    xleft=0, ybottom=0, xright=1, ytop=3, colour.vec=colour.vec, ncolours=10, labels=T, 
#                    var.min.label=round(min(temperature.vector,na.rm=T),1), var.max.label=round(max(temperature.vector,na.rm=T),1),
#                    text.col.legend="yellow")
                })
            }
        }
    })
}

shiny::shinyApp(ui = ui, server = server)