#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# TODO
# auto install required packages if not sys
# plot with ggplot2
# add second y-axis when stim intesity is displayed
# reset input button
# download plot as png and/or pdf
# periscope packg has dowload graph and data fnct as well as reset and others https://cran.r-project.org/web/packages/periscope/index.html


library(shiny)
library(shinythemes)
library(shinyWidgets)


ui <- navbarPage("RM SOP SIM", theme = shinytheme('readable'),
                 # tabPanel("Introduction", 
                 #          # Introduction Tab START
                 #          h1("Introduction")
                 
                 #), # Introduction Tab START
                 tabPanel(
                     # SOR Tab START
                     
                     title = "SOR",
                     
                     plotOutput('plsor'),
                     
                     hr(),
                     
                     fluidRow(
                         
                         column(3, align = 'center',
                                h2("Trial Settings"),
                                sliderInput('sordur', 'Trial Duration',
                                            min=10, max=1000, value=500,
                                            step=1),
                                
                                
                                h2("Plot Settings"),
                                
                                checkboxGroupButtons(
                                    inputId = "sorplinesQ",
                                    label = "Plot for Q",
                                    choices = c("A1", 
                                                "A2",
                                                "I"),
                                    selected = c("A1", "A2"),
                                    justified = TRUE,
                                    checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"))),
                                
                                checkboxGroupButtons(
                                    inputId = "sorplinesX",
                                    label = "Plot for X",
                                    choices = c("A1", 
                                                "A2",
                                                "I"),
                                    selected = c("A1", "A2"),
                                    justified = TRUE,
                                    checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"))),
                                checkboxGroupButtons(
                                    inputId = "sorplinesStim",
                                    label = "Plot Stimuli Duration",
                                    choices = c("Q","X"),
                                    selected = c("Q","X"),
                                    justified = TRUE,
                                    checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"))),
                                
                                checkboxGroupButtons(
                                    inputId = "sorplinesInt",
                                    label = "Plot Stimuli Intensity",
                                    choices = c("Q","X"),
                                    selected = c(),
                                    justified = TRUE,
                                    checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"))),
                                pickerInput(
                                    inputId = "sorpltype",
                                    label = "Select Plot Type", 
                                    choices = c('Activation', 'Deltas'),
                                    selected = 1
                                ),
                                
                                pickerInput(
                                    inputId = "sorplcolsch",
                                    label = "Select Plot Colour Scheme", 
                                    choices = c('Default', 'BW', 'CB'),
                                    selected = 1
                                )
                                
                         ),
                         
                         column(3, align = 'center',
                                h2('Model Settings'),
                                br(),
                                
                                fluidRow(
                                    column(6,
                                           knobInput(
                                               inputId = "sorpd1",
                                               label = "pd1",
                                               value = 0.1,
                                               min = 0,
                                               max = 1,
                                               step = 0.01,
                                               displayPrevious = TRUE, 
                                               displayInput = TRUE,
                                               thickness = .1,
                                               lineCap = "round",
                                               width = 100,
                                               height = 100,
                                               fgColor = "#428BCA",
                                               inputColor = "#428BCA"
                                           )
                                    ),
                                    column(6,
                                           knobInput(
                                               inputId = "sorpd2",
                                               label = "pd2",
                                               value = 0.02,
                                               min = 0,
                                               max = 1,
                                               step = 0.01,
                                               displayPrevious = TRUE, 
                                               displayInput = TRUE,
                                               thickness = .1,
                                               lineCap = "round",
                                               width = 100,
                                               height = 100,
                                               fgColor = "#428BCA",
                                               inputColor = "#428BCA")
                                    )
                                ),
                                
                                h4('Apply Distractor Rule'),
                                switchInput(
                                    inputId = "Id014",
                                    onStatus = "success", 
                                    offStatus = "danger",
                                    value = 1,
                                    size = 'large'
                                ),
                                
                                fluidRow(
                                    column(6, 
                                           align = 'center',
                                           knobInput(
                                               inputId = "sorc1",
                                               label = "C1",
                                               value = 2,
                                               min = 0,
                                               max = 100,
                                               step = 0.1,
                                               displayPrevious = TRUE, 
                                               displayInput = TRUE,
                                               thickness = .1,
                                               lineCap = "round",
                                               width = 100,
                                               height = 100,
                                               fgColor = "#428BCA",
                                               inputColor = "#428BCA")
                                           
                                           ),
                                    column(6, 
                                           align = 'center',
                                           
                                           knobInput(
                                               inputId = "sorc2",
                                               label = "C2",
                                               value = 10,
                                               min = 0,
                                               max = 100,
                                               step = 0.1,
                                               displayPrevious = TRUE, 
                                               displayInput = TRUE,
                                               thickness = .1,
                                               lineCap = "round",
                                               width = 100,
                                               height = 100,
                                               fgColor = "#428BCA",
                                               inputColor = "#428BCA"))
                                ),
                                
                                br(),
                                h2('Save Data'),
                                downloadButton("downloadData", "CSV"),
                                br(),
                                h2('Save Plot'),
                                fluidRow(column(6, downloadButton("downloadPlotPNG", "PNG")),
                                         column(6, downloadButton("downloadPlotPDF", "PDF"))),
                                br()
                                
                                
                         ),
                         
                         column(3,
                                align="center",
                                h2('Sample Phase'),
                                br(),

                                knobInput(
                                    inputId = "sorQp1",
                                    label = "Q p1",
                                    value = 0.8,
                                    min = 0,
                                    max = 1,
                                    step = 0.01,
                                    displayPrevious = TRUE, 
                                    displayInput = TRUE,
                                    thickness = .1,
                                    lineCap = "round",
                                    width = 100,
                                    height = 100,
                                    fgColor = "#428BCA",
                                    inputColor = "#428BCA"
                                ),
                                
                                sliderInput('sorQon1','Duration of S1', min = 0, max = 500, value = c(50,150), step = 1)
                                
                                
                                
                                
                                
                                
                         ),
                         column(3, align = 'center',
                                h2('Test Phase'),
                                br(),
                                
                                fluidRow(
                                    column(6,
                                           knobInput(
                                               inputId = "sorQp1test",
                                               label = "Q p1",
                                               value = 0.8,
                                               min = 0,
                                               max = 1,
                                               step = 0.01,
                                               displayPrevious = TRUE, 
                                               displayInput = TRUE,
                                               thickness = .1,
                                               lineCap = "round",
                                               width = 100,
                                               height = 100,
                                               fgColor = "#428BCA",
                                               inputColor = "#428BCA"
                                           )
                                    ),
                                    column(6,
                                           knobInput(
                                               inputId = "sorXp1test",
                                               label = "X p1",
                                               value = 0.8,
                                               min = 0,
                                               max = 1,
                                               step = 0.01,
                                               displayPrevious = TRUE, 
                                               displayInput = TRUE,
                                               thickness = .1,
                                               lineCap = "round",
                                               width = 100,
                                               height = 100,
                                               fgColor = "#428BCA",
                                               inputColor = "#428BCA")
                                    )
                                ),
                                
                                sliderInput('sorQon2','Duration of Q Test', min = 0, max = 500, value = c(250,350), step = 1),
                                sliderInput('sorXon2','Duration of X Test', min = 0, max = 500, value = c(250,350), step = 1)
                                
                                
                                
                                
                                
                         )
                     )
                     
                 ), # SOR Tab END
                 tabPanel("RR",
                          # RR TAB START
                          h1("Relative Recency")
                          
                 ), # RR TAB END
                 tabPanel("OIC",
                          # OIC TAB START
                          h1("Object in Context")
                          
                 ) # OIC TAB END
) # PAGE END
