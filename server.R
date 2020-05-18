#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$plsor <- renderPlot({
        
        # debug values rm
        # input = list()
        # input$sorQp1 = .8
        # input$sorQon1 = c(50,250)
        # input$sorQp1test = .8
        # input$sorXp1test = .6
        # input$sorQon2 = c(350,550)
        # input$sorXon2 = c(350,550)
        # input$sordur = 700
        # input$sorpltype = 'Activation'
        # input$sorpd1 = 0.1
        # input$sorpd2 = 0.02
        # input$sorplcolsch = 'Default'
        
        # update input scales based on other inputs
        observe(updateSliderInput(session, "sorQon1", max = input$sordur)) # updt end of S1 end max to max trial duration
        observe(updateSliderInput(session, "sorQon2", max = input$sordur)) # updt end of S2 end max to max trial duration
        observe(updateSliderInput(session, "sorXon2", max = input$sordur))
        # observe(updateSliderInput(session, "sorQon2", value = c(input$sorQon1[2], input$sorQon1[2]+100)))  # update on and off of Test to follow S1
        
        # Init values, trusting that matrices are faster than DFs
        sor = matrix(nrow = input$sordur, ncol = 32)
        colnames(sor) = c('p1Q', 'pd1Q', 'pd2Q', 
                          'pIQ', 'pA1Q', 'pA2Q', 'pTotalQ', 
                          'pA1Qgain', 'pA1Qloss','pA2Qgain','pA2Qloss','pIQgain','pIQloss',
                          'pA1Qdelta', 'pA2Qdelta', 'pIQdelta',
                          'p1X', 'pd1X', 'pd2X', 
                          'pIX', 'pA1X', 'pA2X', 'pTotalX', 
                          'pA1Xgain', 'pA1Xloss','pA2Xgain','pA2Xloss','pIXgain','pIXloss',
                          'pA1Xdelta', 'pA2Xdelta', 'pIXdelta')
        
        sor[,'p1Q'] = 0 # set as 0 to init
        sor[,'p1X'] = 0 
        
        sor[input$sorQon1[1]:input$sorQon1[2],'p1Q'] = input$sorQp1
        sor[input$sorQon2[1]:input$sorQon2[2],'p1Q'] = input$sorQp1test
        
        sor[input$sorXon2[1]:input$sorXon2[2],'p1X'] = input$sorXp1test
        
        sor[,'pd1Q'] = input$sorpd1     # set decay rate for Q
        sor[,'pd2Q'] = input$sorpd2     # set decay rate for Q
        sor[,'pd1X'] = input$sorpd1     # set decay rate for X
        sor[,'pd2X'] = input$sorpd2     # set decay rate for X
        sor[,'pIQ'] = 0                 # Make I numeric
        sor[,'pIX'] = 0

        
        # run moment by moment simulation
        for (i in 1:nrow(sor)){
            if (i == 1){
                
                sor[i,'pIQ'] = 1 - sor[i,'p1Q'] # This will be 1 as long as p1 = 0
                sor[i,'pIX'] = 1 - sor[i,'p1X']
                
                # Deltas calculate
                sor[i,'pA1Qgain'] = sor[i,'p1Q']
                sor[i,'pA1Qloss'] = sor[i,'pA1Qgain'] * sor[i,'pd1Q']
                sor[i,'pA1Qdelta'] = sor[i,'pA1Qgain'] - sor[i,'pA1Qloss']
                
                sor[i,'pA1Xgain'] = sor[i,'p1X']
                sor[i,'pA1Xloss'] = sor[i,'pA1Xgain'] * sor[i,'pd1X']
                sor[i,'pA1Xdelta'] = sor[i,'pA1Xgain'] - sor[i,'pA1Xloss']
                
                sor[i,'pA2Qgain'] = sor[i,'pA1Qloss']
                sor[i,'pA2Qloss'] = sor[i,'pA2Qgain'] * sor[i,'pd2Q']
                sor[i,'pA2Qdelta'] = sor[i,'pA2Qgain'] - sor[i,'pA2Qloss']
                
                sor[i,'pA2Xgain'] = sor[i,'pA1Xloss']
                sor[i,'pA2Xloss'] = sor[i,'pA2Xgain'] * sor[i,'pd2X']
                sor[i,'pA2Xdelta'] = sor[i,'pA2Xgain'] - sor[i,'pA2Xloss']
                
                sor[i,'pIQgain'] = sor[i,'pA2Qloss']
                sor[i,'pIQloss'] = sor[i,'p1Q']
                sor[i,'pIQdelta'] = sor[i,'pIQgain'] - sor[i,'pIQloss']
                
                sor[i,'pIXgain'] = sor[i,'pA2Xloss']
                sor[i,'pIXloss'] = sor[i,'p1X']
                sor[i,'pIXdelta'] = sor[i,'pIXgain'] - sor[i,'pIXloss']
                
                # Calculate proportions in each state
                sor[i,'pA1Q'] = sor[i,'pA1Qdelta']
                sor[i,'pA2Q'] = sor[i,'pA2Qdelta']
                sor[i,'pIQ'] = sor[i,'pIQ'] + sor[i,'pIQgain']
                sor[i,'pTotalQ'] = sor[i,'pIQ'] + sor[i,'pA1Q'] + sor[i,'pA2Q']  # total in all states
                
                sor[i,'pA1X'] = sor[i,'pA1Xdelta']
                sor[i,'pA2X'] = sor[i,'pA2Xdelta']
                sor[i,'pIX'] = sor[i,'pIX'] + sor[i,'pIXgain']
                sor[i,'pTotalX'] = sor[i,'pIX'] + sor[i,'pA1X'] + sor[i,'pA2X']  # total in all states
                
            } else {
                
                sor[i,'pIQ'] = sor[i-1,'pIQ']
                sor[i,'pIX'] = sor[i-1,'pIX']
                
                sor[i,'pA1Qgain'] = sor[i,'pIQ'] * sor[i,'p1Q'] # how mauch is activated from I
                sor[i,'pA1Xgain'] = sor[i,'pIX'] * sor[i,'p1X']
                
                sor[i,'pA1Q'] = sor[i-1,'pA1Q'] + sor[i,'pA1Qgain'] # how much is there in total with carried over
                sor[i,'pA1X'] = sor[i-1,'pA1X'] + sor[i,'pA1Xgain']
                
                sor[i,'pA1Qloss'] = sor[i,'pA1Q'] * sor[i,'pd1Q'] # how much decays to A2
                sor[i,'pA1Xloss'] = sor[i,'pA1X'] * sor[i,'pd1X']
                
                sor[i,'pA1Q'] = sor[i,'pA1Q'] - sor[i,'pA1Qloss'] # how much is left after the decay
                sor[i,'pA1X'] = sor[i,'pA1X'] - sor[i,'pA1Xloss']
                
                sor[i,'pA1Qdelta'] = sor[i,'pA1Qgain'] - sor[i,'pA1Qloss']
                sor[i,'pA1Xdelta'] = sor[i,'pA1Xgain'] - sor[i,'pA1Xloss']
                
                sor[i,'pA2Qgain'] = sor[i,'pA1Qloss'] # how much is gained from A1->A2 decay
                sor[i,'pA2Xgain'] = sor[i,'pA1Xloss']
                
                sor[i,'pA2Q'] = sor[i-1,'pA2Q'] + sor[i,'pA2Qgain'] # how much is there with carried over
                sor[i,'pA2X'] = sor[i-1,'pA2X'] + sor[i,'pA2Xgain']
                
                sor[i,'pA2Qloss'] = sor[i,'pA2Q'] * sor[i,'pd2Q'] # how much decays A2->I
                sor[i,'pA2Xloss'] = sor[i,'pA2X'] * sor[i,'pd2X']
                
                sor[i,'pA2Q'] = sor[i,'pA2Q'] - sor[i,'pA2Qloss'] # how much is left after A2->I decay
                sor[i,'pA2X'] = sor[i,'pA2X'] - sor[i,'pA2Xloss']
                
                sor[i,'pA2Qdelta'] = sor[i,'pA2Qgain'] - sor[i,'pA2Qloss']
                sor[i,'pA2Xdelta'] = sor[i,'pA2Xgain'] - sor[i,'pA2Xloss']
                
                sor[i,'pIQgain'] = sor[i,'pA2Qloss'] # how much decayed from A2
                sor[i,'pIXgain'] = sor[i,'pA2Xloss']
                
                sor[i,'pIQloss'] = sor[i,'pA1Qgain']
                sor[i,'pIXloss'] = sor[i,'pA1Xgain']
                
                sor[i,'pIQ'] = sor[i,'pIQ'] + sor[i,'pIQgain'] - sor[i,'pIQloss']
                sor[i,'pIX'] = sor[i,'pIX'] + sor[i,'pIXgain'] - sor[i,'pIXloss']
                
                sor[i,'pIQdelta'] = sor[i,'pIQgain'] - sor[i,'pIQloss']
                sor[i,'pIXdelta'] = sor[i,'pIXgain'] - sor[i,'pIXloss']
                
                sor[i,'pTotalQ'] = sor[i,'pIQ'] + sor[i,'pA1Q'] + sor[i,'pA2Q']
                sor[i,'pTotalX'] = sor[i,'pIX'] + sor[i,'pA1X'] + sor[i,'pA2X']

            }
        }

        # Generate Plots
        
        
        
        # Colour Palette Select
        
        if (input$sorplcolsch == 'Default'){
            col_q = 'red'
            col_x = 'blue'
        } else if (input$sorplcolsch == 'CB'){
            col_q = "#D55E00"
            col_x = "#009E73"
        } else if (input$sorplcolsch == 'BW'){
            col_q = "#000000"
            col_x = "#999999"
        }
        
        if (input$sorpltype == 'Activation'){
            
            ymax = max(c(sor[,'pA1Q'],sor[,'pA2Q'],sor[,'pIQ']))
            # ymin = min(c(sor[,'pA1Q'],sor[,'pA2Q'],sor[,'pIQ']))
            
            plot(0, xlim = c(0,nrow(sor)), bty = 'n', pch = '', ylim = c(-0.08,ymax), xlab = 'Time', ylab = 'Activation')
            
            # Plot activation lines
            if ('A1' %in% input$sorplinesQ){lines(sor[,'pA1Q'], col=col_q, lty = 1, lwd = 2)}
            if ('A2' %in% input$sorplinesQ){lines(sor[,'pA2Q'], col=col_q, lty = 5, lwd = 2)}
            if ('I' %in% input$sorplinesQ){lines(sor[,'pIQ'], col=col_q, lty = 3, lwd = 2)}
            if ('A1' %in% input$sorplinesX){lines(sor[,'pA1X'], col=col_x, lty = 1, lwd = 2)}
            if ('A2' %in% input$sorplinesX){lines(sor[,'pA2X'], col=col_x, lty = 5, lwd = 2)}
            if ('I' %in% input$sorplinesX){lines(sor[,'pIX'], col=col_x, lty = 3, lwd = 2)}
            
            # Plot Stimuli Durations
            if ('Q' %in% input$sorplinesStim){rect(input$sorQon1[1], -.03, input$sorQon1[2], -.02, border = col_q, lwd = 2, col = col_q)}
            if ('Q' %in% input$sorplinesStim){rect(input$sorQon2[1], -.03, input$sorQon2[2], -.02, border = col_q, lwd = 2, col = col_q)}
            if ('X' %in% input$sorplinesStim){rect(input$sorXon2[1], -.06, input$sorXon2[2], -.05, border = col_x, lwd = 2, col = col_x)}
            
            # Plot Stimuli Intensities
            if ('Q' %in% input$sorplinesInt){rect(input$sorQon1[1], 0, input$sorQon1[2], input$sorQp1, col = col_q, density = 10)}
            if ('Q' %in% input$sorplinesInt){rect(input$sorQon2[1], 0, input$sorQon2[2], input$sorQp1test, col = col_q, density = 10)}
            if ('X' %in% input$sorplinesInt){rect(input$sorXon2[1], 0, input$sorXon2[2], input$sorXp1test, col = col_x, density = 5)}

        } else if (input$sorpltype == 'Deltas'){
            
            ymax = max(c(sor[,'pA1Qdelta'],sor[,'pA2Qdelta'],sor[,'pIQdelta'],sor[,'pA1Xdelta'],sor[,'pA2Xdelta'],sor[,'pIXdelta']))
            ymin = min(c(sor[,'pA1Qdelta'],sor[,'pA2Qdelta'],sor[,'pIXdelta'],sor[,'pA1Xdelta'],sor[,'pA2Xdelta'],sor[,'pIXdelta']))
            
            plot(0, xlim = c(0,nrow(sor)), bty = 'n', pch = '', ylim = c(ymin,ymax), xlab = 'Time', ylab = 'Change in Activation')
            
            if ('A1' %in% input$sorplinesQ){lines(sor[,'pA1Qdelta'], col=col_q, lty = 1, lwd = 2)}
            if ('A2' %in% input$sorplinesQ){lines(sor[,'pA2Qdelta'], col=col_q, lty = 5, lwd = 2)}
            if ('I' %in% input$sorplinesQ){lines(sor[,'pIQdelta'], col=col_q, lty = 3, lwd = 2)}
            if ('A1' %in% input$sorplinesX){lines(sor[,'pA1Xdelta'], col=col_x, lty = 1, lwd = 2)}
            if ('A2' %in% input$sorplinesX){lines(sor[,'pA2Xdelta'], col=col_x, lty = 5, lwd = 2)}
            if ('I' %in% input$sorplinesX){lines(sor[,'pIXdelta'], col=col_x, lty = 3, lwd = 2)}
        }
        
        # Download data
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("soprmsim-sor-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
                write.csv(sor, file)
            })

        
        
        
        

    })

})
