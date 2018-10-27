library(shiny)


shinyServer(function(input, output) {
        
        library(reshape)
        library(ggplot2)
        library(forecast)
        library(gtools)
        library(plotly)
        
        data("co2")
        temp <- read.csv(url("https://pkgstore.datahub.io/core/global-temp/annual_csv/data/a26b154688b061cdd04f1df36e4408be/annual_csv.csv"))
        temp <- temp[temp$Year < 1998 & temp$Year > 1958,]
        temp <- cast(temp, Year ~ Source)
        
        M <- matrix(co2, ncol = 12, byrow = TRUE)
        temp$co2 <- rowMeans(M)
        
        ex_fit <- ets(ts(temp$co2, start = 1959, end = 1997, frequency = 1))
        
        temp_fit <- lm(GISTEMP ~ co2, data = temp)
        temp_fit2 <- lm(GCAG ~ co2, data = temp)

        fcast <- reactive({forecast(ex_fit, h= input$years)})
        
        output$co2forecast <-  renderPlot({autoplot(fcast()) + labs(x = "Years", y = "co2 concentrate (ppm)", title = "co2 (ppm) over the years")})

        
        output$GCAGplot <- renderPlot({
                frcast <- fcast()
                predicted_point_mean <- predict(temp_fit, data.frame(co2 = frcast$mean))
                predicted_point2_mean <- predict(temp_fit2, data.frame(co2 = frcast$mean))
                
                predicted_point_lower80 <- predict(temp_fit, data.frame(co2 = frcast$lower[,1]))
                predicted_point2_lower80 <- predict(temp_fit2, data.frame(co2 = frcast$lower[,1]))
                predicted_point_lower90 <- predict(temp_fit, data.frame(co2 = frcast$lower[,2]))
                predicted_point2_lower90 <- predict(temp_fit2, data.frame(co2 = frcast$lower[,2]))
                
                predicted_point_upper80 <- predict(temp_fit, data.frame(co2 = frcast$upper[,1]))
                predicted_point2_upper80 <- predict(temp_fit2, data.frame(co2 = frcast$upper[,1]))
                
                predicted_point_upper90 <- predict(temp_fit, data.frame(co2 = frcast$upper[,2]))
                predicted_point2_upper90 <- predict(temp_fit2, data.frame(co2 = frcast$upper[,2]))
                
                temp_predict_mean <- data.frame(Year = 1997 + 1:input$years, 
                                                GCAG_mean = predicted_point2_mean, 
                                                GISTEMP_mean = predicted_point_mean, 
                                                co2 = frcast$mean)
                
                temp_predict_lower80 <- data.frame(Year = 1997 + 1:input$years, 
                                                   GCAG_lower80 = predicted_point2_lower80, 
                                                   GISTEMP_lower80 = predicted_point_lower80, 
                                                   co2 = frcast$lower[,1])
                
                temp_predict_lower90 <- data.frame(Year = 1997 + 1:input$years,
                                                GCAG_lower90 = predicted_point2_lower90, 
                                                GISTEMP_lower90 = predicted_point_lower90, 
                                                co2 = frcast$lower[,2])
                
                temp_predict_upper80 <- data.frame(Year = 1997 + 1:input$years,
                                                        GCAG_upper80 = predicted_point2_upper80, 
                                                        GISTEMP_upper80 = predicted_point_upper80, 
                                                        co2 = frcast$upper[,1])
                
                temp_predict_upper90 <- data.frame(Year = 1997 + 1:input$years,
                                                        GCAG_upper90 = predicted_point2_upper90, 
                                                        GISTEMP_upper90 = predicted_point_upper90, 
                                                        co2 = frcast$upper[,2])
                
                temp_complete <- smartbind(temp, 
                                           temp_predict_mean, 
                                           temp_predict_lower80,
                                           temp_predict_upper80,
                                           temp_predict_lower90,
                                           temp_predict_upper90)
        
                ggplot(data = temp_complete) + 
                        geom_line(aes(x = Year, y = GCAG), color = "red") + 
                        geom_point(aes(x = Year, y = GCAG_mean), color = "red", alpha = 0.9) +
                        geom_point(aes(x = Year, y = GCAG_lower80), color = "red", alpha = 0.5) +
                        geom_point(aes(x = Year, y = GCAG_upper80), color = "red", alpha = 0.5) +
                        geom_point(aes(x = Year, y = GCAG_lower90), color = "red", alpha = 0.2) +
                        geom_point(aes(x = Year, y = GCAG_upper90), color = "red", alpha = 0.2) +
                        labs(x = "Year", y = "Average Global Temperatur", title = "Global Temperature from 1958 tot 1998 - GCAG measurements")
        })
        
        
        output$GISTEMPplot <- renderPlot({
                frcast <- fcast()
                predicted_point_mean <- predict(temp_fit, data.frame(co2 = frcast$mean))
                predicted_point2_mean <- predict(temp_fit2, data.frame(co2 = frcast$mean))
                
                predicted_point_lower80 <- predict(temp_fit, data.frame(co2 = frcast$lower[,1]))
                predicted_point2_lower80 <- predict(temp_fit2, data.frame(co2 = frcast$lower[,1]))
                predicted_point_lower90 <- predict(temp_fit, data.frame(co2 = frcast$lower[,2]))
                predicted_point2_lower90 <- predict(temp_fit2, data.frame(co2 = frcast$lower[,2]))
                
                predicted_point_upper80 <- predict(temp_fit, data.frame(co2 = frcast$upper[,1]))
                predicted_point2_upper80 <- predict(temp_fit2, data.frame(co2 = frcast$upper[,1]))
                
                predicted_point_upper90 <- predict(temp_fit, data.frame(co2 = frcast$upper[,2]))
                predicted_point2_upper90 <- predict(temp_fit2, data.frame(co2 = frcast$upper[,2]))
                
                temp_predict_mean <- data.frame(Year = 1997 + 1:input$years, 
                                                          GCAG_mean = predicted_point2_mean, 
                                                          GISTEMP_mean = predicted_point_mean, 
                                                          co2 = frcast$mean)
                
                temp_predict_lower80 <- data.frame(Year = 1997 + 1:input$years, 
                                                             GCAG_lower80 = predicted_point2_lower80, 
                                                             GISTEMP_lower80 = predicted_point_lower80, 
                                                             co2 = frcast$lower[,1])
                
                temp_predict_lower90 <- data.frame(Year = 1997 + 1:input$years,
                                                             GCAG_lower90 = predicted_point2_lower90, 
                                                             GISTEMP_lower90 = predicted_point_lower90, 
                                                             co2 = frcast$lower[,2])
                
                temp_predict_upper80 <- data.frame(Year = 1997 + 1:input$years,
                                                             GCAG_upper80 = predicted_point2_upper80, 
                                                             GISTEMP_upper80 = predicted_point_upper80, 
                                                             co2 = frcast$upper[,1])
                
                temp_predict_upper90 <- data.frame(Year = 1997 + 1:input$years,
                                                             GCAG_upper90 = predicted_point2_upper90, 
                                                             GISTEMP_upper90 = predicted_point_upper90, 
                                                             co2 = frcast$upper[,2])
                
                temp_complete <- smartbind(temp, 
                                                     temp_predict_mean, 
                                                     temp_predict_lower80,
                                                     temp_predict_upper80,
                                                     temp_predict_lower90,
                                                     temp_predict_upper90)
                
                ggplot(data = temp_complete) + 
                        geom_line(aes(x = Year, y = GISTEMP), color = "blue") + 
                        geom_point(aes(x = Year, y = GISTEMP_mean), color = "blue", alpha = 0.9) +
                        geom_point(aes(x = Year, y = GISTEMP_lower80), color = "blue", alpha = 0.5) +
                        geom_point(aes(x = Year, y = GISTEMP_upper80), color = "blue", alpha = 0.5) +
                        geom_point(aes(x = Year, y = GISTEMP_lower90), color = "blue", alpha = 0.2) +
                        geom_point(aes(x = Year, y = GISTEMP_upper90), color = "blue", alpha = 0.2) +
                        labs(x = "Year", y = "Average Global Temperatur", title = "Global Temperature from 1958 tot 1998 - GISTEMP measurements")
        })
})

