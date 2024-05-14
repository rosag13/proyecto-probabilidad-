output$histograma_salarios <- renderPlot({
  gemelo_elegido <- switch(input$gemelo_selector_graficos,
                           "Gemelo 1" = gemelos$HRWAGEL,
                           "Gemelo 2" = gemelos$HRWAGEH)
  ggplot(data = data.frame(salario = gemelo_elegido), aes(x = salario)) +
    geom_histogram(binwidth = 1, fill = ifelse(input$gemelo_selector_graficos == "Gemelo 1", "skyblue", "salmon"), color = "black", alpha = 0.5) +
    geom_freqpoly(binwidth = 1, color = "red") +
    labs(x = "Salario por hora", y = "Frecuencia", title = paste("Histograma de Salarios de", input$gemelo_selector_graficos))
})