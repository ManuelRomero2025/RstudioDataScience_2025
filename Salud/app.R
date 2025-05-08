library(shiny)
library(ggplot2)
library(readr)
library(janitor)
library(dplyr)

# Función para imputar moda
detectar_moda <- function(x) {
  moda <- names(which.max(table(x)))
  x[is.na(x)] <- moda
  return(x)
}

# Función para reemplazar outliers con NA
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  return(x)
}

# Cargar y limpiar datos
heart_disease <- read_csv("heart_disease.csv")
Data.1 <- clean_names(as.data.frame(unclass(heart_disease), stringsAsFactors = TRUE))

# Detectar tipos de variables
numeric_vars <- names(Data.1)[sapply(Data.1, is.numeric)]
categorical_vars <- names(Data.1)[sapply(Data.1, is.factor)]

# Limpieza
Data.1[, numeric_vars] <- lapply(Data.1[, numeric_vars], remove_outliers)
Data.1[, numeric_vars] <- lapply(Data.1[, numeric_vars], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
Data.1[, categorical_vars] <- lapply(Data.1[, categorical_vars], detectar_moda)

# UI
ui <- fluidPage(
  titlePanel("Visualización y Análisis de Variables"),
  
  tabsetPanel(
    
    tabPanel("Gráficos Cualitativos",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_cat", "Seleccione variable cualitativa:", 
                             choices = categorical_vars, selected = categorical_vars[1])
               ),
               mainPanel(
                 h4("Gráfico de Barras"),
                 plotOutput("bar_plot"),
                 h4("Gráfico de Sectores"),
                 plotOutput("pie_chart")
               )
             )
    ),
    
    tabPanel(" Cuantitativas (Sin Agrupación)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_num_solo", "Seleccione variable cuantitativa:", 
                             choices = numeric_vars, selected = numeric_vars[1])
               ),
               mainPanel(
                 h4("Histograma"),
                 plotOutput("histograma_solo"),
                 h4("Densidad"),
                 plotOutput("densidad_solo"),
                 h4("Boxplot"),
                 plotOutput("boxplot_solo"),
                 h4("Tabla Resumen (No Agrupada)"),
                 tableOutput("tabla_resumen_solo")
               )
             )
    ),
    
    tabPanel("Cuantitativas por Grupo",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_num", "Seleccione variable cuantitativa:", 
                             choices = numeric_vars, selected = numeric_vars[1]),
                 selectInput("group_var", "Agrupar por variable cualitativa:",
                             choices = categorical_vars, selected = categorical_vars[1])
               ),
               mainPanel(
                 h4("Histograma por Grupo"),
                 plotOutput("histograma"),
                 h4("Densidad por Grupo"),
                 plotOutput("densidad"),
                 h4("Boxplot por Grupo"),
                 plotOutput("boxplot"),
                 h4("Tabla Resumen por Grupo"),
                 tableOutput("tabla_resumen_grupo")
               )
             )
    ),
    
    tabPanel("Diagrama de Correlación",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "Seleccione variable X:", 
                             choices = numeric_vars, selected = numeric_vars[1]),
                 selectInput("y_var", "Seleccione variable Y:", 
                             choices = numeric_vars, selected = numeric_vars[2]),
                 selectInput("color_var", "Variable cualitativa:",
                             choices = c("Ninguna", categorical_vars), selected = "Ninguna")
               ),
               mainPanel(
                 h4("Gráfico de Dispersión con Recta de Regresión"),
                 plotOutput("correlacion_plot"),
                 h4(" Resumen del Modelo Lineal"),
                 verbatimTextOutput("resumen_lm"),
                 h4(" Tabla de Residuos del Modelo"),
                 tableOutput("tabla_residuos")
               )
             )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Gráficos cualitativos
  output$bar_plot <- renderPlot({
    data_bar <- Data.1 %>%
      count(!!sym(input$var_cat)) %>%
      mutate(label = paste0(n))
    
    ggplot(data_bar, aes_string(x = input$var_cat, y = "n", fill = input$var_cat)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label), vjust = -0.5, size = 5) +
      theme_minimal()
  })
  
  output$pie_chart <- renderPlot({
    data_pie <- Data.1 %>%
      count(!!sym(input$var_cat)) %>%
      mutate(pct = round(n / sum(n) * 100, 1),
             label = paste0(pct, "%"))
    
    ggplot(data_pie, aes(x = "", y = n, fill = !!sym(input$var_cat))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
      theme_void()
  })
  
  # Cuantitativas sin agrupación
  output$histograma_solo <- renderPlot({
    ggplot(Data.1, aes_string(x = input$var_num_solo)) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30) +
      geom_text(stat = "bin", aes(label = ..count..), vjust = -0.5) +
      theme_minimal()
  })
  
  output$densidad_solo <- renderPlot({
    ggplot(Data.1, aes_string(x = input$var_num_solo)) +
      geom_density(fill = "skyblue", alpha = 0.5) +
      theme_minimal()
  })
  
  output$boxplot_solo <- renderPlot({
    ggplot(Data.1, aes_string(y = input$var_num_solo)) +
      geom_boxplot(fill = "salmon", width = 0.3) +
      theme_minimal()
  })
  
  output$tabla_resumen_solo <- renderTable({
    datos <- Data.1[[input$var_num_solo]]
    data.frame(
      Variable = input$var_num_solo,
      Media = round(mean(datos, na.rm = TRUE), 2),
      Mediana = round(median(datos, na.rm = TRUE), 2),
      Desviación_Estándar = round(sd(datos, na.rm = TRUE), 2)
    )
  })
  
  # Cuantitativas por grupo
  output$histograma <- renderPlot({
    ggplot(Data.1, aes_string(x = input$var_num, fill = input$group_var)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30, color = "white") +
      theme_minimal()
  })
  
  output$densidad <- renderPlot({
    ggplot(Data.1, aes_string(x = input$var_num, color = input$group_var, fill = input$group_var)) +
      geom_density(alpha = 0.4) +
      theme_minimal()
  })
  
  output$boxplot <- renderPlot({
    ggplot(Data.1, aes_string(x = input$group_var, y = input$var_num, fill = input$group_var)) +
      geom_boxplot() +
      theme_minimal()
  })
  
  output$tabla_resumen_grupo <- renderTable({
    Data.1 %>%
      group_by(.data[[input$group_var]]) %>%
      summarise(
        Media = round(mean(.data[[input$var_num]], na.rm = TRUE), 2),
        Mediana = round(median(.data[[input$var_num]], na.rm = TRUE), 2),
        Desviación_Estándar = round(sd(.data[[input$var_num]], na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      rename(Grupo = !!input$group_var)
  })
  
  # Correlación con opción de color
  output$correlacion_plot <- renderPlot({
    req(input$x_var, input$y_var)
    x <- Data.1[[input$x_var]]
    y <- Data.1[[input$y_var]]
    modelo <- lm(y ~ x)
    coef <- coef(modelo)
    ecuacion <- paste0("y = ", round(coef[1], 2), " + ", round(coef[2], 2), "x")
    r <- round(cor(x, y), 2)
    
    if (input$color_var == "Ninguna") {
      ggplot(Data.1, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "darkblue", alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        annotate("text", 
                 x = min(x, na.rm = TRUE), 
                 y = max(y, na.rm = TRUE), 
                 label = paste(ecuacion, "\n r =", r),
                 hjust = 0, vjust = 1, size = 5, color = "black") +
        theme_minimal()
    } else {
      ggplot(Data.1, aes_string(x = input$x_var, y = input$y_var, color = input$color_var)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        annotate("text", 
                 x = min(x, na.rm = TRUE), 
                 y = max(y, na.rm = TRUE), 
                 label = paste(ecuacion, "\n r =", r),
                 hjust = 0, vjust = 1, size = 5, color = "black") +
        theme_minimal()
    }
  })
  
  output$resumen_lm <- renderPrint({
    req(input$x_var, input$y_var)
    modelo <- lm(Data.1[[input$y_var]] ~ Data.1[[input$x_var]])
    summary(modelo)
  })
  
  output$tabla_residuos <- renderTable({
    req(input$x_var, input$y_var)
    modelo <- lm(Data.1[[input$y_var]] ~ Data.1[[input$x_var]])
    
    data.frame(
      x = Data.1[[input$x_var]],
      y = Data.1[[input$y_var]],
      Predicho = round(fitted(modelo), 2),
      Residuo = round(residuals(modelo), 2)
    ) |> head(20)
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)














