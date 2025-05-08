library(shiny)
library(ggplot2)
library(readr)
library(janitor)
library(corrplot)
library(naniar)

# Cargar los datos y limpiarlos
heart_disease <- read_csv("heart_disease.csv")  # Ruta al archivo
Data.1 <- as.data.frame(unclass(heart_disease), stringsAsFactors = TRUE)
Data.1 <- clean_names(Data.1)

# Función para imputar moda
detectar_moda <- function(x) {
  moda <- names(which.max(table(x)))
  x[is.na(x)] <- moda
  return(x)
}

# Outlier handling
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  return(x)
}

# Procesamiento
numeric_vars <- sapply(Data.1, is.numeric)
Data.1[, numeric_vars] <- lapply(Data.1[, numeric_vars], remove_outliers)
Data.1[, numeric_vars] <- lapply(Data.1[, numeric_vars], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

categorical_vars <- sapply(Data.1, is.factor)
Data.1[, categorical_vars] <- lapply(Data.1[, categorical_vars], detectar_moda)

# Interfaz UI
ui <- fluidPage(
  titlePanel("Análisis Exploratorio: Enfermedad Cardíaca"),
  sidebarLayout(
    sidebarPanel(
      selectInput("genero", "Filtrar por género:", choices = c("Todos", levels(Data.1$gender)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Edad",
                 plotOutput("histEdad"),
                 plotOutput("densidadEdad")
        ),
        tabPanel("Género", plotOutput("barGenero")),
        tabPanel("Enfermedad Cardíaca", 
                 plotOutput("boxEdadEnf"),
                 plotOutput("propGeneroEnf")
        ),
        tabPanel("Presión vs Colesterol", plotOutput("scatterPresCol")),
        tabPanel("Correlaciones", plotOutput("heatmapCorr")),
        tabPanel("Ejercicio", plotOutput("edadEjercicio")),
        tabPanel("Estrés", plotOutput("estresEnf")),
        tabPanel("LDL vs HDL", plotOutput("colesterol"))
      )
    )
  )
)

# Servidor
server <- function(input, output) {
  data_filtrada <- reactive({
    if (input$genero == "Todos") {
      Data.1
    } else {
      subset(Data.1, gender == input$genero)
    }
  })
  
  output$histEdad <- renderPlot({
    ggplot(data_filtrada(), aes(x = age)) +
      geom_histogram(bins = 10, fill = "steelblue", color = "white") +
      labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
      theme_minimal()
  })
  
  output$densidadEdad <- renderPlot({
    ggplot(data_filtrada(), aes(x = age, fill = gender)) +
      geom_density(alpha = 0.5) +
      labs(title = "Densidad de Edad por Género", x = "Edad", y = "Densidad") +
      theme_minimal()
  })
  
  output$barGenero <- renderPlot({
    ggplot(data_filtrada(), aes(x = gender, fill = gender)) +
      geom_bar() +
      labs(title = "Distribución por Género", x = "Género", y = "Conteo") +
      theme_minimal()
  })
  
  output$boxEdadEnf <- renderPlot({
    ggplot(data_filtrada(), aes(x = heart_disease_status, y = age, fill = heart_disease_status)) +
      geom_boxplot() +
      labs(title = "Edad según Estado de Enfermedad", x = "Estado", y = "Edad") +
      theme_minimal()
  })
  
  output$propGeneroEnf <- renderPlot({
    ggplot(data_filtrada(), aes(x = gender, fill = heart_disease_status)) +
      geom_bar(position = "fill") +
      labs(title = "Proporción de Enfermedad por Género", x = "Género", y = "Proporción") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal()
  })
  
  output$scatterPresCol <- renderPlot({
    ggplot(data_filtrada(), aes(x = blood_pressure, y = cholesterol_level, color = heart_disease_status)) +
      geom_point(alpha = 0.7) +
      labs(title = "Presión Arterial vs Colesterol", x = "Presión", y = "Colesterol") +
      theme_minimal()
  })
  
  output$heatmapCorr <- renderPlot({
    cor_mat <- cor(data_filtrada()[, sapply(data_filtrada(), is.numeric)])
    corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8)
  })
  
  output$edadEjercicio <- renderPlot({
    ggplot(data_filtrada(), aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "darkorange", color = "white") +
      facet_wrap(~ exercise_habits) +
      labs(title = "Edad por Hábito de Ejercicio", x = "Edad", y = "Frecuencia") +
      theme_minimal()
  })
  
  output$estresEnf <- renderPlot({
    ggplot(data_filtrada(), aes(x = stress_level, fill = heart_disease_status)) +
      geom_bar(position = "dodge") +
      labs(title = "Estrés vs Enfermedad Cardíaca", x = "Nivel de Estrés", y = "Frecuencia") +
      theme_minimal()
  })
  
  output$colesterol <- renderPlot({
    ggplot(data_filtrada(), aes(x = high_ldl_cholesterol, fill = low_hdl_cholesterol)) +
      geom_bar(position = "dodge") +
      labs(title = "LDL Alto vs HDL Bajo", x = "LDL Alto", y = "Frecuencia", fill = "HDL Bajo") +
      theme_minimal()
  })
}

# Ejecutar App
shinyApp(ui = ui, server = server)
