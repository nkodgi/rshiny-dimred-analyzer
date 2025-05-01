#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(pracma)  
library(elasticnet) 
library(MASS) 
library(kernlab)
library(Rtsne)
library(tidyverse)

#fixes conflicts
library(conflicted)
conflict_prefer("select", "dplyr")

# define UI
ui <- navbarPage (
  
  # app title
  "Interactive Interface for Dimensionality Reduction and Statistical Analysis",
  
  #page 1 to upload/visualize data
  tabPanel("Upload Data",
           fileInput('upload', NULL, buttonLabel = "Upload", 
                     multiple = FALSE, accept = '.csv'),
           tableOutput('files')
           ),
  
  #page 2 to visualize dimensionality reduction
  tabPanel("Dimensionality Reduction",
           sidebarLayout(
             sidebarPanel(
               radioButtons('redMethod', 'Dimensionality Reduction Method',
                            c("PCA", "Sparse PCA", "Kernel PCA", "MDS", "tSNE")),
               downloadButton("downloadData", "Download")
             ),
             mainPanel(plotOutput('dimredPlot'), 
                       uiOutput("dimredText"))
           )
           ),
  
  #page 3 for statistical analysis
  tabPanel(
    "Statistical Analysis",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          'statMethod',
          'Statistical Method',
          c("t-test", "confidence interval")
        ),
        uiOutput("varSelectUI")
      ),
      mainPanel(
        verbatimTextOutput('statResult'),
        plotOutput('statPlot')
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data <- reactive({
    req(input$upload)
    read.csv(input$upload$datapath, stringsAsFactors = FALSE)
  })
  
  output$files <- renderTable({
    
    #reads in the upload data
    data_temp <- input$upload
    
    #ensures that the data is required before doing anything of value
    req(data_temp)
    
    read.csv(data_temp$datapath)
  })
  
  dimred_data <- reactive({
    df <- data()
    
    cancer_data <- df %>%
      select(where(is.numeric)) %>%
      select(-contains("Unnamed")) %>%
      na.omit()
    cancer_data <- scale(cancer_data)
    
    #runs dimensionality reduction
    #PCA
    if (input$redMethod == "PCA") {
      result <- prcomp(cancer_data, center = TRUE, scale. = TRUE)
    }
    #Sparse PCA
    else if (input$redMethod == "Sparse PCA") {
      spca_res <- spca(cancer_data,
                       K      = 2,
                       type   = "predictor",
                       sparse = "varnum",
                       para   = c(10, 10))
      
      sparse_scores <- a
      result <- as.matrix(cancer_data) %*% spca_res$loadings
    }
    #Kernel PCA
    else if (input$redMethod == "Kernel PCA") {
      kpca_res <- kpca(~ ., data = as.data.frame(cancer_data),
                       kernel   = "rbfdot",
                       kpar     = list(sigma = 0.1),
                       features = 2)
      result <- rotated(kpca_res)
    }
    #MDS
    else if (input$redMethod == "MDS") {
      # euclidean distances
      dist_mat <- dist(cancer_data)
      
      # k = 2
      result <- cmdscale(dist_mat, k = 2)
    }
    #tSNE
    else if (input$redMethod == "tSNE") {
      set.seed(42)  
      result <- Rtsne(cancer_data,
                        dims       = 2,      
                        perplexity = 30,    
                        verbose    = TRUE,
                        max_iter   = 1000)
    }
    
    return(result)
    
  })
  
  classification <- reactive({
    factor(data()$diagnosis)
  })
  
  output$dimredPlot <- renderPlot({
    
    req(input$upload)
    
    if (input$redMethod == "PCA") {
      # plot
      plot(dimred_data()$x[,1:2], col = as.factor(classification()),
           main = "PCA Plot",
           xlab = "PC1", ylab = "PC2", pch = 16)
      legend("topright", legend = levels(classification()),
             col = seq_along(levels(classification())), pch = 16)
    } 
  
    else if (input$redMethod == "Sparse PCA") {
      # plot
      plot(dimred_data()[,1], dimred_data()[,2],
           col   = classification(),
           pch   = 16,
           main  = "Sparse PCA (k = 2)",
           xlab  = "Sparse PC1",
           ylab  = "Sparse PC2")
      legend("topright", legend = levels(classification()),
             col = seq_along(levels(classification())), pch = 16)
    }
  
    else if (input$redMethod == "Kernel PCA") {
      # plot first two kernel PCs
      plot(dimred_data()[,1], dimred_data()[,2],
           col   = classification(),
           pch   = 16,
           xlab  = "KPC1",
           ylab  = "KPC2",
           main  = "Kernel PCA (RBF Kernel)")
      legend("topright",
             legend = levels(classification()),
             col    = seq_along(levels(classification())),
             pch    = 16)
      
    }
  
    else if (input$redMethod == "MDS") {
      # plot
      plot(dimred_data()[,1], dimred_data()[,2],
           col   = classification(),
           pch   = 16,
           xlab  = "Dim 1",
           ylab  = "Dim 2",
           main  = "Classical MDS (Euclidean)")
      legend("topright",
             legend = levels(classification()),
             col    = seq_along(levels(classification())),
             pch    = 16)
    }
  
    else if (input$redMethod == "tSNE") {
      # plot 
      plot(dimred_data()$Y,
           col   = classification(),
           pch   = 16,
           xlab  = "t-SNE 1",
           ylab  = "t-SNE 2",
           main  = "t-SNE Projection")
      legend("topright",
             legend = levels(classification()),
             col    = seq_along(levels(classification()),
             pch    = 16))
    }
  
  })
  
  output$dimredText <- renderUI({
    if (input$redMethod == "PCA") {
      p("PCA is a linear technique that finds orthogonal directions (principal components) that capture the greatest variance in the data, projecting high‑dimensional data into a lower‑dimensional subspace while preserving as much variability as possible.")
    }
    
    else if (input$redMethod == "Sparse PCA") {
      p("Sparse PCA is a variant of PCA that introduces sparsity constraints on the loading vectors, outputting principal components that depend on only a small subset of the original features. This is especially useful for interpretability and feature selection.")
    } 
    
    else if (input$redMethod == "Kernel PCA") {
      p("Kernel PCA is an extension of PCA that first maps data nonlinearly into a high‑dimensional feature space via a kernel function, then performs PCA in that space to capture nonlinear structure in the original data.")
    } 
    
    else if (input$redMethod == "MDS") {
      p("Multidimensional scaling (MDS) distance‑based method that looks for low‑dimensional embedding whose pairwise distances approximate the original distances between samples.
")
    } 
    
    else if (input$redMethod == "tSNE") {
      p("T-Distributed stochastic neighbor embedding (tSNE) is a nonlinear technique that models pairwise similarities in high‑dimensional space as probabilities, then finds a low‑dimensional embedding to minimize the divergence between those and the corresponding low‑dimensional similarities. This makes it especially effective at revealing local clusters.
")
    } 
    
  })
  
  output$downloadData <- downloadHandler(
    
    #sets the download file name to the dimensionality reduction method
    filename = function() {
      paste0(input$redMethod, ".csv")
    },
    
    #writes the data to a file
    content = function(file) {
      write.csv(dimred_data(), file)
    }
    
  )
  
  # Dynamically generate input widgets
  output$varSelectUI <- renderUI({
    req(data())
    df <- data()
    cols <- names(df)[sapply(df, is.numeric)]
    
    if (input$statMethod == "t-test") {
      tagList(
        selectInput("numVar1", "Numerical Variable 1", choices = cols),
        selectInput("numVar2", "Numerical Variable 2", choices = cols)
      )
    } else if (input$statMethod == "confidence interval") {
      selectInput("confVar", "Numerical Variable", choices = cols)
    }
  })
  
  output$statResult <- renderPrint({
    df <- data()
    
    if (input$statMethod == "t-test") {
      req(input$numVar1, input$numVar2)
      num1 <- df[[input$numVar1]]
      num2 <- df[[input$numVar2]]
      cat("t-test of ", input$numVar1, "and ", input$numVar2, ":\n")
      print(t.test(num1, num2))
      
    } else if (input$statMethod == "confidence interval") {
      req(input$confVar)
      x <- df[[input$confVar]]
      ci <- t.test(x)
      cat("95% Confidence Interval for the mean of", input$confVar, ":\n")
      print(ci$conf.int)
    }
  })
  
  output$statPlot <- renderPlot({
    df <- data()
    
    if (input$statMethod == "t-test") {
      req(input$numVar1, input$numVar2)
      
      plot_data <- df %>%
        select(diagnosis, all_of(c(input$numVar1, input$numVar2))) %>%
        pivot_longer(cols = -diagnosis, names_to = "variable", values_to = "value") %>%
        mutate(diagnosis = factor(diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")))
      
      ggplot(plot_data, aes(x = variable, y = value, fill = diagnosis)) +
        geom_boxplot(width = 0.6, outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +
        labs(
          title = paste0("Boxplots of ", input$numVar1, " and ", input$numVar2, " by Diagnosis"),
          x = "Variable",
          y = "Value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.title.y = element_text(face = "bold"),
          axis.text = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        ) +
        scale_fill_brewer(palette = "Set2")
      
    } else if (input$statMethod == "confidence interval") {
      req(input$confVar)
      
      plot_data <- data.frame(
        value = df[[input$confVar]],
        diagnosis = factor(df$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
      )
      
      summary_df <- plot_data %>%
        group_by(diagnosis) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          sd_value = sd(value, na.rm = TRUE),
          n = n(),
          .groups = 'drop'
        ) %>%
        mutate(
          error = 1.96 * sd_value / sqrt(n),
          lower = mean_value - error,
          upper = mean_value + error
        )
      
      ggplot(summary_df, aes(x = diagnosis, y = mean_value, fill = diagnosis)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
        labs(
          title = paste0("Mean and 95% CI of ", input$confVar, " by Diagnosis"),
          x = "Diagnosis",
          y = paste("Mean", input$confVar)
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.title.y = element_text(face = "bold"),
          axis.text = element_text(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        ) +
        scale_fill_brewer(palette = "Set2") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
    }
  })
} # <- this closes the `server` function properly!

# Run the app
shinyApp(ui = ui, server = server)
