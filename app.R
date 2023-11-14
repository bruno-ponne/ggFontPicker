library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(showtext)


# Load necessary data
df <- read_tsv("wa_coroners_inquests_v1-1.tsv")
font_list <- read_csv("google-web-font-list.csv")

# Cleaning the dataset and preparing the data

df_prep <- df %>% 
  filter(gender %in% c("f", "m"), verdict != "-") %>% 
  mutate(verdict = recode(verdict, "suicide (delirious)" = "suicide",
                          "suicide (felo de se)" = "suicide",
                          "suicide (insane)" = "suicide"))

table_ver <- data.frame(table(df_prep$verdict))

wc <- df_prep %>%
  filter(!is.na(cause_of_death)) %>% 
  group_by(cause_of_death) %>%
  tally() %>% 
  filter(n>20)




ui <- page_sidebar(
  title = "ggFontPicker",
  theme = bs_theme(bootswatch = "minty"),
  sidebar = sidebar(
    selectInput("font_category", "Font Type", unique(font_list$items__category), selected = "sans-serif"),
    hr(), # Add a horizontal rule
    selectInput("font_family", "Font Family", font_list$items__family, selected = "Montserrat"),
    hr(),
    sliderInput("font_size",
                " Font Size",
                min = 8,  max = 24, value = 12),
    hr(), # Add a horizontal rule
    
    radioButtons("radio", "Plot",
                 choices = list("Verdict" = 1, "Causes of Death" = 2), selected = 1)

  ),
  plotOutput("bar_plot"),
  hr(),
  span(
  print("Notes:"),
  br(),
  print("- These data come from a catalogue of inquests conducted in Westminster between 1760 and 1799. They document investigations into deaths under circumstances that were sudden, unexplained, or suspicious. Provided under a Creative Commons Attribution-ShareAlike 4.0 International License. Source: Sharon Howard, A Catalogue of Westminster Coroners' Inquests 1760-1799, version 2.0 (2018), based on data from www.londonlives.org."),
  br(),
  br(),
  print("- Google Fonts are accessible in R thanks to this package: https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html"), 
  style="font-size:12px;")
)   


server <- function(input, output, session) {
  
  subsetted <- reactive({
    req(input$font_category)
    font_list %>% 
      filter(items__category %in% input$font_category) %>% 
      pull(items__family)
  })
  
  observe({
    
    updateSelectInput(session, "font_family", label = "Font Family", choices = subsetted(),  selected = subsetted()[1])
    
  })

  
  output$bar_plot <- renderPlot({
    
    font_add_google(input$font_family)
    
    showtext_auto()
    
    if (input$radio == 1) {
      
      p <-  ggplot(table_ver) +
        geom_bar(aes(x = reorder(Var1,-Freq, sum), y = Freq), 
                 width = .7, stat = "identity",color = "black", fill = "black") +
        labs(title = "Jury's Veredicts", 
             y = "Frequency", 
             x = "Verdict",
             subtitle = "Westminster Coroners' Inquests 1760-1799")
    }
    
    if (input$radio == 2) {
      
      p <-  ggplot(wc) +
        geom_bar(aes(y = reorder(cause_of_death,n, sum), x = n), 
                 width = .5, stat = "identity",color = "black", fill = "black") +
        labs(title = "Causes of death",
             y = "Cause of Death", 
             x = "Frequency",
             subtitle = "Westminster Coroners' Inquests 1760-1799")
        
    }
    

    p +
      theme_bw() +
      theme(text=element_text(size=input$font_size, 
                              family = input$font_family))
  }, res = 100)
  
  # Solves problem that makes the page go idle every 55 seconds.
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
}

shinyApp(ui, server)
