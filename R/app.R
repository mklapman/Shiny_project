#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(foreign)
library(tidyverse)
library(grid)
library(gridExtra)
library(here)

pew_surv <- read.spss(here("DATA/Typology 17 public.sav"))
pew <- as.tibble(pew_surv) %>%
  filter(age != "Don't know/Refused (VOL.)") %>%
  filter(party %in% c("Republican", "Democrat", "Independent")) %>%
  mutate(agegroup = ifelse(age %in% as.factor(18:29), "18-29", 
                           ifelse(age %in% as.factor(30:59), "30-59", 
                                  ifelse(age %in% as.factor(c(60:96, "97 or older")), "60+",
                                         NA))),
         agegroup = as.factor(agegroup))
pew_info <- as.tibble(pew_surv)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("United States Public Opinion of Parties and Leaders"),
  sidebarLayout(
    sidebarPanel( width=3,
                  "Subset the survey data to view a population of interest",
                  "The default (no checks) produces the whole sample.",
                  checkboxGroupInput("agegroup", "Age Group:", 
                                     choices = levels(pew$agegroup),
                                     hr()),
                  checkboxGroupInput("sex", "Sex:",
                                     choices = levels(pew$sex),
                                     hr()),
                  checkboxGroupInput("cregion", "Region:",
                                     choices = levels(pew$cregion),
                                     hr()),
                  checkboxGroupInput("party", "Party:",
                                     choices = c("Republican", "Democrat", "Independent"),
                                     hr()),
                  "Data obtained from June 2017 Pew Research Center Survey"
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    age. <- input$agegroup
    if (is.null(age.)){
      age. <- levels(pew$agegroup)
    }
    sex. <- input$sex
    if (is.null(sex.)){
      sex. <- levels(pew$sex)
    }
    region. <- input$cregion
    if (is.null(region.)){
      region. <- levels(pew$cregion)
    }
    party. <- input$party
    if (is.null(party.)){
      party. <- c("Republican", "Democrat", "Independent")
    }
    
    
    # draw the histogram with the specified number of bins
    new_pew <- pew %>%
      filter(agegroup %in% age.) %>%
      filter(sex %in% sex.) %>%
      filter(party %in% party.) %>%
      filter(cregion %in% region.)
    
    # qa14eplot <- new_pew %>%
    #   filter(!is.na(qa14e) & qa14e != "Don't know/Refused (VOL.)") %>%
    #   summarise(perc_pos = sum(as.character(qa14e) == "Positive") / length(qa14e),
    #             perc_neg = sum(as.character(qa14e) == "Negative") / length(qa14e),
    #             perc_neither = 1 - perc_pos - perc_neg) %>%
    #   gather(perc_pos, perc_neg, perc_neither, value = "perc", key = "answer") %>%
    #   ggplot() + geom_col(aes(x=answer, y=perc)) + ylim(0,1) + 
    #   labs(title = "View of National News/Media", x = "", y = "") +
    #   scale_x_discrete(labels = c("Negative", "Positive", "Neither"))
    
    
    # qb2plot <- new_pew %>%
    #   filter(!is.na(qb2) & qb2 != "Don't know/Refused (VOL.)") %>%
    #   summarise(perc_sat = sum(as.character(qb2) == "Satisfied") / length(qb2),
    #             perc_dis = 1 - perc_sat) %>%
    #   gather(perc_sat, perc_dis, value = "perc", key = "answer") %>%
    #   ggplot() + geom_col(aes(x=answer, y=perc)) + ylim(0,1) + 
    #   labs(title = "Are you satisfied or dissatisfied with \nhow things are going in this country?", x = "", y = "Proportion") +
    #   scale_x_discrete(labels = c("Dissatisfied", "Satisfied"))
    
    
    fav_ans <- c("Very favorable", "Mostly favorable", "Mostly unfavorable", "Very unfavorable")
    
    #Republican
    qa15aplot <- new_pew %>%
      filter(!is.na(qa15a)) %>%
      filter(qa15a %in% fav_ans) %>%
      summarise(favor = (sum(as.character(qa15a) %in% c("Very favorable", "Mostly favorable")) / length(qa15a)),
                unfavor = 1 - favor) %>%
      gather(favor, unfavor, value = "proportion", key = "answer") %>%
      ggplot() + geom_col(aes(x = answer, y = proportion)) + 
      scale_x_discrete(labels = c("Favorable", "Unfavorable")) +
      ylim(0,1) + labs(title = "View of the Republican Party", x="", y = "") +
      theme(panel.grid = element_blank(), panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
            axis.ticks.y = element_line())
    
    #Democrat
    qa15bplot <- new_pew %>%
      filter(!is.na(qa15b)) %>%
      filter(qa15b %in% fav_ans) %>%
      summarise(favor = (sum(as.character(qa15b) %in% c("Very favorable", "Mostly favorable")) / length(qa15b)),
                unfavor = 1 - favor) %>%
      gather(favor, unfavor, value = "proportion", key = "answer") %>%
      ggplot() + geom_col(aes(x = answer, y = proportion)) + 
      scale_x_discrete(labels = c("Favorable", "Unfavorable")) +
      ylim(0,1) + labs(title = "View of the Democratic Party", x="", y="")  +
      theme(panel.grid = element_blank(), panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
            axis.ticks.y = element_line())
    
    
    favor_plot <- new_pew %>%
      filter(!is.na(qb42a) & !is.na(qb42b) & !is.na(qb42c) & !is.na(qb42d)) %>%
      filter(qb42a %in% fav_ans &
               qb42b %in% fav_ans &
               qb42c %in% fav_ans &
               qb42d %in% fav_ans) %>%
      summarise(trump = (sum(as.character(qb42a) %in% c("Very favorable", "Mostly favorable")) - 
                           sum(as.character(qb42a) %in% c("Very unfavorable", "Mostly unfavorable"))) / 
                  length(qb42a),
                pence = (sum(as.character(qb42b) %in% c("Very favorable", "Mostly favorable")) - 
                           sum(as.character(qb42b) %in% c("Very unfavorable", "Mostly unfavorable"))) / 
                  length(qb42b),
                obama = (sum(as.character(qb42c) %in% c("Very favorable", "Mostly favorable")) - 
                           sum(as.character(qb42c) %in% c("Very unfavorable", "Mostly unfavorable"))) / 
                  length(qb42c),
                hclinton = (sum(as.character(qb42d) %in% c("Very favorable", "Mostly favorable")) - 
                              sum(as.character(qb42d) %in% c("Very unfavorable", "Mostly unfavorable"))) / 
                  length(qb42d)) %>%
      gather(trump, pence, obama, hclinton, value = "net_approval", key = "individual") %>%
      ggplot() + geom_col(aes(x = individual, y = net_approval, fill = net_approval>0)) +
      scale_fill_manual(values = c("#fc8d62", "#66c2a5"), guide=FALSE) +
      scale_x_discrete(labels = c("H. Clinton", "Obama", "Pence", "Trump")) +
      scale_y_continuous(breaks = seq(-1,1,0.25), limits = c(-1,1)) +
      labs(title = "Net approval rating of US leaders", x ="", y = "") + 
      theme(panel.grid = element_blank(), panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
            axis.ticks.y = element_line())
    
    grid.arrange(
      grobs = list(qa15aplot, favor_plot, qa15bplot),
      widths = c(1, 2),
      heights = c(3,2),
      layout_matrix = rbind(c(1, 2),
                            c(3, 2))
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

