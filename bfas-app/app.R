# -------------- Set-up -------------- #

# libraries
require(devtools)

library_list <- data.frame(
    libraries = c("prettyGraphs",
                  "ggplot2",  
                  "ggthemes", 
                  "diagram",  
                  "graphics", 
                  "ggpubr",   
                  "ggridges", 
                  "stringr",  
                  "dplyr",
                  "tidyr",
                  "shiny"),
    versions  = c('2.1.6',
                  '3.3.5',
                  '4.2.4',
                  '1.6.5',
                  '4.1.3',
                  '0.4.0',
                  '0.5.3',
                  '1.4.0',
                  '1.0.8',
                  '1.2.0',
                  '1.7.0')
)

installed_packages <- data.frame(
    libraries = rownames(installed.packages()),
    versions  = as.data.frame(installed.packages())$Version)
installed_packages <- subset(installed_packages,
                             libraries %in% library_list$libraries)

for (pkg in seq_len(nrow(library_list))) {
    
    # pull out required package
    required_pkg   <- library_list[pkg, "libraries"]
    required_vers  <- library_list[pkg, "versions"]
    
    if (required_pkg %in% installed_packages$libraries) {
        
        # pull out package versions
        installed_vers <- installed_packages[which(installed_packages$libraries == required_pkg), "versions"]
        
        if (installed_vers != required_vers) {
            devtools::install_version(package = required_pkg,
                                      version = required_vers,
                                      upgrade = "never")    
        }
    } else {
        devtools::install_version(package = required_pkg,
                                  version = required_vers,
                                  upgrade = "never")
    }
} # END for pkg LOOP

invisible(lapply(library_list$libraries, library, character.only = TRUE))

# BFAS questionnaire
bfas_bank <- data.frame(
  var  = paste0("Q_", c(1:100)),
  item = c("Seldom feel blue.",
            "Am not interested in other people's problems.",
            "Carry out my plans.",
            "Make friends easily.",
            "Am quick to understand things.",
            "Get angry easily.",
            "Respect authority.",
            "Leave my belongings around.",
            "Take charge.",
            "Enjoy the beauty of nature.",
            "Am filled with doubts about things",
            "Feel others' emotions.",
            "Waste my time.",
            "Am hard to get to know.",
            "Have difficulty understanding abstract ideas.",
            "Rarely get irritated.",
            "Believe that I am better than others.",
            "Like order.",
            "Have a strong personality.",
            "Believe in the importance of art.",
            "Feel comfortable with myself.",
            "Inquire about others' well-being.",
            "Find it difficult to get down to work.",
            "Keep others at a distance.",
            "Can handle a lot of information.",
            "Get upset easily.",
            "Hate to seem pushy.",
            "Keep things tidy.",
            "Lack the talent for influencing people.",
            "Love to reflect on things.",
            "Feel threatened easily.",
            "Can't be bothered with other's needs.",
            "Mess things up.",
            "Reveal little about myself.",
            "Like to solve complex problems.",
            "Keep my emotions under control.",
            "Take advantage of others.",
            "Follow a schedule.",
            "Know how to captivate people.",
            "Get deeply immersed in music.",
            "Rarely feel depressed.",
            "Sympathize with others' feelings.",
            "Finish what I start.",
            "Warm up quickly to others.",
            "Avoid philosophical discussions.",
            "Change my mood a lot.",
            "Avoid imposing my will on others.",
            "Am not bothered by messy people.",
            "Wait for others to lead the way.",
            "Do not like poetry.",
            "Worry about things.",
            "Am indifferent to the feelings of others.",
            "Don't put my mind on the task at hand.",
            "Rarely get caught up in the excitement.",
            "Avoid difficult reading material.",
            "Rarely lose my composure.",
            "Rarely put people under pressure.",
            "Want everything to be “just right.",
            "See myself as a good leader.",
            "Seldom notice the emotional aspects of paintings and pictures.",
            "Am easily discouraged.",
            "Take no time for others.",
            "Get things done quickly.",
            "Am not a very enthusiastic person.",
            "Have a rich vocabulary.",
            "Am a person whose moods go up and down easily.",
            "Insult people.",
            "Am not bothered by disorder.",
            "Can talk others into doing things.",
            "Need a creative outlet.",
            "Am not embarrassed easily.",
            "Take an interest in other people's lives.",
            "Always know what I am doing.",
            "Show my feelings when I'm happy.",
            "Think quickly.",
            "Am not easily annoyed.",
            "Seek conflict.",
            "Dislike routine.",
            "Hold back my opinions.",
            "Seldom get lost in thought.",
            "Become overwhelmed by events.",
            "Don't have a soft side.",
            "Postpone decisions.",
            "Have a lot of fun.",
            "Learn things slowly.",
            "Get easily agitated.",
            "Love a good fight.",
            "See that rules are observed.",
            "Am the first to act.",
            "Seldom daydream.",
            "Am afraid of many things.",
            "Like to do things for others.",
            "Am easily distracted.",
            "Laugh a lot.",
            "Formulate ideas clearly.",
            "Can be stirred up easily.",
            "Am out for my own personal gain.",
            "Want every detail taken care of.",
            "Do not have an assertive personality.",
            "See beauty in things that others might not notice.")
)


# -------------- Define UI -------------- #

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "litera"),

  # Application title
  titlePanel("Big Five Aspect Scale"),
  
  # Main panel with questionnaire and info
  mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Take the survey",
                 fluidRow(htmlOutput("instruction")),
                 
                 # randomize bfas question order
                 fluidRow(
                   column(12,
                          lapply(sample(1:100), function(i) {
                            radioButtons(inputId  = paste0(bfas_bank[i, 1]), 
                                         label    = paste0(bfas_bank[i, 2]),
                                         #TODO: preselected for easy testing but should be character(0)
                                         selected = 1,
                                         choiceNames  = c("Strongly disagree", "Disagree",
                                                          "Neither agree nor disagree",
                                                          "Agree", "Strongly agree"),
                                         choiceValues = c(1:5))
                    }))),
                 
                 # demographics
                 fluidRow(htmlOutput("demographics")),

                 fluidRow(
                   column(12,
                          selectInput(inputId = "age",
                                      label   = "What is your age?",
                                      choices = c("Under 12 years old",
                                                  "12-17 years old",
                                                  "18-24 years old",
                                                  "25-34 years old",
                                                  "35-44 years old",
                                                  "45-54 years old",
                                                  "55-64 years old",
                                                  "65-74 years old",
                                                  "75 years or older")),
                          selectInput(inputId = "gender",
                                      label   = "What is your current gender identity?",
                                      choices = c("Female", "Male", "Other", "Prefer not to answer")),
                          selectInput(inputId = "race",
                                      label   = "What race/ethnicity group(s) do you belong to?",
                                      choices = c("Arab or Middle Eastern", 
                                                  "East Asian",
                                                  "South Asian",
                                                  "Black, African, or Caribbean",
                                                  "Hispanic or Latino",
                                                  "Pacific Islander",
                                                  "White, Caucasian, or European",
                                                  "Mixed; Parents are from two different ethnic groups",
                                                  "Other",
                                                  "Prefer not to answer"),
                                      multiple = TRUE),
                          selectInput(inputId = "region",
                                      label   = "What geographical region do you currently live in?",
                                      choices = c("Africa", "Asia", 
                                                  "North America", "South America",
                                                  "Europe", "Oceania")),
                          selectInput(inputId = "religion",
                                      label   = "What is your present religious affiliation, if any?",
                                      choices = c("Buddhist", "Christian",
                                                  "Hindu, Muslim", "Agnostic",
                                                  "Atheist", "Other", 
                                                  "Prefer not to answer")),
                          selectInput(inputId = "relationship",
                                      label   = "What is your relationship status",
                                      choices = c("Single (never married)",
                                                  "Dating one person exclusively",
                                                  "Dating multiple people",
                                                  "Married or in a domestic partnership",
                                                  "Divorced or Separated",
                                                  "Widowed",
                                                  "Prefer not to answer")),
                          selectInput(inputId = "education",
                                      label   = "What level of schooling have you completed?",
                                      choices = c("No school",
                                                  "Primary/elementary school",
                                                  "Some high school",
                                                  "Graduated high school",
                                                  "Trade/Technical/Vocational training",
                                                  "Some undergraduate education (college or university)",
                                                  "Bachelor's degree",
                                                  "Master's degree",
                                                  "Doctoral or professional degree",
                                                  "Prefer not to answer")),
                          selectInput(inputId = "employment",
                                      label   = "What is your current professional or employment status?",
                                      choices = c("Employed for wages",
                                                  "Self-employed",
                                                  "Unemployed",
                                                  "Homemaker",
                                                  "Student",
                                                  "Retired",
                                                  "Prefer not to answer")),
                          selectInput(inputId = "income",
                                      label   = "What is your annual household income in US Dollars?",
                                      choices = c("Less than 10,000",
                                                  "10,000 to 19,999",
                                                  "20,000 to 34,999",
                                                  "35,000 to 49,999",
                                                  "50,000 to 74,999",
                                                  "75,000 to 99,999",
                                                  "Over 100,000",
                                                  "Prefer not to answer")),
                          selectInput(inputId = "english",
                                      label   = "How well do you speak English?",
                                      choices = c("Very well (fluent/native)",
                                                  "Well",
                                                  "Not very well",
                                                  "Not at all", 
                                                  "Prefer not to answer")),
                          selectInput(inputId = "repeat",
                                      label   = "Have you taken this survey before?",
                                      choices = c("Yes", "No"))
                          ))),
        tabPanel("Download",
                 #test data cleaning
                 tableOutput("test")
                 #downloadButton("downloadData", "Download data"),
                 #dataTableOutput("data"),
                 #dataTableOutput("average")
        )))
)

# -------------- Server logic -------------- #


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # measure instructions
  output$instruction <- renderText({paste("<br/> Here are a number of characteristics that may or may not describe you.  
  For example, do you agree that you <i> seldom feel blue </i>?
  Please indicate the extent to which you agree or disagree with each statement listed below.  
  Be as honest as possible, but rely on your initial feeling and do not think too much about each item. <br/> <br/> <br/>")})
  
  output$demographics <- renderText({paste("<br/> ---------------- <br/>
                                           For more accurate results, please also provide us with more demographic information <br/> <br/>")})
  # extract and clean data
  bfas_data <- reactive({
    
    data <- sapply(grep(pattern = "Q_+[[:digit:]]", x = names(input), value = TRUE), 
                        function(x) as.numeric(input[[x]]))
    
    # reverse score
    cols <- c('Q_1','Q_21','Q_41','Q_71',
          'Q_16','Q_36','Q_56','Q_76',
          'Q_2','Q_32','Q_52','Q_62','Q_82',
          'Q_17','Q_37','Q_67','Q_77','Q_87','Q_97',
          'Q_13','Q_23','Q_33','Q_53','Q_83','Q_93',
          'Q_8','Q_48','Q_68','Q_78',
          'Q_14','Q_24','Q_34','Q_54','Q_64',
          'Q_29','Q_49','Q_79','Q_99',
          'Q_15','Q_45','Q_55','Q_85',
          'Q_50','Q_60','Q_80','Q_90')
    
    data[cols] <- 6 - data[cols]
    
    data <- t(data)
    
    # neuroticism scores
    N <- c('Q_1', 'Q_11', 'Q_21', 'Q_31', 'Q_41', 'Q_51', 'Q_61', 'Q_71', 'Q_81', 'Q_91',
           'Q_6', 'Q_16', 'Q_26', 'Q_36', 'Q_46', 'Q_56', 'Q_66', 'Q_76', 'Q_86', 'Q_96')
    Withdrawal <- sum(data[,N[1:10]])/10
    Volatility <- sum(data[,N[11:20]])/10
    bfas_n <- (Withdrawal + Volatility)/2
    data   <- cbind(data, Withdrawal, Volatility, bfas_n)

    # agreeableness scores
    A <- c('Q_2','Q_12', 'Q_22', 'Q_32', 'Q_42', 'Q_52', 'Q_62', 'Q_72', 'Q_82', 'Q_92',
           'Q_7', 'Q_17', 'Q_27', 'Q_37', 'Q_47', 'Q_57', 'Q_67', 'Q_77', 'Q_87', 'Q_97')
    Compassion <- sum(data[,A[1:10]])/10
    Politeness <- sum(data[,A[11:20]])/10
    bfas_a <- (Compassion + Politeness)/2
    data   <- cbind(data, Compassion, Politeness, bfas_a)

    # agreeableness scores
    C <- c('Q_3', 'Q_13', 'Q_23', 'Q_33', 'Q_43', 'Q_53', 'Q_63', 'Q_73', 'Q_83', 'Q_93',
           'Q_8', 'Q_18', 'Q_28', 'Q_38', 'Q_48', 'Q_58', 'Q_68', 'Q_78', 'Q_88', 'Q_98')
    Industriousness <- sum(data[,C[1:10]])/10
    Orderliness     <- sum(data[,C[11:20]])/10
    bfas_c <- (Industriousness + Orderliness)/2
    data   <- cbind(data, Industriousness, Orderliness, bfas_c)

    # extraversion scores
    E <- c('Q_4', 'Q_14', 'Q_24', 'Q_34', 'Q_44', 'Q_54', 'Q_64', 'Q_74', 'Q_84', 'Q_94',
           'Q_9', 'Q_19', 'Q_29', 'Q_39', 'Q_49', 'Q_59', 'Q_69', 'Q_79', 'Q_89', 'Q_99')
    Enthusiasm    <- sum(data[,E[1:10]])/10
    Assertiveness <- sum(data[,E[11:20]])/10
    bfas_e <- (Enthusiasm + Assertiveness)/2
    data   <- cbind(data, Enthusiasm, Assertiveness, bfas_e)

    # openness/intellect scores
    O <- c('Q_5', 'Q_15', 'Q_25', 'Q_35', 'Q_45', 'Q_55', 'Q_65', 'Q_75', 'Q_85', 'Q_95',
           'Q_10', 'Q_20', 'Q_30', 'Q_40', 'Q_50', 'Q_60', 'Q_70', 'Q_80', 'Q_90', 'Q_100')
    Intellect <- sum(data[,O[1:10]])/10
    Openness  <- sum(data[,O[11:20]])/10
    bfas_o <- (Intellect + Openness)/2
    data   <- cbind(data, Intellect, Openness, bfas_o)

    data
    })
  
  output$test <- renderTable(bfas_data())
}

# Run the application 
shinyApp(ui = ui, server = server)
