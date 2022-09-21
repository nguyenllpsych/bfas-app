# -------------- Set-up -------------- #

# libraries
library("shiny")
library("prettyGraphs")
library("ggplot2")
library("diagram")
library("ggthemes")
library("graphics")
library("ggpubr")
library("ggridges")
library("stringr")
library("dplyr")
library("tidyr")

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

# norming data
Aspects <- readRDS("Aspects.RData")
norms   <- readRDS("norms.RData")

# -------------- Define UI -------------- #

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "litera"),

  # Application title
  titlePanel("Big Five Aspect Scale"),
  
  # Main panel  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      
      # First tab with introduction
      tabPanel("Introduction", 
               htmlOutput("introduction")),
      
      # Second tab with the questionnaire
      tabPanel("Take the Survey",
               
               # instruction
               fluidRow(htmlOutput("instruction")),
    
               # randomize bfas question order
               fluidRow(
                 column(12,
                        lapply(sample(1:100), function(i) {
                          radioButtons(
                            inputId  = paste0(bfas_bank[i, 1]), 
                            label    = paste0(bfas_bank[i, 2]),
                            #TODO: preselected for easy testing but should be character(0)
                            selected = 3,
                            choiceNames  = c("Strongly disagree", 
                                             "Disagree",
                                             "Neither agree nor disagree",
                                             "Agree", 
                                             "Strongly agree"),
                            choiceValues = c(1:5))
               }))), # END BFAS
               
               # download
               fluidRow(
                 column(12,
                        htmlOutput("download"),
                        downloadButton("report", "Generate report"))),
           
               
               # demographics
               fluidRow(
                 column(12,
                        htmlOutput("demographics"),
                        numericInput(inputId = "age",
                                     label   = "What is your age?",
                                     value   = character(0),
                                     min     = 1,
                                     max     = 100),
                        selectInput(inputId = "gender",
                                    label   = "What is your current gender identity?",
                                    choices = c("Female", "Male", "Other", "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
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
                                                "Europe", "Oceania",
                                                "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
                        selectInput(inputId = "religion",
                                    label   = "What is your present religious affiliation, if any?",
                                    choices = c("Buddhist", "Christian",
                                                "Hindu, Muslim", "Agnostic",
                                                "Atheist", "Other", 
                                                "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
                        selectInput(inputId = "relationship",
                                    label   = "What is your relationship status",
                                    choices = c("Single (never married)",
                                                "Dating one person exclusively",
                                                "Dating multiple people",
                                                "Married or in a domestic partnership",
                                                "Divorced or Separated",
                                                "Widowed",
                                                "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
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
                                                "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
                        selectInput(inputId = "employment",
                                    label   = "What is your current professional or employment status?",
                                    choices = c("Employed for wages",
                                                "Self-employed",
                                                "Unemployed",
                                                "Homemaker",
                                                "Student",
                                                "Retired",
                                                "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
                        selectInput(inputId = "income",
                                    label   = "What is your annual household income in US Dollars?",
                                    choices = c("Less than 10,000",
                                                "10,000 to 19,999",
                                                "20,000 to 34,999",                                                  
                                                "35,000 to 49,999",
                                                "50,000 to 74,999",
                                                "75,000 to 99,999",
                                                "Over 100,000",
                                                "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
                        selectInput(inputId = "english",
                                    label   = "How well do you speak English?",
                                    choices = c("Very well (fluent/native)",
                                                "Well",
                                                "Not very well",
                                                "Not at all", 
                                                "Prefer not to answer"),
                                    selected = "Prefer not to answer"),
                        selectInput(inputId = "repeat",
                                    label   = "Have you taken this survey before?",
                                    choices = c("No", "Yes"))
               ))) # END demographics
))) # END UI

# -------------- Server logic -------------- #

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # instructions and information
  output$introduction <- renderText({
    "<br/><br/>
    <b>Personality Traits and the Big Five Aspect Scales (BFAS)</b><br/>
    
    Personality traits are people's relatively stable patterns of motivation, 
    emotion, cognition, and behavior across situations. People who score high on
    a trait (for example, happiness) experience and exhibit psychological states
    related to that trait (for example, smiling) more often and more intensely 
    than people who score low on that trait. The scientific Five Factor Model of
    personality, also known as the Big Five, describes five broad personality 
    traits: Neuroticism, Agreeableness, Conscientiousness, Extraversion, and 
    Openness/Intellect. These are not five different personality types: every 
    person has some level of each of these traits, and so a full personality 
    profile requires knowing your level on all five. Although they are typically
    labeled based on one end of the trait, all of them have an opposite end as 
    well that is just as important (for example, Extraversion versus Introversion).
    <br/><br/>
    
    In the Big Five Aspect Scales (BFAS)—which you may complete on the next tab—
    the five broad traits are each split into two narrower traits (called Aspects),
    based on psychometric and genetic evidence. 
    
    <ul>
		<li> Neuroticism: Withdrawal and Volatility </li>
		<li> Agreeableness: Compassion and Politeness </li>
		<li> Conscientiousness: Industriousness and Orderliness </li>
		<li> Extraversion: Enthusiasm and Assertiveness </li>
		<li> Openness/Intellect: Openness and Intellect </li>
		</ul><br/>
    
    <b>How to interpret your feedback</b><br/>
    
    Based on your answers, we estimated your scores on these five broad traits 
    and ten narrower aspects. To illustrate how your personality compares to 
    others, we compared your scores to thousands of other people who have taken 
    the BFAS to generate <b>percentile scores.</b> As an example, if you 
    received a score of 90 on Enthusiasm (an aspect of Extraversion describing 
    friendliness, sociability, and the tendency to experience high energy 
    positive emotions like joy and excitement), your score on that trait is 
    higher than that of 90% of people. With such a high Enthusiasm score, you 
    are probably more friendly, outgoing, and positive than most others. 
    Conversely, if you received a 5th percentile score, your score is higher 
    than 5% of people (and lower than 95% of people), meaning you are probably 
    reserved, quiet, and unexcitable compared to most others.
    <br/><br/>
    
    To help you understand what your score means for your personality, 
    we categorized your score on each trait as <b>low, below average, above average,
    or high.</b> Each category is accompanied by a description of typical patterns 
    of behavior, emotion, and cognition for someone falling in that category. 
    This feedback should be reasonably accurate, but it will not always be 
    perfectly accurate for everyone due to idiosyncrasies in the way people 
    answer the questions. People’s answers can be influenced by their mood, who 
    they compare themselves to, what they recall, and whether they hold a biased
    view of themselves. Some people might find adjacent categories—or a 
    combination of categories—to fit their personality better than their 
    assigned category. For example, someone scoring only slightly below average 
    in Enthusiasm might also refer to the descriptions of someone scoring above 
    average in Enthusiasm. 
    <br/><br/>
    
    <b>Feedback is for personal use only and is not suitable for any clinical, 
    diagnostic, or professional use.</b>"
  })
  
  output$instruction <- renderText({
  paste("<br/> Here are a number of characteristics that may or may not describe
  you.  For example, do you agree that you <i> seldom feel blue </i>?  
  Please indicate the extent to which you agree or disagree with each statement 
  listed below.  Be as honest as possible, but rely on your initial feeling and 
  do not think too much about each item. <br/> <br/> <br/>")})
  
  output$demographics <- renderText({
  paste("<br/> ---------------- <br/>
        As you wait for your report, 
        please also provide us with some more information. <br/> <br/>")})
  
  output$download <- renderText({
  paste("<br/> ---------------- <br/>
        Please click on the button below when you are finished. 
        Please allow for a few minutes to generate the report. <br/> <br/>")})
  
  # extract user inputted bfas data
  bfas_data <- reactive({
    
    data <- sapply(grep(pattern = "Q_+[[:digit:]]", 
                        x = names(input), value = TRUE), 
                        function(x) as.numeric(input[[x]]))
    
    data <- as.data.frame(t(data))
    data
  })
  
  # personal report download
  # will take at least a few seconds!!!
    output$report <- downloadHandler(
      
      filename = "my_personality.pdf",
      content = function(file) {
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(d       = bfas_data(),
                       Aspects = Aspects,
                       norms   = norms)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  } # END server

# Run the application 
shinyApp(ui = ui, server = server)
