packages <-  c("shiny", "shinydashboard", "shinythemes", "shinycssloaders", "tidyverse","DT","plotly",
               "readxl","rlang","shinyjs","doParallel")
inst <- packages[!packages %in% installed.packages()]
if (length(inst)>0) {
  inst_ok = switch(menu(c("Yes", "No"), 
                        title = cat("The following packages and their dependencies will be installed.\n", 
                                    paste0(inst, collapse = "\n"),"\n\nProceed?", sep = "")),
                   TRUE, FALSE)
  if (inst_ok) install.packages(inst) else stop("Aborted")
}
rm(inst,packages)

options(scipen=999) 
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(DT)
library(plotly)
library(readxl)
library(rlang)

# ---- Required Datasets----
# IRT Parameter Tab
prm_summary <- read_csv("App files\\Final IRT parameters_2019_long format_2020-08-18.csv") %>% 
  mutate_at("State_ItemID", as.character)
d1_states <- unique(prm_summary$State)[unique(prm_summary$State) != "State_6_and_7"]
# d1_states <- unique(prm_summary$State) %>% .[. %in% state.abb] %>% c(., "AIRCore")
d1_grades <- unique(prm_summary$Grade)
d1_stats <- unique(prm_summary$Statistics)
d1_compare <- names(prm_summary)[str_detect(names(prm_summary), "^value_")]

state_mean_var =  read_csv("App files\\State_mean_variance.csv")

# Classical Statistics Tab and Distribution Plot Tab
classical_stats_assertion <- suppressWarnings(
  read_xlsx("App files\\Appendix A. Classical Statistics for Science Items.xlsx",
            sheet = 1, skip = 1, guess_max = 10000) %>% 
    rename_all(.funs = funs(str_replace_all(., " |-|\\/", "_"))) %>% 
    rename_all(.funs = funs(str_replace_all(., "[.\r\n]", ""))) %>% 
    # rename_with( ~ str_replace_all(.x, " |-|\\/", "_")) %>% 
    # rename_with( ~ str_replace_all(.x, "[.\r\n]", "")) %>% 
    mutate(Master_AssertionID = paste0(Master_ItemID, "_",Assertion_ID)) %>% 
    mutate_at("ItemID", as.character)
)
d2_states <- unique(classical_stats_assertion$State) %>% sort()
d2_grades <- unique(classical_stats_assertion$Grade_Band)
d2_roles <- unique(classical_stats_assertion$Role)
state.name <- state.abb <- d2_states[-1] # temporarily used to make the code work when state names are anonymous
# Mean Classical Statistics Tab
classical_stats_item <- read_xlsx("App files\\Appendix A. Classical Statistics for Science Items.xlsx",
                                  sheet = 3, skip = 1, guess_max = 10000) %>% 
  rename_all(.funs = funs(str_replace_all(., " |-|\\/", "_"))) %>% 
  rename_all(.funs = funs(str_replace_all(., "[.\r\n]", "_"))) %>% 
  rename_all(.funs = funs(str_replace_all(., "_{2,}", "_"))) %>% 
  mutate_at("ItemID", as.character)
# rename_with( ~ str_replace_all(.x, " |-|\\/", "_")) %>% 
# rename_with( ~ str_replace_all(.x, "[.\r\n]", ""))

# Software Comparison Tab
software_comparison <- read_csv("App files\\Software_Comparison_Results.csv")
if ("Deviance" %in% unique(software_comparison$statistics)) {
  Deviance <- filter(software_comparison, statistics == "Deviance")
}
Variance <- filter(software_comparison, !str_detect(statistics, "i_\\d+"), statistics != "Deviance") %>% rename(Master_ItemID = statistics)
Intercept <- filter(software_comparison, str_detect(statistics, "i_\\d+")) %>% rename(Master_AssertionID = statistics)
d3_cols <- colnames(software_comparison)[-1]



# +++ wrap some of the repeated codes into functions
# +++ add a link to sample item in heading?
# +++ add static page showing BP? "BluePrint Viewer"
ui = dashboardPage(skin = "blue",
                   dashboardHeader(title = "NGSS Psychometrics Toolkit", 
                                   titleWidth = 350
                   ),
                   dashboardSidebar(width = 350,
                                    tags$style(HTML(".sidebar-menu>li>a { font-size: 20px; }")), # menuItem css controls
                                    tags$style(HTML(".treeview-menu>li>a { font-size: 18px!important; }")), # menuSubItem css controls
                                    sidebarMenu(
                                      id = "sidebarmenu",
                                      menuItem("", tabName = "default"), # add an invisible menuItem here so that all panels looks folded at initial app launch
                                      menuItem("IRT Parameter", tabName = "dash1", icon = icon("th")),
                                      conditionalPanel(
                                        condition = "input.sidebarmenu == 'dash1'",
                                        selectInput("dash1_state", label = HTML('<p style="color:grey;">Select State</p>'),
                                                    choices = d1_states),
                                        selectInput("dash1_grade", label = HTML('<p style="color:grey;">Select Grade</p>'),
                                                    choices = d1_grades),
                                        radioButtons("dash1_compare",label = "Compare to previous years' results?",
                                                     choices = list(Yes=1,No=0),selected = 0, inline = TRUE)
                                      ),
                                      conditionalPanel(
                                        condition = "input.sidebarmenu == 'dash1' && input.dash1_compare == 1",
                                        selectInput("dash1_prm1", label = HTML('<p style="color:grey;">Select Parameter</p>'),
                                                    choices = d1_compare),
                                        selectInput("dash1_prm2", label = HTML('<p style="color:grey;">Select Parameter</p>'),
                                                    choices = d1_compare)
                                      ),
                                      hr(),
                                      hr(),
                                      menuItem("Classical Statistics", 
                                               menuSubItem("One State Exploration", tabName = "dash2_one"),
                                               menuSubItem("Between State Comparison", tabName = "dash2_two"),
                                               tabName = "dash2", icon = icon("th")),
                                      conditionalPanel(
                                        condition = "input.sidebarmenu == 'dash2_one'",
                                        selectInput("dash2_one_state", label = HTML('<p style="color:grey;">Select State</p>'),
                                                    choices = d2_states),
                                        selectInput("dash2_one_grade", label = HTML('<p style="color:grey;">Select Grade</p>'),
                                                    choices = d2_grades)
                                      ),
                                      conditionalPanel(
                                        condition = "input.sidebarmenu == 'dash2_two'",
                                        selectInput("dash2_two_state1", label = HTML('<p style="color:grey;">Select State 1</p>'),
                                                    choices = c("",d2_states)),
                                        selectInput("dash2_two_state2", label = HTML('<p style="color:grey;">Select State 2</p>'),
                                                    choices = c("",d2_states)),
                                        selectInput("dash2_two_grade", label = HTML('<p style="color:grey;">Select Grade</p>'),
                                                    choices = c("",d2_grades))
                                      ),
                                      hr(),
                                      hr(),
                                      menuItem("Software Comparison", tabName = "dash3", icon = icon("th")),
                                      conditionalPanel(
                                        condition = "input.sidebarmenu == 'dash3'",
                                        selectInput("dash3_stat", label = HTML('<p style="color:grey;">Select Statistics</p>'),
                                                    choices = c("Intercept","Variance")),
                                        selectInput("dash3_software1", label = HTML('<p style="color:grey;">Select Software & Settings</p>'),
                                                    choices = d3_cols),
                                        selectInput("dash3_software2", label = HTML('<p style="color:grey;">Select Software & Settings</p>'),
                                                    choices = d3_cols)
                                      )
                                    )
                   ),
                   dashboardBody(useShinyjs(),
                                 tabItems(
                                   tabItem(tabName = "dash1",
                                           h3("IRT Parameter Exploration"),
                                           tabsetPanel(id = "dash1_tabset",
                                                       tabPanel(
                                                         "Summary", 
                                                         fluidRow(
                                                           column(6,
                                                                  box(selectInput("dash1_stat", label = HTML('<p style="color:grey;">Select Statistics to Display</p>'),
                                                                                  choices = c("Cluster Variance","Difficulty")),
                                                                      plotOutput("dash1_plot", click = "plot_click"),
                                                                      tags$p("Highest/Lowest Entries", style = "font-size: 150%; font-family:Helvetica; font-weight: bold; color:black; background-color:aliceblue"),
                                                                      # HTML('<h4 style="color:DodgerBlue;">Highest/Lowest Entries</h4>'),
                                                                      splitLayout(cellWidths = c("10%", "8%", "20%","20%"), style = "height: 60px",
                                                                                  h5(strong("Select Top"),HTML('&nbsp;')),
                                                                                  numericInput("dash1_topn", label = NULL, value = 10, min=1),
                                                                                  div(style = "height: 200px",
                                                                                      selectInput("dash1_hilo", label = NULL, choices = c("Highest","Lowest"),selected = "Highest")),
                                                                                  h5(HTML('&nbsp;'),strong("Entries"))),
                                                                      DTOutput("dash1_DT"),
                                                                      title="Summary Statistics", status = "primary", solidHeader = T, width = 12)
                                                           ),
                                                           column(6,
                                                                  box(uiOutput("dash1_ui_select"),
                                                                      checkboxInput("dash1_check_info", label=HTML('<p style="color:grey;">Show Item Infomation Plot?</p>')),
                                                                      DTOutput("dash1_display"),
                                                                      plotOutput("dash1_item_info") %>% withSpinner(),
                                                                      title="Item Parameter and Infomation Plot", status = "primary", solidHeader = T, width = 12)
                                                           )
                                                         )
                                                       ),
                                                       tabPanel(
                                                         "Comparison", 
                                                         fluidRow(
                                                           column(6,
                                                                  box(plotlyOutput("dash1_tab2_plot"),
                                                                      title="Parameter Comparison among Calbiration Results", status = "primary", solidHeader = T, width = 12)
                                                           )
                                                         )
                                                       )
                                           )
                                           
                                   ),
                                   # --------------
                                   tabItem(tabName = "dash2_one",
                                           h3("Classical Statistics Summary"),
                                           tabsetPanel(
                                             tabPanel("Assertion Level",
                                                      fluidRow(
                                                        
                                                        column(
                                                          6,
                                                          box(title="Average P-value and Biserial Summary", status = "primary", solidHeader = T, width = 12,
                                                              fluidRow(column(6,
                                                                              selectInput("dash2_one_assertion_role", "Select Item Role",
                                                                                          choices = d2_roles)),
                                                                       column(6,
                                                                              selectInput("dash2_one_assertion_stat", "Select Statistics",
                                                                                          choices = c("P_Value","Biserial")))
                                                              ),
                                                              plotOutput("dash2_one_assertion_average")),
                                                          box(textAreaInput("dash2_one_assertion_comments","Put comments here", rows = 10),
                                                              title = "Comments",status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12
                                                          )
                                                        ),
                                                        column(
                                                          6,
                                                          box(title = "Assertion Statistics Summary Plots",status = "primary", solidHeader = TRUE, width = 12,
                                                              h4("Filter By"),
                                                              fluidRow(column(4,
                                                                              selectInput("dash_2_one_assertion_select4","Select Role",choices = c("ALL","FT","OP"))),
                                                                       column(4,
                                                                              selectInput("dash_2_one_assertion_select5","Select Type",choices = c("ALL","Cluster","SA"))),
                                                                       column(4,
                                                                              selectInput("dash_2_one_assertion_select6","Select Discipline",choices = c("ALL","ESS","LS","PS")))
                                                              ),
                                                              h4("Group By"),
                                                              selectizeInput("dash_2_one_assertion_select_grouping","Select Grouping Variable",
                                                                             multiple=TRUE,
                                                                             choices =  c("Role","Type","Discipline"),
                                                                             options = list(maxItems = 2)),
                                                              h4("Choose Statistics"),
                                                              selectInput("dash_2_one_assertion_select_stats",NULL,
                                                                          choices =  c("P_Value", "Biserial")),
                                                              plotlyOutput("dash2_one_assertion_summary_plot")
                                                          )
                                                        ),
                                                        box(title = "Summary Statistics",status = "primary", solidHeader = TRUE, width = 12,
                                                            DTOutput("dash2_one_assertion_DT")
                                                        )
                                                      )
                                             ),
                                             tabPanel("Item Level",
                                                      fluidRow(
                                                        column(
                                                          6,
                                                          box(title="Item Count", status = "primary", solidHeader = TRUE, width = 12,
                                                              fluidRow(column(4,
                                                                              selectInput("dash_2_one_item_select1","Select Role",choices = c("ALL","FT","OP"))),
                                                                       column(4,
                                                                              selectInput("dash_2_one_item_select2","Select Type",choices = c("ALL","Cluster","SA"))),
                                                                       column(4,
                                                                              selectInput("dash_2_one_item_select3","Select Discipline",choices = c("ALL","ESS","LS","PS")))
                                                              ),
                                                              plotlyOutput("dash2_pie", width = "100%", height = "100%") %>% withSpinner()
                                                          ),
                                                          box(textAreaInput("dash2_one_item_comments","Put comments here", rows = 10),
                                                              title = "Comments",status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12
                                                          )
                                                        ),
                                                        column(
                                                          6,
                                                          box(title = "Item Statistics Summary Plots",status = "primary", solidHeader = TRUE, width = 12,
                                                              h4("Filter By"),
                                                              fluidRow(column(4,
                                                                              selectInput("dash_2_one_item_select4","Select Role",choices = c("ALL","FT","OP"))),
                                                                       column(4,
                                                                              selectInput("dash_2_one_item_select5","Select Type",choices = c("ALL","Cluster","SA"))),
                                                                       column(4,
                                                                              selectInput("dash_2_one_item_select6","Select Discipline",choices = c("ALL","ESS","LS","PS")))
                                                              ),
                                                              h4("Group By"),
                                                              selectizeInput("dash_2_one_item_select_grouping","Select Grouping Variable",
                                                                             multiple=TRUE,
                                                                             choices =  c("Role","Type","Discipline"),
                                                                             options = list(maxItems = 2)),
                                                              h4("Choose Statistics"),
                                                              selectInput("dash_2_one_item_select_stats",NULL,
                                                                          choices =  c("Average_of_P_Value", "Min_of_P_Value", "Max_of_P_Value",
                                                                                       "Average_of_Biserial", "Min_of_Biserial", "Max_of_Biserial",
                                                                                       "Number_of_Assertions", "Percentile_80", "No_Assertion_Percentile_80")),
                                                              plotlyOutput("dash2_one_item_summary_plot")
                                                          )
                                                        ),
                                                        box(title = "Summary Statistics",status = "primary", solidHeader = TRUE, width = 12,
                                                            DTOutput("dash2_one_item_DT")
                                                        )
                                                      )
                                             )
                                           )
                                   ),
                                   # --------------
                                   tabItem(tabName = "dash2_two",
                                           h3("Classical Statistics Comparison"),
                                           fluidRow(
                                             column(
                                               6,
                                               box(title="Betweem State Comparison", status = "primary", solidHeader = T, width = 12,
                                                   fluidRow(
                                                     column(
                                                       6,
                                                       selectInput("dash2_two_stat", "Select Statistics", choices = c("P_Value","Biserial"))),
                                                     column(
                                                       6,
                                                       selectInput("dash2_two_point_desc", "Select Point Description",
                                                                   choices = c("Master_AssertionID", "State_ItemID", "Grade_Band", "Type", "Discipline","N"),
                                                                   multiple = TRUE, selected = "Master_AssertionID"))
                                                   ),
                                                   h4("Filter By"),
                                                   fluidRow(column(4,
                                                                   selectInput("dash_2_two_filter4","Select Role",choices = c("ALL","FT","OP"))),
                                                            column(4,
                                                                   selectInput("dash_2_two_filter5","Select Type",choices = c("ALL","Cluster","SA"))),
                                                            column(4,
                                                                   selectInput("dash_2_two_filter6","Select Discipline",choices = c("ALL","ESS","LS","PS")))
                                                   ),
                                                   plotlyOutput("dash2_two_scatter")
                                               ),
                                               box(textAreaInput("dash2_two_comments","Put comments here", rows = 10),
                                                   title = "Comments",status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12)
                                             ),
                                             column(
                                               6,
                                               box(title="Point Clicked", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                   uiOutput("dash2_two_heading1"),
                                                   uiOutput("dash2_two_hr1"),
                                                   fluidRow(
                                                     column(5, htmlOutput("dash2_two_clicked_summary")),
                                                     column(7, tableOutput("dash2_two_clicked_summary2"))
                                                   ),
                                                   uiOutput("dash2_two_heading2"),
                                                   uiOutput("dash2_two_hr2"),
                                                   DTOutput("dash2_two_clicked")
                                               )
                                             )
                                           )
                                   ),
                                   # ----------------
                                   tabItem(tabName = "dash3",
                                           h3("Software Comparison"),
                                           fluidRow(
                                             column(6,
                                                    box(plotlyOutput("dash3_software_scatter"),
                                                        title="Software Comparison", status = "primary", solidHeader = TRUE, width = 12),
                                                    box(textAreaInput("dash3_comments","Put comments here", rows = 10),
                                                        title = "Comments",status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12)
                                             ),
                                             column(6,
                                                    uiOutput("show_deviance"),
                                                    box(splitLayout(cellWidths = c("10%", "8%"),
                                                                    h5(strong("Select Top"),HTML('&nbsp;')),
                                                                    numericInput("dash3_topn", label = NULL, value = 10, min=1),
                                                                    h5(HTML('&nbsp;'),strong("Entries"))),
                                                        DTOutput("dash3_DT"),
                                                        title = "Max Difference",status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12)
                                             )
                                           )
                                   )
                                 )
                   )
)

#########################################################
##################  SERVER ##############################
#########################################################

server <- function(input, output, session) {
  # ========== Tab 1 ============
  d1_dat <- reactive({
    if (input$dash1_state %in% c("State6","State7")) {
      bind_rows(filter(prm_summary, State == input$dash1_state, Grade == input$dash1_grade),
                filter(prm_summary, State == "State_6_and_7", Grade == input$dash1_grade))
    } else {
      filter(prm_summary, State == input$dash1_state, Grade == input$dash1_grade)
    }
  })
  # !!!!!!! THIS CURRENTLY INCLUDE FT ITEMS, UPDATE IT LATER !!!!!!!!!
  output$dash1_plot <- renderPlot({
    temp = d1_dat() %>% 
      filter(tolower(Statistics) %in% tolower(input$dash1_stat)) %>% 
      select(State, Grade, Master_ItemID, State_ItemID, value_final) %>% 
      mutate(group = "") # pseudo group. It's just for showing the legend
    if (tolower(input$dash1_stat) == "difficulty") {
      temp2 = state_mean_var %>% 
        filter(State == input$dash1_state, Grade == input$dash1_grade)
      pop.mean = temp2$value_final[temp2$Statistics=="State mean"]
      pop.sd = sqrt(temp2$value_final[temp2$Statistics=="State variance"])
      ggplot(temp, aes(x=value_final)) + geom_histogram(aes(fill = factor(group)), color="black",binwidth = 0.5, show.legend=TRUE) +
        scale_fill_manual("Item Difficulty",values = "light blue") + 
        #@@@ overlay the proficiency distribution
        stat_function( 
          fun = function(x, mean, sd, n, bw){ 
            dnorm(x = x, mean = mean, sd = sd) * n * bw
          }, 
          args = c(mean = pop.mean, sd = pop.sd, n = sum(!is.na(temp$value_final)), bw = 0.5),
          aes(colour = "")) +
        scale_colour_manual("Student Proficicency", values = c("black")) + 
        scale_x_continuous(breaks = round(seq(min(temp$value_final,na.rm = TRUE), max(temp$value_final,na.rm = TRUE), 1))) + 
        labs(title =paste0("Item Difficulty and Student Proficiency Distributions"), x = "Difficulty Parameter") +
        theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"),
              axis.title.x = element_text(size=16),
              axis.title.y = element_text(size=16),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12))+
        guides(color=guide_legend(override.aes=list(fill = NA))) + 
        annotate(geom="text", x = Inf, y = Inf, label = paste0("State Mean: ",round(pop.mean,2)),size = 6, hjust=1, vjust=1.5)+
        annotate(geom="text", x = Inf, y = Inf, label = paste0("State Standard Deviation: ",round(pop.sd,2)),size = 6, hjust=1, vjust=3)
    } else {
      ggplot(temp, aes(x=value_final)) + geom_histogram(aes(fill = factor(group)), color="black",binwidth = 0.5, show.legend=TRUE) +
        scale_fill_manual(input$dash1_stat,values = "light blue") + 
        labs(title =paste0("Cluster Variance Distribution"), x = "Cluster Variance") +
        theme(plot.title = element_text(hjust = 0.5, size = 16,face = "bold"),
              axis.title.x = element_text(size=16),
              axis.title.y = element_text(size=16),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12))
    }
    
    
    
  })
  
  output$dash1_DT <- renderDT({
    if (input$dash1_stat == "Cluster Variance") {
      temp <- filter(d1_dat(), Statistics == "Cluster variance") %>% 
        select(Master_ItemID = Dimension, State_ItemID, value_final)
    } else {
      temp <- filter(d1_dat(), Statistics == "Difficulty") %>% 
        select(Master_AssertionID = Dimension, State_ItemID, value_final)
    }
    
    if (input$dash1_hilo == "Highest") {
      temp %>% slice_max(value_final, n=input$dash1_topn, with_ties=FALSE)
    } else {
      temp %>% slice_min(value_final, n=input$dash1_topn, with_ties=FALSE)
    }
  }, filter = "top",options = list(scrollX = TRUE))
  
  output$dash1_ui_select <- renderUI({
    temp = d1_dat() %>% 
      filter(!is.na(State_ItemID)) %>% 
      filter(!is.na(value_final))
    selectInput("dash1_select", label=HTML('<p style="color:grey;">Select an Item to Display</p>'),
                choices = temp$State_ItemID, multiple = FALSE)
  })
  
  output$dash1_display <- renderDT({
    d1_dat() %>% 
      filter(State_ItemID %in% input$dash1_select) %>% 
      select(State_ItemID, Dimension, value_final) 
  },  options = list(scrollX = TRUE, dom = 'tip'))
  
  observeEvent(input$dash1_select, {
    updateCheckboxInput(session, "dash1_check_info", value = FALSE)
    output$dash1_item_info <- renderPlot({})
  })
  
  observe({
    if (input$dash1_check_info==TRUE) {
      output$dash1_item_info <- renderCachedPlot({
        d1_info_dat = d1_dat() %>% 
          filter(State_ItemID %in% input$dash1_select) %>% 
          select(State, Grade, State_ItemID, Dimension, value_final)
        cluster_ids = unique(d1_info_dat$Dimension[str_detect(d1_info_dat$Dimension,"^\\d+_\\d+")])
        d1_info_dat = d1_info_dat %>%
          group_by(State_ItemID) %>%
          mutate(type=ifelse(str_extract(Dimension,"\\d+_\\d+") %in% cluster_ids,"cluster","standalone"))
        
        SA_parm = d1_info_dat %>%
          filter(type == "standalone") %>%
          rename(b=value_final) %>%
          mutate(a=1, g=0) %>%
          select(a, b, g, State_ItemID, Dimension)
        if(nrow(SA_parm)==0) SA_parm=NULL
        
        cl_temp = d1_info_dat %>%
          ungroup() %>%
          filter(type == "cluster")
        if(nrow(cl_temp)==0) {
          Cluster_parm=NULL
        } else {
          cl_temp = cl_temp %>%
            select(State_ItemID) %>% distinct() %>%
            mutate(position = 1:n()) %>%
            right_join(cl_temp, "State_ItemID") %>%
            mutate(variance = ifelse(str_detect(Dimension, "^\\d+_\\d+"), "Y","N"))
          cl_var = filter(cl_temp, variance=="Y")
          cl_diff = filter(cl_temp, variance=="N")
          Cluster_parm = inner_join(cl_diff, select(cl_var, State_ItemID, value_final), "State_ItemID") %>%
            rename(b=value_final.x, cluster_var=value_final.y) %>%
            mutate(a=1, g=0) %>%
            select(a, b, cluster_var, position, State_ItemID, Dimension)
        }
        
        theta = seq(-6,6,by=0.01)
        library(doParallel)
        NumCore=detectCores(all.tests=FALSE, logical=TRUE)-2 # keep 2 cores idle
        registerDoParallel(cores=NumCore)
        r <- foreach(1:NumCore, .packages = c('dplyr', 'tibble')) %dopar% {
          source("./LW Function.R")
          source("./Item info_Function.R")
        }
        itx1 <- iter(theta)
        out <- foreach(i = itx1) %dopar% {
          item.info(i, SA_parm, Cluster_parm, n.nodes = 21)
        }
        out=as_tibble(t(simplify2array(out)))
        names(out)[-1] = paste0("i_",names(out)[-1])
        
        ggplot() +
          geom_point(data = out, aes_string(x = "theta", y = names(out)[2]), color="red", size=1) +
          theme(axis.title.x = element_text(size=20),
                axis.title.y = element_text(size=20),
                axis.text.x = element_text(size=16),
                axis.text.y = element_text(size=16))
      }, 
      cacheKeyExpr = list(input$dash1_select))
    } else {
      output$dash1_item_info <- renderPlot({})
    }
  })
  
  
  # Tab 1-2 Comparison: This currently shows difference between 2018 and 2019 calibration results.
  # in future years, this section can be used for presenting item parameter drift results
  observe({
    if(input$dash1_compare == 1) {
      showTab("dash1_tabset", target = "Comparison", select = TRUE)
    }
    if(input$dash1_compare == 0) {
      hideTab("dash1_tabset", target = "Comparison")
    }
  })
  
  output$dash1_tab2_plot <- renderPlotly({
    temp <- d1_dat() %>% 
      select(input$dash1_prm1, input$dash1_prm2, State, Grade, Dimension, State_ItemID, Statistics)
    if (input$dash1_stat == "Cluster Variance") {
      temp <- filter(temp, Statistics == "Cluster variance") %>% 
        dplyr::rename(Master_ItemID = Dimension)
    } else {
      temp <- filter(temp, Statistics == "Difficulty") %>% 
        dplyr::rename(Master_AssertionID = Dimension)
    }
    
    d1_tab2_correlation <- round(cor(temp[,input$dash1_prm1],temp[,input$dash1_prm2], use="pairwise.complete.obs"),3)
    
    d1_tab2_p = ggplot(temp, aes_string(x=input$dash1_prm1, y=input$dash1_prm2)) +  
      geom_point(color = "brown", size = 0.5) + 
      ggtitle("Parameter Comparison among Calibration Results") + 
      theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 15))
    
    ggplotly(d1_tab2_p) %>% 
      layout(annotations = list(xref="paper", yref="paper", x = 0.1, y = 0.9, 
                                text = paste0("cor = ",d1_tab2_correlation), showarrow = F, font = list(size = 16)))
  })
  
  
  # ========== Tab 2 ============
  # +++ how to display DIF results
  
  
  ### Tab 2-1-1 One State Classical stats summary (Assertion Level)
  # +++ Display DIF 
  # +++ Add modalDialoug to remind user DIF uses multiple stat data when available and not all item's DIF were computed due to small sample size
  
  # --- Average pvalue and biserial summary ---
  output$dash2_one_assertion_average <- renderPlot({
    temp_dat = filter(classical_stats_assertion, 
                      State == input$dash2_one_state, 
                      Grade_Band == input$dash2_one_grade,
                      Role == input$dash2_one_assertion_role) %>% 
      group_by(Item_Science_Discipline, Item_Type) %>% 
      dplyr::summarize(pvalue = mean(P_Value, na.rm = TRUE), biserial = mean(Biserial, na.rm = TRUE))
    allcombo = expand_grid(c("ESS", "LS", "PS"), c("Cluster","SA")) %>% 
      rename(c(Item_Science_Discipline=1,Item_Type=2))
    temp_dat = full_join(allcombo, temp_dat, by = c("Item_Science_Discipline", "Item_Type")) %>% 
      arrange(Item_Science_Discipline, Item_Type)
    
    yvar = sym(switch(input$dash2_one_assertion_stat, "P_Value" = expr(pvalue), "Biserial" = expr(biserial)))
    ylimit = switch(input$dash2_one_assertion_stat, "P_Value" = 0.7, "Biserial" = 0.7)
    ggplot(data = temp_dat, aes(x = interaction(Item_Science_Discipline, Item_Type, lex.order = T), y = !!yvar, group = 1)) + 
      geom_bar(position = "dodge", stat = "identity", width = .3) +
      annotate(geom = "text", x = seq_len(nrow(temp_dat)), y = -.025, label = temp_dat$Item_Type, size = 6) +
      annotate(geom = "text", x = 1.5 + 2 * (0:2), y = -.05, label = unique(temp_dat$Item_Science_Discipline), size = 6.5) +
      coord_cartesian(xlim = c(0.25, 6.75), ylim = c(0, ylimit), expand = FALSE, clip = "off") +
      scale_y_continuous(breaks = seq(0, ylimit, .1)) +
      labs(title=paste(input$dash2_one_state, input$dash2_one_grade, input$dash2_one_assertion_role, "Assertions Average", input$dash2_one_assertion_stat)) + 
      theme(plot.margin = unit(c(1, 1, 3, 1), "lines"),
            text = element_text(size = 20),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  })
  
  
  # --- Summary statistics plot ---
  observe({
    input4 = input$dash_2_one_assertion_select4
    input5 = input$dash_2_one_assertion_select5
    input6 = input$dash_2_one_assertion_select6
    if (sum(input4!="ALL",input5!="ALL",input6!="ALL")==3) {
      updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c(""),selected = NULL)
      disable("dash_2_one_assertion_select_grouping")
    } else if (sum(input4!="ALL",input5!="ALL",input6!="ALL")==2) {
      enable("dash_2_one_assertion_select_grouping")
      if (input4=="ALL") {
        updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c("Role"))
      } else if (input5=="ALL") {
        updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c("Type"))
      } else if (input6=="ALL") {
        updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c("Discipline"))
      }
    } else if (sum(input4!="ALL",input5!="ALL",input6!="ALL")==1) {
      enable("dash_2_one_assertion_select_grouping")
      if (input4!="ALL") {
        updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c("Type","Discipline"))
      } else if (input5!="ALL") {
        updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c("Role","Discipline"))
      } else if (input6!="ALL") {
        updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c("Role","Type"))
      }
    } else {
      updateSelectizeInput(session, "dash_2_one_assertion_select_grouping", choices=c("Role","Type","Discipline"))
      enable("dash_2_one_assertion_select_grouping")
    }
  })
  
  output$dash2_one_assertion_summary_plot <- renderPlotly({
    input4 = input$dash_2_one_item_select4
    input5 = input$dash_2_one_item_select5
    input6 = input$dash_2_one_item_select6
    filter_switch = list(switch(input4, ALL=TRUE, expr(Role==!!input4)),
                         switch(input5, ALL=TRUE, expr(Type==!!input5)),
                         switch(input6, ALL=TRUE, expr(Discipline==!!input6))
    )
    input_data <- classical_stats_assertion %>% 
      filter(State == input$dash2_one_state, Grade_Band == input$dash2_one_grade) %>% 
      rename(Discipline=Item_Science_Discipline, Type=Item_Type) %>% 
      filter(!!filter_switch[[1]],!!filter_switch[[2]],!!filter_switch[[3]])
    
    # Add a helper function to dynamically choose aesthetics according to user input
    plot_ly_helper_assertion <- function(xvar, yvar, colorvar) {
      xvar <- enexpr(xvar)
      yvar <- enexpr(yvar)
      colorvar <- enexpr(colorvar)
      expr(p <- plot_ly(data=input_data, x= !!xvar, y= !!yvar, type="box", color = !!colorvar) %>% 
             layout(boxmode = "group"))
    }
    
    grouping_chosen <- input$dash_2_one_assertion_select_grouping
    stats_chosen <- input$dash_2_one_assertion_select_stats
    
    if (is.null(grouping_chosen)) {
      fig = eval(plot_ly_helper_assertion(xvar=NULL, yvar = ~!!sym(stats_chosen), colorvar=NULL))
    }
    if (length(grouping_chosen)==1) {
      fig = eval(plot_ly_helper_assertion(xvar=~!!sym(grouping_chosen), yvar = ~!!sym(stats_chosen), colorvar=NULL))
    }
    if (length(grouping_chosen)==2) {
      fig = eval(plot_ly_helper_assertion(xvar=~!!sym(grouping_chosen[[1]]), yvar = ~!!sym(stats_chosen), colorvar=~!!sym(grouping_chosen[[2]])))
    }
    fig
  })
  
  # --- Showing spreadsheet as is ---
  output$dash2_one_assertion_DT <- renderDT({
    classical_stats_assertion %>% 
      filter(State == input$dash2_one_state, Grade_Band == input$dash2_one_grade) %>% 
      select(-State,-Grade_Band,-Master_AssertionID) %>% 
      rename(Discipline=Item_Science_Discipline)
  }, filter = "top", options = list(scrollX = TRUE, dom = 'lftip'))
  
  ### Tab 2-1-2 One State Classical stats summary (Item Level)
  # +++ Display DIF
  
  # --- Item Count Pie Chart ---
  output$dash2_pie <- renderPlotly({
    # An indicator for the variables to filter (implicitly) based on user input; TRUE indicates not filtering anything
    input1 = input$dash_2_one_item_select1
    input2 = input$dash_2_one_item_select2
    input3 = input$dash_2_one_item_select3
    filter_switch = list(switch(input1, ALL=TRUE, expr(Role==!!input1)),
                         switch(input2, ALL=TRUE, expr(Item_Type==!!input2)),
                         switch(input3, ALL=TRUE, expr(Item_Science_Discipline==!!input3))
    )
    # An indicator for which variables need to be grouped (implicitly) based on user input
    grouping_chosen = c("Role","Item_Type","Item_Science_Discipline")[map_lgl(filter_switch,is.logical)]
    # Data after applying filtering
    input_data <- classical_stats_item %>% 
      filter(State == input$dash2_one_state, Grade_Band == input$dash2_one_grade) %>% 
      filter(!!filter_switch[[1]],!!filter_switch[[2]],!!filter_switch[[3]])
    
    # Define a helper function to generate the code for plotting pie charts.
    # It dynamically determines the number of pie charts needed according to user inputs
    add_pie_helper = function(grouping_var, rnum, cnum) {
      grouping_var=ensym(grouping_var)
      exprs(
        data <- group_by(input_data, !!grouping_var), 
        data <- summarise(data, N=n()),
        p <- add_pie(p, data=data,labels=~!!grouping_var, values = ~N, type = 'pie',
                     name = "p1", domain = list(row=!!rnum,column=!!cnum),
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     hoverinfo = 'text',
                     text = ~paste(N, ' items'),
                     marker = list(line = list(color = 'white', width = 1)))
      )
    }
    p <- plot_ly()
    # use pmap function to generate a sequence of all pieces of codes needed to dynamically plot pie charts 
    temp_df = data.frame(grouping_var=grouping_chosen,rnum=rep(0,length(grouping_chosen)), cnum=seq_along(grouping_chosen)-1)
    temp = unlist(pmap(temp_df,add_pie_helper))
    # Evaluate the generated codes. An plotly object "p" is eventually created, carrying all the plots to be shown to the user
    for (j in 1:(length(grouping_chosen)*3)) {
      res <- eval(temp[[j]])
      invisible(res)
    }
    # Apply additional layout settings. It use an grid approach to create pie chart subplots
    p %>% layout(showlegend = F,
                 # title = "Number of Items", 
                 grid=list(rows=1, columns=nrow(temp_df)),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
      layout(autosize = F, width = 650, height = 300)
  })
  
  # --- Summary statistics plot ---
  observe({
    input4 = input$dash_2_one_item_select4
    input5 = input$dash_2_one_item_select5
    input6 = input$dash_2_one_item_select6
    if (sum(input4!="ALL",input5!="ALL",input6!="ALL")==3) {
      updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c(""),selected = NULL)
      disable("dash_2_one_item_select_grouping")
    } else if (sum(input4!="ALL",input5!="ALL",input6!="ALL")==2) {
      enable("dash_2_one_item_select_grouping")
      if (input4=="ALL") {
        updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c("Role"))
      } else if (input5=="ALL") {
        updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c("Type"))
      } else if (input6=="ALL") {
        updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c("Discipline"))
      }
    } else if (sum(input4!="ALL",input5!="ALL",input6!="ALL")==1) {
      enable("dash_2_one_item_select_grouping")
      if (input4!="ALL") {
        updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c("Type","Discipline"))
      } else if (input5!="ALL") {
        updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c("Role","Discipline"))
      } else if (input6!="ALL") {
        updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c("Role","Type"))
      }
    } else {
      updateSelectizeInput(session, "dash_2_one_item_select_grouping", choices=c("Role","Type","Discipline"))
      enable("dash_2_one_item_select_grouping")
    }
  })
  
  output$dash2_one_item_summary_plot <- renderPlotly({
    input4 = input$dash_2_one_item_select4
    input5 = input$dash_2_one_item_select5
    input6 = input$dash_2_one_item_select6
    filter_switch = list(switch(input4, ALL=TRUE, expr(Role==!!input4)),
                         switch(input5, ALL=TRUE, expr(Type==!!input5)),
                         switch(input6, ALL=TRUE, expr(Discipline==!!input6))
    )
    input_data <- classical_stats_item %>% 
      filter(State == input$dash2_one_state, Grade_Band == input$dash2_one_grade) %>% 
      rename(Discipline=Item_Science_Discipline, Type=Item_Type) %>% 
      filter(!!filter_switch[[1]],!!filter_switch[[2]],!!filter_switch[[3]])
    
    # Add a helper function to dynamically choose aesthetics according to user input
    plot_ly_helper <- function(xvar, yvar, colorvar) {
      xvar <- enexpr(xvar)
      yvar <- enexpr(yvar)
      colorvar <- enexpr(colorvar)
      expr(p <- plot_ly(data=input_data, x= !!xvar, y= !!yvar, type="box", color = !!colorvar) %>% 
             layout(boxmode = "group"))
    }
    
    plot_ly_helper2 <- function(xvar, colorvar) {
      xvar <- enexpr(xvar)
      colorvar <- enexpr(colorvar)
      expr(p <- plot_ly(data=input_data, x= !!xvar, type="histogram", color = !!colorvar, legendgroup = !!colorvar) %>% 
             layout(barmode = "group", bargap=0.3, bargroupgap=0.1))
    }
    
    plot_ly_helper3 <- function(xvar, colorvar, filtervar) {
      filter_levels <- unlist(unique(input_data[, filtervar]))
      xvar <- enexpr(xvar)
      colorvar <- enexpr(colorvar)
      p <- list()
      for (i in 1:length(filter_levels)) {
        temp_data <- input_data %>% filter(!!sym(filtervar) == filter_levels[[i]])
        if (i==length(filter_levels)) {
          p[[i]] <- eval(expr(plot_ly(data=temp_data, x= !!xvar, type="histogram", color = !!colorvar, legendgroup = !!colorvar, showlegend = TRUE) %>%
                                layout(barmode = "group", bargap=0.3, bargroupgap=0.1) %>% 
                                add_annotations(
                                  text = filter_levels[[i]],
                                  x = 0.5,
                                  y = 1,
                                  yref = "paper",
                                  xref = "paper",
                                  xanchor = "middle",
                                  yanchor = "top",
                                  showarrow = FALSE,
                                  font = list(size = 15)
                                )))
        } else {
          p[[i]] <- eval(expr(plot_ly(data=temp_data, x= !!xvar, type="histogram", color = !!colorvar, legendgroup = !!colorvar, showlegend = FALSE) %>% 
                                layout(barmode = "group", bargap=0.3, bargroupgap=0.1) %>% 
                                add_annotations(
                                  text = filter_levels[[i]],
                                  x = 0.5,
                                  y = 1,
                                  yref = "paper",
                                  xref = "paper",
                                  xanchor = "middle",
                                  yanchor = "top",
                                  showarrow = FALSE,
                                  font = list(size = 15)
                                )))
        }
      }
      names(p) <- paste0("p",1:length(p))
      p
    }
    
    grouping_chosen <- input$dash_2_one_item_select_grouping
    stats_chosen <- input$dash_2_one_item_select_stats
    
    if (stats_chosen %in% c("Number_of_Assertions","Percentile_80")) { # show histogram
      if (is.null(grouping_chosen)) {
        fig = eval(plot_ly_helper2(xvar=~!!sym(stats_chosen), colorvar=NULL))
      }
      if (length(grouping_chosen)==1) {
        fig = eval(plot_ly_helper2(xvar=~!!sym(stats_chosen), colorvar=~!!sym(grouping_chosen)))
      }
      if (length(grouping_chosen)==2) {
        temp_fig = plot_ly_helper3(xvar=~!!sym(stats_chosen), colorvar=~!!sym(grouping_chosen[[1]]), filtervar=grouping_chosen[[2]])
        fig = eval(expr(subplot(!!!syms(names(temp_fig)), nrows = length(temp_fig), shareX = TRUE)), envir = temp_fig)
      }
    } else { # show boxplot
      if (is.null(grouping_chosen)) {
        fig = eval(plot_ly_helper(xvar=NULL, yvar = ~!!sym(stats_chosen), colorvar=NULL))
      }
      if (length(grouping_chosen)==1) {
        fig = eval(plot_ly_helper(xvar=~!!sym(grouping_chosen), yvar = ~!!sym(stats_chosen), colorvar=NULL))
      }
      if (length(grouping_chosen)==2) {
        fig = eval(plot_ly_helper(xvar=~!!sym(grouping_chosen[[1]]), yvar = ~!!sym(stats_chosen), colorvar=~!!sym(grouping_chosen[[2]])))
      }
    }
    fig
  })
  
  
  # --- Showing spreadsheet as is ---
  output$dash2_one_item_DT <- renderDT({
    classical_stats_item %>% 
      filter(State == input$dash2_one_state, Grade_Band == input$dash2_one_grade) %>% 
      select(-State,-Grade_Band, -`Available_for_2020?`) %>% 
      rename(Discipline=Item_Science_Discipline)
  }, filter = "top", options = list(scrollX = TRUE, dom = 'lftip'))
  
  ### Tab 2-2 Two State Classical stats comparison
  # +++ Should be able to feed a classical stats final summary file and compare it with existing stats from previous year
  
  createDataFrame = reactive({
    req(classical_stats_item, input$dash2_two_state1, input$dash2_two_state2, input$dash2_two_grade, input$dash2_two_stat, input$dash2_two_point_desc)
    dat1 = filter(classical_stats_assertion, State == input$dash2_two_state1, Grade_Band==input$dash2_two_grade) %>% 
      select(input$dash2_two_stat, Master_AssertionID, Grade_Band, N, ItemID, Role, Item_Type, Item_Science_Discipline)
    dat2 = filter(classical_stats_assertion, State == input$dash2_two_state2, Grade_Band==input$dash2_two_grade) %>% 
      select(input$dash2_two_stat, Master_AssertionID, Grade_Band, N, ItemID, Role, Item_Type, Item_Science_Discipline)
    suffix1 = ifelse(input$dash2_two_state1 %in% state.name, paste0("_",state.abb[state.name == input$dash2_two_state1]), "_All_States")
    suffix2 = ifelse(input$dash2_two_state2 %in% state.name, paste0("_",state.abb[state.name == input$dash2_two_state2]), "_All_States")
    if (suffix1==suffix2) {suffix2 = paste0(suffix2, ".y")}
    dat_merged = inner_join(dat1, dat2, by = c("Master_AssertionID","Grade_Band","Role","Item_Type","Item_Science_Discipline"), suffix = c(suffix1, suffix2))
    
    
    input4 = input$dash_2_two_filter4
    input5 = input$dash_2_two_filter5
    input6 = input$dash_2_two_filter6
    filter_switch = list(switch(input4, ALL=TRUE, expr(Role==!!input4)),
                         switch(input5, ALL=TRUE, expr(Type==!!input5)),
                         switch(input6, ALL=TRUE, expr(Discipline==!!input6))
    )
    input_data <- dat_merged %>% 
      rename(Discipline=Item_Science_Discipline, Type=Item_Type) %>% 
      filter(!!filter_switch[[1]],!!filter_switch[[2]],!!filter_switch[[3]])
    input_data
  })
  
  output$dash2_two_scatter <- renderPlotly({
    dash2_two_assertion_data = createDataFrame()
    suffix1 = ifelse(input$dash2_two_state1 %in% state.name, paste0("_",state.abb[state.name == input$dash2_two_state1]), "_All_States")
    suffix2 = ifelse(input$dash2_two_state2 %in% state.name, paste0("_",state.abb[state.name == input$dash2_two_state2]), "_All_States")
    suffixes = syms(names(dash2_two_assertion_data)[str_detect(names(dash2_two_assertion_data), input$dash2_two_stat)])
    
    correlations = round(cor(select(dash2_two_assertion_data, !!!suffixes))[1,2],3)
    
    # @@ Add additional hover texts based on user choices. 
    # @@ Use rlang's quosiquotation to unquote additional_aes in the expression that calls ggplot and then evaluate the expression
    additional_aes = input$dash2_two_point_desc
    if ("N" %in% additional_aes) {
      additional_aes <- additional_aes[additional_aes!="N"]
      additional_aes <- c(additional_aes, paste0("N",suffix1), paste0("N",suffix2))
    }
    if ("State_ItemID" %in% additional_aes) {
      if (input$dash2_two_state1 == "All States" && input$dash2_two_state2 != "All States") {
        additional_aes <- additional_aes[additional_aes!="State_ItemID"]
        additional_aes <- c(additional_aes, paste0("ItemID",suffix2))
      } else if (input$dash2_two_state1 != "All States" && input$dash2_two_state2 == "All States") {
        additional_aes <- additional_aes[additional_aes!="State_ItemID"]
        additional_aes <- c(additional_aes, paste0("ItemID",suffix1))
      } else if (input$dash2_two_state1 == "All States" && input$dash2_two_state2 == "All States") {
        additional_aes <- additional_aes[additional_aes!="State_ItemID"]
      } else {
        additional_aes <- additional_aes[additional_aes!="State_ItemID"]
        additional_aes <- c(additional_aes, paste0("ItemID",suffix1), paste0("ItemID",suffix2))
      }
    }
    
    additional_aes <- syms(additional_aes)
    # additional_aes = setNames(additional_aes, paste0("label",seq_along(additional_aes)))
    names(additional_aes) = paste0("label",seq_along(additional_aes))
    p = eval(expr(ggplot(dash2_two_assertion_data, 
                         aes(x = !!suffixes[[1]] , y = !!suffixes[[2]], key = Master_AssertionID,
                             !!!additional_aes)))) + 
      geom_point(color = "firebrick", size = 0.5) + 
      ggtitle("Comparison Between Two States") + 
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p) %>% 
      layout(annotations = list(xref="paper", yref="paper", x = 0.1, y = 0.9, 
                                text = paste0("cor = ",correlations), showarrow = F, font = list(size = 16))) %>% 
      event_register("plotly_click")
  })
  
  d2_clicked_df_item <- reactive({
    if (!("" %in% c(input$dash2_two_state1, input$dash2_two_state2, input$dash2_two_grade))) {
      d0 <- event_data("plotly_click")
      Master_ItemID_clicked <- str_extract(d0$key, "^\\d+_\\d+")
      classical_stats_item %>% 
        filter(Master_ItemID %in% Master_ItemID_clicked, State %in% c(input$dash2_two_state1,input$dash2_two_state2))
    } else {
      tibble()
    }
  })
  
  d2_clicked_df_assertion <- reactive({
    if (!("" %in% c(input$dash2_two_state1, input$dash2_two_state2, input$dash2_two_grade))) {
      d0 <- event_data("plotly_click")
      Master_ItemID_clicked <- str_extract(d0$key, "^\\d+_\\d+")
      AssertionID_clicked <- str_replace(d0$key, "^\\d+_\\d+_","")
      return(list(Master_ItemID_clicked=Master_ItemID_clicked, AssertionID_clicked=AssertionID_clicked))
    } else {
      tibble()
    }
  })
  
  observe({
    if (nrow(d2_clicked_df_item())>=1) {
      output$dash2_two_heading1 <- renderUI({
        fluidRow(column(6, tags$b("Item Level Summary", style = "font-size: 150%; font-family:Helvetica; color:#000000")))
      })
      
      output$dash2_two_hr1 <- renderUI({
        tags$hr(style="border-color: black;")
      })
      
      output$dash2_two_heading2 <- renderUI({
        fluidRow(column(6, tags$b("Assertions from This Item", style = "font-size: 150%; font-family:Helvetica; color:#000000")))
      })
      
      output$dash2_two_hr2 <- renderUI({
        tags$hr(style="border-color: black;")
      })
    }
  })
  
  
  output$dash2_two_clicked_summary <- renderPrint({
    if (nrow(d2_clicked_df_item())>=1) {
      d2_two_DT <- d2_clicked_df_item()
      details <- if(nrow(d2_two_DT) == 0) {
        br()
      } else if(nrow(d2_two_DT) >= 1) {
        p(tags$b("Master_ItemID: ", style = "font-size: 130%; font-family:Helvetica; color:#000000"), tags$em(d2_two_DT$Master_ItemID[1], style = "font-size: 130%;"), br(),
          tags$b("Role: ", style = "font-size: 130%; font-family:Helvetica; color:#000000"), tags$em(d2_two_DT$Role[1], style = "font-size: 130%;"), br(),
          tags$b("Item Type: ", style = "font-size: 130%; font-family:Helvetica; color:#000000"), tags$em(d2_two_DT$Item_Type[1], style = "font-size: 130%;"), br(),
          tags$b("Science Discipline: ", style = "font-size: 130%; font-family:Helvetica; color:#000000"), tags$em(d2_two_DT$Item_Science_Discipline[1], style = "font-size: 130%;")) }
      details
    }
  })
  
  output$dash2_two_clicked_summary2 <- renderTable({
    if (nrow(d2_clicked_df_item())>=1) {
      d2_two_DT <- d2_clicked_df_item() %>% 
        select(State, ItemID, Average_of_P_Value, Average_of_Biserial, Percentile_80) %>% 
        gather(Statistics, value, -State) %>% spread(State, value)
      if(nrow(d2_two_DT) == 0) { } else {bind_rows(d2_two_DT %>% slice(3), d2_two_DT %>% slice(-3))}
    }
  })
  
  output$dash2_two_clicked <- renderDT({
    if (length(d2_clicked_df_assertion())>0) {
      temp <- d2_clicked_df_assertion()
      d2_two_DT <- classical_stats_assertion %>% 
        filter(Master_ItemID %in% temp$Master_ItemID_clicked, State %in% c(input$dash2_two_state1,input$dash2_two_state2)) %>% 
        select(State,ItemID,Assertion_ID,N,P_Value,Biserial)
      datatable(d2_two_DT, rownames= FALSE, options = list(pageLength = 50, scrollX = TRUE)) %>% {
        if (length(temp$AssertionID_clicked)>0)
          formatStyle(.,
                      "Assertion_ID",
                      target = 'row',
                      backgroundColor = styleEqual(temp$AssertionID_clicked, c('yellow'))
          )
      }
    }
  })
  
  ### Tab 3 Software Comparison
  observe({
    temp = input$dash3_software1
    updateSelectInput(session, "dash3_software2", choices = d3_cols[d3_cols!=temp])
  })
  
  output$dash3_software_scatter = renderPlotly({
    d3_dat = get(input$dash3_stat)
    d3_dat = select(d3_dat, 1, input$dash3_software1, input$dash3_software2)
    d3_dat$diff = abs(d3_dat[,2] - d3_dat[,3])
    d3_correlation = round(cor(d3_dat[,2],d3_dat[,3]),3)
    
    d3_p = ggplot(d3_dat, aes_string(x=input$dash3_software1, y=input$dash3_software2, label1 = colnames(d3_dat)[1])) +  
      geom_point(color = "brown", size = 0.5) + 
      ggtitle("CT G5 Parameter Comparison Between Software Outputs") + 
      theme(plot.title = element_text(lineheight=.8, hjust = 0.5, vjust = 1, size = 15))
    # panel.background = element_rect(fill = 'grey50'))
    
    ggplotly(d3_p) %>% 
      layout(annotations = list(xref="paper", yref="paper", x = 0.1, y = 0.9, 
                                text = paste0("cor = ",d3_correlation), showarrow = F, font = list(size = 16)))
  })
  
  if (exists("Deviance")) {
    output$d3_deviance_value1 = renderText({as.numeric(Deviance[1,input$dash3_software1])})
    output$d3_deviance_value2 = renderText({as.numeric(Deviance[1,input$dash3_software2])})
    output$show_deviance = renderUI({
      box(
        splitLayout(cellWidths = c(paste0(max(nchar(input$dash3_software1), nchar(input$dash3_software2))+2,"%"), "15%"),
                    h5(strong(paste0(input$dash3_software1, ":"))),
                    verbatimTextOutput(outputId="d3_deviance_value1")),
        splitLayout(cellWidths = c(paste0(max(nchar(input$dash3_software1), nchar(input$dash3_software2))+2,"%"), "15%"),
                    h5(strong(paste0(input$dash3_software2, ":"))),
                    verbatimTextOutput(outputId="d3_deviance_value2")),
        title = "Deviance",status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12)
    })
  }
  
  output$dash3_DT <- renderDT({
    d3_dat = get(input$dash3_stat)
    d3_dat = select(d3_dat, 1, input$dash3_software1, input$dash3_software2)
    d3_dat$diff = round(d3_dat[,2] - d3_dat[,3], 3)
    d3_dat$abs_diff = abs(d3_dat$diff)
    d3_dat %>% slice_max(abs_diff, n=input$dash3_topn, with_ties=FALSE)
  }, options = list(scrollX = TRUE, dom = 'lftip')) # l - entries box, f - search box, t - table, i - info summary, p - page; This is the default setting. Letter position matters
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
