# How to run in terminal:
# R -e "shiny::runApp('app.R')"

library(shiny)
library(data.table)
library(xlsx)
library(stringr)

ui <- fluidPage(

    tabsetPanel(

      tabPanel("Welcome Screen", fluid = TRUE,
                 mainPanel(
                   tags$h1("Welcome!"),
                   tags$p("This tool assists in calculation of differential alignment, which is a measure of the extent to which an outcome measure favors one treatment condition over the other. Details of how differential alignment is conceptualized and calculated are provided in:"),
                   tags$p("Taylor, J. A., Polanin, J. R., Kowalski, S. M., Wilson, C. D., & Stuhlsatz, M. A. (2022). Addressing test fairness in education research: A process for quantifying the alignment between outcome measures and education interventions. Social Sciences & Humanities Open, 6(1), 100312."),
                   tags$h2("Step 1"),
                   tags$p("Please go to the \"Pre-Assessment\" tab to ensure you have the required data before moving on."),
                   tags$h2("Step 2"),
                   tags$p("Go to the \"Download Data Format\" tab to properly format your data for calculations."),
                   tags$h2("Step 3"),
                   tags$p("Go to the \"Perform the Analysis\" tab and upload your data to perform the necessary calculations."),
                   tags$h3("More Info"),
                   tags$p("If you have any questions please contact Dr. Joseph Taylor at", a("jtlr19@gmail.com", href="mailto: jtlr19@gmail.com")),
                   tags$p("A tutorial", a("Youtube video", href="mailto: jtlr19@gmail.com"), "is available.")
                 )
               ),
      tabPanel("#1 Pre-Assessment", fluid = TRUE,
                 mainPanel(
                   tags$h1("Instructions"),
                   tags$p("Please answer the following questions to ensure you have the required data to perform the analysis."),
                    tags$p("1. For the outcome measure, do you know how many test items were associated with each topic?"),
                    fluidRow(
                    radioButtons("Question1","",choices=c('Yes'=1,'No'=2),selected = "_None")),
                                                uiOutput("Answer1"),
                    tags$br(),
                    tags$p("2. Do you know how many instructional days/hours (or curriculum pages) were devoted to instruction on topics addressed by the outcome measure and do you know that for both the treatment and comparison groups?"),
                    fluidRow(
                    radioButtons("Question2","",choices=c('Yes'=1,'No'=2),selected = "_None")),
                                                uiOutput("Answer2"),
                    tags$br(),
                    tags$p("3. (If you answered \"No\" on #2:) Can you estimate the number of days/hours/pages of instructional time the that developers of the treatment group and comparison group programs intended to be devoted to each of the topics on the test?"),
                    fluidRow(
                    radioButtons("Question3","",choices=c('Yes'=1,'No'=2),selected = "_None")),
                                                uiOutput("Answer3"),
                    tags$br(),
                    tags$p("4. Did your comparison group include one instructional program or multiple? "),
                    fluidRow(
                    radioButtons("Question4","",choices=c('One'=1,'Multiple'=2),selected = "_None")),
                                                uiOutput("Answer4")
                 )
                 ),

      tabPanel("#2 Download Data Format", fluid = TRUE,
                 mainPanel(
                   tags$h1("Instructions"),
                   tags$p("Input the number of instructional programs used in the comparison group, and then the names of the topics on the test."),
                     # Copy the line below to make a text input box
                  numericInput("number_of_comparison_groups", "Number of Instructional Programs",1,1),
                  uiOutput("BoxError"),
                  hr(),
                  textAreaInput("subject_names", "Topic:\n(Split Subjects with \"Pipe\" Charater | )",value = "A Topic | Another Topic | Etc.",resize = "both"),
                  hr(),
                  downloadButton("downloadTemplate", "Download Input Template")
                 )
               ),
      tabPanel("#3 Perform the Analysis", fluid = TRUE,
                 mainPanel(
                   tags$h1("Upload data here:"),
                   tags$p("Upload the completed Excel file, and then press the \"Submit\" button."),
                  hr(),
                  fileInput("uploaded_file", "Upload Completed Excel Sheet", accept = ".xlsx"),
                  actionButton("run_analysis", "Run Analysis"),
                  tableOutput('result_table'),
                  tableOutput('overall_diff_alignment_table')
                 )
               )
      )
    )

# Define server logic here:
server <- function(input, output) {

# Perform the Analysis on Button Click ----

observeEvent(input$run_analysis, {

inFile <- input$uploaded_file
wb <- loadWorkbook(inFile$datapath)
number_of_sheets <- length(names(getSheets(wb)))
number_of_sheets

if (number_of_sheets == 3) {

data <- read.xlsx(inFile$datapath, 1)
test_emphasis_data <- read.xlsx(inFile$datapath, 2)
group_data <- read.xlsx(inFile$datapath, 3)

data <- as.data.table(data)
test_emphasis_data <- as.data.table(test_emphasis_data)
group_data <- as.data.table(group_data)

# Change readable names to variables that are used in the calculation.
setnames(data, c("subject",	"treatment_group", "control_group"))
setnames(test_emphasis_data, c("subject",	"test_emphasis"))
setnames(group_data, c("group",	"measurement_total"))
group_data[, 1] <- c("treatment_group", "control_group")

long_data <- melt(data, id.vars = "subject")

merged_data <- long_data[group_data, on = c(variable = "group")]

test_emphasis_data[, test_emphasis_proportion := test_emphasis / sum(test_emphasis)]

merged_data[, proportion := value / measurement_total, by = variable]

wide_data <- dcast(merged_data, subject ~ variable, value.var = "proportion")

full_data <- wide_data[test_emphasis_data[, .(subject, test_emphasis_proportion)], on = "subject"]

full_data[, subject_differential_alignment := (treatment_group - control_group) * test_emphasis_proportion]

overall_differential_alignment <- full_data[, .(`Overall Differential Alignment` = sum(subject_differential_alignment) )] 


setcolorder(full_data, c("subject","test_emphasis_proportion", "treatment_group", "control_group", "subject_differential_alignment"))


setnames(full_data, c("Test Topic","Test Proportional Emphasis", "Treatment Proportional Emphasis", "Control Proportional Emphasis", "Differential Alignment"))


output$result_table <- renderTable(full_data, digits = 4)
output$overall_diff_alignment_table <- renderTable(overall_differential_alignment, digits = 4)


}

if (number_of_sheets == 4) {

comparison_groups <- read.xlsx(inFile$datapath, 1)
test_emphasis <- read.xlsx(inFile$datapath, 2)
control_group_characteristics <- read.xlsx(inFile$datapath, 3)
treatment_group_characteristics <- read.xlsx(inFile$datapath, 4)

comparison_groups <- as.data.table(comparison_groups)
test_emphasis <- as.data.table(test_emphasis)
control_group_characteristics <- as.data.table(control_group_characteristics)
treatment_group_characteristics <- as.data.table(treatment_group_characteristics)

# Change readable names to variables that are used in the calculation.
setnames(comparison_groups, 1, "subject")
setnames(comparison_groups, 2, "Treatment Group")

col_name_test <- names(comparison_groups)[-1:-2]
col_name_test <- str_replace_all(col_name_test, "\\.", " ")
col_name_test <- str_replace_all(col_name_test,"  Days Hours Pages ", "")
some_length <- names(comparison_groups)
end_of_col_names <- length(some_length)
setnames(comparison_groups, 3:end_of_col_names, col_name_test)

setnames(test_emphasis, c("subject", "test_emphasis"))
setnames(control_group_characteristics, c("group", "measurement_total", "sample_size"))
setnames(treatment_group_characteristics, c("group", "measurement_total"))

test_emphasis[, proportion := test_emphasis / sum(test_emphasis)]

measurment_totals <- rbindlist(list(control_group_characteristics[,.(group, measurement_total)], treatment_group_characteristics))

long_data <- melt(comparison_groups, id.vars = "subject")
long_data <- long_data[measurment_totals, on = c(variable = "group")]
long_data[, proportion := value / measurement_total]

treatment_proportional_emphasis <- long_data[variable == "Treatment Group", .(subject, treatment_proportion = proportion)]

control_group_proportional_emphasis <- long_data[variable != "Treatment Group", .(subject, variable, control_group_proportion = proportion)]

temp <- control_group_proportional_emphasis[control_group_characteristics[, .(group, sample_size)], on = c(variable = "group")]

weighted_proportional_emphasis <- temp[,
     .( weighted_proportional_emphasis = sum(control_group_proportion * sample_size) / sum(control_group_characteristics$sample_size) ), by = subject]

temp <- weighted_proportional_emphasis[treatment_proportional_emphasis, on = "subject"]
temp <- temp[test_emphasis[, .(subject, test_emphasis = proportion)], on= "subject"]

subject_proportional_emphasis <- temp[, .(subject,test_emphasis, treatment_proportion,weighted_proportional_emphasis, weighted_subject_proportional_emphasis = (treatment_proportion - weighted_proportional_emphasis) * test_emphasis) ]

overall_differential_alignment <- subject_proportional_emphasis[, .("Overall Differential Alignment" = sum(weighted_subject_proportional_emphasis))]

colnames(subject_proportional_emphasis) <- c("Test Topic", "Test Proportional Emphasis", "Treatment Proportional Emphasis", "Weighted Proportional Emphasis of Comparison Programs", "Differential Alignment")

output$result_table <- renderTable(subject_proportional_emphasis, digits = 4)
output$overall_diff_alignment_table <- renderTable(overall_differential_alignment, digits = 4)


}

})


# Pre-Assessment Guidance ---------

output$Answer1 <- renderUI({
    actionID <- input$Question1
    if(!is.null(actionID)){
      if(input$Question1 == 1){message = "Good! Move on to the next question."}
      if(input$Question1 == 2){message = "You will need to figure out how many test items were associated with each topic before moving on."}
      if(input$Question1 == 1){p(message,style="background-color:#BFF7BB")} else {
      p(message, style="background-color:#FFA8A8")
    }} else
    {""}
})

output$Answer2 <- renderUI({
    actionID <- input$Question2
    if(!is.null(actionID)){
      if(input$Question2 == 1){message = "Good! Move on to question #4."}
      if(input$Question2 == 2){message = "Before continuing, you will need to first determine how many instructional days/hours/pages were devoted to instruction on each test topic for both the treatment and comparison programs. Move on to question #3."}
      if(input$Question2 == 1){p(message,style="background-color:#BFF7BB")} else {
      p(message, style="background-color:#FFA8A8")
    }} else
    {""}
})


output$Answer3 <- renderUI({
    actionID <- input$Question3
    if(!is.null(actionID)){
      if(input$Question3 == 1){message = "Good! Move on to the next question."}
      if(input$Question3 == 2){message = "You need to gather this information before calculating differential alignment."}
      if(input$Question3 == 1){p(message,style="background-color:#BFF7BB")} else {
      p(message, style="background-color:#FFA8A8")
    }} else
    {""}
})


output$Answer4 <- renderUI({
    actionID <- input$Question4
    if(!is.null(actionID)){
      if(input$Question4 == 1){message = "On the next page ensure that the number 1 is entered in the \"Number of Instructional Programs\" Box."}
      if(input$Question4 == 2){message = "On the next page ensure that a number greater than 1 is entered in the \"Number of Instructional Programs\" Box."}
      if(input$Question4 == 1){p(message,style="background-color:yellow")} else {
      p(message, style="background-color:yellow")
    }} else
    {""}
})

# Downloadable xlsx of user input ----

output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("data_input", ".xlsx", sep = "")
    },
    content = function(file) {

input_vec <- unlist(strsplit(input$subject_names, "\\Q|\\E", perl = TRUE))
#input_vec <- c("Hello there|\nanothe subject|\nhistory| scientific\ninquery| howdy doo| ")
input_vec <- unlist(strsplit(input_vec, "\\Q|\\E", perl = TRUE))
input_vec <- gsub("\n"," ",input_vec)
subject_names <- trimws(input_vec, which = "both")
empty_values <- subject_names[1:length(subject_names)] != ""
subject_names <- subject_names[empty_values]


if (input$number_of_comparison_groups == 1) {
# Example 1
## Comparison Groups Tab

user_input <- subject_names

subjects <- user_input 
column_names_te <- c("Test Topic","Treatment Group (Days/Hours/Pages)","Control Group (Days/Hours/Pages)")
comparison_group_template <- data.frame(matrix(0, length(subjects), length(column_names_te)))
colnames(comparison_group_template) <- column_names_te
comparison_group_template[1] <- subjects
write.xlsx(comparison_group_template, file=file, sheetName = "Comparison Groups", append = FALSE, row.names = FALSE)

## Test Emphasis Tab

column_names_te <- c("Test Topic","Test Items Per Topic")
test_emphasis_template <- data.frame(matrix(0, length(subjects), length(column_names_te)))
colnames(test_emphasis_template) <- column_names_te
test_emphasis_template[1] <- subjects
write.xlsx(test_emphasis_template, file=file, sheetName = "Test Emphasis", append = TRUE, row.names = FALSE)

## Group Characteristics Tab

column_names_gc <- c("Treatment Condition", "Total Days/Pages/Hours")
groups <- c("Treatment", "Control/Comparison")
group_characteristics_template <- data.frame(matrix(0, length(groups), length(column_names_gc)))
colnames(group_characteristics_template) <- column_names_gc
group_characteristics_template[1] <- groups 
write.xlsx(group_characteristics_template, file=file, sheetName = "Group Characteristics", append = TRUE, row.names = FALSE)

}

if (input$number_of_comparison_groups != 1) {

user_input <- subject_names

input <- input$number_of_comparison_groups

subjects <- user_input 
column_names <- c("Test Topic", "Treatment Group (Days/Hours/Pages)")
clean_row_names <- c()
for (x in 1:input) {
col_name <- str_interp("Control Program ${x} (Days/Hours/Pages)")
 column_names <- append(column_names, col_name)
current_row_name <- str_interp("Control Program ${x}")
 clean_row_names <- append(clean_row_names, current_row_name)
}
comparison_groups_template <- data.frame(matrix(0, length(subjects), length(column_names)))
colnames(comparison_groups_template) <- column_names
comparison_groups_template[1] <- subjects
write.xlsx(comparison_groups_template, file=file, sheetName = "Comparison Groups", append = FALSE, row.names = FALSE)

## Test Emphasis Tab

column_names_te <- c("Test Topic", "Test Items Per Topic")
test_emphasis_template <- data.frame(matrix(0, length(subjects), length(column_names_te)))
colnames(test_emphasis_template) <- column_names_te 
test_emphasis_template[1] <- subjects
write.xlsx(test_emphasis_template, file=file, sheetName = "Test Emphasis", append = TRUE, row.names = FALSE)

## Control Group Characteristics Tab

column_names_cgc <- c("Control Group", "Total Days/Pages/Hours", "Group Sample Size")
group_names <- clean_row_names
control_group_characteristics_template <- data.frame(matrix(0, length(group_names), length(column_names_cgc)))
control_group_characteristics_template[1] <- group_names
colnames(control_group_characteristics_template) <- column_names_cgc
write.xlsx(control_group_characteristics_template, file=file, sheetName = "Control Group Characteristics", append = TRUE, row.names = FALSE)

## Treatment Group Characteristics Tab

column_names_tgc <- c("Treatment Condition", "Total Days/Pages/Hours")
#treatment_group_name <- column_names[2]
treatment_group_name <- "Treatment Group"
treatment_group_characteristics_template <- data.frame(matrix(0, length(treatment_group_name), length(column_names_tgc)))
treatment_group_characteristics_template[1] <- treatment_group_name
colnames(treatment_group_characteristics_template) <- column_names_tgc
write.xlsx(treatment_group_characteristics_template, file=file, sheetName = "Treatment Group Characteristics", append = TRUE, row.names = FALSE)

}
    }
  )

}



# Run the application
shinyApp(ui = ui, server = server)
