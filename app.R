#
# This Shiny app implements ONA API key capture through a field
#


library(tidyjson) # BEWARE its filter() masks the tidyverse one!
library(curl)
library(shiny)
library(DT) # so we use DT::renderDataTable()
library(readr)
library(readxl)
library(dplyr)
library(forcats) # for as_factor
library(plotly) # for interactive graphics
library(scales) # for nicer scales
library(magrittr) # for %<>% only
library(reactlog)

# tell shiny to log all reactivity
reactlog_enable()

## HARDCODED PARAMS ##
ona_API_server_prefix <- "https://ona.ilri.org/api/v1"

firstExpDay <- -2 # hardcoded here: first day with observations, Day 0 being vacc/chall day.

# the temperature is missing from the scoring_columns because it is the result
# of a transformation (computed variable)
scoring_columns <- c("inappetence", "recumbency", "haemorrhage", "jntswelling",
                           "breathing", "odschrge", "diarrhoea", "urine", "vomit")
# NEVER-to-display columns (list can contain non-existent columns, no prob):
# dont_display_columns <- c("animtech")
dont_display_columns <- character()
 
# Note we will also add to the non-displayable columns all those starting with "_".

## END HARDCODED PARAMS ##


## ANCILLARY FUNCTIONS ##

# the following function is used mainly to trim prefixes like "grp1_sec1b/"
# from column names (will be used as a function called by dplyr::rename())
remove_group_prefix <- function (s) {
  # input is a mere string: we trim the prefix upto and including the **first** occurrence (scanning left to right) of a slash (/)
  s %>% stringr::str_replace("[^/]*/", "")
}


# According to https://stackoverflow.com/questions/74562346/prevent-ggplotly-to-change-the-legends-style-created-in-ggplot,
# we have to fix plotly's handling of legends for geom_line(),
# because it ignores its show.legend option
solid_lines_legend <- function(plotly_obj) {
  # the input to this function is a plotly output.
  # this fix borrows heavily from the one by https://stackoverflow.com/users/5329073/kat
  # here: https://stackoverflow.com/questions/74562346/prevent-ggplotly-to-change-the-legends-style-created-in-ggplot
  # BEWARE: lines that are dash-only WILL NOT appear in the legend
  lapply(1:length(plotly_obj$x$data),
         function(j) {
           if(plotly_obj$x$data[[j]]$mode == "lines") {
             if(plotly_obj$x$data[[j]]$line$dash == "dash" |
                nchar(plotly_obj$x$data[[j]]$name) == 0) # anonymous line: do not legend
               plotly_obj$x$data[[j]]$showlegend <<- F
             else
               plotly_obj$x$data[[j]]$showlegend <<- T
           } # endif
         }) #endfunction j #end lapply
  plotly_obj
} #endfunction solid_lines_legend

## END USEFUL FUNCTIONS ##

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("ASF clinical assessment: King scores"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      passwordInput(
        "API_key",
        "Please paste your ONA API key:",
        placeholder = "Please copy-paste, don't type!"
        ),
      selectInput(
        "experiment",
        "IACUC number (with timespan of observations recorded)",
        choices = character(),
        multiple = F
      ),
      radioButtons(
        "displayType",
        "Table display",
        choices = c(
          `Condensed (clinical observations only)` = "Condensed",
          `Full (all columns, including calculated variables)` = "Full"),
        selected = "Condensed"
      ),
      radioButtons(
        "timeDisplay",
        "Day/date display",
        choiceNames = list(
          paste("Experimental day (counting from", firstExpDay, "upwards)"),
          "Actual calendar date"),
        choiceValues = list("experimentDay", "date"),
        selected = "experimentDay"
      ),
      
      
      radioButtons(
        "filtering_on",
        "Filtering table and views on:",
        choices = c(`individual animals` = "animalID", `experimental groups` = "group"),
        selected = "animalID"
      ),
      
      selectInput(
        "filter",
        "Filter:",
        choices = c("(all)"),
        multiple = T
      ),
      
      
      radioButtons(
        "colourMapping",
        "Colour mapping in graphs",
        choices = c(`individual animals` = "animalID", `experimental groups` = "group"),
        selected = "animalID"
      ),
      #textOutput("warningOnGroups"),
      #br(),
    
      numericInput(
        "RI_threshold",
        "King score threshold to display on graphs (e.g. for humane endpoint)",
        value = 6,
        min = 0,
        max = 10,
        step = 0.1
      ),
      
      selectInput(
        "additionalVar",
        "What to visualize apart from King scores?",
        choices = character(),
        multiple = F
      ),
      width = 3
    ),
    
    
    # Show the main plot: King scores for animals over time
    mainPanel(dataTableOutput("mainDataTable"),
              plotlyOutput("mainPlot"),
              plotlyOutput("additionalPlot"), # and the additional plot
              width = 9)
  )
)

# Define server logic
server <- function(input, output, session) {
  
  
  output$warningOnGroups <- renderText({
    "(the above will trigger an error when \"Groups\" is selected but the input\
    data doesn't have the information on experimental groups: \ 
    please check your input file)"
  })
  
  # the following filters trim the table display when
  # input$displayType == "Condensed"
  data_cols_to_display <- c("date", "experimentDay", "animalID", "temp", "temp_score",
                            scoring_columns, "needvetexam", "comments", "total_King")
  
  # we first instantiate the all_dataset() reactive tibble object:
  all_datasets <- reactive({
    req(input$API_key)
    # building the curl request to get the data
    h <- new_handle(verbose = TRUE) # we don't need to keep this verbose option, it's just for debugging/curiosity
    handle_setheaders(h,
                      "Content-Type" = "application/json",
                      "Authorization" = paste("Token", input$API_key))
    # note the above triggers an error HTTP/2 401 on the first request in case credentials are wrong.
    
    # launching an API call to list all the forms (i.e. animal trials) owned by the "ASF trials" org.
    forms <- curl_fetch_memory(paste0(ona_API_server_prefix, "/forms?owner=asfv_trials"), handle = h)
    # then we will get all the project IDs, filtering on the forms that are downloadable.
    
    # But for now, catch the errors...
    forms$content %>% rawToChar() %>% as.tbl_json() -> json_result
    
    # if json_result is not an array, it means the API token was invalid:
    if (!is_json_array(json_result)) {
      # the following also works on a passwordInput field:
      updateTextInput(session, "API_key", value = "", placeholder = "Sorry, you entered a wrong key.") 
      return(NULL) # so that the current reactive is not truthy
    }
    
    # otherwise, we proceed:
    json_result %<>% gather_array() %>% spread_all() %>% as_tibble()
    
    # at this stage, if the tibble is empty, this means the user is not allowed to see any trial:
    if (nrow(json_result) == 0) {
      # the following also works on a passwordInput field:
      updateTextInput(session, "API_key", value = "",
                      placeholder = "Sorry, your ONA credentials don't allow you to view any trial.") 
      return(NULL) # so that the current reactive is not truthy
    }
    
    # the filter on `downloadable` is supposed to discard the hidden (test) forms
    json_result %>% filter(downloadable == TRUE) %>% select(title, formid) -> form_ids # a tibble
    
    # title contains the iacuc number, formid is the form id
    # and we gather all the data (but, beware: possibly, not all forms have the same variables, so we'll have to rbind() loosely?)
    # launching the API calls for each form, in a loop (can be improved by writing functionally with purrr)
    df <- tibble(iacuc = character())
    
    for (i in 1:nrow(form_ids)) {
      response <- curl_fetch_memory(paste0(ona_API_server_prefix, "/data/", form_ids[[i, "formid"]]), handle = h)
      
      # the JSON object from the ONA API, when converted ToChar, is a character vector of length **1** (a single array),
      # so we first make a call to gather_array().
      response$content %>% rawToChar() %>% as.tbl_json() %>%
        gather_array() %>% spread_all() %>% as_tibble() %>% select(-document.id) %>% 
        mutate(iacuc = form_ids[[i, "title"]], .before = 1) -> one_dataset
      bind_rows(df, one_dataset) -> df
    }

    
    # we drop all the "grp_sec1b/" prefixes as well as dummy experiments
    df %<>% rename_with(.cols = starts_with("grp_sec1b/"), .fn = remove_group_prefix) %>% 
      filter(iacuc != "DUMMY") %>% filter(is.na(comments) | comments != 'Test form by Erick')
    
    # TODO: merge potentially multiple records corresponding to the same animal, same day (several successive submissions)
    # and containing partial recordings
    
    # removing the columns we never want displayed:
    df %<>% select(!starts_with("_")) %<>% select(-any_of(dont_display_columns))

    # all columns above have to be mutated to numeric:
    df %<>% mutate(across(any_of(c(scoring_columns, "needvetexam")), as.numeric))
    
    # then we will add a column with the King score for each animal, for each day.
    df %<>% mutate(temp_score = case_when(
      temp < 39.0 ~ 0L,
      temp >= 39.0 & temp < 39.5 ~ 1L,
      temp >= 39.5 & temp < 40.0 ~ 2L,
      temp >= 40.0 & temp <= 40.5 ~ 3L,
      temp > 40.5 & temp <= 41.0 ~ 4L,
      .default = 5L), .after = temp) %>% rowwise() %>% 
      mutate(total_King = sum(c_across(all_of(c("temp_score", scoring_columns)))), .before = needvetexam)
      
    # lubrify the observation date:
    df %<>% mutate(date = lubridate::date(`obdate`), .keep = "unused")
    # rename and factor animal IDs and groups:
    df %<>% mutate(animalID = factor(`grp_id/animid`), group = factor(`grp_id/expgroup`), .keep = "unused")
    
    # "None" comments are simply NAs
    df %<>% mutate(comments = na_if(trimws(comments), "None"))
    
    # we add a numeric column called "experimentDay", *** starting at -2 *** for the first
    # data collection point, experiment-wise
    df %<>% group_by(iacuc) %>% mutate(first_date = min(date, na.rm = T), experimentDay = as.integer(date - first_date + firstExpDay))
    
    return(ungroup(df))
  })
  
  # we dynamically recompute the list of experiments to pick from:
  observe({
    req(input$API_key, all_datasets())
    all_datasets() %>% group_by(iacuc) %>% summarize(
      first_date = first(first_date), # already calculated
      last_date = max(date, na.rm = T)) %>%
    arrange(desc(last_date)) %>% # sorting the experiments, last being first 
    mutate(
        fullname = paste0(iacuc, " (", first_date, " – ", last_date, ")"),
        .keep = "all") -> temp_table
    temp_table %>% pull(iacuc) -> named_vec
    temp_table %>% pull(fullname) -> names(named_vec)
    # the display values will contain the timespan of recordings,
    # and the selected value by default is the first listed (last experiment)
    updateSelectInput(session, "experiment", choices = named_vec)
  })
  
  # reactively set the dataset
  dataset <- reactive({
    req(input$API_key, input$experiment)
    
    # we filter on the selected iacuc
    return(all_datasets() %>% filter(iacuc == input$experiment))
  })
    
  filtered_dataset <- reactive({
    # we further filter on the filter, if present
    d <- dataset()
    if(is.null(input$filter) | "(all)" %in% input$filter)
      return(d)
    else
      return(d %>% filter(.data[[input$filtering_on]] %in% input$filter))
  })
  
  # and set the list of columns to pick from, to filter on a column
  observe({
    req(input$experiment)
    updateSelectInput(session, "additionalVar", choices = setdiff(colnames(dataset()), c("total_King", "animalID")), selected = "temp")
  })
  
  # we dynamically recompute the list of entities to filter on:
  observe({
    req(input$experiment)
    updateSelectInput(session, "filter",
                      choices = c("(all)", sort(unique(as.character(
      `[[`(dataset(), input$filtering_on))))))
  })
  
  # calculating the right layers for the time display:
  customized_time_display <- reactive({
    if(input$timeDisplay == "date")
      list(geom_line(aes(x = date)),
           theme(axis.text.x = element_text(angle = 45, hjust = 1)),
           scale_x_continuous(breaks = breaks_width(1), labels = lubridate::as_date),
           labs(x = "Observation date"))
    else # display the experimental day instead
      list(geom_line(aes(x = experimentDay)),
           scale_x_continuous(breaks = breaks_width(1)),
           labs(x = "Day in experiment"))
  })
  
  # main data table output
  output$mainDataTable <- renderDataTable({
    req(input$experiment)
    
    d <- filtered_dataset()
    if(input$displayType == "Condensed")
      d %<>% select(any_of(data_cols_to_display))

    # and we build the datatable with its options:
    d %>% datatable(extensions = c('Buttons', 'FixedHeader'), options = list(
      dom = 'lBfrtip',
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )),
      fixedHeader = T,
      scrollX = T,
      pageLength = 10,
      lengthMenu = list(c(5, 10, 20, 50, 100, -1), c("5", "10", "20", "50", "100", "all"))
    ))
  })
  
  
  # careful in the following: we use hardcoded column names, including animalID
  # TRICK: there is a computed column experimentDuration, NA only for the virtual
  # observations. We use that to filter the stuff we are plotting.
  # We plot dashed lines underneath, and then solid lines on top.
  # In all the graphs, setting group = animalID makes sure we keep one line per animal
  # no matter what the other aesthetics are.
  output$mainPlot <- renderPlotly({
    req(filtered_dataset())
    validate(
      need(input$colourMapping == "animalID" | ("group" %in% colnames(dataset()) & any(!is.na(dataset() %>% pull(group)))),
           "You asked for a colour mapping on groups, but your input table doesn't contain \
           any information on groups for this trial.")
    )
    
    filtered_dataset() %>%
      ggplot(aes(group = animalID, y = total_King, color = !!sym(input$colourMapping))) +
      customized_time_display() +
      labs(title = "King scores",
           y = "King score") -> p
    if(isTruthy(input$RI_threshold)) p + geom_hline(yintercept = input$RI_threshold) -> p
    ggplotly(p) #%>% solid_lines_legend()
  })
  
  # careful in the following: we use hardcoded column names, including animalID
  output$additionalPlot <- renderPlotly({
    req(filtered_dataset(), input$additionalVar)
    validate(
      need(input$colourMapping == "animalID" | ("group" %in% colnames(dataset()) & any(!is.na(dataset() %>% pull(group)))),
           "You asked for a colour mapping on groups, but your input table doesn't contain \
           any information on groups for this trial.")
    )
    
    filtered_dataset() %>%
      ggplot(aes(group = animalID, y = !!sym(input$additionalVar), color = !!sym(input$colourMapping))) +
      customized_time_display() +
      labs(title = input$additionalVar, y = input$additionalVar) -> p
    ggplotly(p) %>% solid_lines_legend()
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
