#' Interactively explore experience data
#'
#' @description Launch a shiny application to interactively explore drivers of
#' experience.
#'
#' `dat` must be an `exposed_df` object. An error will be thrown is any other
#' object type is passed. If `dat` has transactions attached, the app will
#' contain features for both termination and transaction studies. Otherwise,
#' the app will only support termination studies.
#'
#' If nothing is passed to `predictors`, all columns names in `dat` will be
#' used (excluding the policy number, status, termination date, exposure,
#' transaction counts, and transaction amounts columns).
#'
#' The `expected` argument is optional. As a default, any column names
#' containing the word "expected" are used.
#'
#' # Layout
#'
#' ## Filters
#'
#' The sidebar contains filtering widgets for all variables passed
#' to the `predictors` argument.
#'
#' ## Study options
#'
#' ### Grouping variables
#'
#' This box includes widgets to select grouping variables for summarizing
#' experience. The "x" widget is also used as the x variable in the plot output.
#' Similarly, the "Color" and "Facets" widgets are used for color and facets in
#' the plot. Multiple faceting variables are allowed. For the table output,
#' "x", "Color", and "Facets" have no particular meaning beyond the order in
#' which of grouping variables are displayed.
#'
#' ### Study type
#'
#' This box also includes a toggle to switch between termination studies and
#' transaction studies (if available).
#'
#' #### Termination studies
#'
#' The expected values checkboxes are used to activate and deactivate expected
#' values passed to the `expected` argument. This impacts the table output
#' directly and the available "y" variables for the plot. If there are no
#' expected values available, this widget will not appear. The "Weight by"
#' widget is used to specify which column, if any, contains weights for
#' summarizing experience.
#'
#' #### Transaction studies
#'
#' The transaction types checkboxes are used to activate and deactivate
#' transaction types that appear in the plot and table outputs. The available
#' transaction types are taken from the `trx_types` attribute of `dat`.
#' In the plot output, transaction type will always appear as a faceting
#' variable. The "Transactions as % of" selector will expand the list of
#' available "y" variables for the plot and impact the table output directly.
#' Lastly, a checkbox exists that allows for all transaction types to be
#' aggregated into a single group.
#'
#' ## Output
#'
#' ### Plot Tab
#'
#' This tab includes a plot and various options for customization:
#'
#' - y: y variable
#' - Geometry: plotting geometry
#' - Add Smoothing?: activate to plot loess curves
#' - Free y Scales: activate to enable separate y scales in each plot.
#'
#' ### Table
#'
#' This tab includes a data table.
#'
#' ### Export Data
#'
#' This tab includes a download button that will save a copy of the summarized
#' experience data.
#'
#' ## Filter Information
#'
#' This box contains information on the original number of exposure records,
#' the number of records after filters are applied, and the percentage of
#' records retained.
#'
#' @param dat An `exposed_df` object.
#' @param predictors A character vector of independent variables in `dat` to
#' include in the shiny app.
#' @param expected A character vector of expected values in `dat` to include
#' in the shiny app.
#' @param distinct_max Maximum number of distinct values allowed for `predictors`
#' to be included as "Color" and "Facets" grouping variables. This input
#' prevents the drawing of overly complex plots. Default value = 25.
#' @param title Optional. Title of the shiny app. If no title is provided,
#' a descriptive title will be generated based on attributes of `dat`.
#'
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#'
#' @examples
#'
#' if (interactive()) {
#'   study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
#'   expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))
#'
#'   study_py <- study_py |>
#'     mutate(expected_1 = expected_table[pol_yr],
#'            expected_2 = ifelse(inc_guar, 0.015, 0.03)) |>
#'     add_transactions(withdrawals) |>
#'     left_join(account_vals, by = c("pol_num", "pol_date_yr"))
#'
#'   exp_shiny(study_py)
#' }
#'
#' @export
exp_shiny <- function(dat,
                      predictors = names(dat),
                      expected = names(dat)[grepl("expected", names(dat))],
                      distinct_max = 25L,
                      title) {

  rlang::check_installed("shiny")
  rlang::check_installed("bslib")
  rlang::check_installed("thematic")

  verify_exposed_df(dat)
  # check for presence of transactions
  all_trx_types <- verify_get_trx_types(dat, required = FALSE)
  has_trx <- !is.null(all_trx_types)
  trx_cols <- names(dat)[grepl("^trx_(n|amt)_", names(dat))]

  # ungroup data if needed
  dat <- ungroup(dat)

  if (any(!c(predictors, expected) %in% names(dat))) {
    rlang::inform("All predictors and expected values must be columns in `dat`. Unexpected values will be removed.")
    predictors <- predictors[predictors %in% names(dat)]
    expected <- expected[expected %in% names(dat)]
  }

  total_rows <- nrow(dat)

  # organize predictors
  preds <- data.frame(predictors = predictors) |>
    # drop non-predictors (if any)
    filter(!predictors %in% c("pol_num", "status",
                              "term_date","exposure", trx_cols)) |>
    mutate(class1 = purrr::map_chr(predictors, ~ class(dat[[.x]])[[1]]),
           order = dplyr::case_when(
             class1 == "Date" ~ 1,
             class1 == "logical" ~ 2,
             class1 %in% c("character", "factor") ~ 3,
             class1 %in% c("numeric", "integer", "double") ~ 4,
             TRUE ~ 5
           ),
           is_number = purrr::map_lgl(predictors,
                                      ~ is.numeric(dat[[.x]])),
           n_unique = purrr::map_int(predictors,
                                     ~ dplyr::n_distinct(dat[[.x]])),
           scope = purrr::map2(predictors, class1,
                               ~ if (.y %in% c("Date", "numeric",
                                               "integer", "double")) {
                                 range(dat[[.x]], na.rm = TRUE)
                               } else {
                                 unique(dat[[.x]])
                               }
           )
    ) |>
    arrange(order) |>
    select(-order)

  preds_small <- filter(preds, n_unique <= distinct_max)$predictors

  yVar_exp <- c("q_obs", "n_claims", "claims", "exposure", "credibility")
  if (has_trx) {
    yVar_trx <- c("trx_util", "trx_freq", "trx_n", "trx_flag",
                  "trx_amt", "avg_trx", "avg_all")
    available_studies <- c("Termination study" = "exp",
                           "Transaction study" = "trx")
  } else {
    yVar_trx <- NULL
    available_studies <- c("Termination study" = "exp")
  }

  # function to make input widgets
  widget <- function(x,
                     checkbox_limit = 8) {

    inputId <- paste("i", x, sep = "_")
    info <- filter(preds, predictors == x)
    choices <- info$scope[[1]]

    if (is.null(dat[[x]])) {
      rlang::abort(
        glue::glue("Error creating an input widget for {x}. {x} does not exist in the input data.")
      )
    }

    if (is.numeric(dat[[x]])) {

      shiny::sliderInput(
        inputId, shiny::strong(x),
        min = choices[[1]],
        max = choices[[2]],
        value = choices
      )

    } else if (lubridate::is.Date(dat[[x]])) {

      shiny::dateRangeInput(
        inputId, shiny::strong(x),
        start = choices[[1]],
        end = choices[[2]],
        min = choices[[1]],
        max = choices[[2]],
        startview = "year"
      )

    } else if (is.character(dat[[x]]) || is.logical(dat[[x]]) ||
               is.factor(dat[[x]])) {

      if (length(choices) > checkbox_limit) {
        shiny::selectInput(
          inputId, shiny::strong(x),
          choices = choices, selected = choices,
          multiple = TRUE
        )
      } else {
        shiny::checkboxGroupInput(
          inputId, shiny::strong(x),
          choices = choices, selected = choices
        )
      }

    } else {

      rlang::abort(
        glue::glue("Error creating an input widget for {x}. {x} is of class {class(dat[[x]]) |> paste(collapse = ', ')}, which is not supported.")
      )

    }

  }

  # function to build filter expressions
  expr_filter <- function(x) {

    inputId <- paste("i", x, sep = "_")


    res <- if (is.numeric(dat[[x]]) || lubridate::is.Date(dat[[x]])) {
      # numeric or date
      glue::glue("between({x}, input${inputId}[[1]], input${inputId}[[2]])")
    } else {
      # categorical
      glue::glue("{x} %in% input${inputId}")

    }

    rlang::parse_expr(res)

  }

  widgetPred <- function(fun) {
    function(inputId, label, width,
             choices = c("None", preds$predictors), ...) {
      shiny::column(
        width = width,
        fun(inputId, shiny::strong(label),
            choices = choices, ...)
      )
    }
  }

  selectPred <- widgetPred(shiny::selectInput)
  checkboxGroupPred <- widgetPred(shiny::checkboxGroupInput)

  # expected values set up
  if (length(expected) > 0) {

    has_expected <- TRUE

    expected_widget <- checkboxGroupPred("ex_checks", "Expected values:", 4,
                                         choices = expected,
                                         selected = expected)

  } else {
    has_expected <- FALSE
    expected_widget <- NULL
  }

  # transactions set up
  if (has_trx) {

    percent_of_choices <- filter(preds, is_number)$predictors

    trx_tab <- shiny::tabPanel(
      "Transaction study",
      value = "trx",
      shiny::fluidRow(
        checkboxGroupPred("trx_types_checks", "Transaction types:", 4,
                          all_trx_types, selected = all_trx_types),
        selectPred("pct_checks", "Transactions as % of:", 4,
                   choices = percent_of_choices, multiple = TRUE),
        shiny::column(
          width = 4,
          shiny::checkboxInput("trx_combine",
                               shiny::strong("Combine transactions?"),
                               value = FALSE)
        )
      )
    )

  } else {
    trx_tab <- NULL
  }

  if (missing(title)) {
    title <- paste(attr(dat, "target_status"), collapse = "/") |>
      paste("Experience Study",
            if (has_trx) {
              glue::glue("and {paste(all_trx_types, collapse = '/')} Transaction Study")})
  }

  ui <- shiny::fluidPage(

    theme = bslib::bs_theme(bootswatch = "flatly"),

    shiny::titlePanel(title),

    shiny::sidebarLayout(
      shiny::sidebarPanel(

        shiny::h3("Filters"),

        # add filter widgets
        purrr::map(preds$predictors, widget),

      ),

      shiny::mainPanel(

        shiny::wellPanel(

          shiny::h3("Study options"),

          shiny::h4("Grouping variables"),
          shiny::em("The variables selected below will be used as grouping variables in the plot and table outputs. Multiple variables can be selected as facets."),
          shiny::fluidRow(
            selectPred("xVar", "x:", 4),
            selectPred("colorVar", "Color:", 4,
                       choices = c("None", preds_small)),
            selectPred("facetVar", "Facets:", 4, multiple = TRUE,
                       choices = preds_small)
          ),

          shiny::h4("Study type"),
          shiny::tabsetPanel(
            id = "study_type",
            type = "pills",

            shiny::tabPanel("Termination study",
                            value = "exp",
                            shiny::fluidRow(
                              expected_widget,
                              selectPred("weightVar", "Weight by:", 4,
                                         choices = c("None",
                                                     filter(preds, is_number)$predictors))
                            )),
            trx_tab

          )
        ),

        shiny::h3("Output"),

        shiny::tabsetPanel(
          type = "pills",
          shiny::tabPanel(
            "Plot",
            shiny::br(),
            shiny::fluidRow(
              selectPred("yVar", "y:", 4, choices = yVar_exp),
              shiny::column(
                width = 4,
                shiny::radioButtons("plotGeom",
                                    shiny::strong("Geometry:"),
                                    choices = c("Bars" = "bars",
                                                "Lines and Points" = "lines"))
              ),
              shiny::column(
                width = 4,
                shiny::checkboxInput("plotSmooth",
                                     shiny::strong("Add Smoothing?"),
                                     value = FALSE)
              )
            ),

            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::checkboxInput("plot2ndY",
                                     shiny::strong("Second y-axis?"),
                                     value = FALSE)
              ),
              shiny::column(
                width = 4,
                shiny::checkboxInput("plotFreeY",
                                     shiny::strong("Free y Scales?"),
                                     value = FALSE)
              )

            ),

            shiny::plotOutput("xpPlot")),
          shiny::tabPanel(
            "Table",
            shiny::br(),
            gt::gt_output("xpTable")
          ),
          shiny::tabPanel(
            "Export Data",
            shiny::br(),
            shiny::downloadButton("xpDownload", "Download")
          )
        ),

        shiny::h3("Filter information"),
        shiny::verbatimTextOutput("filterInfo")

      )
    )
  )

  server <- function(input, output, session) {

    thematic::thematic_shiny()

    yVar_exp2 <- shiny::reactive({
      c(yVar_exp,
        input$ex_checks,
        glue::glue("ae_{input$ex_checks}"),
        glue::glue("adj_{input$ex_checks}"),
        "All termination rates",
        if (length(input$ex_checks) > 0) "All A/E ratios")
    })

    yVar_trx2 <- shiny::reactive({
      c(yVar_trx,
        glue::glue("pct_of_{input$pct_checks}_w_trx"),
        glue::glue("pct_of_{input$pct_checks}_all"),
        input$pct_checks,
        glue::glue("{input$pct_checks}_w_trx"))
    })

    # update y variable selections in response to inputs
    shiny::observe(
      shiny::updateSelectInput(
        session, "yVar", choices =
          if (input$study_type == "exp") {
            yVar_exp2()
          } else {
            yVar_trx2()
          }
      )
    ) |>
      shiny::bindEvent(input$study_type, input$ex_checks, input$pct_checks)

    # disable color input when using special plots
    shiny::observe(
      if (input$yVar %in% c("All termination rates", "All A/E ratios")) {
        shiny::updateSelectInput(
          session, "colorVar", choices = "Series", selected = "Series")
      } else {
        shiny::updateSelectInput(
          session, "colorVar", choices = c("None", preds_small),
          selected = "None")

      }
    ) |>
      shiny::bindEvent(input$yVar, input$colorVar)

    # reactive data
    rdat <- shiny::reactive({

      keep <- purrr::imap_lgl(preds$predictors,
                              ~ length(setdiff(preds$scope[[.y]],
                                               input[[paste0("i_", .x)]])) > 0)
      filters <- purrr::map(preds$predictors[keep], expr_filter)

      dat |>
        filter(!!!filters)
    })

    # experience study
    rxp <- shiny::reactive({

      .groups <- c(input$xVar, input$colorVar, input$facetVar)
      .groups <- .groups[!.groups %in% c("None", "Series")]

      if (input$weightVar == "None") {
        wt <- NULL
      } else {
        wt <- input$weightVar
        if (wt %in% as.character(.groups)) {
          rlang::abort("Error: the weighting variable cannot be one of the grouping (x, color, facets) variables.")
        }
      }

      ex <- if (has_expected) {
        input$ex_checks
      }

      if (input$study_type == "exp") {
        rdat() |>
          group_by(dplyr::across(dplyr::all_of(.groups))) |>
          exp_stats(wt = wt, credibility = TRUE, expected = ex)
      } else {
        rdat() |>
          group_by(dplyr::across(dplyr::all_of(.groups))) |>
          trx_stats(percent_of = input$pct_checks,
                    trx_types = input$trx_types_checks,
                    combine_trx = input$trx_combine)
      }

    })

    output$xpPlot <- shiny::renderPlot({

      if (input$study_type == "exp" && input$yVar %in% yVar_trx2()) return()
      if (input$study_type == "trx" && input$yVar %in% yVar_exp2()) return()

      dat <- rxp()

      x <- if (input$xVar != "None") {
        rlang::sym(input$xVar)
      } else {
        dat[["All"]] <- ""
        rlang::sym("All")
      }

      color <- if (input$colorVar != "None") {
        rlang::sym(input$colorVar)
      }

      # set up y-variable and plotting function
      if (input$yVar == "All termination rates") {
        y <- rlang::expr(Rate)
        plot.fun <- plot_termination_rates
      } else if (input$yVar == "All A/E ratios") {
        y <- rlang::expr(`A/E ratio`)
        plot.fun <- plot_actual_to_expected
      } else {
        y <- rlang::sym(input$yVar)
        plot.fun <- autoplot
      }

      mapping <- ggplot2::aes(!!x, !!y, color = !!color,
                              fill = !!color, group = !!color)

      # y labels
      if (input$yVar %in% c("claims", "n_claims", "exposure",
                            "trx_n", "trx_flag", "trx_amt",
                            "avg_trx", "avg_all",
                            input$pct_checks,
                            paste0(input$pct_checks, "_w_trx"))) {
        y_labels <- scales::label_comma(accuracy = 1)
      } else if (input$yVar == "trx_freq") {
        y_labels <- scales::label_comma(accuracy = 0.1)
      } else {
        y_labels <- scales::label_percent(accuracy = 0.1)
      }

      if (is.null(input$facetVar)) {
        p <- dat |> plot.fun(mapping = mapping, geoms = input$plotGeom,
                             y_labels = y_labels,
                             second_axis = input$plot2ndY)
      } else {

        facets <- rlang::syms(input$facetVar)
        if (input$study_type == "trx") {
          facets <- append(facets, rlang::sym("trx_type"))
        }

        p <- dat |> plot.fun(!!!facets, mapping = mapping,
                             geoms = input$plotGeom,
                             y_labels = y_labels,
                             scales =
                               if (input$plotFreeY) "free_y" else "fixed",
                             second_axis = input$plot2ndY)
      }

      if (input$plotSmooth) p <- p + ggplot2::geom_smooth(method = "loess",
                                                          formula = y ~ x)

      p +
        ggplot2::theme_light() +
        ggplot2::theme(axis.text = ggplot2::element_text(size = ggplot2::rel(0.95)),
                       strip.text = ggplot2::element_text(size = ggplot2::rel(1)),
                       strip.background = ggplot2::element_rect(fill = "#43536b")
        )

    }, res = 92)

    output$xpTable <- gt::render_gt({
      rxp() |> autotable()
    })

    # filter information
    output$filterInfo <- shiny::renderPrint({
      glue::glue("Total records = {scales::label_comma()(total_rows)}
                 Remaining records = {scales::label_comma()(nrow(rdat()))}
                 % Data remaining = {scales::label_percent(accuracy=0.1)(nrow(rdat())/total_rows)}")
    })

    output$xpDownload <- shiny::downloadHandler(
      filename = function() {
        file.path(tempdir(), paste0(input$study_type, "-data-", Sys.Date(), ".csv"))
      },
      content = function(file) {
        readr::write_csv(rxp(), file)
      }
    )

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
