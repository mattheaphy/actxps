#' Interactively explore experience data
#'
#' @description Launch a shiny application to interactively explore drivers of
#' experience.
#'
#' `dat` must be an `exposed_df` object. An error will be thrown is any other
#' object type is passed.
#'
#' If nothing is passed to `predictors`, all columns names in `dat` will be
#' used (excluding the policy number, status, termination date, and exposure
#' columns).
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
#' ## Variable Selection
#'
#' This box includes widgets to select grouping variables for summarizing
#' experience. The "x" widget is also used as the x variable in the plot output.
#' Similarly, the "Color" and "Facets" widgets are used for color and facets in
#' the plot. Multiple faceting variables are allowed. For the table output,
#' "x", "Color", and "Facets" have no particular meaning beyond the order in
#' which of grouping variables are displayed.
#'
#' The expected values checkboxes are used to activate and deactivate expected
#' values passed to the `expected` argument. This impacts the table output
#' directly and the available "y" variables in the plot. If there are no
#' expected values available, this widget will not appear. The "Weight by"
#' widget is used to specify which column, if any, contains weights for
#' summarizing experience.
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
#'   set.seed(123)
#'   study_py <- study_py |>
#'   dplyr::mutate(expected_1 = expected_table[pol_yr],
#'                 expected_2 = ifelse(inc_guar, 0.015, 0.03))
#'
#'   exp_shiny(study_py)
#' }
#'
#' @export
exp_shiny <- function(dat,
                      predictors = names(dat),
                      expected = names(dat)[grepl("expected", names(dat))],
                      distinct_max = 25L) {

  if (!is_exposed_df(dat)) {
    rlang::abort("`dat` is not an `exposed_df` object. Try converting `dat` to an `exposed_df` using `as_exposed_df`.")
  }

  if (any(!c(predictors, expected) %in% names(dat))) {
    rlang::inform("All predictors and expected values must be columns in `dat`. Unexpected values will be removed.")
    predictors <- predictors[predictors %in% names(dat)]
    expected <- expected[expected %in% names(dat)]
  }

  total_rows <- nrow(dat)

  # organize predictors
  preds <- data.frame(predictors = predictors) |>
    # drop non-predictors (if any)
    dplyr::filter(!predictors %in% c("pol_num", "status",
                                     "term_date","exposure")) |>
    dplyr::mutate(class1 = purrr::map_chr(predictors, ~ class(dat[[.x]])[[1]]),
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
                                            ~ dplyr::n_distinct(dat[[.x]]))) |>
    dplyr::arrange(order) |>
    dplyr::select(-order)

  preds_small <- dplyr::filter(preds, n_unique <= distinct_max)$predictors

  yVar_basic <- c("q_obs", "n_claims", "claims", "exposure", "credibility")

  # function to make input widgets
  widget <- function(x,
                     checkbox_limit = 8) {

    inputId <- paste("i", x, sep = "_")

    if (is.null(dat[[x]])) {
      rlang::abort(
        glue::glue("Error creating an input widget for {x}. {x} does not exist in the input data.")
      )
    }

    if (is.numeric(dat[[x]])) {

      shiny::sliderInput(
        inputId, shiny::strong(x),
        min = min(dat[[x]], na.rm = TRUE),
        max = max(dat[[x]], na.rm = TRUE),
        value = range(dat[[x]], na.rm = TRUE)
      )

    } else if (lubridate::is.Date(dat[[x]])) {

      date_range <- range(dat[[x]], na.rm = TRUE)

      shiny::dateRangeInput(
        inputId, shiny::strong(x),
        start = date_range[[1]],
        end = date_range[[2]],
        min = date_range[[1]],
        max = date_range[[2]],
        startview = "year"
      )

    } else if (is.character(dat[[x]]) || is.logical(dat[[x]]) ||
               is.factor(dat[[x]])) {

      choices <- unique(dat[[x]])

      if (length(choices) > checkbox_limit) {
        shiny::selectInput(
          inputId, shiny::strong(x),
          choices = choices, selected = choices,
          multiple = TRUE
        )
      } else {
        shiny::checkboxGroupInput(
          inputId, shiny::strong(x),
          choices = unique(dat[[x]]), selected = choices
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
      glue::glue("dplyr::between({x}, input${inputId}[[1]], input${inputId}[[2]])")
    } else {
      # categorical
      glue::glue("{x} %in% input${inputId}")

    }

    rlang::parse_expr(res)

  }

  selectPred <- function(inputId, label, width,
                         choices = c("None", preds$predictors), ...) {
    shiny::column(
      width = width,
      shiny::selectInput(inputId, shiny::strong(label),
                         choices = choices, ...)
    )
  }

  # expected values set up
  if (length(expected) > 0) {

    has_expected <- TRUE

    expected_widget <-
      shiny::column(
        width = 4,
        shiny::checkboxGroupInput("ex_checks",
                                  shiny::strong("Expected values:"),
                                  choices = expected)
      )
  } else {
    has_expected <- FALSE
    expected_widget <- NULL
  }

  ui <- shiny::fluidPage(

    theme = bslib::bs_theme(bootswatch = "flatly"),

    shiny::titlePanel(paste(attr(dat, "target_status"), collapse = "/") |>
                        paste("Experience Study")),

    shiny::sidebarLayout(
      shiny::sidebarPanel(

        shiny::h3("Filters"),

        # add filter widgets
        purrr::map(preds$predictors, widget),

      ),

      shiny::mainPanel(

        shiny::wellPanel(
          shiny::h3("Variable Selection"),
          shiny::em("The variables selected below will be used as grouping variables in the plot and table outputs. Multiple variables can be selected as facets."),
          shiny::fluidRow(
            selectPred("xVar", "x:", 4),
            selectPred("colorVar", "Color:", 4,
                       choices = c("None", preds_small)),
            selectPred("facetVar", "Facets:", 4, multiple = TRUE,
                       choices = preds_small)
          ),
          shiny::fluidRow(
            expected_widget,
            selectPred("weightVar", "Weight by:", 4,
                       choices = c("None",
                                   dplyr::filter(preds, is_number)$predictors))
          )),

        shiny::h3("Output"),

        shiny::tabsetPanel(
          type = "pills",
          shiny::tabPanel(
            "Plot",
            shiny::br(),
            shiny::fluidRow(
              selectPred("yVar", "y:", 4, choices = yVar_basic),
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

        shiny::h3("Filter Information"),
        shiny::verbatimTextOutput("filterInfo")

      )
    )
  )

  server <- function(input, output, session) {

    thematic::thematic_shiny()

    # update y variable selections in response to expected value outputs
    shiny::observe(
      shiny::updateSelectInput(
        session, "yVar", choices = c(yVar_basic,
                                     input$ex_checks,
                                     glue::glue("ae_{input$ex_checks}"),
                                     glue::glue("adj_{input$ex_checks}"))
      )
    ) |>
      shiny::bindEvent(input$ex_checks)

    # reactive data
    rdat <- shiny::reactive({

      filters <- purrr::map(preds$predictors, expr_filter)

      dat |>
        dplyr::filter(!!!filters)
    })

    # experience study
    rxp <- shiny::reactive({

      .groups <- c(input$xVar, input$colorVar, input$facetVar)
      .groups <- .groups[.groups != "None"]

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

      rdat() |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.groups))) |>
        exp_stats(wt = wt, credibility = TRUE, expected = ex)
    })

    output$xpPlot <- shiny::renderPlot({

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

      y <- rlang::sym(input$yVar)

      mapping <- ggplot2::aes(!!x, !!y, color = !!color,
                              fill = !!color, group = !!color)

      # y labels
      if (input$yVar %in% c("claims", "n_claims", "exposure")) {
        y_labels <- scales::label_comma(accuracy = 1)
      } else {
        y_labels <- scales::label_percent(accuracy = 0.1)
      }

      if (is.null(input$facetVar)) {
        p <- dat |> autoplot(mapping = mapping, geoms = input$plotGeom,
                             y_labels = y_labels)
      } else {
        facets <- rlang::syms(input$facetVar)
        p <- dat |> autoplot(!!!facets, mapping = mapping,
                             geoms = input$plotGeom,
                             y_labels = y_labels,
                             scales =
                               if (input$plotFreeY) "free_y" else "fixed")
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
                 % Data Remaining = {scales::label_percent(accuracy=0.1)(nrow(rdat())/total_rows)}")
    })

    output$xpDownload <- shiny::downloadHandler(
      filename = function() {
        file.path(tempdir(), paste0("exp-data-", Sys.Date(), ".csv"))
      },
      content = function(file) {
        readr::write_csv(rxp(), file)
      }
    )

  }

  # Run the application
  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app)

}
