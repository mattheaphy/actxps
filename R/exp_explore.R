#' Interactively explore experience data
#'
#' @description Launch a shiny application to interactively explore drivers of
#' experience.
#'
#' **temp** details here. note filters and data visualization, also removal
#' of reserved column names from predictors. also explain exposed_df importance
#'
#' @param dat An `exposed_df` object
#' @param predictors A character vector of independent variables in `dat` to
#' include in the shiny app.
#' @param expected A character vector of expected values in `dat` to include
#' in the shiny app.
#'
#' @return `NULL`
#'
#' @examples
#'
#' \dontrun{1}
#'
#' @export
exp_shiny <- function(dat,
                      predictors = names(dat),
                      expected = stringr::str_subset(names(dat), "expected")) {

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
                                             ~ is.numeric(dat[[.x]]))) |>
    dplyr::arrange(order) |>
    dplyr::select(-order)

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
        inputId, x,
        min = min(dat[[x]], na.rm = TRUE),
        max = max(dat[[x]], na.rm = TRUE),
        value = range(dat[[x]], na.rm = TRUE)
      )

    } else if (lubridate::is.Date(dat[[x]])) {

      date_range <- range(dat[[x]], na.rm = TRUE)

      shiny::dateRangeInput(
        inputId, x,
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
          inputId, x,
          choices = choices, selected = choices,
          multiple = TRUE
        )
      } else {
        shiny::checkboxGroupInput(
          inputId, x,
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
      shiny::selectInput(inputId, label,
                         choices = choices, ...)
    )
  }

  # expected values set up
  if (length(expected) > 0) {

    has_expected <- TRUE

    expected_widget <-
      column(
        width = 4,
        shiny::checkboxGroupInput("ex_checks", "Expected values:",
                                  choices = expected)
      )
  } else {
    has_expected <- FALSE
    expected_widget <- NULL
  }

  ui <- shiny::fluidPage(

    shiny::titlePanel(paste(attr(dat, "target_status"), collapse = "/") |>
                        paste("Experience Study")),

    shiny::sidebarLayout(
      shiny::sidebarPanel(

        shiny::h3("Filters"),

        # add filter widgets
        purrr::map(preds$predictors, widget),

      ),

      shiny::mainPanel(

        shiny::h3("Variable Selection"),

        shiny::wellPanel(
          shiny::fluidRow(
            selectPred("xVar", "x:", 4),
            selectPred("colorVar", "Color:", 4),
            selectPred("facetVar", "Facets:", 4, multiple = TRUE,
                       choices = preds$predictors)
          ),
          shiny::fluidRow(
            expected_widget,
            selectPred("weightVar", "Weight by:", 4,
                       choices = c("None",
                                   dplyr::filter(preds, is_number)$predictors))
          )),

        shiny::h3("Output"),

        shiny::tabsetPanel(
          shiny::tabPanel(
            "Plot",
            shiny::br(),
            shiny::fluidRow(
              selectPred("yVar", "y:", 4,
                         choices = c("q_obs", "n_claims", "claims",
                                     "exposure", "credibility")),
              shiny::column(
                width = 4,
                shiny::radioButtons("plotGeom",
                                    "Geometry:",
                                    choices = c("Bars" = "bars",
                                                "Lines and Point" = "lines"))
              ),
              shiny::column(
                width = 4,
                shiny::checkboxInput("plotSmooth",
                                     "Add Smoothing?",
                                     value = FALSE)
              )
            ),
            shiny::plotOutput("xpPlot")),
          shiny::tabPanel(
            "Table",
            shiny::br(),
            gt::gt_output("xpTable")
          )
        ),

        shiny::h3("Filter Information"),
        shiny::verbatimTextOutput("filterInfo")

      )
    )
  )

  server <- function(input, output) {

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
                             y_labels = y_labels)
      }

      if (input$plotSmooth) p <- p + ggplot2::geom_smooth(method = "loess",
                                                          formula = y ~ x)

      p + ggplot2::theme_light()

    })

    output$xpTable <- gt::render_gt({
      rxp() |> autotable()
    })

    # filter information
    output$filterInfo <- shiny::renderPrint({
      glue::glue("Total records = {scales::label_comma()(total_rows)}
                 Remaining records = {scales::label_comma()(nrow(rdat()))}
                 % Data Remainings = {scales::label_percent(accuracy=0.1)(nrow(rdat())/total_rows)}")
    })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
