#' Interactively explore experience data
#'
#' @description Launch a shiny application to interactively explore drivers of
#' experience.
#'
#' **temp** details here. note filters and data visualization, also removal
#' of reserved column names from predictors. also explain exposed_df importance
#'
#' @param dat An `exposed_df` object
#' @param predictors A character vector of independent variables to include in the
#' shiny app.
#'
#' @return `NULL`
#'
#' @examples
#'
#' \dontrun{1}
#'

exp_explore <- function(dat, predictors = names(dat)) {

  if (!is_exposed_df(dat)) {
    rlang::abort("`dat` is not an `exposed_df` object. Try converting `dat` to an `exposed_df` using `as_exposed_df`.")
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
                  )) |>
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

  ui <- shiny::fluidPage(

    shiny::titlePanel("Experience Data Explorer"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(

        shiny::h3("Filters"),

        # add filter widgets
        purrr::map(preds$predictors, widget),

      ),

      shiny::mainPanel(
        shiny::fluidRow(
          shiny::h3("Variable Selection")
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::h3("Plot")),
          shiny::column(
            width = 6,
            shiny::h3("Table")),
        ),
        shiny::plotOutput("xpPlot"),

        shiny::h3("Filter Information"),
        shiny::verbatimTextOutput("filterInfo"),

        # temporary!!!
        gt::gt(preds)
      )
    )
  )

  server <- function(input, output) {

    # reactive data
    rdat <- reactive({

      filters <- purrr::map(preds$predictors, expr_filter)

      dat |>
        dplyr::filter(!!!filters)
    })

    output$xpPlot <- shiny::renderPlot({

      rdat() |>
        dplyr::group_by(pol_yr, inc_guar) |>
        exp_stats() |>
        ggplot2::ggplot(ggplot2::aes(pol_yr, q_obs, fill = inc_guar)) +
        ggplot2::geom_col(position = "dodge")

    })

    # filter information
    output$filterInfo <- shiny::renderPrint({
      glue::glue("Total records = {scales::label_comma()(total_rows)}
                 Remaining records = {scales::label_comma()(nrow(rdat()))}")
    })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
