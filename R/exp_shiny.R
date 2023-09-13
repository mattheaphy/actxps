#' Interactively explore experience data
#'
#' @description Launch a Shiny application to interactively explore drivers of
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
#' The sidebar contains filtering widgets organized by data type for all
#' variables passed to the `predictors` argument.
#'
#' At the top, information is shown on the original number of exposure records,
#' the number of records after filters are applied, and the percentage of
#' records remaining.
#'
#' ## Grouping variables
#'
#' This box includes widgets to select grouping variables for summarizing
#' experience. The "x" widget is also used as the x variable in the plot output.
#' Similarly, the "Color" and "Facets" widgets are used for color and facets in
#' the plot. Multiple faceting variables are allowed. For the table output,
#' "x", "Color", and "Facets" have no particular meaning beyond the order in
#' which of grouping variables are displayed.
#'
#' ## Study type
#'
#' This also includes a toggle to switch between termination studies and
#' transaction studies (if available). Different options are available for each
#' study type.
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
#' Lastly, a toggle exists that allows for all transaction types to be
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
#' - Second y-axis: activate to enable a second y-axis
#' - Second axis y: y variable to plot on the second axis
#' - Add Smoothing: activate to plot loess curves
#' - Confidence intervals: If available, add error bars for confidence intervals
#'   around the selected y variable
#' - Free y Scales: activate to enable separate y scales in each plot
#' - Log y-axis: activate to plot all y-axes on a log-10 scale
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
#' @param dat An `exposed_df` object.
#' @param predictors A character vector of independent variables in `dat` to
#' include in the Shiny app.
#' @param expected A character vector of expected values in `dat` to include
#' in the Shiny app.
#' @param distinct_max Maximum number of distinct values allowed for
#' `predictors` to be included as "Color" and "Facets" grouping variables. This
#' input prevents the drawing of overly complex plots. Default value = 25.
#' @param title Optional. Title of the Shiny app. If no title is provided,
#' a descriptive title will be generated based on attributes of `dat`.
#' @param theme The name of a theme passed to the `preset` argument of
#' `bslib::bs_theme()`. Alternatively, a complete Bootstrap theme created using
#' `bslib::bs_theme()`.
#'
#' @inheritParams exp_stats
#'
#' @return No return value. This function is called for the side effect of
#' launching a Shiny application.
#'
#' @examples
#'
#' if (interactive()) {
#'   study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
#'   expected_table <- c(seq(0.005, 0.03, length.out = 10),
#'                       0.2, 0.15, rep(0.05, 3))
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
                      title,
                      credibility = TRUE,
                      conf_level = 0.95,
                      cred_r = 0.05,
                      theme = "shiny") {

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

  if (is.null(predictors) || all(is.na(predictors))) {
    rlang::warn("`predictors` cannot be NULL or NA. Reverting to the default of `predictors = names(dat)`")
    predictors <- names(dat)
  }

  if (any(!c(predictors, expected) %in% names(dat))) {
    rlang::inform("All predictors and expected values must be columns in `dat`. Unexpected values will be removed.")
    predictors <- predictors[predictors %in% names(dat)]
    expected <- expected[expected %in% names(dat)]
  }

  if (!inherits(theme, "bs_theme")) {
    if (is.character(theme)) {
      theme <- bslib::bs_theme(preset = theme)
    } else {
      rlang::abort("`theme` must be a single character string or a `bs_theme`")
    }
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
           filter_group = dplyr::case_when(
             class1 == "Date" ~ "Dates",
             class1 %in% c("logical", "character", "factor") ~ "Categorical",
             class1 %in% c("numeric", "integer", "double") ~ "Numeric",
             TRUE ~ "Unknown"
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

  yVar_exp <- c("q_obs", "n_claims", "claims", "exposure",
                if (credibility) "credibility")
  if (has_trx) {
    yVar_trx <- c("trx_util", "trx_freq", "trx_n", "trx_flag",
                  "trx_amt", "avg_trx", "avg_all", "exposure")
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

  # create a tooltip with an info icon
  info_tooltip <- function(...) {
    bslib::tooltip(shiny::icon("circle-info"), ...)
  }

  # expected values set up
  if (length(expected) > 0) {

    has_expected <- TRUE

    expected_widget <- checkboxGroupPred("ex_checks", "Expected values:", 6,
                                         choices = expected,
                                         selected = expected)

  } else {
    has_expected <- FALSE
    expected_widget <- NULL
  }

  # transactions set up
  if (has_trx) {

    percent_of_choices <- filter(preds, is_number)$predictors

    trx_tab <- bslib::nav_panel(
      list(
        "Transaction study",
        info_tooltip(
          'Choose transaction types and "percent of" variables that appear in
          the plot and table outputs. If desired, combine all transaction types
          into a single group.')
      ),
      value = "trx",
      shiny::fluidRow(
        checkboxGroupPred("trx_types_checks", "Transaction types:", 4,
                          all_trx_types, selected = all_trx_types),
        selectPred("pct_checks", "Transactions as % of:", 4,
                   choices = percent_of_choices, multiple = TRUE),
        shiny::column(
          width = 4,
          bslib::input_switch("trx_combine",
                              shiny::strong("Combine transactions"),
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

  # function factory for output file names
  export_path <- function(study_type, x, extension) {
    function() {
      file.path(tempdir(), paste0(study_type, "-", x, "-",
                                  Sys.Date(), ".", extension))
    }
  }


  ui <- bslib::page_sidebar(

    theme = theme,
    fillable = FALSE,
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(".html-fill-container > .html-fill-item {
                                    overflow: visible; }
                                    .html-fill-container > .no-overflow {
                                    overflow: auto; }"))
    ),

    title = title,

    sidebar = bslib::sidebar(

      title = "Filters",
      width = "300px",

      bslib::input_switch("play",
                          list("Reactivity ",
                               shiny::icon("play"), "/",
                               shiny::icon("pause")),
                          value = TRUE),
      bslib::value_box(
        title = "% data remaining",
        value = shiny::textOutput("rem_pct"),
        showcase = shiny::plotOutput("filter_pie",
                                     height = "60px", width = "60px")
      ) |>
        bslib::tooltip(paste0("Original row count: ",
                              scales::label_comma()(total_rows)),
                       shiny::textOutput("rem_rows")),

      # add filter widgets
      bslib::accordion(
        split(preds, preds$filter_group)[
          c("Dates", "Categorical", "Numeric")] |>
          purrr::keep(\(x) length(x) > 0) |>
          purrr::map(\(x) bslib::accordion_panel(
            title = x$filter_group[[1]],
            purrr::map(x$predictors, widget)
          ))
      )

    ),

    bslib::layout_column_wrap(
      width = 400,
      heights_equal = "row",

      bslib::card(

        bslib::card_header("Grouping variables",
                           info_tooltip(
                             "The variables selected below will be used as
                             grouping variables in the plot and table outputs.
                             Multiple variables can be selected as facets.")
        ),
        shiny::fluidRow(
          selectPred("xVar", "x:", 4),
          selectPred("colorVar", "Color:", 4,
                     choices = c("None", preds_small)),
          selectPred("facetVar", "Facets:", 4, multiple = TRUE,
                     choices = preds_small)
        )
      ),

      bslib::navset_card_tab(
        id = "study_type",
        title = "Study type",

        bslib::nav_panel(
          list("Termination study",
               info_tooltip(
                 "Choose expected values (if available) that appear in the plot
                 and table outputs. If desired, select a weighting variable
                 for summarizing experience.")
          ),
          value = "exp",
          shiny::fluidRow(
            expected_widget,
            selectPred("weightVar", "Weight by:", 6,
                       choices = c("None",
                                   filter(preds, is_number)$predictors))
          )),

        trx_tab

      )
    ),

    bslib::navset_bar(
      title = "Output",
      bslib::nav_panel(
        "Plot",
        bslib::card(
          bslib::card_header(
            "Plot inputs",
            info_tooltip(
              shiny::markdown(
                '<div style="text-align: left">

                - `y`-axis variable selection
                - `Second y-axis` toggle and variable
                - `Geometry` for plotting
                - `Add smoothing`: add smooth loess curves
                - `Confidence intervals`: If available, draw confidence interval
                error bars
                - `Free y-scales`: enable separate `y` scales in each subplot
                - `Log y-axis`: plot y-axes on a log-10 scale
                - The grouping variables selected above will determine the
                  variable on the `x`-axis, the color variable, and faceting
                  variables used to create subplots.

                </div>'),
              custom_class = "left-tip"
            )),
          shiny::fluidRow(
            shiny::column(
              width = 8,

              shiny::fluidRow(
                selectPred("yVar", "y:", 6, choices = yVar_exp),
                shiny::column(
                  width = 6,
                  shiny::radioButtons("plotGeom",
                                      shiny::strong("Geometry:"),
                                      choices = c("Bars" = "bars",
                                                  "Lines and Points" = "lines",
                                                  "Points" = "points"))
                ),
              ),

              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  bslib::input_switch("plot2ndY",
                                      shiny::strong("Second y-axis"),
                                      value = FALSE)
                ),
                selectPred("yVar_2nd", "Second axis y:", 6, choices = yVar_exp,
                           selected = "exposure"),
              )),

            shiny::column(
              width = 4,
              bslib::input_switch("plotSmooth",
                                  shiny::strong("Add smoothing"),
                                  value = FALSE),
              bslib::input_switch("plotCI",
                                  shiny::strong("Confidence intervals"),
                                  value = FALSE),
              bslib::input_switch("plotFreeY",
                                  shiny::strong("Free y-scales"),
                                  value = FALSE),
              bslib::input_switch("plotLogY",
                                  shiny::strong("Log y-axis"),
                                  value = FALSE)
            ))
        ),

        bslib::card(
          class = "no-overflow",
          full_screen = TRUE,
          bslib::card_header(
            bslib::popover(
              shiny::icon("gear"),
              bslib::input_switch("plotResize", "Resize plot", value = FALSE),
              shiny::sliderInput("plotHeight", "Height (pixels):",
                                 200, 1500, value = 500, step = 50),
              shiny::sliderInput("plotWidth", "Width (pixels):",
                                 200, 1500, value = 1500, step = 50)
            )
          ),
          bslib::card_body(
            class = "no-overflow",
            shiny::plotOutput("xpPlot", height = "500px")
          )
        )
      ),

      bslib::nav_panel(
        "Table",
        bslib::card(
          class = "no-overflow",
          full_screen = TRUE,
          bslib::card_header(
            bslib::popover(
              shiny::icon("gear"),
              bslib::input_switch("tableCI",
                                  shiny::strong("Confidence intervals"),
                                  value = FALSE),
              bslib::input_switch("tableCredAdj",
                                  shiny::strong("Credibility-weighted termination rates"),
                                  value = FALSE),
              bslib::input_switch("tableColorful",
                                  shiny::strong("Include color scales"),
                                  value = TRUE),
              shiny::sliderInput("tableDecimals",
                                 shiny::strong("Decimals:"),
                                 value = 1, min = 0, max = 5),
              shiny::sliderInput("tableFontsize",
                                 shiny::strong("Font size multiple:"),
                                 value = 100, min = 50, max = 150, step = 5)
            )
          ),
          bslib::card_body(
            class = "no-overflow",
            gt::gt_output("xpTable")
          )
        )
      ),

      bslib::nav_spacer(),
      bslib::nav_menu(
        title = list(shiny::icon("download"), "Export"),
        align = "right",
        bslib::nav_item(
          shiny::downloadLink("xpDownload", "Summary data (.csv)"),
          shiny::downloadLink("plotDownload", "Plot (.png)"),
          shiny::downloadLink("tableDownload", "Table (.png)")
        )
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
    shiny::observe({

      new_choices <- if (input$study_type == "exp") {
        yVar_exp2()
      } else {
        yVar_trx2()
      }

      shiny::updateSelectInput(
        session, "yVar", choices = new_choices,
        selected = if (input$yVar %in% new_choices) {
          input$yVar
        } else {
          new_choices[[1]]
        }
      )

    }) |>
      shiny::bindEvent(input$study_type, input$ex_checks, input$pct_checks)

    shiny::observe({

      if (input$study_type == "exp") {

        new_choices_2 <- yVar_exp2()[
          !yVar_exp2() %in% c("All termination rates", "All A/E ratios")]
        if(input$yVar == "All termination rates") {
          new_choices_2 <- new_choices_2[
            !new_choices_2 %in% c('q_obs', input$ex_checks)]
        } else if (input$yVar == "All A/E ratios") {
          new_choices_2 <- new_choices_2[
            !new_choices_2 %in% paste0("ae_", input$ex_checks)]
        }

      } else {
        new_choices_2 <- yVar_trx2()
      }

      shiny::updateSelectInput(
        session, "yVar_2nd", choices = new_choices_2,
        selected = if (input$yVar_2nd %in% new_choices_2) {
          input$yVar_2nd
        } else {
          "exposure"
        }
      )

    }) |>
      shiny::bindEvent(input$study_type, input$ex_checks, input$pct_checks,
                       input$yVar)

    # disable color input when using special plots
    shiny::observe(
      if (input$yVar %in% c("All termination rates", "All A/E ratios")) {
        shiny::updateSelectInput(
          session, "colorVar", choices = "Series", selected = "Series")
      } else {
        shiny::updateSelectInput(
          session, "colorVar", choices = c("None", preds_small),
          selected = if (input$colorVar %in% c("None", preds_small)) {
            input$colorVar
          } else {
            "None"
          }
        )
      }
    ) |>
      shiny::bindEvent(input$yVar, input$ex_checks)

    # notification in pause mode
    shiny::observe({
      if (!input$play) {
        shiny::showNotification("Reactivity is paused...", duration = NULL,
                                id = "paused", closeButton = FALSE)
      } else {
        shiny::removeNotification("paused")
      }
    })

    # reactive data
    rdat <- shiny::reactive({

      shiny::validate(
        shiny::need(input$play, "Paused")
      )

      keep <- purrr::imap_lgl(preds$predictors,
                              ~ length(setdiff(preds$scope[[.y]],
                                               input[[paste0("i_", .x)]])) > 0)
      filters <- purrr::map(preds$predictors[keep], expr_filter)

      dat |>
        filter(!!!filters)
    })

    # experience study
    rxp <- shiny::reactive({

      shiny::validate(shiny::need(nrow(rdat()) > 0,
                                  "No data remaining after applying filters."))

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
          exp_stats(wt = wt, credibility = credibility, expected = ex,
                    conf_level = conf_level, cred_r = cred_r,
                    conf_int = TRUE)
      } else {
        rdat() |>
          group_by(dplyr::across(dplyr::all_of(.groups))) |>
          trx_stats(percent_of = input$pct_checks,
                    trx_types = input$trx_types_checks,
                    combine_trx = input$trx_combine,
                    conf_int = TRUE)
      }

    })

    # plot output
    rplot <- shiny::reactive({

      if (input$study_type == "exp" && input$yVar %in% yVar_trx2() &
          !input$yVar == "exposure") return()
      if (input$study_type == "trx" && input$yVar %in% yVar_exp2() &
          !input$yVar == "exposure") return()

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
        if (!all(c(input$yVar, as.character(color)) %in% names(rxp()))) return()
        y <- rlang::sym(input$yVar)
        plot.fun <- autoplot
      }

      second_y <- rlang::sym(input$yVar_2nd)

      mapping <- ggplot2::aes(!!x, !!y, color = !!color,
                              fill = !!color, group = !!color)

      # y labels
      get_y_labels <- function(x) {
        if (x %in% c("claims", "n_claims", "exposure",
                     "trx_n", "trx_flag", "trx_amt",
                     "avg_trx", "avg_all",
                     input$pct_checks,
                     paste0(input$pct_checks, "_w_trx"))) {
          scales::label_comma(accuracy = 1)
        } else if (x == "trx_freq") {
          scales::label_comma(accuracy = 0.1)
        } else {
          scales::label_percent(accuracy = 0.1)
        }
      }

      y_labels <- get_y_labels(input$yVar)
      second_y_labels <- get_y_labels(input$yVar_2nd)

      if (is.null(input$facetVar)) {
        p <- dat |> plot.fun(mapping = mapping, geoms = input$plotGeom,
                             y_labels = y_labels,
                             second_axis = input$plot2ndY,
                             second_y = !!second_y,
                             second_y_labels = second_y_labels,
                             y_log10 = input$plotLogY,
                             conf_int_bars = input$plotCI)
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
                             second_axis = input$plot2ndY,
                             second_y = !!second_y,
                             second_y_labels = second_y_labels,
                             y_log10 = input$plotLogY,
                             conf_int_bars = input$plotCI)
      }

      if (input$plotSmooth) p <- p + ggplot2::geom_smooth(method = "loess",
                                                          formula = y ~ x)

      p +
        ggplot2::theme_light() +
        ggplot2::theme(axis.text =
                         ggplot2::element_text(size = ggplot2::rel(0.95)),
                       strip.text =
                         ggplot2::element_text(size = ggplot2::rel(1)),
                       strip.background =
                         ggplot2::element_rect(fill = "#43536b")
        )

    })

    # table output
    output$xpPlot <- shiny::renderPlot(
      {rplot()},
      res = 92,
      height = function() if (input$plotResize) input$plotHeight else "auto",
      width = function() if (input$plotResize) input$plotWidth else "auto")

    rtable <- shiny::reactive({
      # for an unknown reason, the table doesn't react to changes in decimals
      # alone as if it were wrapped in isolate(). force() resolves the issue
      force(input$tableDecimals)
      if (input$study_type == "exp") {
        rxp() |> autotable(show_conf_int = input$tableCI,
                           show_cred_adj = input$tableCredAdj,
                           colorful = input$tableColorful,
                           decimals = input$tableDecimals,
                           fontsize = input$tableFontsize)
      } else {
        rxp() |> autotable(show_conf_int = input$tableCI,
                           colorful = input$tableColorful,
                           decimals = input$tableDecimals,
                           fontsize = input$tableFontsize)
      }
    })

    output$xpTable <- gt::render_gt({rtable()})

    # filter information
    output$rem_pct <- shiny::renderText({
      scales::label_percent(accuracy=1)(nrow(rdat()) / total_rows)
    })
    output$rem_rows <- shiny::renderText({
      paste0("Remaining rows: ", scales::label_comma()(nrow(rdat())))
    })

    output$filter_pie <- shiny::renderPlot({
      data.frame(name = c("included", "excluded"),
                 n = c(nrow(rdat()), total_rows - nrow(rdat()))) |>
        mutate(ymax = cumsum(n) / sum(n),
               ymin = dplyr::lag(ymax, default = 0)) |>
        ggplot2::ggplot(ggplot2::aes(ymin = ymin, ymax = ymax,
                                     xmin = 3, xmax = 4, fill = name)) +
        ggplot2::geom_rect() +
        ggplot2::coord_polar(theta = "y", direction = -1) +
        ggplot2::xlim(c(2, 4)) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_manual(values = c("#BBBBBB", "#033C73"))
    }, res = 92)

    # exporting
    output$xpDownload <- shiny::downloadHandler(
      filename = export_path(input$study_type, "data", "csv"),
      content = function(file) {
        readr::write_csv(rxp(), file)
      }
    )

    output$plotDownload <- shiny::downloadHandler(
      filename = export_path(input$study_type, "plot", "png"),
      content = function(file) {
        ggplot2::ggsave(file, plot = rplot(),
                        height = if (input$plotResize) input$plotHeight else NA,
                        width = if (input$plotResize) input$plotWidth else NA,
                        units = "px",
                        dpi = 92)
      }
    )

    output$tableDownload <- shiny::downloadHandler(
      filename = export_path(input$study_type, "table", "png"),
      content = function(file) {
        gt::gtsave(rtable(), file)
      }
    )

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
