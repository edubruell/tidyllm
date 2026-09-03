# tidyllm example app: "What does this regression say?"
#
# Fits a linear model to a small public dataset, then streams two explanations
# of the fitted coefficients side by side while the app stays responsive.
#
# Everything numeric is computed in R. The model is only ever shown the
# coefficient table and asked to narrate it.
#
# Run with:  tidyllm_example_app("model_explainer")
# Defaults to a local Ollama model, so it needs no API key and costs nothing.

library(shiny)
library(tidyllm)

# ---- data ------------------------------------------------------------------
# Only datasets that ship with base R, plus one optional extra, so the app runs
# on a clean machine with no downloads.

available_datasets <- function() {
  sets <- list(
    "Swiss fertility (1888 provinces)" = list(
      data = datasets::swiss,
      default_y = "Fertility",
      context = paste(
        "47 French-speaking provinces of Switzerland around 1888.",
        "Fertility is a standardised fertility measure, Agriculture the share of",
        "males working in agriculture, Examination and Education are percentages",
        "of army draftees with high marks and with schooling beyond primary,",
        "Catholic the share of Catholics, Infant.Mortality live births who live",
        "less than one year. All variables are in percent except Fertility."
      )
    ),
    "Life-cycle savings (50 countries, 1960-70)" = list(
      data = datasets::LifeCycleSavings,
      default_y = "sr",
      context = paste(
        "Aggregate savings data for 50 countries, averaged over 1960-1970.",
        "sr is the aggregate personal savings rate, pop15 and pop75 the",
        "percentage of population under 15 and over 75, dpi real per-capita",
        "disposable income in US dollars, ddpi the percentage growth rate of dpi."
      )
    )
  )

  if (requireNamespace("wooldridge", quietly = TRUE)) {
    wage1 <- wooldridge::wage1
    wage1$lwage <- log(wage1$wage)
    sets[["Wages (US CPS 1976)"]] <- list(
      data = wage1[, c("lwage", "educ", "exper", "tenure", "female", "married", "nonwhite")],
      default_y = "lwage",
      context = paste(
        "526 workers from the 1976 US Current Population Survey.",
        "lwage is the log of average hourly earnings, so coefficients are",
        "approximately proportional effects. educ, exper and tenure are years of",
        "schooling, labour-market experience and tenure with the current employer.",
        "female, married and nonwhite are indicator variables."
      )
    )
  }
  sets
}

DATASETS <- available_datasets()

numeric_vars <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

# ---- the model, and how it is written down for a language model ------------

fit_model <- function(df, y, x) {
  form <- stats::as.formula(paste(y, "~", paste(x, collapse = " + ")))
  stats::lm(form, data = df)
}

coef_table <- function(fit) {
  cf <- summary(fit)$coefficients
  data.frame(
    term      = rownames(cf),
    estimate  = round(cf[, 1], 4),
    std_error = round(cf[, 2], 4),
    statistic = round(cf[, 3], 2),
    p_value   = signif(cf[, 4], 3),
    row.names = NULL
  )
}

# A markdown table is what actually goes into the prompt. Pasting the numbers
# verbatim, rather than describing them, is what keeps the model from inventing
# any of its own.
model_as_markdown <- function(fit) {
  tab <- coef_table(fit)
  s <- summary(fit)
  header <- paste0("| ", paste(names(tab), collapse = " | "), " |")
  rule   <- paste0("| ", paste(rep("---", ncol(tab)), collapse = " | "), " |")
  rows   <- apply(tab, 1, function(r) paste0("| ", paste(trimws(r), collapse = " | "), " |"))
  paste(
    paste("Model:", paste(deparse(stats::formula(fit)), collapse = " ")),
    paste("Observations:", stats::nobs(fit)),
    paste0("R-squared: ", round(s$r.squared, 3),
           " (adjusted ", round(s$adj.r.squared, 3), ")"),
    paste0("Residual standard error: ", round(s$sigma, 4)),
    "",
    "Coefficients (OLS, classical standard errors):",
    header, rule, paste(rows, collapse = "\n"),
    sep = "\n"
  )
}

# The context describes the whole dataset, so both prompts have to say which of
# its variables are actually in this specification. Without that line the model
# happily narrates a coefficient for a variable the user unchecked.
spec_note <- function(fit, data) {
  used <- all.vars(stats::formula(fit))
  left_out <- setdiff(names(data), used)
  paste0(
    "The fitted model uses exactly these variables and no others: ",
    paste(used, collapse = ", "), ".",
    if (length(left_out)) paste0(
      " These variables are in the dataset but were deliberately left out of the model: ",
      paste(left_out, collapse = ", "), ".") else ""
  )
}

prompt_plain <- function(fit, context, data) {
  llm_message(paste(
    "You explain regression output to someone who knows a little statistics",
    "but does not work with regressions every day.",
    "\n\nData: ", context,
    "\n\n", spec_note(fit, data),
    "\n\n", model_as_markdown(fit),
    "\n\nWrite four short paragraphs, no headings and no bullet lists.",
    "Say what the model is fitted to, then walk through the two or three most",
    "interesting coefficients: sign, size in the units of the data, and whether",
    "the estimate is precise enough to take seriously.",
    "Close with what the R-squared does and does not tell us.",
    "\n\nWrite in plain prose without em-dashes.",
    "Use only the numbers in the table above. Never state a number that is",
    "not there, and never describe a coefficient as an effect of one variable",
    "on another; these are associations."
  ))
}

prompt_referee <- function(fit, context, data) {
  llm_message(paste(
    "You are a careful referee reading this regression for the first time.",
    "\n\nData: ", context,
    "\n\n", spec_note(fit, data),
    "\n\n", model_as_markdown(fit),
    "\n\nList the three or four objections you would actually raise, most",
    "serious first, each as one short paragraph beginning with a bold phrase",
    "naming the problem. Think about omitted variables, reverse causality,",
    "sample selection, functional form and how the standard errors were",
    "computed. Be concrete about this specification rather than generic.",
    "Write in plain prose without em-dashes.",
    "Do not invent numbers that are not in the table."
  ))
}

# ---- providers -------------------------------------------------------------
# The dropdown is the point: the same app, the same code path, four APIs.

# `concurrent` is not a stylistic choice. A streaming send_chat() returns once
# the response *headers* arrive, and a server that answers one request at a time
# does not send headers for the second request until the first is finished; on a
# stock Ollama the second call would therefore block the whole Shiny session,
# which also stops anything from draining the first stream. So local models get
# the two explanations one after the other, and cloud providers get them at once.
# Set OLLAMA_NUM_PARALLEL=2 before starting Ollama to run the two together there
# as well.
# Models are named only where a cheap one is wanted for a demo; every entry is
# an ordinary provider call, so swap in whatever model you have access to.
PROVIDERS <- list(
  "Ollama (local, no API key)" = list(call = quote(ollama(.model = "qwen3.5:4b")),
                                      concurrent = FALSE),
  "Claude"                     = list(call = quote(claude()),  concurrent = TRUE),
  "OpenAI"                     = list(call = quote(openai(.model = "gpt-5.6-luna")),
                                      concurrent = TRUE),
  "Gemini"                     = list(call = quote(gemini()),  concurrent = TRUE)
)

# ---- ui --------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .stream-box { line-height: 1.45; min-height: 12em;
                  border-left: 3px solid #ddd; padding-left: 12px; }
    .stream-title { font-weight: 600; margin-bottom: .4em; }
    .status { color: #777; font-size: 90%; }
  "))),
  titlePanel("What does this regression say?"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("dataset", "Dataset", names(DATASETS)),
      selectInput("y", "Outcome", character(0)),
      checkboxGroupInput("x", "Predictors", character(0)),
      selectInput("provider", "Provider", names(PROVIDERS)),
      actionButton("explain", "Explain this model", class = "btn-primary"),
      actionButton("cancel", "Cancel"),
      tags$hr(),
      textInput("followup", "Ask a follow-up", placeholder = "Why is that coefficient negative?"),
      actionButton("ask", "Ask"),
      tags$hr(),
      div(class = "status", textOutput("status"))
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(6, tags$div(class = "stream-title", "The model"), tableOutput("coefs")),
        column(6, plotOutput("coefplot", height = "280px"))
      ),
      tags$hr(),
      fluidRow(
        column(6,
               tags$div(class = "stream-title", "In plain English"),
               tags$div(class = "stream-box", uiOutput("plain"))),
        column(6,
               tags$div(class = "stream-title", "What a referee would object to"),
               tags$div(class = "stream-box", uiOutput("referee")))
      )
    )
  )
)

# ---- server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Jobs live in a plain environment, not in reactiveValues: they are handles to
  # something running, not values the UI should invalidate on.
  jobs <- new.env(parent = emptyenv())
  jobs$plain <- NULL
  jobs$referee <- NULL
  jobs$conversation <- NULL
  jobs$referee_prompt <- NULL
  jobs$provider <- NULL

  plain_text   <- reactiveVal("")
  referee_text <- reactiveVal("")
  status_text  <- reactiveVal("Pick a specification and press Explain.")

  current <- reactive({
    spec <- DATASETS[[input$dataset]]
    spec
  })

  observeEvent(input$dataset, {
    spec <- DATASETS[[input$dataset]]
    vars <- numeric_vars(spec$data)
    updateSelectInput(session, "y", choices = vars, selected = spec$default_y)
    updateCheckboxGroupInput(session, "x",
                             choices = setdiff(vars, spec$default_y),
                             selected = setdiff(vars, spec$default_y))
  })

  observeEvent(input$y, {
    spec <- DATASETS[[input$dataset]]
    vars <- setdiff(numeric_vars(spec$data), input$y)
    updateCheckboxGroupInput(session, "x", choices = vars,
                             selected = intersect(input$x, vars))
  }, ignoreInit = TRUE)

  fit <- reactive({
    req(input$y, input$x)
    fit_model(current()$data, input$y, input$x)
  })

  output$coefs <- renderTable(coef_table(fit()), digits = 4)

  output$coefplot <- renderPlot({
    f <- fit()
    cf <- summary(f)$coefficients
    cf <- cf[rownames(cf) != "(Intercept)", , drop = FALSE]
    est <- cf[, 1]; se <- cf[, 2]
    # Coefficients are on wildly different scales, so each is shown in units of
    # its own standard error: the plot is about precision, not about size.
    z <- est / se
    op <- graphics::par(mar = c(4, 8, 1, 1)); on.exit(graphics::par(op))
    graphics::plot(z, seq_along(z), yaxt = "n", ylab = "", xlab = "estimate / standard error",
                   xlim = range(c(z, -2.5, 2.5)), pch = 19)
    graphics::axis(2, at = seq_along(z), labels = rownames(cf), las = 1)
    graphics::abline(v = 0, col = "grey60")
    graphics::abline(v = c(-1.96, 1.96), col = "grey80", lty = 2)
  })

  stop_jobs <- function() {
    jobs$referee_prompt <- NULL
    for (nm in c("plain", "referee")) {
      job <- jobs[[nm]]
      if (!is.null(job) && check_job(job) == "running") cancel_job(job)
    }
  }

  # Two jobs, one event loop. Neither call blocks, so both panels fill in at the
  # same time and the sliders keep responding while they do.
  start_referee <- function() {
    jobs$referee <- send_chat(
      jobs$referee_prompt, eval(jobs$provider), .stream = TRUE,
      .on_chunk = function(delta) referee_text(paste0(isolate(referee_text()), delta))
    )
    jobs$referee_prompt <- NULL
  }

  observeEvent(input$explain, {
    stop_jobs()
    plain_text(""); referee_text("")
    provider <- PROVIDERS[[input$provider]]
    spec <- current()
    f <- fit()

    conversation <- prompt_plain(f, spec$context, spec$data)
    jobs$conversation   <- conversation
    jobs$provider       <- provider$call
    jobs$referee_prompt <- prompt_referee(f, spec$context, spec$data)
    jobs$referee        <- NULL

    jobs$plain <- send_chat(
      conversation, eval(provider$call), .stream = TRUE,
      .on_chunk = function(delta) plain_text(paste0(isolate(plain_text()), delta))
    )
    if (isTRUE(provider$concurrent)) {
      start_referee()
      status_text("Streaming two explanations...")
    } else {
      status_text("Streaming the first explanation; the second follows after it.")
    }
  })

  # A follow-up continues the plain-English conversation. tidyllm's history is
  # immutable, so the new turn is the old message plus this one; nothing is
  # mutated in place and the previous LLMMessage is still valid.
  observeEvent(input$ask, {
    req(nzchar(input$followup), !is.null(jobs$plain))
    if (check_job(jobs$plain) != "done") {
      status_text("Wait for the first answer before asking a follow-up.")
      return()
    }
    conversation <- fetch_job(jobs$plain) |> llm_message(input$followup)
    jobs$conversation <- conversation
    plain_text(paste0(plain_text(), "\n\n> ", input$followup, "\n\n"))
    jobs$plain <- send_chat(
      conversation, eval(PROVIDERS[[input$provider]]$call), .stream = TRUE,
      .on_chunk = function(delta) plain_text(paste0(isolate(plain_text()), delta))
    )
    updateTextInput(session, "followup", value = "")
  })

  observeEvent(input$cancel, {
    stop_jobs()
    status_text("Cancelled.")
  })

  # Status is polled rather than pushed: a job that fails does so between
  # chunks, and there is no delta to carry the news.
  observe({
    invalidateLater(400, session)
    if (is.null(jobs$plain)) return()

    # The queued second job, for providers that cannot take both at once.
    if (!is.null(jobs$referee_prompt) && check_job(jobs$plain) != "running") {
      start_referee()
    }

    states <- vapply(c("plain", "referee"), function(nm) {
      job <- jobs[[nm]]
      if (is.null(job)) "none" else check_job(job)
    }, character(1))
    if (!is.null(jobs$referee_prompt)) states["referee"] <- "running"
    if (any(states == "error")) {
      status_text("One of the requests failed. See the R console for the error.")
    } else if (all(states %in% c("done", "none"))) {
      status_text("Done.")
    } else if (any(states == "running")) {
      status_text(paste0("Streaming... ",
                         nchar(isolate(plain_text())) + nchar(isolate(referee_text())),
                         " characters so far"))
    }
  })

  output$status  <- renderText(status_text())
  # Rendered as markdown on every chunk, so the emphasis the prompts ask for
  # arrives formatted rather than as literal asterisks.
  output$plain   <- renderUI(markdown(plain_text()))
  output$referee <- renderUI(markdown(referee_text()))

  # Closing the browser tab must not leave a connection open and a callback
  # scheduled against a session that no longer exists.
  session$onSessionEnded(function() stop_jobs())
}

shinyApp(ui, server)
