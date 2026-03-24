# ============================================================
# app.R — Suicide Mortality Trends (CDC WONDER, ICD-10 X60-X84)
# Data file expected at: data/NEWDATASET_VER1.csv
# ============================================================
# last edited 02/25/2026 (rubric updates: Overview tab + 3 tests + Conclusions/Policy)
library(shiny)
library(tidyverse)

# ---- Load data once at startup ----
file_path <- "data/NEWDATASET_VER1.csv"
stopifnot(file.exists(file_path))

wonder_raw <- readr::read_csv(file_path, show_col_types = FALSE)

# Lightweight "clean_names" without janitor
clean_names_simple <- function(nms) {
  nms <- gsub("[^A-Za-z0-9]+", "_", nms)
  nms <- gsub("^_+|_+$", "", nms)
  tolower(nms)
}
names(wonder_raw) <- clean_names_simple(names(wonder_raw))

# Helper: safely check for optional columns
has_col <- function(df, col) col %in% names(df)

# Format helpers
fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}
fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_, format.pval(p, digits = 3, eps = 0.001))
}

# ---- Preprocess: type-cast + collapse cause-of-death rows (if present) ----
wonder_pre <- wonder_raw %>%
  mutate(
    state = as.factor(state),
    year  = as.integer(year),
    deaths = as.numeric(deaths),
    population = as.numeric(population)
  )

# Ensure optional fields exist before grouping
if (!has_col(wonder_pre, "sex"))  wonder_pre$sex  <- factor("All")
if (!has_col(wonder_pre, "race")) wonder_pre$race <- factor("All")
wonder_pre$sex  <- as.factor(wonder_pre$sex)
wonder_pre$race <- as.factor(wonder_pre$race)

# Collapse across cause-of-death codes/labels if they exist in the export.
# CDC WONDER exports often repeat population across cause codes, so we keep population once per group.
collapse_needed <- has_col(wonder_pre, "cause_of_death") || has_col(wonder_pre, "cause_of_death_code")
group_cols <- c("state", "year", "sex", "race")

if (collapse_needed) {
  wonder <- wonder_pre %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      deaths = sum(deaths, na.rm = TRUE),
      population = max(population, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  wonder <- wonder_pre
}

# Reliability flag (used by the checkbox)
wonder <- wonder %>%
  mutate(unreliable_rate = ifelse(is.na(deaths), FALSE, deaths < 20))

# Optional fields (won’t break if missing)
if (!has_col(wonder, "sex"))  wonder$sex  <- factor("All")
if (!has_col(wonder, "race")) wonder$race <- factor("All")
wonder$sex  <- as.factor(wonder$sex)
wonder$race <- as.factor(wonder$race)

year_min <- min(wonder$year, na.rm = TRUE)
year_max <- max(wonder$year, na.rm = TRUE)

# ---- UI ----
ui <- fluidPage(
  tags$style(HTML("
    .small-note { color:#555; font-size: 0.95em; }
    .callout { background:#f7f7f7; border-left: 4px solid #999; padding: 12px 14px; border-radius: 6px; margin: 10px 0; }
    .card { background:white; border: 1px solid #e5e5e5; border-radius: 10px; padding: 14px; margin: 10px 0; }
    .card h4 { margin-top: 0; }
    .hr-tight { margin-top: 8px; margin-bottom: 8px; }
  ")),

  titlePanel("Suicide Mortality Trends (CDC WONDER: ICD-10 X60–X84)"),

  sidebarLayout(
    sidebarPanel(
      tags$div(class = "small-note",
               "Rates displayed in this app are computed as deaths / population × 100,000 (crude rates).",
               tags$br(),
               "Use the checkbox to exclude small-count rows (often unstable)."
      ),
      hr(class = "hr-tight"),

      selectInput("state", "State:", choices = sort(unique(wonder$state))),

      sliderInput("year_range", "Year range:",
                  min = year_min, max = year_max,
                  value = c(year_min, year_max),
                  step = 1, sep = ""),

      selectInput("sex", "Sex:", choices = c("All", sort(unique(as.character(wonder$sex))))),
      selectInput("race", "Race:", choices = c("All", sort(unique(as.character(wonder$race))))),
      checkboxInput("exclude_unreliable", "Exclude unreliable rows (deaths < 20 / flagged unreliable)", value = TRUE),

      hr(),

      numericInput("top_k", "Top k states (ranking plot):", value = 5, min = 1, max = 25, step = 1),
      checkboxInput("show_points", "Show points on trend", value = TRUE),

      hr(),
      downloadButton("download_filtered", "Download filtered data (CSV)"),
      downloadButton("download_trend_plot", "Download trend plot (PNG)")
    ),

    mainPanel(
      tabsetPanel(
        # 1) Overview/About (self-explanatory requirement)
        tabPanel(
  "Overview / About",
  tags$div(style = "max-width: 950px;",

           tags$h3("What this app is"),
           tags$p(
             "This Shiny app explores U.S. suicide mortality patterns using CDC WONDER mortality data.",
             "It is designed to be a self-contained, interactive summary of trends and differences across states and demographic groups."
           ),

           tags$div(class="callout",
                    tags$b("Quick start:"), " Choose a state and year range in the left panel. ",
                    "Optionally filter by sex and/or race. Then use the tabs below to answer each research question."
           ),

           tags$h4("Objective"),
           tags$p(
             "Describe how suicide mortality rates change over time and how they differ by state, sex, and race, to support public health discussion and prevention planning.",
             "This app helps identify where rates are highest or rising fastest so users can prioritize further investigation and evidence-based prevention planning."
           ),

           tags$h4("Research questions"),
           tags$ul(
             tags$li(tags$b("RQ1 (Time trend): "), "How do suicide mortality rates change over time in the selected state and filters?"),
             tags$li(tags$b("RQ2 (Geography): "), "Which states have the highest rates in a selected year, and which states are increasing the fastest?"),
             tags$li(tags$b("RQ3 (Disparities): "), "How do rates differ by sex and race (within available categories)?")
           ),

           tags$h4("Data and definitions"),
           tags$ul(
             tags$li(tags$b("Source: "), "CDC WONDER mortality data (death certificate–based NVSS data)."),
             tags$li(tags$b("Outcome: "), "Suicide deaths defined using ICD-10 intentional self-harm codes (commonly summarized as X60–X84 in this project)."),
             tags$li(tags$b("Measures: "), "Deaths, population, and crude rate per 100,000."),
             tags$li(tags$b("Rate shown: "), "Deaths per 100,000 population (crude), computed as deaths / population × 100,000.")
           ),

           tags$h4("Data quality and reliability"),
           tags$ul(
             tags$li("Small death counts can make rates unstable (especially for smaller populations or narrow subgroups)."),
             tags$li("Some very small counts may be suppressed in CDC WONDER outputs (commonly 0–9, depending on settings/table)."),
             tags$li("This app includes an option to exclude rows flagged as unreliable or with deaths < 20.")
           ),

           tags$h4("How to use this app (step-by-step)"),
           tags$ol(
             tags$li("Pick a state and set the year range."),
             tags$li("Optionally filter by sex and/or race."),
             tags$li("Turn on “Exclude unreliable rows” if you want more stable estimates."),
             tags$li("Use the tabs to explore trends, compare states, and review statistical tests + conclusions.")
           ),

           tags$h4("Methods (plain language)"),
           tags$ul(
             tags$li(tags$b("Trend plot: "), "Shows how the crude rate changes year-to-year for your selected filters."),
             tags$li(tags$b("State ranking: "), "Computes rates for all states in a single year and ranks the top-k."),
             tags$li(tags$b("Fastest increasing states: "), "Fits a simple linear trend (rate ~ year) per state and ranks by slope."),
             tags$li(tags$b("Poisson regression (with offset): "), "Models death counts while accounting for population size to estimate rate changes over time.")
           ),

           tags$h4("What each tab shows"),
           tags$ul(
             tags$li(tags$b("Trends: "), "Time series of the selected state's rate (answers RQ1)."),
             tags$li(tags$b("Compare states: "), "Top-k ranking of state rates for a single year (answers RQ2)."),
             tags$li(tags$b("Fastest increasing: "), "States with steepest positive slopes over the selected period (answers RQ2)."),
             tags$li(tags$b("Statistical tests: "), "Three tests with results and interpretations (supports RQ1–RQ3)."),
             tags$li(tags$b("Conclusions & policy: "), "Key takeaways, limitations, future work, and policy relevance."),
             tags$li(tags$b("Data: "), "Preview of the filtered rows used in the analysis.")
           ),

           tags$hr(),
           tags$h4("If you need support"),
           tags$p(tags$em("If you or someone you know is struggling, call or text 988 (U.S. Suicide & Crisis Lifeline)."))
  )
),

        # 2) Trends
        tabPanel(
          "Trends",
          tags$div(class="card",
                   tags$p("This plot updates based on your selections and shows the crude suicide death rate over time (deaths per 100,000)."),
                   plotOutput("trendPlot", height = 450)
          )
        ),

        # 3) Compare states
        tabPanel(
          "Compare states",
          tags$div(class="card",
                   tags$p("This bar chart compares states for the last year in your selected range (right end of the slider)."),
                   plotOutput("rankPlot", height = 450)
          )
        ),

        # 4) Fastest increasing
        tabPanel(
          "Fastest increasing",
          tags$div(class="card",
                   tags$p("This chart ranks states by their estimated annual change (slope) in the selected year range and filters."),
                   plotOutput("slopePlot", height = 450)
          )
        ),

        # 5) Statistical tests (≥3) with results
        tabPanel(
          "Statistical tests",
          tags$div(class="card",
                   tags$h4("Test 1: Poisson regression trend model (population-adjusted)"),
                   tags$p(class="small-note",
                          "Model: deaths ~ year with offset log(population), using yearly aggregates for your selected state and filters. ",
                          "Reported effect is an Incidence Rate Ratio (IRR) per 1-year increase."
                   ),
                   tableOutput("test1_tbl"),
                   verbatimTextOutput("test1_interp")
          ),
          tags$div(class="card",
                   tags$h4("Test 2: Spearman correlation between year and rate"),
                   tags$p(class="small-note",
                          "Nonparametric correlation test using yearly rate values (deaths per 100,000)."),
                   tableOutput("test2_tbl"),
                   verbatimTextOutput("test2_interp")
          ),
          tags$div(class="card",
                   tags$h4("Test 3: Poisson rate ratio test (first year vs last year)"),
                   tags$p(class="small-note",
                          "Compares the rate in the first year vs the last year in your selected range (population-adjusted)."),
                   tableOutput("test3_tbl"),
                   verbatimTextOutput("test3_interp")
          ),
          tags$div(class="card",
                   tags$h4("Optional: Do trends differ by sex or race? (interaction test)"),
                   tags$p(class="small-note",
                          "If your filters leave multiple sex or race groups, this tests whether the trend over time differs by group (year × group)."),
                   tableOutput("interaction_tbl"),
                   verbatimTextOutput("interaction_interp")
          )
        ),

        # 6) Conclusions & Policy
        tabPanel(
          "Conclusions & policy",
          tags$div(style = "max-width: 950px;",
                   tags$div(class="card",
                            tags$h4("Key findings for your current selections"),
                            uiOutput("findings_ui")
                   ),
                   tags$div(class="card",
                            tags$h4("Limitations"),
                            tags$ul(
                              tags$li("This is descriptive and ecological (population-level), not a causal analysis of individual risk."),
                              tags$li("Small counts can lead to unstable rates; excluding deaths < 20 helps but does not eliminate uncertainty."),
                              tags$li("Grouping categories (race/sex) can mask within-group differences, and categories depend on available reporting.")
                            )
                   ),
                   tags$div(class="card",
                            tags$h4("Future work"),
                            tags$ul(
                              tags$li("Add age groups and age-adjusted rates; explore differences by method of suicide where available."),
                              tags$li("Incorporate context variables (e.g., rurality, unemployment, firearm access proxies) for stronger policy discussion."),
                              tags$li("Explore changepoints (e.g., joinpoint-style trends) and compare pre/post periods.")
                            )
                   ),
                   tags$div(class="card",
                            tags$h4("Policy relevance (interpretation guidance)"),
                            uiOutput("policy_ui")
                   ),
                   tags$hr(),
                   tags$p(tags$em("If you or someone you know is struggling, call or text 988 (U.S. Suicide & Crisis Lifeline)."))
          )
        ),

        # 7) Data preview
        tabPanel(
          "Data",
          tags$div(class="card",
                   tags$p("First 25 rows of the filtered dataset used for your selected state and filters."),
                   tableOutput("previewTbl")
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Filtered to SELECTED STATE + inputs (used for trend + tests)
  filtered <- reactive({
    df <- wonder %>%
      filter(
        state == input$state,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )

    if (input$sex != "All" && has_col(df, "sex")) df <- df %>% filter(as.character(sex) == input$sex)
    if (input$race != "All" && has_col(df, "race")) df <- df %>% filter(as.character(race) == input$race)
    if (isTRUE(input$exclude_unreliable)) df <- df %>% filter(!unreliable_rate)

    df
  })

  # Selected state aggregated to YEAR (for trend plot + tests)
  state_year <- reactive({
    filtered() %>%
      group_by(year) %>%
      summarise(
        deaths = sum(deaths, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        rate_per_100k = 100000 * deaths / population,
        .groups = "drop"
      ) %>%
      filter(is.finite(rate_per_100k), population > 0)
  })

  # Same as above but only counts + population (for Poisson with offset)
  state_year_counts <- reactive({
    filtered() %>%
      group_by(year) %>%
      summarise(
        deaths = sum(deaths, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(population > 0)
  })

  output$trendPlot <- renderPlot({
    df <- state_year()
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = year, y = rate_per_100k)) +
      geom_line(linewidth = 1) +
      labs(
        title = paste("Trend:", input$state,
                      if (input$sex != "All") paste("| Sex:", input$sex) else "",
                      if (input$race != "All") paste("| Race:", input$race) else ""),
        x = "Year",
        y = "Deaths per 100,000 (crude)"
      )

    if (isTRUE(input$show_points)) p <- p + geom_point(size = 2)
    p
  })

  # Ranking plot: compare all states in a single selected year (latest in slider)
  output$rankPlot <- renderPlot({
    year_pick <- input$year_range[2]

    df <- wonder %>%
      filter(year == year_pick)

    if (input$sex != "All" && has_col(df, "sex")) df <- df %>% filter(as.character(sex) == input$sex)
    if (input$race != "All" && has_col(df, "race")) df <- df %>% filter(as.character(race) == input$race)
    if (isTRUE(input$exclude_unreliable)) df <- df %>% filter(!unreliable_rate)

    rank_df <- df %>%
      group_by(state) %>%
      summarise(
        deaths = sum(deaths, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        rate_per_100k = 100000 * deaths / population,
        .groups = "drop"
      ) %>%
      filter(is.finite(rate_per_100k), population > 0) %>%
      arrange(desc(rate_per_100k)) %>%
      slice_head(n = input$top_k)

    req(nrow(rank_df) > 0)

    ggplot(rank_df, aes(x = reorder(state, rate_per_100k), y = rate_per_100k)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Top", input$top_k, "states by suicide rate in", year_pick),
        x = "State",
        y = "Deaths per 100,000 (crude)"
      )
  })

  # Slope plot: fastest increasing states in selected year range and filters (not restricted to one state)
  output$slopePlot <- renderPlot({
    df <- wonder %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])

    if (input$sex != "All" && has_col(df, "sex")) df <- df %>% filter(as.character(sex) == input$sex)
    if (input$race != "All" && has_col(df, "race")) df <- df %>% filter(as.character(race) == input$race)
    if (isTRUE(input$exclude_unreliable)) df <- df %>% filter(!unreliable_rate)

    st_yr <- df %>%
      group_by(state, year) %>%
      summarise(
        deaths = sum(deaths, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        rate_per_100k = 100000 * deaths / population,
        .groups = "drop"
      ) %>%
      filter(is.finite(rate_per_100k), population > 0)

    slopes <- st_yr %>%
      group_by(state) %>%
      filter(n() >= 5) %>%  # avoid very short time series
      summarise(
        slope = coef(lm(rate_per_100k ~ year))[2],
        .groups = "drop"
      ) %>%
      arrange(desc(slope)) %>%
      slice_head(n = 15)

    req(nrow(slopes) > 0)

    ggplot(slopes, aes(x = reorder(state, slope), y = slope)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "States with the steepest increases in suicide rates (slope by year)",
        x = "State",
        y = "Estimated annual change (deaths per 100,000)"
      )
  })

  # ------------------------------
  # Statistical Test 1: Poisson trend (selected state, selected filters)
  # ------------------------------
  output$test1_tbl <- renderTable({
    df <- state_year_counts()
    if (nrow(df) < 4) return(data.frame(Note = "Not enough years to fit the trend model (need ≥ 4)."))

    fit <- glm(
      deaths ~ year,
      family = poisson(link = "log"),
      offset = log(population),
      data = df
    )
    s <- summary(fit)$coefficients
    beta <- s["year", "Estimate"]
    se <- s["year", "Std. Error"]
    pval <- s["year", "Pr(>|z|)"]

    irr <- exp(beta)
    ci_low <- exp(beta - 1.96 * se)
    ci_high <- exp(beta + 1.96 * se)

    # quick overdispersion diagnostic (Pearson chi-square / df)
    pearson <- sum(residuals(fit, type = "pearson")^2, na.rm = TRUE)
    disp <- pearson / df.residual(fit)

    data.frame(
      Metric = c("IRR per 1-year increase", "95% CI (lower)", "95% CI (upper)", "p-value", "Overdispersion (Pearson/df)", "N years"),
      Value  = c(fmt_num(irr, 4), fmt_num(ci_low, 4), fmt_num(ci_high, 4), fmt_p(pval), fmt_num(disp, 2), nrow(df)),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "s")

  output$test1_interp <- renderText({
    df <- state_year_counts()
    if (nrow(df) < 4) {
      return("Interpretation: Not enough years in the selected range to estimate a time trend.")
    }
    fit <- glm(deaths ~ year, family = poisson(link = "log"), offset = log(population), data = df)
    s <- summary(fit)$coefficients
    beta <- s["year", "Estimate"]; se <- s["year", "Std. Error"]; pval <- s["year", "Pr(>|z|)"]
    irr <- exp(beta); ci <- exp(beta + c(-1, 1) * 1.96 * se)

    direction <- if (irr > 1) "increasing" else if (irr < 1) "decreasing" else "flat"

    paste0(
      "Interpretation:\n",
      "- Estimated trend is ", direction, " (IRR = ", fmt_num(irr, 4), ").\n",
      "- 95% CI: [", fmt_num(ci[1], 4), ", ", fmt_num(ci[2], 4), "].\n",
      "- p-value: ", fmt_p(pval), ".\n",
      "If overdispersion is large (Pearson/df >> 1), consider a Negative Binomial model."
    )
  })

  # ------------------------------
  # Statistical Test 2: Spearman correlation (year vs rate)
  # ------------------------------
  output$test2_tbl <- renderTable({
    df <- state_year()
    if (nrow(df) < 4) return(data.frame(Note = "Not enough years for correlation test (need ≥ 4)."))

    ct <- suppressWarnings(cor.test(df$year, df$rate_per_100k, method = "spearman", exact = FALSE))
    data.frame(
      Metric = c("Spearman rho", "p-value", "N years"),
      Value  = c(fmt_num(unname(ct$estimate), 3), fmt_p(ct$p.value), nrow(df)),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "s")

  output$test2_interp <- renderText({
    df <- state_year()
    if (nrow(df) < 4) return("Interpretation: Not enough years to test correlation between year and rate.")
    ct <- suppressWarnings(cor.test(df$year, df$rate_per_100k, method = "spearman", exact = FALSE))
    rho <- unname(ct$estimate)
    dir <- if (rho > 0) "positive" else if (rho < 0) "negative" else "no"
    paste0(
      "Interpretation:\n",
      "- There is a ", dir, " monotonic association between year and rate (rho = ", fmt_num(rho, 3), ").\n",
      "- p-value: ", fmt_p(ct$p.value), "."
    )
  })

  # ------------------------------
  # Statistical Test 3: Poisson rate ratio (first year vs last year)
  # ------------------------------
  output$test3_tbl <- renderTable({
    df <- state_year_counts() %>% arrange(year)
    if (nrow(df) < 2) return(data.frame(Note = "Need at least 2 years to compare first vs last year."))

    first <- df %>% slice(1)
    last  <- df %>% slice(n())

    pt <- poisson.test(
      x = c(first$deaths, last$deaths),
      T = c(first$population, last$population)
    )

    rr <- unname(pt$estimate)  # rate ratio
    ci <- unname(pt$conf.int)

    data.frame(
      Metric = c("Rate ratio (last / first)", "95% CI (lower)", "95% CI (upper)", "p-value", "First year", "Last year"),
      Value  = c(fmt_num(rr, 4), fmt_num(ci[1], 4), fmt_num(ci[2], 4), fmt_p(pt$p.value), first$year, last$year),
      check.names = FALSE
    )
  }, striped = TRUE, spacing = "s")

  output$test3_interp <- renderText({
    df <- state_year_counts() %>% arrange(year)
    if (nrow(df) < 2) return("Interpretation: Need at least 2 years to compare first vs last year.")

    first <- df %>% slice(1)
    last  <- df %>% slice(n())
    pt <- poisson.test(x = c(first$deaths, last$deaths), T = c(first$population, last$population))

    rr <- unname(pt$estimate)
    direction <- if (rr > 1) "higher" else if (rr < 1) "lower" else "about the same"
    paste0(
      "Interpretation:\n",
      "- The estimated rate in ", last$year, " is ", direction, " than in ", first$year,
      " (rate ratio = ", fmt_num(rr, 4), ").\n",
      "- p-value: ", fmt_p(pt$p.value), "."
    )
  })

  # ------------------------------
  # Optional interaction test: year × sex OR year × race (selected state; other filter held constant)
  # ------------------------------
  output$interaction_tbl <- renderTable({
    # Decide which interaction is feasible given current filters.
    base <- wonder %>%
      filter(state == input$state,
             year >= input$year_range[1], year <= input$year_range[2])

    if (input$race != "All" && has_col(base, "race")) base <- base %>% filter(as.character(race) == input$race)
    if (input$sex != "All" && has_col(base, "sex"))  base <- base %>% filter(as.character(sex) == input$sex)
    if (isTRUE(input$exclude_unreliable)) base <- base %>% filter(!unreliable_rate)

    # If sex == All, try sex interaction
    if (input$sex == "All" && has_col(base, "sex") && n_distinct(as.character(base$sex)) >= 2) {
      df <- base %>%
        group_by(year, sex) %>%
        summarise(deaths = sum(deaths, na.rm = TRUE),
                  population = sum(population, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(population > 0)
      if (nrow(df) < 8) return(data.frame(Note = "Not enough group-years to test year × sex interaction."))

      fit0 <- glm(deaths ~ year + sex, family = poisson(link="log"), offset = log(population), data = df)
      fit1 <- glm(deaths ~ year * sex, family = poisson(link="log"), offset = log(population), data = df)
      a <- anova(fit0, fit1, test = "Chisq")
      p_int <- a$`Pr(>Chi)`[2]

      return(data.frame(
        Test = "Likelihood ratio test: add year × sex",
        `p-value` = fmt_p(p_int),
        check.names = FALSE
      ))
    }

    # If race == All, try race interaction
    if (input$race == "All" && has_col(base, "race") && n_distinct(as.character(base$race)) >= 2) {
      df <- base %>%
        group_by(year, race) %>%
        summarise(deaths = sum(deaths, na.rm = TRUE),
                  population = sum(population, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(population > 0)
      if (nrow(df) < 8) return(data.frame(Note = "Not enough group-years to test year × race interaction."))

      fit0 <- glm(deaths ~ year + race, family = poisson(link="log"), offset = log(population), data = df)
      fit1 <- glm(deaths ~ year * race, family = poisson(link="log"), offset = log(population), data = df)
      a <- anova(fit0, fit1, test = "Chisq")
      p_int <- a$`Pr(>Chi)`[2]

      return(data.frame(
        Test = "Likelihood ratio test: add year × race",
        `p-value` = fmt_p(p_int),
        check.names = FALSE
      ))
    }

    data.frame(Note = "Interaction test not available with the current filters. Set Sex = All or Race = All (and keep enough data) to enable it.")
  }, striped = TRUE, spacing = "s")

  output$interaction_interp <- renderText({
    "Interpretation: A small p-value suggests that the time trend differs across groups (the slope is not the same for all sex or race categories)."
  })

  # ------------------------------
  # Conclusions & Policy: dynamic findings
  # ------------------------------
  output$findings_ui <- renderUI({
    df <- state_year() %>% arrange(year)
    if (nrow(df) == 0) return(tags$p("No data available for your current filters."))

    y0 <- df$year[1]; y1 <- df$year[nrow(df)]
    r0 <- df$rate_per_100k[1]; r1 <- df$rate_per_100k[nrow(df)]
    delta <- r1 - r0
    pct <- ifelse(is.finite(r0) && r0 > 0, 100 * delta / r0, NA_real_)

    dir <- if (delta > 0) "increased" else if (delta < 0) "decreased" else "stayed about the same"

    # rank info for selected year_pick
    year_pick <- input$year_range[2]
    all_states <- wonder %>% filter(year == year_pick)
    if (input$sex != "All" && has_col(all_states, "sex")) all_states <- all_states %>% filter(as.character(sex) == input$sex)
    if (input$race != "All" && has_col(all_states, "race")) all_states <- all_states %>% filter(as.character(race) == input$race)
    if (isTRUE(input$exclude_unreliable)) all_states <- all_states %>% filter(!unreliable_rate)

    rank_df <- all_states %>%
      group_by(state) %>%
      summarise(deaths = sum(deaths, na.rm=TRUE),
                population = sum(population, na.rm=TRUE),
                rate_per_100k = 100000 * deaths / population,
                .groups="drop") %>%
      filter(is.finite(rate_per_100k), population > 0) %>%
      arrange(desc(rate_per_100k)) %>%
      mutate(rank = row_number())

    rank_row <- rank_df %>% filter(state == input$state) %>% slice(1)
    rank_text <- if (nrow(rank_row) == 1) {
      paste0("In ", year_pick, ", ", input$state, " ranks #", rank_row$rank, " out of ", nrow(rank_df),
             " for the selected filters (rate = ", fmt_num(rank_row$rate_per_100k, 2), " per 100,000).")
    } else {
      paste0("Ranking for ", input$state, " in ", year_pick, " is not available under current filters.")
    }

    tags$div(
      tags$ul(
        tags$li(
          paste0("From ", y0, " to ", y1, ", the rate in ", input$state, " ", dir,
                 " from ", fmt_num(r0, 2), " to ", fmt_num(r1, 2), " per 100,000",
                 if (!is.na(pct)) paste0(" (", fmt_num(pct, 1), "% change).") else "."
          )
        ),
        tags$li(rank_text),
        tags$li("See the Statistical tests tab for formal tests supporting the observed patterns (trend model + correlation + first vs last year).")
      )
    )
  })

  output$policy_ui <- renderUI({
    df <- state_year() %>% arrange(year)
    if (nrow(df) == 0) return(tags$p("No policy interpretation available without data."))

    y0 <- df$year[1]; y1 <- df$year[nrow(df)]
    r0 <- df$rate_per_100k[1]; r1 <- df$rate_per_100k[nrow(df)]
    delta <- r1 - r0

    tags$div(
      tags$p("Policy interpretation should focus on targeting and planning rather than causal claims. Based on your current selections:"),
      tags$ul(
        tags$li(
          if (delta > 0) {
            paste0("An increasing pattern from ", y0, " to ", y1,
                   " may support prioritizing prevention planning and resource allocation in ", input$state, ".")
          } else if (delta < 0) {
            paste0("A decreasing pattern from ", y0, " to ", y1,
                   " suggests improvement, but continued monitoring is important (rates may still be high).")
          } else {
            paste0("A flat pattern from ", y0, " to ", y1, " suggests stability; consider examining subgroups (sex/race) or other risk contexts.")
          }
        ),
        tags$li("Small-count instability matters: use the “Exclude unreliable” option for more stable comparisons."),
        tags$li("Use this app to identify where rates are highest / rising fastest, then connect those patterns to evidence-based prevention strategies (e.g., means safety, crisis response capacity, access to care).")
      )
    )
  })

  # ------------------------------
  # Data preview
  # ------------------------------
  output$previewTbl <- renderTable({
    filtered() %>%
      select(any_of(c("state", "year", "sex", "race", "deaths", "population", "crude_rate_chr", "unreliable_rate"))) %>%
      arrange(year) %>%
      head(25)
  }, striped = TRUE, spacing = "s")

  # ------------------------------
  # Downloads
  # ------------------------------
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("suicide_filtered_", as.character(input$state), "_", input$year_range[1], "-", input$year_range[2], ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered(), file)
    }
  )

  output$download_trend_plot <- downloadHandler(
    filename = function() {
      paste0("trend_", as.character(input$state), "_", input$year_range[1], "-", input$year_range[2], ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      df <- state_year()
      if (nrow(df) > 0) {
        p <- ggplot(df, aes(x = year, y = rate_per_100k)) +
          geom_line(linewidth = 1) +
          labs(
            title = paste("Trend:", input$state,
                          if (input$sex != "All") paste("| Sex:", input$sex) else "",
                          if (input$race != "All") paste("| Race:", input$race) else ""),
            x = "Year",
            y = "Deaths per 100,000 (crude)"
          )
        if (isTRUE(input$show_points)) p <- p + geom_point(size = 2)
        print(p)
      }
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
