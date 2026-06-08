# ============================================================
# app.R  —  AI Persuasion Studies EDA Dashboard
# Mirrors the 3-page Power BI report:
#   1. Studies Description
#   2. Prompts & Instructions
#   3. Debriefing & LLMs
#
# Data expected in data/data.xlsx with two sheets:
#   "study_details"       (parent, 1 row per study)
#   "intervention_details" (child, many rows per study)
# ============================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(scales)
library(stringr)

# ============================================================
#  COLOUR SYSTEM
# ============================================================

CAT_COLOURS <- c(
  "Yes"                = "#2196F3",
  "Yes, explicitly"    = "#1565C0",
  "Yes, implicitly"    = "#64B5F6",
  "Yes, implicitly (e.g., debate an AI to convince it)" = "#64B5F6",
  "Partially"          = "#FF9800",
  "No"                 = "#F44336",
  "Not Applicable"     = "#9E9E9E",
  "Deemed Exempt"      = "#BDBDBD",
  "Not Reported"       = "#CFD8DC",
  "Unclear"            = "#B0BEC5"
)

FALLBACK_PAL <- c(
  "#4CAF50","#9C27B0","#00BCD4","#FF5722",
  "#607D8B","#E91E63","#3F51B5","#009688"
)

make_colours <- function(vals) {
  lvls    <- unique(na.omit(as.character(vals)))
  known   <- lvls[lvls %in% names(CAT_COLOURS)]
  unknown <- lvls[!lvls %in% names(CAT_COLOURS)]
  fb      <- setNames(rep_len(FALLBACK_PAL, length(unknown)), unknown)
  c(CAT_COLOURS[known], fb)
}

# ============================================================
#  COUNTRY COORDINATES LOOKUP
# ============================================================

COUNTRY_COORDS <- tibble(
  country = c(
    "USA","UK","Germany","France","Netherlands",
    "Canada","Australia","Italy","Spain","Sweden",
    "China","Japan","South Korea","India","Brazil",
    "Argentina","Mexico","Norway","Denmark","Finland",
    "Belgium","Austria","Switzerland","Poland","Portugal",
    "Ireland","New Zealand","Singapore","Israel","Turkey"
  ),
  lat = c(
    37.1, 51.5, 51.2, 46.2, 52.1,
    56.1,-25.3, 41.9, 40.4, 60.1,
    35.9, 36.2, 35.9, 20.6,-14.2,
    -38.4, 23.6, 60.5, 56.3, 64.5,
    50.5, 47.5, 46.8, 51.9, 39.4,
    53.4,-40.9,  1.4, 31.0, 38.9
  ),
  lon = c(
    -95.7,  -0.1,  10.5,   2.2,   5.3,
    -106.3, 133.8,  12.6,  -3.7,  18.6,
    104.2, 138.3, 127.8,  78.9, -51.9,
    -63.6, -102.5,  8.5,   9.5,  26.0,
    4.5,  14.6,   8.2,  19.1,  -8.2,
    -8.2, 172.5, 103.8,  34.9,  35.2
  )
)

# ============================================================
#  DATA LOADING
# ============================================================

load_data <- function() {
  
  # ── study_details ──────────────────────────────────────────
  studies <- tryCatch(
    read_excel("data/human_reviewer_extraction_clean.xlsx", sheet = "study_details"),
    error = function(e) {
      message("data/data.xlsx not found — using synthetic data for preview.")
      tibble(
        study_id            = 1:40,
        # Simulate comma-separated multi-country entries
        country             = sample(
          c("USA", "UK", "Germany", "USA, UK", "France, Germany",
            "Netherlands", "Canada", "Australia", "Italy, Spain", "Sweden"),
          40, TRUE
        ),
        publication_type    = sample(c("Journal article","Preprint","Conference paper"),
                                     40, TRUE, prob = c(.6,.3,.1)),
        ethical_approval    = sample(c("Yes","No","Not Reported","Deemed Exempt"),
                                     40, TRUE, prob = c(.5,.05,.35,.1)),
        experimental_design = sample(c("Yes","No"), 40, TRUE, prob = c(.8,.2)),
        online_platform     = sample(c("Yes","No","Not Reported"), 40, TRUE,
                                     prob = c(.55,.3,.15))
      )
    }
  )
  
  # ── study_countries: one row per study × country ───────────
  #   Handles cells like "USA, UK" or "Germany; France" or "Italy/Spain"
  study_countries <- studies |>
    select(study_id, country) |>
    mutate(country = str_split(country, "[,;/]+")) |>
    unnest(country) |>
    mutate(country = str_trim(country)) |>
    filter(!is.na(country), country != "") |>
    left_join(COUNTRY_COORDS, by = "country")
  
  # ── intervention_details ───────────────────────────────────
  interventions <- tryCatch(
    read_excel("data/human_reviewer_extraction_clean.xlsx", sheet = "intervention_details") |>
      pivot_longer(
        cols         = starts_with("Domain standardised"),
        names_to     = "domain_slot",
        values_to    = "Domain",
        values_drop_na = TRUE
      ) |>
      select(-domain_slot) |>
      pivot_longer(
        cols         = starts_with("LLM name"),
        names_to     = "llm_slot",
        values_to    = "LLM",
        values_drop_na = TRUE
      ) |>
      select(-llm_slot),
    error = function(e) {
      domains <- c("Health","Politics","Consumer","Environment","Finance","Other")
      llms    <- c("GPT-4","GPT-3.5","Claude","Gemini","LLaMA","GPT-4o",
                   "Other","Not Reported")
      yn_cols <- c("Yes","No","Partially","Not Reported")
      tibble(
        intervention_id     = 1:120,
        study_id            = sample(1:40, 120, TRUE),
        Domain              = sample(domains, 120, TRUE),
        LLM                 = sample(llms, 120, TRUE,
                                     prob = c(.35,.2,.1,.1,.05,.1,.05,.05)),
        is_interactive      = sample(yn_cols, 120, TRUE, prob=c(.3,.5,.1,.1)),
        ai_disclosed        = sample(c("Yes, explicitly","Yes, implicitly","No","Not Reported"),
                                     120, TRUE, prob=c(.25,.15,.4,.2)),
        prompt_available    = sample(yn_cols, 120, TRUE, prob=c(.4,.3,.15,.15)),
        prompt_states_goal  = sample(c("Yes","No","Unclear","Not Applicable"),
                                     120, TRUE, prob=c(.35,.3,.2,.15)),
        prompt_hides_goal   = sample(c("Yes","No","Unclear","Not Applicable"),
                                     120, TRUE, prob=c(.1,.6,.2,.1)),
        instructions_avail  = sample(yn_cols, 120, TRUE, prob=c(.5,.25,.15,.1)),
        instructions_reveal = sample(c("Yes, explicitly","Yes, implicitly (e.g., debate an AI to convince it)",
                                       "No","Not Reported"),
                                     120, TRUE, prob=c(.3,.2,.35,.15)),
        debriefing_reported = sample(c("Yes","No","Not Reported"),
                                     120, TRUE, prob=c(.45,.35,.2)),
        debrief_discloses_ai  = sample(c("Yes","No","Not Applicable","Not Reported"),
                                       120, TRUE, prob=c(.3,.1,.4,.2)),
        debrief_discloses_goal= sample(c("Yes","No","Not Applicable","Not Reported"),
                                       120, TRUE, prob=c(.25,.15,.4,.2))
      )
    }
  )
  
  list(studies = studies, study_countries = study_countries, interventions = interventions)
}

dat                <- load_data()
studies_raw        <- dat$studies
study_countries_raw <- dat$study_countries   # ← new: one row per study × country
interventions_raw  <- dat$interventions

# ============================================================
#  HELPERS
# ============================================================

# Simple bar chart — no inner title
bar_chart <- function(df, col, flip = FALSE) {
  counts <- df |>
    count(.data[[col]], name = "n") |>
    filter(!is.na(.data[[col]])) |>
    mutate(label = as.character(.data[[col]]))
  
  colours <- make_colours(counts$label)
  
  p <- counts |>
    ggplot(aes(
      x    = if (flip) reorder(label, n) else label,
      y    = n,
      fill = label,
      text = paste0(label, ": ", n)
    )) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = colours) +
    scale_y_continuous(expand = expansion(mult = c(0, .12))) +
    labs(x = NULL, y = "Count") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title  = element_blank(),
      axis.text.x = element_text(angle = if (!flip) 30 else 0,
                                 hjust = 1, size = 8)
    )
  
  if (flip) p <- p + coord_flip()
  ggplotly(p, tooltip = "text") |> layout(showlegend = FALSE)
}

# Stacked column chart by domain — no inner title
domain_stacked_chart <- function(df, fill_col) {
  dat <- df |>
    count(Domain, .data[[fill_col]], name = "n") |>
    filter(!is.na(Domain), !is.na(.data[[fill_col]])) |>
    rename(fill_val = .data[[fill_col]])
  
  colours <- make_colours(dat$fill_val)
  
  lvl_order <- c(
    intersect(names(CAT_COLOURS), unique(dat$fill_val)),
    setdiff(unique(dat$fill_val), names(CAT_COLOURS))
  )
  dat <- mutate(dat, fill_val = factor(fill_val, levels = lvl_order))
  
  p <- dat |>
    ggplot(aes(
      x    = Domain,
      y    = n,
      fill = fill_val,
      text = paste0(Domain, " / ", fill_val, ": ", n)
    )) +
    geom_col(position = "stack") +
    scale_fill_manual(values = colours, name = NULL, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(x = NULL, y = "N interventions") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_blank(),
      axis.text.x     = element_text(angle = 25, hjust = 1, size = 9),
      legend.position = "bottom",
      legend.text     = element_text(size = 8)
    )
  
  ggplotly(p, tooltip = "text") |>
    layout(
      legend = list(
        orientation = "h",
        x           = 0,
        xanchor     = "left",
        y           = -0.4,
        yanchor     = "top"
      ),
      margin = list(b = 80)
    )
}

# ============================================================
#  CUSTOM CSS — dark blue active/hover nav tabs
# ============================================================

nav_css <- "
  .navbar {
    background-color: #60b7fc !important;
  }
  /* Active tab text */
  .navbar-nav .nav-link.active,
  .navbar-nav .nav-item.active .nav-link {
    color: #071d36 !important;
    font-weight: 600;
  }
  /* Hover tab text */
  .navbar-nav .nav-link:hover {
    color: #1565C0 !important;
  }
"

# ============================================================
#  UI
# ============================================================

ui <- page_navbar(
  title = tags$span(
    tags$img(
      src    = "transparency-icon.png",
      height = "28px",
      style  = "margin-right:8px; vertical-align:middle;"
    ),
    "AI Persuasion Studies — EDA"
  ),
  theme    = bs_theme(bootswatch = "flatly", primary = "#2196F3", font_scale = 0.9),
  fillable = FALSE,
  header   = tags$head(tags$style(HTML(nav_css))),
  
  sidebar = sidebar(
    width = 240,
    title = "Filters",
    # Choices drawn from the normalised study_countries table
    selectizeInput("filter_country", "Country",
                   choices  = sort(unique(study_countries_raw$country)),
                   selected = NULL, multiple = TRUE,
                   options  = list(placeholder = "All countries")),
    selectizeInput("filter_pub_type", "Publication type",
                   choices  = sort(unique(studies_raw$publication_type)),
                   selected = NULL, multiple = TRUE,
                   options  = list(placeholder = "All types")),
    selectizeInput("filter_domain", "Intervention domain",
                   choices  = sort(unique(interventions_raw$Domain)),
                   selected = NULL, multiple = TRUE,
                   options  = list(placeholder = "All domains")),
    hr(),
    actionButton("reset_filters", "Reset filters",
                 class = "btn-outline-secondary btn-sm w-100"),
    hr(),
    p(strong("Summary"), style = "font-size:0.85rem;"),
    uiOutput("sidebar_summary")
  ),
  
  # ── PAGE 1: Studies Description ───────────────────────────
  nav_panel(
    "Studies Description", icon = icon("globe"),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Geographic distribution of studies"),
           leafletOutput("map", height = 340)),
      card(card_header("Publication type (N studies)"),
           plotlyOutput("pub_type_chart", height = 300))
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Ethical approval reported (N studies)"),
           plotlyOutput("ethical_chart", height = 260)),
      card(card_header("Has experimental design (N studies)"),
           plotlyOutput("exp_design_chart", height = 260)),
      card(card_header("Online recruiting platform sample (N studies)"),
           plotlyOutput("online_plat_chart", height = 260))
    )
  ),
  
  # ── PAGE 2: Prompts & Instructions ────────────────────────
  nav_panel(
    "Prompts & Instructions", icon = icon("comment-dots"),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Total interventions",
                value = textOutput("n_interventions", inline = TRUE),
                showcase = icon("flask"),     theme = "primary"),
      value_box(title = "Total studies",
                value = textOutput("n_studies_vb", inline = TRUE),
                showcase = icon("book-open"), theme = "info"),
      value_box(title = "Domains covered",
                value = textOutput("n_domains", inline = TRUE),
                showcase = icon("tags"),      theme = "success")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Is the persuasion interactive? (N interventions)"),
           plotlyOutput("interactive_chart",  height = 260)),
      card(card_header("Participants informed they interact with AI? (N interventions)"),
           plotlyOutput("disclosure_chart",   height = 260))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Prompt available? (N interventions)"),
           plotlyOutput("prompt_avail_chart", height = 260)),
      card(card_header("Prompt states persuasion goal? (N interventions)"),
           plotlyOutput("prompt_goal_chart",  height = 260))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Prompt hides persuasion goal from participant? (N interventions)"),
           plotlyOutput("prompt_hide_chart",  height = 260)),
      card(card_header("Participant instructions available? (N interventions)"),
           plotlyOutput("instr_avail_chart",  height = 260))
    ),
    card(card_header("Instructions reveal persuasive intent? (N interventions)"),
         plotlyOutput("instr_reveal_chart", height = 260)),
  ),
  
  # ── PAGE 3: Debriefing & LLMs ─────────────────────────────
  nav_panel(
    "Debriefing & LLMs", icon = icon("robot"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Debriefing procedure reported? (N interventions)"),
           plotlyOutput("debrief_chart",      height = 280)),
      card(card_header("Debriefing discloses AI involvement? (N interventions)"),
           plotlyOutput("debrief_ai_chart",   height = 280)),
      card(card_header("Debriefing discloses persuasive intent? (N interventions)"),
           plotlyOutput("debrief_goal_chart", height = 280))
    ),
    card(card_header("Number of interventions by LLM (N interventions)"),
         plotlyOutput("llm_treemap", height = 380))
  ),
  
  # ── PAGE 4: By Domain ─────────────────────────────────────
  nav_panel(
    "By Domain", icon = icon("layer-group"),
    
    h5("Prompts & Instructions — by domain",
       style = "margin: 8px 16px 4px; font-weight:600;"),
    
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Is the persuasion interactive? — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_interactive",     height = 350)),
      card(card_header("AI disclosure to participants — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_disclosure",      height = 350))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Prompt available? — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_prompt_avail",    height = 350)),
      card(card_header("Prompt states persuasion goal? — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_prompt_goal",     height = 350))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Prompt hides persuasion goal? — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_prompt_hide",     height = 350)),
      card(card_header("Participant instructions available? — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_instr_avail",     height = 350))
    ),
    card(card_header("Instructions reveal persuasive intent? — by domain (N intervention-domain pairs)"),
         plotlyOutput("d_instr_reveal",  height = 350)),
    
    h5("Debriefing — by domain",
       style = "margin: 16px 16px 4px; font-weight:600;"),
    
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Debriefing reported? — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_debrief",         height = 350)),
      card(card_header("Debriefing discloses AI? — by domain (N intervention-domain pairs)"),
           plotlyOutput("d_debrief_ai",      height = 350))
    ),
    card(card_header("Debriefing discloses persuasive intent? — by domain (N intervention-domain pairs)"),
         plotlyOutput("d_debrief_goal",    height = 350))
  )
)

# ============================================================
#  SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ── Reset ─────────────────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "filter_country",  selected = character(0))
    updateSelectizeInput(session, "filter_pub_type", selected = character(0))
    updateSelectizeInput(session, "filter_domain",   selected = character(0))
  })
  
  # ── Filtered study IDs via study_countries ─────────────────
  #   When a country filter is active we find matching study_ids via
  #   study_countries_raw, then pass those IDs down to all other reactives.
  filtered_study_ids <- reactive({
    if (length(input$filter_country) > 0) {
      study_countries_raw |>
        filter(country %in% input$filter_country) |>
        pull(study_id) |>
        unique()
    } else {
      unique(studies_raw$study_id)
    }
  })
  
  # ── Filtered studies ───────────────────────────────────────
  filtered_studies <- reactive({
    df <- studies_raw |> filter(study_id %in% filtered_study_ids())
    if (length(input$filter_pub_type) > 0)
      df <- filter(df, publication_type %in% input$filter_pub_type)
    df
  })
  
  # ── Filtered study_countries (for the map) ─────────────────
  #   Respects all active filters so the map updates in sync.
  filtered_study_countries <- reactive({
    study_countries_raw |>
      filter(study_id %in% filtered_studies()$study_id)
  })
  
  # ── Filtered interventions ─────────────────────────────────
  filtered_interventions <- reactive({
    df <- interventions_raw |>
      filter(study_id %in% filtered_studies()$study_id)
    if (length(input$filter_domain) > 0)
      df <- filter(df, Domain %in% input$filter_domain)
    df
  })
  
  interventions_unique <- reactive({
    filtered_interventions() |> distinct(intervention_id, .keep_all = TRUE)
  })
  
  interventions_by_domain <- reactive({
    filtered_interventions() |>
      distinct(intervention_id, Domain, .keep_all = TRUE)
  })
  
  # ── Sidebar summary ───────────────────────────────────────
  output$sidebar_summary <- renderUI({
    tagList(
      tags$small(strong(nrow(filtered_studies())), " studies selected"), br(),
      tags$small(strong(n_distinct(filtered_interventions()$intervention_id)),
                 " interventions selected")
    )
  })
  
  # ── PAGE 1 ────────────────────────────────────────────────
  
  output$map <- renderLeaflet({
    # Aggregate at the country level from the normalised table so that a study
    # with two countries shows up as a marker in each of them.
    counts <- filtered_study_countries() |>
      filter(!is.na(lat), !is.na(lon)) |>
      group_by(country, lat, lon) |>
      summarise(n = n_distinct(study_id), .groups = "drop")
    
    leaflet(counts) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(
        lng         = ~lon,
        lat         = ~lat,
        radius      = ~pmax(6, sqrt(n) * 4),
        color       = "#2196F3",
        fillOpacity = 0.7,
        stroke      = FALSE,
        label       = ~paste0(country, ": ", n, " study/studies")
      )
  })
  
  output$pub_type_chart    <- renderPlotly(bar_chart(filtered_studies(), "publication_type",        flip = TRUE))
  output$ethical_chart     <- renderPlotly(bar_chart(filtered_studies(), "ethical_approval"))
  output$exp_design_chart  <- renderPlotly(bar_chart(filtered_studies(), "experimental_design"))
  output$online_plat_chart <- renderPlotly(bar_chart(filtered_studies(), "online_recruit_platform", flip = TRUE))
  
  # ── PAGE 2 ────────────────────────────────────────────────
  
  output$n_interventions <- renderText(n_distinct(filtered_interventions()$intervention_id))
  output$n_studies_vb <- renderText({
  n_distinct(filtered_interventions()$study_id)
})
  output$n_domains       <- renderText(n_distinct(filtered_interventions()$Domain, na.rm = TRUE))
  
  output$interactive_chart  <- renderPlotly(bar_chart(interventions_unique(), "is_interactive"))
  output$disclosure_chart   <- renderPlotly(bar_chart(interventions_unique(), "ai_disclosed"))
  output$prompt_avail_chart <- renderPlotly(bar_chart(interventions_unique(), "prompt_available"))
  output$prompt_goal_chart  <- renderPlotly(bar_chart(interventions_unique(), "prompt_states_goal"))
  output$prompt_hide_chart  <- renderPlotly(bar_chart(interventions_unique(), "prompt_hides_goal"))
  output$instr_avail_chart  <- renderPlotly(bar_chart(interventions_unique(), "instructions_avail"))
  output$instr_reveal_chart <- renderPlotly(bar_chart(interventions_unique(), "instructions_reveal", flip = TRUE))
  
  # ── PAGE 3 ────────────────────────────────────────────────
  
  output$debrief_chart      <- renderPlotly(bar_chart(interventions_unique(), "debriefing_reported"))
  output$debrief_ai_chart   <- renderPlotly(bar_chart(interventions_unique(), "debrief_discloses_ai"))
  output$debrief_goal_chart <- renderPlotly(bar_chart(interventions_unique(), "debrief_discloses_goal"))
  
  output$llm_treemap <- renderPlotly({
    df <- filtered_interventions() |>
      count(LLM, name = "n") |>
      filter(!is.na(LLM), LLM != "")
    plot_ly(df, type = "treemap", labels = ~LLM, parents = "", values = ~n,
            textinfo = "label+value+percent root",
            marker = list(colors = FALLBACK_PAL)) |>
      layout(margin = list(t = 10, b = 10))
  })
  
  # ── PAGE 4: By Domain ─────────────────────────────────────
  
  output$d_interactive   <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "is_interactive"))
  output$d_disclosure    <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "ai_disclosed"))
  output$d_prompt_avail  <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "prompt_available"))
  output$d_prompt_goal   <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "prompt_states_goal"))
  output$d_prompt_hide   <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "prompt_hides_goal"))
  output$d_instr_avail   <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "instructions_avail"))
  output$d_instr_reveal  <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "instructions_reveal"))
  output$d_debrief       <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "debriefing_reported"))
  output$d_debrief_ai    <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "debrief_discloses_ai"))
  output$d_debrief_goal  <- renderPlotly(domain_stacked_chart(interventions_by_domain(), "debrief_discloses_goal"))
}

# ============================================================
#  LAUNCH
# ============================================================

shinyApp(ui, server)