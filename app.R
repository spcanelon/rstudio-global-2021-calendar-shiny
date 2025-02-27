# load libraries
library(shiny)
library(bslib)
library(lubridate)
library(calendar)
library(reactable)
library(glue)
# source
source("R/data_cleaning.R")
source("R/stack.R")
# source("R/tz.R")
source("R/cards.R")

Sys.setenv(TZ = "UTC")

# define Bootstrap theme
theme <- bs_theme(
  version = 4, #TODO update to 5
  bootswatch = "lux",
  # bg = "#F4F3EE",
  # fg = "#0F0D0A",
  # primary = "#447099",
  # secondary = "#75AADB",
  # success = "#A4C689",
  # warning = "#fdbe4b",
  primary = "#4f3d63",
  secondary = "#6aae8a",
  success = "#113421",
  warning = "#bf7836",
  danger = "#666666",
  # info = "#CBD4DD",
  info = "#1f78b4",
  "font-size-base" = "1rem",
  base_font = font_collection(font_google("Atkinson Hyperlegible", local = FALSE), "Nunito", "sans-serif"),
  code_font = font_google("Atkinson Hyperlegible Mono", local = FALSE)
)

# session_type
# "Hands-on"
# "Panel"
# "Demo"
# "Pre-registration - Hands-on"
# "Commons"
# "Networking"
# "Special"

# import and wrangle schedule data
# schedule <- readr::read_csv("data/schedule_example.csv")
schedule <- readr::read_csv("data/schedule.csv") |> dplyr::arrange(start_datetime)
# create unique event ID
schedule$id <- seq_len(nrow(schedule))
# fix typo in original date year
# year(schedule$time_gmt) <- 2021
# create line break between speakers
# schedule$name <- gsub("\n", ", ", schedule$name)
# schedule$speakers <- gsub(schedule$speakers, pattern = ", ", replacement = "\n")
schedule$speakers <- gsub(schedule$speakers, pattern = ", ", replacement = "<br>")


ui <- navbarPage(
  # 'rstudio::global("schedule")',
  'NICAR 2025 Calendar',
  theme = theme,
  collapsible = TRUE,
  tabPanel(
    title = "Schedule",
    id = "schedule",
    # skip link
    a(
      "If you're using a screen reader, you may find the official ",
      # "RStudio Global conference website is better suited. Do you want to go there now?",
      "NICAR 2025 conference website is better suited. Do you want to go there now?",
      class = "screenreader-text",
      `tab-index` = 1,
      # href = "https://global.rstudio.com/student/all_events"
      href = "https://schedules.ire.org/nicar-2025/"
    ),
    div(
      class = "container-fluid",
      style = "max-width: 1600px",
      div(
        class = "row",
        div(
          class = "col-lg-3 order-1 order-lg-2 sidebar",
          uiOutput("your_talks"),
          # selectInput("tz", "Your Timezone", choices = available_timezones(), selected = "UTC", width = "100%"),
          div(
            class = "row",
            div(
              class = "col-6 col-lg-12",
              textInput("sch_search", "Search", width = "100%"),
              # radioButtons("sch_day", "Day",
              #              c("First" = "one", "Second" = "two", "All" = "all"),
              #              inline = TRUE, selected = c("all"), width = "100%"),
              radioButtons("sch_day", "Day",
                           c("Thursday" = "one", "Friday" = "two", "Saturday" = "three", "Sunday" = "four", "All" = "all"),
                           inline = TRUE, selected = c("all"), width = "100%"),
              # sliderInput("sch_hours", "Hours in Your Time Zone", value = c(0, 24),
              #             min = 0, max = 24, step = 1, post = ":00", width = "100%")
              sliderInput("sch_hours", "Hours", value = c(7, 17),
                          min = 0, max = 24, step = 1, post = ":00", width = "100%")
              # https://stackoverflow.com/a/72646088
              # {
              # # customTicks <- seq(from = 10, to = 34)
              # a <- c(12, seq(1,12), seq(1,11)) |> as_tibble()
              # customTicks <- a |> mutate(value2 = if_else(value == 12 & row_number() != 1, "12 PM", as.character(value))) |> pull()
              # customSlider <- sliderInput(
              #   inputId = "sch_hours",
              #   label = "Hours",
              #   min = 1,
              #   max = 24,
              #   value = c(8,16),
              #   step = 12,
              #   ticks = FALSE,
              #   round = TRUE
              # )
              # htmltools::tagQuery(customSlider)$find("input")$addAttrs("data-values" = paste0(customTicks, collapse = ", "))$allTags()
              # }
            ),
            div(
              class = "col-6 col-lg-12",
              selectizeInput(#"sch_presenter", "Presenter",
                # choices = sort(unique(schedule$name)),
                "sch_presenter", "Speakers",
                choices = sort(unique(schedule$speakers)),
                multiple = TRUE, width = "100%"),
              selectizeInput(#"sch_type", "Talk Type",
                             # choices = sort(unique(schedule$type)),
                             "sch_type", "Session Type",
                             choices = sort(unique(schedule$session_type)),
                             multiple = TRUE, width = "100%"),
              selectizeInput(#"sch_topic", "Talk Topic",
                             # choices = sort(unique(schedule$topic)),
                             "sch_topic", "Skill Level",
                             choices = sort(unique(schedule$skill_level)),
                             multiple = TRUE, width = "100%")
            )
          )
        ),
        div(
          class = "col-lg-9 order-2 order-lg-1",
          reactable::reactableOutput("schedule"),
          helpText(
            class = "mt-3",
            tags$a(
              # href = "https://global.rstudio.com",
              # code("rstudio::global")
              href = "https://www.ire.org/training/conferences/nicar-2025/",
              code("NICAR 2025")
            ),
            # "will be held Thursday 2021-01-21 and Friday 2021-01-22."
            "will be held Thursday 2025-03-06 to Sunday 2025-03-09."
          ),
          htmltools::htmlDependency(
            # name = "rstudio-global-calendar",
            name = "nicar-2025-calendar",
            version = "0.0.1",
            src = "www",
            script = "extra.js",
            stylesheet = "extra.css"
          )
        )
      )
    )
  ),
  # About tab
  tabPanel(
    title = "About",
    id = "about",
    div(
      class = "container-fluid",
      style = "max-width: 900px",
      h2(class = "text-monospace",
         HTML("Hi there!")
      ),
      p(HTML("My name is Silvia Canelón, and this is my first time attending NICAR!<br>If you see me around, please say hello. &#xFE0F;👋")
        ),
      p("You can also get in touch on",
        tags$a(href = "https://bsky.app/profile/silviacanelon.com", "Bluesky", .noWS = "after"),
        " or through",
        tags$a(href = "https://silviacanelon.com/contact", "my website.", .noWS = "after")
        ),
      h2(
        class = "text-monospace",
        "NICAR 2025"
      ),
      strong(HTML("March 6-9, 2025 &mdash; Minneapolis, MN<br><br>")),
      strong("From",
             tags$a(href = "https://www.ire.org/training/conferences/", "Investigative Reporters & Editors (IRE):", .noWS = "after")),
      p(
        HTML("<blockquote>",
        "Join us for our annual data journalism conference where you'll",
        "have access to trainings on some of the most powerful tools a journalist",
        "can have in their kit. Sessions will also feature the most tried and tested",
        "data analysis techniques and investigative skills out there.",
        "</blockquote>")
        # "Our goal is to make rstudio::global(2021) our most inclusive and",
        # "global event, making the most of the freedom from geographical and",
        # "economic constraints that comes with an online event. That means that",
        # "the conference will be free, designed around participation from every",
        # "time zone, and have speakers from around the world."
      ),
      p(
        a(
          "Register Now",
          href = "https://www.ire.org/training/conferences/nicar-2025/nicar25-registration/",
          class = "btn btn-primary"
        ),
        a(
          tags$a(
            href = "https://schedules.ire.org/nicar-2025/",
            class = "btn btn-success",
            "Official Schedule"
          )
        )
      ),
      tags$hr(class = "my-4"),
      h2("About this app", class = "text-monospace"),
      p(
        HTML("This app was built with &#x2665; by"),
        tags$a(href = "https://silviacanelon.com", "Silvia Canelón", .noWS = "after"),
        " using the packages below, and adapted from an app created by",
        tags$a(href = "https://github.com/gadenbuie/rstudio-global-2021-calendar", "Garrick Aden-Buie."),
        HTML("You can find"),
        tags$a(href = "https://github.com/spcanelon/nicar-2025-calendar", "the full source code"),
        "on GitHub."
      ),
      # p("This app was adapted from one created by",
      #   tags$a(href = "https://github.com/gadenbuie/rstudio-global-2021-calendar", "Garrick Aden-Buie"),
      #   "for RStudio Conf in 2021"),
      div(
        class = "d-flex flex-wrap align-items-stretch justify-content-between",
        card(
          "shiny",
          posit_hex("shiny"),
          "https://shiny.rstudio.com",
          "Shiny is an R package that makes it easy to build interactive web apps straight from R."
        ),
        card(
          "renv",
          posit_hex("renv"),
          "https://rstudio.github.io/renv",
          "The renv package helps you create reproducible environments for your R projects. Use renv to make your R projects more: isolated, portable, and reproducible."
        ),
        card(
          "bslib",
          posit_hex("bslib"),
          "https://rstudio.github.io/bslib/",
          "Tools for creating custom Bootstrap themes, making it easier to style Shiny apps & R Markdown documents directly from R without writing unruly CSS and HTML."
        ),
        card(
          "R6",
          posit_hex("R6"),
          "https://r6.r-lib.org/",
          "Encapsulated object-oriented programming for R."
        ),
        card(
          "glue",
          posit_hex("glue"),
          "https://glue.tidyverse.org",
          "Glue strings to data in R. Small, fast, dependency free interpreted string literals."
        ),
        card(
          "lubridate",
          posit_hex("lubridate"),
          "https://lubridate.tidyverse.org",
          "Make working with dates in R just that little bit easier."
        ),
        card(
          "calendar",
          NULL,
          "https://github.com/ATFutures/calendar",
          "Create, read, write, and work with iCalendar (.ics, .ical or similar) files in R."
        ),
        card(
          "reactable",
          NULL,
          "https://glin.github.io/reactable/index.html",
          "Interactive data tables for R, based on the React Table library and made with reactR."
        ),
        card(
          "prettyunits",
          NULL,
          "https://github.com/r-lib/prettyunits",
          "Pretty, human readable formatting of quantities."
        ),
      )
    )
  )
)


server <- function(input, output, session) {
  selected_talks <- Stack$new()
  selected_in_current_view <- reactiveVal()

  # select conference day
  schedule_view <- reactive({
    if (isTruthy(input$sch_day)) {
      if (input$sch_day == "one") {
        schedule <- schedule[
          # schedule$time_gmt < ymd_hms("2021-01-22 04:00:00", tz = "UTC"),
          schedule$day == "Thursday",
        ]
      } else if (input$sch_day == "two") {
        schedule <- schedule[
          # schedule$time_gmt >= ymd_hms("2021-01-22 04:00:00", tz = "UTC"),
          schedule$day == "Friday",
        ]
      } else if (input$sch_day == "three") {
        schedule <- schedule[
          # schedule$time_gmt >= ymd_hms("2021-01-22 04:00:00", tz = "UTC"),
          schedule$day == "Saturday",
        ]
      } else if (input$sch_day == "four") {
        schedule <- schedule[
          # schedule$time_gmt >= ymd_hms("2021-01-22 04:00:00", tz = "UTC"),
          schedule$day == "Sunday",
        ]
      }
    }
    # select Your Timezone
    # schedule$time <- with_tz(schedule$time_gmt, input$tz)
    schedule$time <- with_tz(schedule$start_datetime, tzone = "US/Central")
    if (isTruthy(input$sch_hours)) {
      schedule <- schedule[
        # hour(schedule$time) >= input$sch_hours[1] & hour(schedule$time) <= input$sch_hours[2],
        hour(schedule$time) >= input$sch_hours[1] & hour(schedule$time) <= input$sch_hours[2],
      ]
    }
    # use keyword Search
    if (shiny::isTruthy(input$sch_search)) {
      schedule <- schedule[
        # grepl(input$sch_search, tolower(paste(schedule$title_text, schedule$abstract_text))),
        grepl(input$sch_search, tolower(paste(schedule$session_title, schedule$session_description, schedule$speakers))),
      ]
    }
    # select/search Presenter
    if (isTruthy(input$sch_presenter)) {
      # schedule <- schedule[schedule$name %in% input$sch_presenter, ]
      schedule <- schedule[schedule$speakers %in% input$sch_presenter, ]
    }
    # select/search Talk Type
    if (isTruthy(input$sch_type)) {
      # schedule <- schedule[schedule$type %in% input$sch_type, ]
      schedule <- schedule[schedule$session_type %in% input$sch_type, ]
    }
    # select/search Talk Topic
    if (isTruthy(input$sch_topic)) {
      # schedule <- schedule[schedule$topic %in% input$sch_topic, ]
      schedule <- schedule[schedule$skill_level %in% input$sch_topic, ]
    }
    # schedule$info <- schedule$talk_id
    schedule$info <- schedule$session_id
    # common_vars <- c(
    #   "id", "info", "talk_id", "type", "title_text", "name", "time",
    #   "duration", "track", "topic", "url"
    # )
    common_vars <- c(
      "id", "info", "session_id", "session_type", "session_title",
      "start_datetime", "duration_formatted", "skill_level", "location", "url"
    )
    schedule <- schedule[, common_vars]
    # schedule[, common_vars] |> View()
    schedule
  })

  selected_by_user_current_view <- reactive(getReactableState("schedule", "selected"))

  observeEvent(selected_by_user_current_view(), {
    current <- selected_talks$stack()
    on.exit(ignore_schedule_change(FALSE))
    if (!is.null(current) && is.null(selected_by_user_current_view()) && ignore_schedule_change()) {
      return()
    }
    in_view <- intersect(current, schedule_view()$id)

    if (is.null(selected_by_user_current_view()) && length(in_view)) {
      selected_talks$remove(in_view)
      return()
    }

    selected <- schedule_view()$id[selected_by_user_current_view()]

    talks_to_add <- setdiff(selected, current)
    talks_to_drop <- setdiff(in_view, selected)

    if (length(talks_to_add)) {
      selected_talks$add(talks_to_add)
    }
    if (length(talks_to_drop)) {
      selected_talks$remove(talks_to_drop)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$your_talks <- renderUI({
    req(selected_talks$stack())
    tagList(
      downloadButton(
        "download_calendar",
        class = "d-block mb-3 btn-primary",
        glue(
          "Download Calendar ({n} talk{s})",
          n = length(selected_talks$stack()),
          s = if (length(selected_talks$stack()) == 1) "" else "s"
        )
      ),
      p(class = "text-right", actionLink("reset", "Reset Selection"))
    )
  })

  output$download_calendar <- downloadHandler(
    # filename = "rstudio-global-talks.ics",
    filename = "nicar-2025-sessions.ics",
    content = function(file) {
      talks <- schedule[schedule$id %in% selected_talks$stack(), ]
      # talks$start_time <- with_tz(talks$time_gmt, tzone = input$tz)
      # talks$end_time <- talks$start_time + seconds(talks$duration)
      talks$start_time <- with_tz(talks$start_datetime, tzone = "US/Central")
      talks$end_time <- with_tz(talks$end_datetime, tzone = "US/Central")
      talk_events <- lapply(seq_len(nrow(talks)), function(idx) {
        # desc <- paste0("Presenter: ", talks$name[[idx]], "\n\n", talks$abstract_text[[idx]])
        desc <- paste0("Speakers: ", talks$speakers[[idx]], "\n\n", talks$session_description[[idx]])
        desc <- gsub("\n", "\\n", desc, fixed = TRUE)
        desc <- strwrap(desc, 75)
        desc <- paste(desc, collapse = " \n ")
        desc <- gsub(",", "\\,", desc)
        ev <- calendar::ic_event(
          start_time = talks$start_time[[idx]],
          end_time = talks$end_time[[idx]],
          # summary = talks$title_text[[idx]],
          summary = talks$session_title[[idx]],
          more_properties = TRUE,
          event_properties = c(
            DESCRIPTION = desc,
            URL = talks$url[[idx]],
            LOCATION = talks$location[[idx]]
          )
        )
        ev
      })
      calendar::ic_write(do.call(rbind, talk_events), file)
    }
  )

  observeEvent(input$reset, {
    selected_talks$update(NULL)
    reactable::updateReactable(
      "schedule",
      selected = NA
    )
  })

  ignore_schedule_change <- reactiveVal(FALSE)

  output$schedule <- reactable::renderReactable({
    ignore_schedule_change(TRUE)
    reactable(
      schedule_view(),
      selection = "multiple",
      defaultSelected = which(schedule_view()$id %in% isolate(selected_talks$stack())),
      highlight = TRUE,
      borderless = TRUE,
      columns = list(
        # talk_id = colDef(show = FALSE),
        session_id = colDef(show = FALSE),
        id = colDef(show = FALSE),
        url = colDef(show = FALSE),
        # time = colDef(
        start_datetime = colDef(
          name = "Time",
          minWidth = 100,
          html = TRUE,
          cell = function(value) {
            strftime(
              value,
              # format = '<span class="white-space:pre;">%a</span> %H:%M',
              # tz = input$tz
              format = '<span class="white-space:pre;">%a</span> %I:%M %p',
              tz = "US/Central"
            )
          }
        ),
        # duration = colDef(
        duration_formatted = colDef(
          name = "Length",
          minWidth = 75,
          # cell = function(value, index) prettyunits::pretty_sec((value %/% 60) * 60)
        ),
        # type = colDef(
        session_type = colDef(
          name = "Type",
          html = TRUE,
          align = "left",
          cell = function(value) {
            value <- paste(value)
            glue(
              '<span class="badge badge-pill badge-{type}">{value}</span>',
              type = switch(
                value,
                # keynote = "primary",
                # lightning = "warning",
                # talk = "success",
                Networking = "info",
                Panel = "success",
                Demo = "secondary",
                Special = "danger",
                Commons = "success",
                "Hands-on" = "warning",
                "light"
              ),
              value = paste0(toupper(substr(value, 1, 1)), substr(value, 2, nchar(value)))
            )
          }
        ),
        # track = colDef(
        location = colDef(
          name = "Location",
          html = TRUE,
          minWidth = 120,
          align = "right"
        ),
        # topic = colDef(name = "Topic", minWidth = 100, align = "center"),
        skill_level = colDef(
          name = "Skill Level",
          html = TRUE,
          minWidth = 120,
          align = "center",
          cell = function(value) {
            if (!is.na(value)) {
              glue(
                '<span class="badge badge-pill badge-{type}">{value}</span>',
                type = switch(
                  paste(value),
                  Beginner = "info",
                  Intermediate = "secondary",
                  Advanced = "success",
                  "light"
                )
              )
            }
          }
        ),
        # name = colDef(name = "Presenter", minWidth = 200),
        # speakers = colDef(name = "Presenter", minWidth = 200),
        # title_text = colDef(
        session_title = colDef(
          name = "Title",
          minWidth = 150,
          html = TRUE,
          cell = JS("
            function(cellInfo) {
              var url = cellInfo.row['url']
              return url ?
                '<a href=\"' + url + '\" target=\"_blank\" title=\"Go to Official Talk Page\">' + cellInfo.value + '<a>' :
                cellInfo.value
            }
          ")
        ),
        # session_description = colDef(
        #   name = "Description",
        #   minWidth = 510,
        #   html = TRUE
        # ),
        # speakers = colDef(
        #   name = "Speakers",
        #   minWidth = 250,
        #   html = TRUE
        # ),
        info = colDef(
          name = "",
          html = TRUE,
          # minWidth = 60,
          minWidth = 50,
          sortable = FALSE,
          class = "cell-info-button",
          cell = function(value) {
            if (!isTruthy(value)) return()
            tags$button(
              class = "btn btn-light btn-talk-more-info",
              `data-value` = value,
              title = "More info...",
              # icon("info")
              icon("info-circle")
            )
          },
          style = list(
            position = "sticky",
            left = 30,
            background = "#fff",
            zIndex = 1,
            borderRight = "2px solid #eee"
          ),
          headerStyle = list(
            position = "sticky",
            left = 30,
            background = "#fff",
            zIndex = 1,
            borderRight = "2px solid #eee"
          )
        ),
        .selection = colDef(
          width = 30,
          style = list(
            cursor = "pointer",
            position = "sticky",
            left = 0,
            background = "#fff",
            zIndex = 1
          ),
          headerStyle = list(
            cursor = "pointer",
            position = "sticky",
            left = 0,
            background = "#fff",
            zIndex = 1
          )
        )
      )
    )
  })

  observeEvent(input$talk_more_info, {
    # talk <- schedule[!is.na(schedule$talk_id) & schedule$talk_id == as.numeric(input$talk_more_info), ]
    talk <- schedule[!is.na(schedule$session_id) & schedule$session_id == as.numeric(input$talk_more_info), ]
    req(nrow(talk))

    # speaker_names <- strsplit(talk$name[[1]], ", ")[[1]]
    speaker_names <- talk$speakers[[1]]
    # speaker_bios <- strsplit(talk$bio_html[[1]], "\n</p>\n<p>")[[1]]
    # if (length(speaker_bios) == 2) {
    #   speaker_bios[1] <- paste0(speaker_bios[1], "</p>")
    #   speaker_bios[2] <- paste0("<p>", speaker_bios[2])
    # }


    # html_speaker_bio <- function(idx) {
    #   # spkr_name <- speaker_names[idx]
    #   spkr_name <- talk$speakers[idx]
    #   # spkr_bio <- speaker_bios[idx]
    #   spkr_img <- tolower(gsub("[ '-]", "", spkr_name))
    #   spkr_img <- if (file.exists(file.path("www", "speakers", paste0(spkr_img, ".png")))) {
    #     file.path("speakers", paste0(spkr_img, ".png"))
    #   } else if (file.exists(file.path("www", "speakers", paste0(spkr_img, ".jpg")))) {
    #     file.path("speakers", paste0(spkr_img, ".jpg"))
    #   }
    #   tagList(
    #     h2(spkr_name),
    #     if (!is.null(spkr_img)) {
    #       div(
    #         class = "row",
    #         div(
    #           class = "col-sm-3 order-1 order-sm-2",
    #           tags$img(
    #             src = spkr_img,
    #             style = "max-width: 100%",
    #             class = "rounded-lg"
    #           )
    #         ),
    #         # div(
    #         #   class = "col-sm-9 order-2 order-sm-1",
    #         #   HTML(spkr_bio)
    #         # )
    #       )
    #     } else HTML(spkr_bio)
    #   )
    # }

    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        # title = talk$title_text[[1]],
        title = talk$session_title[[1]],
        # h2("Abstract"),
        # HTML(talk$abstract_html[[1]]),
        h2("Description"),
        HTML(talk$session_description[[1]]),
        h2("Speakers"),
        HTML(talk$speakers[[1]]),
        # lapply(seq_along(speaker_names), html_speaker_bio),
        footer = list(
          tags$a(
            href = talk$url[[1]],
            class = "btn btn-success",
            target = "_blank",
            "Go To Talk Page"
          ),
          modalButton("OK")
        )
      )
    )
  })

  # observeEvent(input$browser_tz, {
  #   if (input$browser_tz %in% OlsonNames()) {
  #     updateSelectInput(session, "tz", selected = input$browser_tz)
  #   }
  # })
}

shinyApp(ui = ui, server = server)
