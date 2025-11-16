# Load libraries

library(ggplot2)
library(readxl)
library(here)
library(dplyr)
library(scales)
library(ggtext)
library(ggrepel)
library(lubridate)
library(purrr)


# Load data and define key variables

load_data <- function(file_path, sheet_name, type = c("boxoffice", "production")) {
  type <- match.arg(type)

  # Read and clean
  df <- read_xlsx(file_path, sheet = sheet_name) %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      month_num = match(tolower(month), tolower(month.name)) # convert month names to numbers
    )

  # Find latest year, quarter, month
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  latest_month_num <- df %>%
    filter(year == latest_year, quarter == latest_quarter) %>%
    summarise(max_month = max(month_num, na.rm = TRUE)) %>%
    pull(max_month)
  latest_month <- month.name[latest_month_num]

  # Label functions
  make_boxoffice_label <- function(year, latest_month_num) {
    period <- case_when(
      latest_month_num == 3  ~ "Q1",
      latest_month_num == 6  ~ "H1",
      latest_month_num == 9  ~ "Q1–3",
      latest_month_num == 12 ~ "FY",
      TRUE ~ NA_character_
    )
    paste0(period, " ", year)
  }

  make_production_label <- function(year, latest_month_num) {
    paste0("YE ", month.abb[latest_month_num], " ", year)
  }

  # Add labels into dataframe
  df <- df %>%
    mutate(
      label = case_when(
        type == "boxoffice"  ~ make_boxoffice_label(year, latest_month_num),
        type == "production" ~ make_production_label(year, latest_month_num)
      )
    )
  
  latest_label <- tail(df$label, 1)  # last row's label
  latest_period <- sub(" .*", "", latest_label) 

  # Return everything
  return(list(
    data = df,
    latest_year = latest_year,
    latest_quarter = latest_quarter,
    latest_month = latest_month,
    latest_month_num = latest_month_num,
    latest_label = latest_label,
    latest_period = latest_period
  ))
}


# Plots

# Box office

uk_box_office <- function() {
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    ungroup() %>%
    # Order labels chronologically using year + month_num
    mutate(label = factor(label, levels = label[order(year, month_num)]))

  ggplot(df, aes(x = label, y = uk_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +
    geom_text(aes(label = scales::comma(round(uk_box_office_m, 0))), 
              vjust = 1.5, color = 'white') +
    labs(
      title = paste0('UK box office, January to ', data_and_vars$latest_month, ' (', 
                      data_and_vars$latest_period, '), £ million'),
      subtitle = 'All titles on release, including event titles',
      x = '',
      y = ''
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal()
}


uk_roi_box_office <- function() {
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    ungroup() %>%
    # Order labels chronologically using year + month_num
    mutate(label = factor(label, levels = label[order(year, month_num)]))
  
  # Plot
  ggplot(df, aes(x = label, y = uk_roi_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#783df6') +  
    geom_text(aes(label = scales::comma(uk_roi_box_office_m)), vjust = 1.5, color = 'white') + 
    labs(
      title = paste0('UK and Republic of Ireland box office, January to ', data_and_vars$latest_month, ' (', 
                      data_and_vars$latest_period, '), £ million'),
      subtitle = "<span style='color:#783df6'>All films</span> released in calendar year, excluding event titles",
      x = '', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


## NEEDS UPDATING
uk_market_share <- function() {
  df$year <- as.numeric(as.character(df$year))

  ggplot(df, aes(x = factor(year), y = box_office_m)) +
    # First layer: all films
    geom_bar(aes(fill = ifelse(film_type == "all_titles", "all_titles", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Second layer: all UK qualifying films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "all_uk_qualifying", "all_uk_qualifying", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Text labels for relevant bars 
    geom_text(data = df_filtered %>% filter(film_type == "all_uk_qualifying"), 
              aes(label = scales::comma(round(box_office_m, 0)), vjust = 1.5),
              color = 'white') +
    labs(
      title = 'UK and Republic of Ireland box office, £ million',
      subtitle = "For <span style='color:#1197FF'>**all UK qualifying films**</span> 
                  released in calendar year, excluding event titles",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'all_uk_qualifying' = '#1197FF'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}

# NEEDS UPDATING
uk_market_share_indie <- function() {
  df$year <- as.numeric(as.character(df$year))

  ggplot(df, aes(x = factor(year), y = box_office_m)) +
    # First layer: all films
    geom_bar(aes(fill = ifelse(film_type == "all_titles", "all_titles", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Second layer: all UK qualifying films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "all_uk_qualifying", "all_uk_qualifying", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Third layer: uk independent films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "uk_independent", "uk_independent", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Text labels for relevant bars
    geom_text(data = df_filtered %>% filter(film_type == "uk_independent"), 
              aes(label = scales::comma(round(box_office_m, 0)), vjust = -0.5),
              color = '#e50076') +
    labs(
      title = 'UK and Republic of Ireland box office, £ million',
      subtitle = "For <span style='color:#e50076'>**all UK independent films**</span> 
                  released in calendar year, excluding event titles",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'all_uk_qualifying' = 'lightgrey',
                                 'uk_independent' = '#e50076'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


uk_market_share_percent <- function() {
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    ungroup() %>%
    # Order labels chronologically using year + month_num, removing duplicates
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))

  df$film_type <- factor(df$film_type, levels = c("other_uk_qualifying", "uk_independent"))

  ggplot(df, aes(x = label, y = market_share_percent)) +
    # Layer 1: All films (no stacking)
    geom_bar(aes(fill = "all_titles"), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Layer 2: Stacked bars for uk_independent and other_uk_qualifying (stacked on top of all_titles)
    geom_bar(data = df %>% filter(film_type %in% c("other_uk_qualifying", "uk_independent")), 
             aes(fill = film_type), 
             stat = 'identity', position = 'stack', na.rm = TRUE) + 
    # Text labels for relevant bars (only for uk_independent and other_uk_qualifying)
    geom_text(data = df %>% filter(film_type %in% c("other_uk_qualifying", "uk_independent")), 
              aes(label = scales::comma(round(market_share_percent, 0))), 
              position = position_stack(vjust = 0.5),
              color = 'white') +
    labs(
      title = paste0('Share of UK and Republic of Ireland box office, January to ', data_and_vars$latest_month, ' (', 
                      data_and_vars$latest_period, ')'),
      subtitle = "For <span style='color:#e50076'>**all UK independent films**</span> 
                  and <span style='color:#1197FF'>**other UK qualifying films**</span>",
      x = '',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'other_uk_qualifying' = '#1197FF',
                                 'uk_independent' = '#e50076'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


# Admissions

uk_admissions <- function() {
  df <- data_and_vars$data %>%
    filter(quarter <= data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    summarise(
      admissions_m = sum(admissions_m, na.rm = TRUE),
      # get the latest month_num within this label for ordering
      month_num = max(month_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Order labels chronologically
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))
  
  ggplot(df, aes(x = label, y = admissions_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +
    geom_text(aes(label = scales::comma(round(admissions_m, 0))),
              vjust = 1.5, color = 'white') +
    labs(
      title = paste0('UK admissions, January to ', data_and_vars$latest_month, ' (', 
                      data_and_vars$latest_period, '), million'),
      subtitle = 'All titles on release, including event titles',
      x = '',
      y = ''
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal()
}


uk_admissions_month <- function() {
  df <- data_and_vars$data %>%
    mutate(
      month = factor(month, levels = month.name),
      color = ifelse(year == data_and_vars$latest_year, "#e50076", "#D3D3D3")
    )

  ggplot(df, aes(x = month, y = admissions_m, group = year, color = factor(year))) +
    geom_line(aes(color = color), size = 1) +
    geom_point(aes(color = color), size = 2) +
    geom_text(
      data = df %>% group_by(year) %>% filter(row_number() == n()),
      aes(label = year, x = month, y = admissions_m, color = color),
      hjust = 0, vjust = 0.5, size = 5, fontface = "bold",
      position = position_nudge(x = 0.05)
    ) +
    labs(
      title = 'UK admissions, million, by month',
      subtitle = 'All titles on release, including event titles',      
      x = '',
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    scale_x_discrete(expand = expansion(mult = 0.1)) +  
    scale_color_identity() + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}


# Production

all_production_first <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]
  
  # Filter data for all years but only latest_quarter
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))

  df_revised <- df %>%
    filter(production_type == 'all') %>%
    group_by(label, category) %>%
    filter(!(status == 'first_reported' & any(status == 'revised'))) %>%
    ungroup()

  df_first <- df %>%
    filter(production_type == 'all') %>%
    group_by(label, category) %>%
    filter(status == 'first_reported') %>%
    ungroup()

  df_total_revised <- df_revised %>%
    group_by(label) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  df_total_first <- df_first %>%
    group_by(label) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  ggplot(df_total_revised, aes(x = label, y = total_metric)) +
    geom_bar(stat = 'identity', fill = 'grey', alpha = 0) +
    geom_text(aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5, alpha = 0) +
    geom_bar(data = df_total_first, stat = 'identity', fill = '#783df6') +
    geom_text(data = df_total_first, aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5) +
    labs(
      title = paste0("UK production ", metric_display, ", in the year ending (YE) ", data_and_vars$latest_month),
      subtitle = paste0("Film and HETV projects starting principal photography, 
                        <span style='color:#783df6'>**first reported**</span> data"),
      x = '',
      y = ''
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


all_production_revised <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]  
  
  # Filter data for all years but only latest_quarter
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))

  df_revised <- df %>%
    filter(production_type == 'all') %>%
    group_by(label, category) %>%
    filter(!(status == 'first_reported' & any(status == 'revised'))) %>%
    ungroup()

  df_first <- df %>%
    filter(production_type == 'all') %>%
    group_by(label, category) %>%
    filter(status == 'first_reported') %>%
    ungroup()

  df_total_revised <- df_revised %>%
    group_by(label) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  df_total_first <- df_first %>%
    group_by(label) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  ggplot(df_total_revised, aes(x = label, y = total_metric)) +
    geom_bar(stat = 'identity', fill = 'darkgrey') +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5) +  # Position total labels slightly above the top of the bars
    geom_bar(data = df_total_first, stat = 'identity', fill = '#783df6') +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5) + 
    labs(
      title = paste0("UK production ", metric_display, ", in the year ending (YE) ", data_and_vars$latest_month),
      subtitle = paste0("Film and HETV projects starting principal photography, 
                        <span style='color:#783df6'>**first reported**</span> and 
                        <span style='color:darkgrey'>**revised**</span> data"), 
      x = '', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


film_hetv_production_revised <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]

  # Filter data for all years but only latest_quarter
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))
    
  df_filtered <- df %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(label, category) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  ggplot(df_filtered, aes(x = label, y = .data[[metric]], fill = category)) +
    geom_bar(stat = 'identity') +  
    geom_text(aes(label = scales::comma(round(.data[[metric]], 0))), 
              position = position_stack(vjust = 0.9), 
              color = 'white') + 
    labs(
      title = paste0("UK production ", metric_display, ", in the year ending (YE) ", data_and_vars$latest_month),
      subtitle = paste0("<span style='color:#e50076'>**Film**</span> 
            and <span style='color:#1197FF'>**HETV**</span> projects
            starting principal photography"), 
      x = '', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('film' = '#e50076', 
                                 'hetv' = '#1197FF')) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


film_hetv_production_revised_percentage <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]  
  
  # Filter data for all years but only latest_quarter
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))
     
  df_filtered <- df %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(label, category) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup() %>%
    # Calculate the total spend per year
    group_by(label) %>%
    mutate(total_metric_year = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup() %>%
    # Calculate the proportion for each category in each year
    mutate(proportion = .data[[metric]] / total_metric_year)

  ggplot(df_filtered, aes(x = label, y = proportion, fill = category)) +
    geom_bar(stat = 'identity') +  
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
              position = position_stack(vjust = 0.5), 
              color = 'white') +  # Position the labels at the center of each segment
    labs(
      title = paste0("UK production ", metric_display, " in the year ending (YE) ", data_and_vars$latest_month,
                    ", % by category"),
      subtitle = paste0("<span style='color:#e50076'>**Film**</span> 
            and <span style='color:#1197FF'>**HETV**</span> projects
            starting principal photography"), 
      x = '', 
      y = '') +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c('film' = '#e50076', 
                                 'hetv' = '#1197FF')) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


production_revised <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )
  
  category_display_names <- c(
    film = "Film",
    hetv = "HETV"
)
  
  category_colours <- c(
    film = "#e50076",
    hetv = "#1197FF"
  )

  metric_display <- metric_display_names[[metric]]
  category_display <- category_display_names[[category_select]]
  category_colour <- category_colours[[category_select]]
  
  # Filter data for all years but only latest_quarter
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))
    
  df_filtered <- df %>%
    filter(category == category_select) %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(label, production_type) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  ggplot(df_filtered, aes(x = label, y = .data[[metric]])) +
    geom_bar(stat = 'identity', , fill = category_colour) +  
    geom_text(aes(label = scales::comma(round(.data[[metric]], 0)), 
              vjust = 1.5), 
              color = 'white') + 
    labs(
      title = paste0("UK production ", metric_display, ", in the year ending (YE) ", data_and_vars$latest_month),
      subtitle = paste0(
                "<span style='color:", category_colour, "'>**",
                category_display, "**</span> projects starting principal photography"), 
      x = '', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}

production_breakdown_revised <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )
  
  category_display_names <- c(
    film = "Film",
    hetv = "HETV"
)
  
  category_colours <- c(
    film = "#e50076",
    hetv = "#1197FF"
  )

  metric_display <- metric_display_names[[metric]]
  category_display <- category_display_names[[category_select]]
  category_colour <- category_colours[[category_select]]

  # Filter data for all years but only latest_quarter
  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))

  df_total <- df %>%
    filter(category == category_select) %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(label, production_type) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  df_filtered <- df %>%
    filter(category == category_select, production_type != 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(label, production_type) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  df_filtered <- df_filtered %>%
    group_by(label) %>%
    filter(
      !(production_type == 'inward_investment_and_co_production' & any(production_type %in% c('inward_investment', 'co_production')))
    ) %>%
    ungroup()

  ggplot(df_filtered, aes(x = label, y = .data[[metric]])) +
    geom_bar(data = df_total, stat = 'identity', fill = 'darkgrey') +  
    geom_line(aes(group = production_type), color = category_colour, size = 1) + 
    geom_point(color = category_colour, size = 2) +  # Add points to highlight values
    geom_text_repel(
      aes(label = scales::comma(round(.data[[metric]], 0))),  # Round to 0dp & add comma separator
      nudge_y = 10,
      direction = 'y',  # Only adjust in the vertical direction
      size = 4, 
      fontface = 'bold',
      color = 'white',  # White text color
      box.padding = 0.2,  # Adds slight padding around text
      segment.color = NA  # Removes connector lines
    ) +
    geom_text_repel(
      data = df_filtered %>% 
      group_by(production_type) %>% 
      filter(year == max(year)) %>%
      mutate(production_type = recode(production_type, 
        'inward_investment' = 'INW',
        'co_production' = 'COP',  
        'domestic_uk' = 'DOM',
        'inward_investment_and_co_production' = 'INW+COP'
      )),
      aes(x = label,
          y = .data[[metric]],
          label = production_type),
      color = category_colour, 
      size = 5, fontface = "bold",
      direction = 'both',
      nudge_x = 0.3,
      segment.color = NA
    ) +  
    labs(
      title = paste0("UK production ", metric_display, ", in the year ending (YE) ", data_and_vars$latest_month),
      subtitle = paste0(
                "<span style='color:", category_colour, "'>**",
                category_display, "**</span> projects starting principal photography"), 
      x = '', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


# Certification

certification <- function() {
  # Mappings
  metric_display_names <- c(
    uk_spend_m = "UK spend, £ million",
    count = "count"
  )

  category_display_names <- c(
    film = "Film",
    hetv = "HETV",
    animation_tv = "Animation TV",
    childrens_tv = "Children's TV",
    video_games = "Video games"
  )

  cert_type_display_names <- c(
    cultural_test = "cultural test",
    co_production = "co-production"
  )

  cert_type_colours <- c(
    cultural_test = "#e50076",
    co_production = "#1197FF"
  )
  metric_display <- metric_display_names[[metric]]
  category_display <- category_display_names[[category_select]]

  df <- data_and_vars$data %>%
    filter(quarter == data_and_vars$latest_quarter) %>%
    group_by(year, label) %>%
    ungroup()

  df_filtered <- df %>%
    filter(category == category_select) %>%
    filter(cert_status == cert_status_select) %>%
    mutate(label = factor(label, levels = unique(label[order(year, month_num)])))

  # Build dynamic coloured cert_type names for title
  cert_types_present <- unique(df_filtered$cert_type)
  cert_type_labels <- purrr::map_chr(cert_types_present, function(ct) {
    paste0("<span style='color:", cert_type_colours[[ct]], "'>", cert_type_display_names[[ct]], "</span>")
  })
  cert_type_text <- paste(cert_type_labels, collapse = " and ")

  # Compute top-of-bar position for co_production labels
  df_co_labels <- df_filtered %>%
    group_by(label) %>%
    summarise(
      total_height = sum(.data[[metric]], na.rm = TRUE),
      co_production_sum = sum(.data[[metric]][cert_type == "co_production"], na.rm = TRUE)
    ) %>%
    filter(co_production_sum > 0) %>%  # only years with co_production
    mutate(
      label_text = scales::comma(round(co_production_sum, 0)),
      y_pos = total_height + 0.02 * max(total_height)  # add a small offset above the bar
    )

  ggplot(df_filtered, aes(x = label, y = .data[[metric]], fill = cert_type)) +
    geom_bar(stat = "identity", position = "stack") +

    # cultural_test labels inside their segment
    geom_text(
      data = df_filtered %>% filter(cert_type == "cultural_test"),
      aes(label = scales::comma(round(.data[[metric]], 0))),
      position = position_stack(vjust = 0.5),
      color = "white"
    ) +

    # co_production labels above the stacked bar
    geom_text(
      data = df_co_labels,
      aes(x = label, y = y_pos, label = label_text),
      color = cert_type_colours[["co_production"]],
      inherit.aes = FALSE
    ) +
    
    scale_fill_manual(values = cert_type_colours) +
    labs(
      title = paste0(category_display, " ", cert_status_select, " certifications, year ending (YE) ", data_and_vars$latest_month, ", ", metric_display), 
      subtitle = paste0("All ", cert_type_text, " certifications"),
      x = "",
      y = ""
    ) +
    scale_x_discrete(labels = scales::wrap_format(10)) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.subtitle = ggtext::element_markdown()
    )
  }
