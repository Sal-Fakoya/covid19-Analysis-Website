

# Case Study: Explore COVID-19 in USA:

# function that renames state:
renameState <- function(df)
{
  df <- df %>% 
    rename_with(.data = .,
                .fn = ~ "state",
                .cols = contains("state"))
  
  return(df)
}

# Initial Data Inspection:

## function that checks and counts missing values for each table:
countNA <- function(df) {
  require(tidyverse)
  require(cowplot)
  
  # Count NAs using summarise across all columns
  df_na_count <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "col", values_to = "NAs") %>%
    mutate(NA_perc = round(NAs / nrow(df) * 100, 2))
  

  
  # Absolute NA count plot
  p1 <- df_na_count %>%
    ggplot(aes(x = col, y = NAs, fill = col)) +
    geom_col(show.legend = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, face = "bold")) +
    scale_fill_viridis_d(option = "magma") +
    scale_x_discrete(labels = function(x) str_trunc(x, width = 10, ellipsis = "...")) +
    geom_text(aes(label = NAs), vjust = 0)
  
  # Relative NA percentage plot
  p2 <- df_na_count %>%
    ggplot(aes(x = col, y = NA_perc, fill = col)) +
    geom_col(show.legend = FALSE) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, face = "bold")) +
    scale_fill_viridis_d(option = "magma") +
    scale_x_discrete(labels = function(x) str_trunc(x, width = 12, ellipsis = "...")) +
    geom_text(aes(label = paste0(NA_perc, "%")), vjust = 0)
  
  return(plot_grid(p1, p2, nrow = 2))
}




# function to check time span:
checkTimeSpan <- function(df) 
{
  # summarise time spans - table level:
  require(tidyverse)
  
  table_count <- df %>% 
    summarise(distinct_dates = n_distinct(date),
              min_date = min(date),
              max_date = max(date),
              date_range = max_date - min_date)
  print(table_count)
  
  # show distinct dates count for each state:
  p1 <- df %>% 
    # count distinct dates for each state:
    group_by(state) %>% 
    summarise(distinct_dates = n_distinct(date)) %>% 
    # show counts
    ungroup()  %>% 
    ggplot(mapping = aes(x = state,
                         y = distinct_dates)) +
    geom_col() + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))
  
  return(p1)
  
}



#----------------------------------Data Wrangling Functions:
# Function for matching US-states and states in the tables:

# name of final state column:
# states_base = list of US_States
# data = source
# col_name = is the name of the column where the states are present.

matchStates <- function(states_base = states_list,
                        data,
                        col_name)
{
  require(tidystringdist)
  require(rlang)
  
  # extract unique state names from given data source.
  states_data <- data %>% 
    distinct(state)
  
  # create table of all combinations: state pairs:
  states_comb <- expand.grid(states_base = states_list %>% 
                               pull(state_base),
                             state = states_data %>% pull(state))
  
  # compute string distance:
  t1 <- tidy_stringdist(df = states_comb,
                  v1 = states_base,
                  v2 = state,
                  method = "osa") %>% 
    # sort best name match per state and add matching rank:
    arrange(states_base, osa) %>% 
    group_by(states_base) %>% 
    mutate(rank = row_number()) %>% 
    ungroup() %>% 
    # filter top ranks:
    filter(rank == 1) %>% 
    select(states_base, {{ col_name }} := state)
  
  # t2 is an easier fix...
  t2 <- tidy_stringdist(df = states_comb,
                        v1 = states_base,
                        v2 = state,
                        method = "osa") %>% 
    filter(osa == 0) %>% 
    select(states_base, state) %>% 
    arrange(states_base)
  
  return (t1)
  
}



# function that plots confirmed cases and deaths total count over time on a state level:
plotConfirmedCasesTotal <- function(data = df.main, region.group)
{
  
  # data:
  plot_data <- data %>% 
    filter(region_group == region.group)
  
  # confirmed cases absolute count
  p11 <- plot_data %>% 
    ggplot(mapping = aes(x = date,
                         y = confirmed_total,
                         group = state,
                         color = state)) +
    geom_line() +
    geom_point(alpha = 0.7) +
    scale_colour_viridis_d() +
    xlab("Date") +
    ylab("Number of Confirmed cases total") +
    ggtitle(paste("Infected Cases /", region.group, sep = " ")) +
    theme_minimal()
  
  ## confirmed cases relative count:
  p21 <- plot_data %>% 
    ggplot(mapping = aes(x = date,
                         y = confirmed_totalPerc,
                         group = state,
                         color = state)) +
    geom_line() +
    geom_point(alpha = 0.7) +
    scale_colour_viridis_d() +
    xlab("Date") +
    ylab("Percentage(%) of Confirmed cases total") +
    ggtitle(paste("Infected Cases /", region.group, sep = " ")) +
    theme_minimal()

  
  # death cases absolute count
  p12 <- plot_data %>% 
    ggplot(mapping = aes(x = date,
                         y = deaths_total,
                         group = state,
                         color = state)) +
    geom_line() +
    geom_point(alpha = 0.7) +
    scale_colour_viridis_d() +
    xlab("Date") +
    ylab("Number of Deaths total") +
    ggtitle(paste("Death /", region.group, sep = " ")) +
    theme_minimal()
  
  
  ## confirmed cases relative count:
  p22 <- plot_data %>% 
    ggplot(mapping = aes(x = date,
                         y = deaths_totalPerc,
                         group = state,
                         color = state)) +
    geom_line() +
    geom_point(alpha = 0.7) +
    scale_colour_viridis_d() +
    xlab("Date") +
    ylab("Percentage(%) of Deaths total") +
    ggtitle(paste("Death /", region.group, sep = " ")) +
    theme_minimal()
  
  
  plot <- plot_grid(p11, p12, p21, p22, nrow = 2, ncol = 2)
  
  #plot_export
  
  if (!dir.exists("./explore"))
  {
    dir.create("./explore")
  }
  
  ggsave(filename = paste("./explore/01_confirmed_cases_and_deaths",
                          region.group, ".png"),
         plot = plot,
         width = 30, height = 20, 
         units = "cm",
         dpi = 1200)
  return (plot)
}


#-------------------------- function that plots confirmed cases and deaths for Moving average of 7 days:
plot7DayAverage <- function(data = df.main, region.group)
{
  # data
  plot_data <-  data %>% 
    filter(region_group == region.group)
  
  # confirmed cases:
  p1 <- plot_data %>% 
    ggplot(aes(x = date,
           y = `confirmed_daily_cases 7d Avg`,
           group = state,
           color = state)) +
    geom_line(show.legend = T, alpha = 0.8, linewidth = 0.6) +
    geom_point(show.legend = T, alpha = 0.8, size = 0.8) +
    scale_colour_viridis_d() +
    xlab("Date") +
    ylab("Number of Confirmed Cases 7 Day Average") +
    ggtitle("Infected daily cases /", region.group) +
    theme_minimal()
  
  p2 <- plot_data %>% 
    ggplot(aes(x = date,
           y = `death_daily_cases 7d Avg`,
           group = state,
           color = state)) +
    geom_line(show.legend = T, alpha = 0.8, linewidth = 0.6) +
    geom_point(show.legend = T, alpha = 0.8, size = 0.8) +
    scale_colour_viridis_d() +
    theme_minimal() +
    xlab("Date") +
    ylab("Number of Death on a 7 Day Average") +
    ggtitle("Deaths /", region.group) 
  
  plot <- plot_grid(p1, p2, nrow =  2)
  
  if (!dir.exists("./explore"))
  {
    dir.create("./explore")
  }
  
  ggsave(filename = paste("./explore/03_confirmed_cases_and_deaths_on_7Day_Avg",
                          region.group, ".png"),
         plot = plot,
         width = 35, height = 25, units = "cm",
         dpi = 1200)
  
  return(plot)
    
}

# a better Moving average function:
plot7DayAverage <- function(data = df.main, region.group, output_dir = "./explore") 
  {
  # Ensure necessary libraries are loaded
  require(ggplot2)
  require(cowplot)
  require(viridis)
  require(rlang)
  
  # Verify that region.group exists
  if (!region.group %in% unique(data$region_group)) {
    stop("Error: The specified region.group does not exist in the data.")
  }
  
  # Filter data
  plot_data <- data %>% 
    filter(region_group == region.group)
  
  # Helper function for creating plots
  create_plot <- function(data = plot.data, 
                          y_var, title_text, y_label_text) {
    data %>%
      ggplot(aes(x = date, y = {{ y_var }}, group = state, color = state)) +
      geom_line(alpha = 0.8, linewidth = 0.8) +
      geom_point(alpha = 0.8, size = 0.8, show.legend = FALSE) +
      scale_colour_viridis_d() +
      labs(title = paste(title_text, "-", region.group),
           x = "Date",
           y = y_label_text,
           color = "State") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # Create plots
  p1 <- create_plot(plot_data, 
                    `confirmed_daily_cases 7d Avg`, 
                    "Infected Daily Cases", 
                    "7-Day Moving Average of Confirmed Cases")
  p2 <- create_plot(plot_data, 
                    `death_daily_cases 7d Avg`, 
                    "Deaths", 
                    "7-Day Moving Average of Deaths")
  
  # Combine plots
  combined_plot <- plot_grid(p1, p2, nrow = 2)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Construct filename
  filename <- file.path(output_dir, paste0("03_confirmed_cases_and_deaths_on_7Day_Avg_", region.group, ".png"))
  
  # Save plot
  ggsave(filename = filename,
         plot = combined_plot,
         width = 35, height = 25, units = "cm",
         dpi = 1200)
  
  # Return the combined plot
  return(combined_plot)
}


# Does vaccination help fighting the COVID-19 pandemic? i.e does vaccination help decrease covid-19 confirmed cases and death toll?

# Plot vaccince doses
plot_vaccineDosesTotal_7DayAVg <- function(data = df.main, region.group, output_dir = "./explore") 
{
  # Ensure necessary libraries are loaded
  require(ggplot2)
  require(cowplot)
  require(viridis)
  require(rlang)
  
  # Verify that region.group exists
  if (!region.group %in% unique(data$region_group)) {
    stop("Error: The specified region.group does not exist in the data.")
  }
  
  # Filter data
  plot_data <- data %>% 
    filter(region_group == region.group)
  
  # Helper function for creating plots
  create_plot <- function(data = plot.data, 
                          y_var, title_text, y_label_text) {
    data %>%
      ggplot(aes(x = date, y = {{ y_var }}, group = state, color = state)) +
      geom_line(alpha = 0.8, linewidth = 0.8) +
      geom_point(alpha = 0.8, size = 0.8, show.legend = FALSE) +
      scale_colour_viridis_d() +
      labs(title = paste(title_text, "-", region.group),
           x = "Date",
           y = y_label_text,
           color = "State") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # Create plots
  p1 <- create_plot(plot_data, 
                    `confirmed_daily_cases 7d Avg`, 
                    "Infected Daily Cases", 
                    "7-Day Moving Average of Confirmed Cases")
  p2 <- create_plot(plot_data, 
                    `death_daily_cases 7d Avg`, 
                    "Deaths", 
                    "7-Day Moving Average of Deaths")
  
  p3 <- create_plot(plot_data,
                    vaccine_doses_total,
                    "Vaccine Doses",
                    "7-Day Moving Average of Vaccince Doses")
  
  # Combine plots
  combined_plot <- plot_grid(p1, p2, p3, nrow = 3)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Construct filename
  filename <- file.path(output_dir, paste0("04_total_vaccine_doses", region.group, ".png"))
  
  # Save plot
  ggsave(filename = filename,
         plot = combined_plot,
         width = 35, height = 25, units = "cm",
         dpi = 1200)
  
  # Return the combined plot
  return(combined_plot)
}



# Show on map how number of COVID-case have changed over time (monthly)

plotCovid19Map_overTime <- function(data = df.main, var,
                                    output_dir = "./explore") 
  {
  
  # Ensure the necessary libraries are loaded
  require(ggplot2)
  require(cowplot)
  require(viridis)
  require(rlang)
  
  # Filter and prepare data
  plot_data <- data %>% 
    filter(date_snapshot_flag) %>% 
    select(region, state_, state, date, confirmed_total, deaths_total, vaccine_doses_total, stringency_index_for_display) %>% 
    left_join(map_data("state"), by = c("state_" = "region"))
  
  # Helper function for creating plots
  create_plot <- function(data, var, title_text) {
    data %>%
      ggplot(aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill = var), 
                   color = "black", show.legend = TRUE) +
      facet_wrap(~ date) +
      ggtitle(title_text) +
      theme_bw() + 
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(hjust = 0.5)) + 
      scale_fill_gradient(low = "white", high = "red")
  }
  
  # Create plot based on variable
  title <- paste("Map of Covid-19", gsub("_", " ", var), "Over Time")
  plot <- create_plot(plot_data, var, title)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Construct filename
  filename <- file.path(output_dir, paste0("06_Map_of_", var, "_over_time.png"))
  
  # Save plot
  ggsave(filename = filename, plot = plot, width = 35,
         height = 25, units = "cm", dpi = 1200)
  
  # Return the plot
  return(plot)
}





# function that plots covid-19 indicators on state level:

# Function to plot COVID-19 indicators at the state level
plotCovid19Indicators_stateLevel <- function(data = df.main, state_selection, output_dir = "./explore") {
  
  # Filter data for the selected state
  df_main_state <- data %>% 
    filter(state == state_selection)
  
  # Plot for total count
  firstPlot <- df_main_state %>% select(date, confirmed_total, 
                                 deaths_total, vaccine_doses_total) %>% 
    pivot_longer(cols = -date, names_to = "indicator", 
                 values_to = "value") %>% 
    mutate(indicator = factor(indicator, levels = c("confirmed_total", "deaths_total", "vaccine_doses_total"))) %>% 
    ggplot(aes(x = date,  y = value, fill = indicator)) +
        geom_area(color = "black") +
        scale_fill_manual(values = c("brown1", "black", "forestgreen")) +
        xlab("Date") +
        ylab("Total count") +
        theme_minimal() +
    ggtitle(paste0("State: ", state_selection, " - COVID-19 indicators over observed time"))
  
  
  
  # Plot for 7 day averages:
  secondPlot <- df_main_state %>% select(date, `confirmed_daily_cases 7d Avg`, `death_daily_cases 7d Avg`,  `daily_vacc_doses 7d Avg`) %>% 
    pivot_longer(cols = -date, names_to = "indicator", 
                 values_to = "value") %>% 
    mutate(value = na_if(value, 0)) %>% 
    mutate(indicator = factor(indicator, levels = c("confirmed_daily_cases 7d Avg", "death_daily_cases 7d Avg",  "daily_vacc_doses 7d Avg"))) %>% 
    ggplot(aes(x = date,
                          y = value, 
                          group = indicator,
                          color = indicator)) +
    geom_line(size = 0.9) +
    geom_point(size = 0.9) +
    scale_y_log10() +
    scale_color_manual(values = c("brown1", "black", "forestgreen")) +
    xlab("Date") +
    ylab("Government Response") +
    theme_minimal()
  
    
    # Plot for Government Response
    thirdPlot <- df_main_state %>% 
      select(date, government_response_index_for_display, containment_health_index_for_display, economic_support_index_for_display, stringency_index_for_display) %>% 
      pivot_longer(cols = -date, names_to = "index", 
                   values_to = "value") %>% 
      ggplot(aes(x = date, y = value, fill = index)) +
      geom_area(color = "black") +
      scale_fill_viridis_d() +
      scale_y_continuous(limits = c(0, 300)) + 
      xlab("Date") +
      ylab("State Response") +
      theme_minimal()
    
 
  combined_plot <- plot_grid(firstPlot, secondPlot, thirdPlot,
                             nrow = 3)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Construct filenames
  filename_combined <- file.path(output_dir, paste0("07_CombinedPlot_of_COVID19_Indicators_", state_selection, ".png"))
  
  # Save combined plot
  ggsave(filename = filename_combined, 
         plot = combined_plot, width = 35, 
         height = 25, units = "cm", dpi = 1200)
  
  return(combined_plot)
}








