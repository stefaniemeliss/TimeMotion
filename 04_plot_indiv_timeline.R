source(file = "03_process_data_fin.R")


# determine colours
fill_col <- c(
  rep(blue, sum(grepl("^A[0-9]", lvl))),
  rep(red, sum(grepl("^B[0-9]", lvl))),
  rep(yellow, sum(grepl("^C[0-9]", lvl))),
  rep(navy, sum(grepl("^D[0-9]", lvl))),
  rep(cyan, sum(grepl("^E[0-9]", lvl))),
  rep(green, sum(grepl("^F[0-9]", lvl))),
  rep(black40, sum(grepl("^G[0-9]", lvl))),
  rep(coral, sum(grepl("^TS[0-9]", lvl))),
  rep(teal, sum(grepl("^SW[0-9]", lvl))),
  rep(orange, sum(grepl("^SA[0-9]", lvl))),
  rep(purple, sum(grepl("^SC[0-9]", lvl)))
)
names(fill_col) <- lvl

for (i in 1:length(pseudo)) {
  
  
  # subset data for teacher
  id <- obs_ids[i]
  ps <- pseudo[i]
  
  act <- orig_indiv[orig_indiv$Obs_Id == id & orig_indiv$Display_Name != "", ]
  stake <- orig_inter[orig_inter$Obs_Id == id & orig_inter$Display_Name != "", ]
  loc <- orig_loco[orig_loco$Obs_Id == id & orig_loco$Display_Name != "", ]
  
  data <- act %>%
    bind_rows(stake) %>%
    bind_rows(loc)
  
  # Convert Unix times to POSIXct
  # orig_indiv$time_start <- as.POSIXct(orig_indiv$Start_Unix, origin = "1970-01-01", tz = "Europe/London")
  # orig_indiv$time_end <- orig_indiv$time_start + orig_indiv$Duration_Seconds
  
  data$time_start <- as.POSIXct(data$Start_Unix, origin = "1970-01-01", tz = "Europe/London")
  data$time_end <- data$time_start + data$Duration_Seconds
  data$time_mid <- data$time_start + (data$Duration_Seconds / 2 )
  
  # # Declare factors
  # data$category <- sapply(data$Name, clean_items)
  # data$category <- factor(data$category, levels = lvl)
  # 
  # # add labels
  # data$label <- sub("\\..*", "", data$category)
  
  # # Declare display name as factor
  # levels <- sort(unique(orig_indiv$Display_Name[orig_indiv$Display_Name != ""]))
  # levels <- unique(orig_indiv$Display_Name)
  # sort(c(levels, "D6. Self-study"))
  # 
  # 
  # orig_indiv$Display_Name <- factor(orig_indiv$Display_Name, levels = levels)
  # 
  # colours <- c(ambition_palette_bright, black, white, ambition_palette_accent, navy, black40, navy40, "green")
  # names(colours) <- levels
  library(ggplot2)
  
  # Create a data frame for the background (full day) box
  background <- data.frame(
    start = data$Start_Unix[1],
    end = data$Start_Unix[nrow(data)] + data$Duration_Seconds[nrow(data)]
  )
  
  background <- data.frame(
    start = as.POSIXct(format(min(data$time_start), "%Y-%m-%d 07:00:00"), tz = "Europe/London"),
    end = as.POSIXct(format(min(data$time_start), "%Y-%m-%d 17:30:00"), tz = "Europe/London")
  )
  
  
  p <- ggplot() + ambition_theme +
    # Grey background for the day
    geom_rect(
      data = background,
      aes(
        xmin = 1, xmax = 1.1,  # x position for each orig_individual (will adjust below)
        ymin = start, ymax = end
      ),
      fill = "grey90"
    ) +
    # Task rectangles
    geom_rect(
      data = data,
      aes(
        xmin = 1,
        xmax = 1.2,
        ymin = time_start,
        ymax = time_end,
        fill = Label
      ), colour = white, linewidth = 0.05
    ) +
    scale_fill_manual(values = fill_col) +
    scale_y_datetime(
      name = "Time of day",
      breaks = seq(background$start, background$end, by = "30 mins"),
      date_labels = "%H:%M"
    ) +
    facet_grid(cols = vars(Dimension)) +
    theme(legend.position = "right") + 
    guides(fill = guide_legend(ncol = 1)) + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )
  
  test <- data %>%
    group_by(Dimension) %>%
    arrange(time_start) %>%
    mutate(
      window_start = time_start - 30
    ) %>%
    # mutate(
    #   x_lab = if_else(Duration_Seconds >= 90, 1.1, 1.2)
    # ) %>%
    # mutate(
    #   x_lab2 = if_else(Duration_Seconds < 90 & lag(Duration_Seconds) < 30, 1.3, x_lab)
    # ) %>%
    as.data.frame()
  
  library(slider)
  library(lubridate)
  test <- data %>%
    group_by(Dimension) %>%
    arrange(time_mid) %>%
    mutate(
      n = if_else(Duration_Seconds < 90, 1, 0),
      n_prev_90s = slider::slide_index_dbl(
        .x = n,
        .i = time_mid,
        .f = sum,
        .before = lubridate::dseconds(90),
        .complete = FALSE
      )
    ) %>%
    mutate(
      x_lab = if_else(Duration_Seconds >= 90, 1.1, 
                      1.15 + n_prev_90s * 0.05)
    ) %>%
    mutate(
      check = x_lab == lag(x_lab)
    ) %>%
    mutate(
      x_lab2 = if_else(Duration_Seconds < 90 & n_prev_90s > 1 & check == T, x_lab + 0.05, x_lab) 
    )
  p +
    # 1) Label longer segments *inside* the rectangle
    geom_text(
      data = test,
      aes(x = x_lab, y = time_mid, label = Code),
      size = 3, colour = "black", check_overlap = T
    )
  ggsave(file.path(getwd(), paste0(ps, ".jpg")), unit = "cm", height = 100, width = 50)
  ggsave(file.path(dir_data_in, paste0(ps, ".jpg")), unit = "cm", height = 100, width = 50)
}
