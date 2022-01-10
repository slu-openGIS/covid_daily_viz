# regional plot b/c/e - 7-day average counts ####

regional_count <- function(.data, region, point_data, state_data, region_data, plot_data, palette){
  
  # create region label
  if (region == "St. Louis"){
    region_label <- "St. Louis Metro Focus (MO Counties Only)\n"
  } else if (region == "Kansas City"){
    region_label <- "Kansas City Metro Focus (MO Counties Only)\n"
  } else if (region == "Outstate"){
    region_label <- "Outstate Focus\n"
  }
  
  # create ribbon objects
  state_error1 <- read_csv("data/MO_HEALTH_Covid_Tracking/data/region/region_meso.csv",
                            col_types = cols(region = col_character()
                            )) %>%
    filter(report_date == as.Date("2021-01-10") | report_date == as.Date("2021-01-19")) %>%
    filter(region == "Missouri")
  
  state_error2 <- read_csv("data/MO_HEALTH_Covid_Tracking/data/region/region_meso.csv",
                            col_types = cols(region = col_character()
                            )) %>%
    filter(report_date == as.Date("2021-03-07") | report_date == as.Date("2021-03-16")) %>%
    filter(region == "Missouri")
  
  state_error3 <- read_csv("data/MO_HEALTH_Covid_Tracking/data/region/region_meso.csv",
                            col_types = cols(region = col_character()
                            )) %>%
    filter(report_date == as.Date("2021-04-15") | report_date == as.Date("2021-04-25")) %>%
    filter(region == "Missouri")
  
  state_error4 <- read_csv("data/MO_HEALTH_Covid_Tracking/data/region/region_meso.csv",
                            col_types = cols(region = col_character()
                            )) %>%
    filter(report_date == as.Date("2021-11-16") | report_date == as.Date("2021-12-06")) %>%
    filter(region == "Missouri")
  
  # construct plot
  p <- ggplot() +
    geom_area(data = state_error1, mapping = aes(x = report_date, y = case_avg), alpha = .45) +
    geom_area(data = state_error2, mapping = aes(x = report_date, y = case_avg), alpha = .45) +
    geom_area(data = state_error3, mapping = aes(x = report_date, y = case_avg), alpha = .45) +
    geom_area(data = state_error4, mapping = aes(x = report_date, y = case_avg), alpha = .45) +
    geom_line(.data, mapping = aes(x = report_date, y = case_avg, color = factor_var), size = 2) +
    geom_point(data = point_data, mapping = aes(x = report_date, y = case_avg, color = factor_var), 
               size = 4, show.legend = FALSE) +
    geom_point(data = state_data$peak_tbl, mapping = aes(x = report_date, y = case_avg), 
               size = 4, shape = 16) +
    geom_point(data = region_data$peak_tbl, mapping = aes(x = report_date, y = case_avg), 
               size = 4, shape = 16) +
    geom_label_repel(data = state_data$peak_tbl, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = state_data$peak_y, nudge_x = state_data$peak_x, size = 5) +
    geom_label_repel(data = region_data$peak_tbl, mapping = aes(x = report_date, y = case_avg, label = text),
                    nudge_y = region_data$peak_y, nudge_x = region_data$peak_x, size = 5) +
    scale_color_manual(values = palette, name = "Region") +
    scale_y_continuous(limits = c(0, state_data$top_val), breaks = seq(0, state_data$top_val, by = 500)) +
    scale_x_date(date_breaks = plot_data$date_breaks, date_labels = "%b") +
    labs(
      title = "Pace of New COVID-19 Cases in Missouri",
      subtitle = paste0(region_label, as.character(plot_data$plot_date), " through ", as.character(plot_data$date)),
      caption = paste0(plot_data$caption_text, "\nShaded areas represent smoothed lines to address data reporting issues"),
      x = "Date",
      y = "7-day Average of New Cases"
    ) +
    sequoia_theme(base_size = 22, background = "white") +
    theme(axis.text.x = element_text(angle = plot_data$x_angle))
  
  # add current points
  if (state_data$current_display == TRUE){
    p <- p + geom_label_repel(data = state_data$current_tbl, 
                             mapping = aes(x = report_date, y = case_avg, label = text), 
                             nudge_y = state_data$current_y, nudge_x = state_data$current_x, size = 5) 
  }
  
  if (region_data$current_display == TRUE){
    p <- p + geom_label_repel(data = region_data$current_tbl, 
                             mapping = aes(x = report_date, y = case_avg, label = text), 
                             nudge_y = region_data$current_y, nudge_x = region_data$current_x, size = 5)   
  }
  
  # return output
  return(p)
  
}

# state/county plot e - facet rate ####

facet_rate <- function(.data, type, subtype = NULL, pal, x_breaks, y_breaks, y_upper_limit, highlight, plot_date, date, title, caption, last3 = FALSE){
  
  # create name
  if (type == "metro" | type == "metro HHS"){
    scale_name <- "Metro Area"
  } else if (type == "county"){
    scale_name <- "County"
  } else if (type == "state"){
    scale_name <- "State"
  }
  
  # construct plot
  if (type == "metro HHS"){
    p <- ggplot(.data) +
      geom_line(mapping = aes(x = report_date, y = covid_per_cap, color = factor_var), 
                size = 2, show.legend = FALSE)    
  } else {
    p <- ggplot(.data) +
      geom_line(mapping = aes(x = report_date, y = case_avg_rate, color = factor_var), 
                size = 2, show.legend = FALSE)   
  }
  
  # optionally highlight trends
  if (type == "metro" | type == "county"){
    p <- p + gghighlight(geoid %in% highlight, use_direct_label = FALSE, use_group_by = FALSE)
  } else if (type == "state"){
    p <- p + gghighlight(state %in% highlight, use_direct_label = FALSE, use_group_by = FALSE)
  }
    
 
  if (type == "metro HHS"){
    y_string <- "COVID Patients per 1,000 Staffed Beds"
    caption_string <- caption
  } else if (type != "metro HHS"){
    y_string <- "7-Day Average Rate per 100,000"
    caption_string <- caption
  }
  
  if (last3 == FALSE){
    date_label_val <- "%b"
  } else if (last3 == TRUE){
    date_label_val <- "%d %b"
  }
  
  # finish plot
  p <- p +
    scale_colour_manual(values = pal, name = scale_name) +
    scale_x_date(date_breaks = x_breaks, date_labels = date_label_val) +
    scale_y_continuous(limits = c(0,y_upper_limit), breaks = seq(0, y_upper_limit, by = y_breaks)) + 
    labs(
      title = title,
      x = "Date",
      y = y_string,
      caption = caption_string
    ) +
    sequoia_theme(base_size = 22, background = "white") +
    theme(axis.text=element_text(size = 15))
  
  # add subtitle and captions
  if (is.null(subtype) == TRUE){
    p <- p + labs(
      subtitle = paste0(as.character(plot_date), " through ", as.character(date))
    )
  } else if (is.null(subtype) == FALSE){
    
    if (subtype == "Southeast"){
      p <- p + labs(
        subtitle = paste0("Southeast Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Mid-Missouri"){
      p <- p + labs(
        subtitle = paste0("Mid-Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "St. Joseph"){
      p <- p + labs(
        subtitle = paste0("St. Joseph and Northwestern Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Northern"){
      p <- p + labs(
        subtitle = paste0("Northern Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Ozark"){
      p <- p + labs(
        subtitle = paste0("Lake of the Ozarks and South-Central Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Southwest"){
      p <- p + labs(
        subtitle = paste0("Southwest Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Cape"){
      p <- p + labs(
        subtitle = paste0("Cape Girardeau Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "West-Central"){
      p <- p + labs(
        subtitle = paste0("West-Central Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Springfield"){
      p <- p + labs(
        subtitle = paste0("Springfield Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Ozark Mountains"){
      p <- p + labs(
        subtitle = paste0("Ozark Mountains Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "Kansas City"){
      p <- p + labs(
        subtitle = paste0(as.character(plot_date), " through ", as.character(date))
      )
    } else if (subtype == "Northeastern"){
      p <- p + labs(
        subtitle = paste0("Northeastern Missouri Focus\n",as.character(plot_date), 
                          " through ", as.character(date))
      )
    } else if (subtype == "St. Louis"){
      p <- p + labs(
        subtitle = paste0(as.character(plot_date), " through ", as.character(date))
      )
    }
    
  }
  
  if (last3 == TRUE){
    p <- p + labs(subtitle = paste0(p$labels$subtitle, " (Last Three Weeks)"))
  }
  
  # add facet
  if (type == "metro" | type == "metro HHS"){
    p <- p + facet_wrap(~short_name)
  } else if (type == "county" & is.null(subtype) == TRUE){
    p <- p + facet_wrap(~county)
  } else if (type == "county" & is.null(subtype) == FALSE){
    
    if (subtype %in% c("Kansas City", "St. Louis")){
      p <- p + facet_wrap(~county)
    } else {
      p <- p + facet_wrap(~county_fct)
    }
    
  } else if (type == "state"){
    p <- p + facet_wrap(~state)
  }
  
  # return output
  return(p)
  
}

cumulative_rate <- function(.data, point_data, type, subtype = NULL, plot_values, highlight, y_upper_limit, pal, title, caption){

  # create name
  if (type == "metro" | type == "metro HHS"){
    scale_name <- "Metro Area"
  } else if (type == "county"){
    scale_name <- "County"
  } else if (type == "state"){
    scale_name <- "State"
  }
  
  if (type == "state"){
    
    plot_values$county_rate_val <- 1000
    
  }
  
  # build main part of plot
  p <- ggplot() +
    geom_line(.data, mapping = aes(x = report_date, y = case_rate, color = factor_var), size = 2) +
    geom_point(point_data, mapping = aes(x = report_date, y = case_rate, color = factor_var), 
               size = 4, show.legend = FALSE)
  
  # optionally highlight trends
  if (type == "metro" | type == "county"){
    p <- p + gghighlight(geoid %in% highlight, use_direct_label = FALSE, use_group_by = FALSE)
  } else if (type == "state"){
    p <- p + gghighlight(state %in% highlight, use_direct_label = FALSE, use_group_by = FALSE)
  }
  
  # finish plot
  p <- p + 
    scale_colour_manual(values = pal, name = scale_name) +
    scale_x_date(date_breaks = plot_values$date_breaks, date_labels = "%b") +
    scale_y_continuous(limits = c(0,y_upper_limit), breaks = seq(0, y_upper_limit, by = plot_values$county_rate_val)) + 
    labs(
      title = title,
      x = "Date",
      y = "Rate per 1,000 Individuals",
      caption = caption
    ) +
    sequoia_theme(base_size = 22, background = "white") +
    theme(axis.text.x = element_text(angle = plot_values$x_angle)) 
  
  # add subtitle
  p <- plot_subtype(plot = p, plot_values = plot_values, subtype = subtype)
  
  # return output
  return(p)
  
}

plot_subtype <- function(plot, plot_values, subtype){
  
  # add subtitle and captions
  if (is.null(subtype) == TRUE){
    plot <- plot + labs(subtitle = paste0(as.character(plot_values$plot_date), 
                                          " through ", as.character(plot_values$date)))
  } else if (is.null(subtype) == FALSE){
    
    if (subtype == "Southeast"){
      plot <- plot + labs(
        subtitle = paste0("Southeast Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Mid-Missouri"){
      plot <- plot + labs(
        subtitle = paste0("Mid-Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "St. Joseph"){
      plot <- plot + labs(
        subtitle = paste0("St. Joseph and Northwestern Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Northern"){
      plot <- plot + labs(
        subtitle = paste0("Northern Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Ozark"){
      plot <- plot + labs(
        subtitle = paste0("Lake of the Ozarks and South-Central Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Southwest"){
      plot <- plot + labs(
        subtitle = paste0("Southwest Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Cape"){
      plot <- plot + labs(
        subtitle = paste0("Cape Girardeau Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "West-Central"){
      plot <- plot + labs(
        subtitle = paste0("West-Central Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Springfield"){
      plot <- plot + labs(
        subtitle = paste0("Springfield Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Ozark Mountains"){
      plot <- plot + labs(
        subtitle = paste0("Ozark Mountains Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Kansas City"){
      plot <- plot + labs(
        subtitle = paste0(as.character(plot_values$plot_date), " through ", as.character(plot_values$date))
      )
    } else if (subtype == "Northeastern"){
      plot <- plot + labs(
        subtitle = paste0("Northeastern Missouri Focus\n",as.character(plot_values$plot_date), 
                          " through ", as.character(plot_values$date))
      )
    }
    
  }
  
  return(plot)
  
}

