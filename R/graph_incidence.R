## This is the graphical functions to graph the incidence and exposure

## Define functions ----

#### Plot incidence ----

ggplot_theme <- function(){
  theme_dark(base_size = 12, base_family = "sans") +
    theme(strip.text.x = element_text(face = "bold") ## Make facet titles bold
    # theme(panel.background = element_rect(fill = "white", colour = "black"),
    #       strip.background = element_rect(fill = "black", colour = "black")
           )
}

plot_incidence <- function(incidence_data_input){
  
  incidence_data <- copy(incidence_data_input)
  setnames(incidence_data, "incidence_reported", "Claims")
#  incidence_data %<>% mutate(IBNR = incidence_ind - Claims )
  
  incidence_data %>% mutate(fy_period_label = factor(fy_period_label, ordered = T))
  
  viz_vars <- c("Claims", "IBNR")
  
  data_long <- incidence_data %>% 
    pivot_longer( cols = all_of(viz_vars), names_to = "set", values_to = ".value")
  
  data_long %<>% mutate(set = factor(set, levels = c("IBNR","Claims"), ordered = T))
  
  incidence_plot <- ggplot(data_long, aes(x = fy_period_label, y = .value , fill = set)) +
    geom_col() +
    geom_shadowtext(aes(x = fy_period_label, y = 0, label = fy_period_label), hjust = 0, colour = "white", check_overlap = T, angle = 90) +
    facet_wrap(~fy, nrow = 1) +
    scale_x_discrete("FY") +
    scale_y_continuous("Claims development (count)", breaks = extended_breaks()) +
    scale_colour_brewer(type = "qual", palette = "Spectral", aesthetics = "fill", direction = -1) +
    ggplot_theme() +
    theme(axis.text.x = element_blank(),
          legend.title = element_blank()
    )
  return(incidence_plot)
}

plot_incidence_development <- function(incidence_data_input){
  
  incidence_data <- copy(incidence_data_input)
  setnames(incidence_data, "incidence_reported", "Claims")
#  incidence_data %<>% mutate(IBNR = incidence_ind - Claims )
  
  incidence_data %>% mutate(fy_period_label = factor(fy_period_label, ordered = T))
  
  viz_vars <- c("Claims", "IBNR")
  
  data_long <- incidence_data %>% 
    pivot_longer( cols = all_of(viz_vars), names_to = "set", values_to = ".value")
  
  data_long %<>% mutate(set = factor(set, levels = c("IBNR","Claims"), ordered = T))
    
  incidence_plot <- ggplot(data_long, aes(x = fy_period_label, y = .value , fill = set)) +
    geom_col() +
    geom_shadowtext(aes(x = fy_period_label, y = 0, label = fy_period_label), hjust = 0, colour = "white", check_overlap = T, angle = 90) +
    facet_wrap(~fy, nrow = 1) +
    scale_x_discrete("FY") +
    scale_y_continuous("Claims development (count)", breaks = extended_breaks()) +
    scale_colour_brewer(type = "qual", palette = "Spectral", aesthetics = "fill", direction = -1) +
    ggplot_theme() +
    theme(axis.text.x = element_blank(),
          legend.title = element_blank()
          )
    return(incidence_plot)
}
