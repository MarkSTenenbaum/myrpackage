library(tidyverse)
options(dplyr.summarise.inform = FALSE)


addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 8, spaceLegend = 1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize),
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

library(showtext)
library(ggthemes)
library(ggsci)

# Plot theme
theme_gg <- function(base_size = 14, base_family = "font", legend = FALSE){
  font_add_google('Alegreya Sans', "font")
  showtext_auto()
  showtext_opts(dpi = 300)

  if(legend == TRUE){
    theme_minimal() +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(text=element_text(size=14, family="font"))
  }
  else{
    theme_minimal() +
      theme(legend.position = "none") +
      theme(legend.title = element_blank()) +
      theme(text=element_text(size=14, family="font"))
  }
}
# weighted proportions
propfuncwt <- function(data, var, weight, group){
  if(missing(group)){
    probs2 <- data %>%
      dplyr::count({{var}}, wt = {{weight}}) %>%
      mutate(pct = prop.table(n))
    return(probs2)
  }
  else{
    probs <- data %>%
      group_by({{ group }}) %>%
      dplyr::count({{var}}, wt = {{weight}}) %>%
      mutate(pct = prop.table(n))
    return(probs)
  }
}

# unweighted proportions
propfunc <- function(data, var, group){
  if(missing(group)){
    probs2 <- data %>%
      dplyr::count({{var}}) %>%
      mutate(pct = prop.table(n))
    return(probs2)
  }
  else{
    probs <- data %>%
      group_by({{ group }}) %>%
      dplyr::count({{var}}) %>%
      mutate(pct = prop.table(n))
    return(probs)
  }
}
