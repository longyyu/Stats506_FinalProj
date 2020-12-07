# Stats 506, F20, Final Project

# This script computes the proportion of different wall materials of US 
# commercial buildings by construction years and census regions. 

# Data Source: 
# the 2012 US Commercial Building Energy Consumption Survey (CBECS)
# https://www.eia.gov/consumption/commercial/data/2012/index.php?view=microdata

# Author: Yanyu Long, longyyu@umich.edu
# Updated: December 7, 2020

# 79: -------------------------------------------------------------------------
# setwd("E:/git/Stats506_private/final_proj")

# Libraries: ------------------------------------------------------------------
library(data.table) # we will be using data.table for computation
library(dplyr) # to make use of piping `%>%`
library(ggplot2) # for plotting

# Data: -----------------------------------------------------------------------

## specify file path
data_lib = "./data/"
cbecs_file_2012 = paste0(data_lib, "2012_public_use_data_aug2016.csv")
cb_file_2012 = paste0(data_lib, "2012microdata_codebook.xlsx")

## data processing helper functions
split_level_label = function(mapping){
  # Extract level and label information from string `mapping`
  # Input: mapping - the string to be splitted
  # Output: a length-2 character list with the first element being level and 
  #         the second being label
  # Example: split_level_label("'1' = 'Northeast'") -> c("1", "Northeast")
  mapping_splitted = {mapping %>% stringr::str_split(pattern = "=")}[[1]]
  names(mapping_splitted) = c("level", "label")
  return(
    sapply(mapping_splitted, 
           function(x) gsub(pattern = "'", replacement = "", x) %>%
                       stringr::str_trim())
  )
}

decode_factor = function(var_to_decode, var_cb, codes){
  # Apply factor labels to variables
  # Inputs:
  #   var_to_decode - vector, input vector to be changed to factor
  #   var_cb - char, variable name in the codebook
  #   codes - data.table, a codebook of factor levels and labels
  # Output: var_to_decode converted to a factor with levels given in codes
  with(
    codes %>% copy() %>%
      .[variable == var_cb, ] %>%
      .[, levels := lapply(levels, as.numeric)],
    factor(
      as.numeric(var_to_decode),
      levels = levels[[1]], labels = labels[[1]],
      ordered = TRUE
    )
  )
}

## code book data
codebook = readxl::read_xlsx(cb_file_2012) %>%
  as.data.table() %>%
  .[, .(variable = `Variable\r\nname`, 
        mapping_str = `Values/Format codes`)] %>%
  .[variable %in% c("REGION", "YRCONC", "WLCNS"), ] %>%
  .[, mapping_list := stringr::str_split(mapping_str, pattern = "\\r\\n")] %>%
  # unnest the code book to extract levels and labels information
  # from `mapping_list`
  .[, .(mapping = unlist(mapping_list)), by = variable] %>%
  .[, mapping_splitted := lapply(mapping, split_level_label)] %>%
  .[, `:=`(level = sapply(mapping_splitted, '[', 'level'),
           label = sapply(mapping_splitted, '[', 'label'))] %>%
  # nest the code book back to one row per variable
  .[, .(levels = list(level), labels = list(label)), by = variable]


## 2012 CBECS and replicate weights data
cbecs = read.delim(cbecs_file_2012, sep = ",") %>%
  as.data.table()
col_wt = names(cbecs)[grep("FINALWT[0-9]", names(cbecs))]
rep_weights = cbecs[, .SD, .SDcols = c("PUBID", col_wt)] %>%
  melt(id.vars = "PUBID",
       measure.vars = patterns("^FINALWT"),
       variable.name = "rep",
       value.name = "rep_wt") %>%
  setnames(old = "PUBID", new = "id") %>%
  .[, rep := gsub("FINALWT", "", rep) %>% as.integer()]

cbecs = cbecs[, .(
  id = PUBID, wt = FINALWT,
  region = decode_factor(REGION, "REGION", codebook), 
  yr_conc = decode_factor(YRCONC, "YRCONC", codebook),
  wall_material = decode_factor(WLCNS, "WLCNS", codebook)
)]

# Compute point estimates and CIs ---------------------------------------------
calc_estimate = function(vars_group, var_target) {
  # Compute point estimates and 95% confidence intervals 
  # for variable `var_target`, grouped by `vars_group`
  # Inputs:
  #   vars_group - a vector of strings, names of variables to group data by
  #   var_target - a string, name of the target variable
  # Output: 
  #   a data.table with the following columns:
  #   vars_group, var_target, percent, lwr, upr
  
  point_est = cbecs %>%
    .[, .(weighted_freq = sum(wt)), 
      by = c(vars_group, var_target)] %>%
    .[, percent := weighted_freq / sum(weighted_freq) * 100, 
      by = vars_group] %>%
    .[, -"weighted_freq"]
  
  point_est_rep = cbecs %>%
    merge(rep_weights, by = "id") %>%
    .[, .(weighted_freq_rep = sum(rep_wt)), 
      by = c(vars_group, "rep", var_target)] %>%
    .[, percent_rep := weighted_freq_rep / sum(weighted_freq_rep) * 100, 
      by = c(vars_group, "rep")] %>%
    .[, -"weighted_freq_rep"]
  
  variance = point_est %>%
    merge(point_est_rep, by = c(vars_group, var_target)) %>%
    .[, .(var = sum({(percent_rep - percent)^2})), 
      by = c(vars_group, var_target)]
  
  m = qnorm(0.975)
  return(
    point_est %>%
      merge(variance, by = c(vars_group, var_target)) %>%
      .[, `:=`(lwr = percent - m * sqrt(var),
               upr = percent + m * sqrt(var))] %>%
      .[, -"var"]
  )
}

est_by_yr = calc_estimate(
  vars_group = "yr_conc", 
  var_target = "wall_material"
)

est_by_yr_region = calc_estimate(
  vars_group = c("region", "yr_conc"), 
  var_target = "wall_material"
)

# Create figures to present the data ------------------------------------------
dodge_width = .8
wall_materials_old = levels(cbecs$wall_material)
wall_materials_new = gsub(" \\(.*\\)", "", wall_materials_old) %>%
  stringr::str_wrap(width = 40)

pic_yr = copy(est_by_yr) %>%
  .[, wall_material := factor(
    as.character(wall_material),
    levels = wall_materials_old, 
    labels = wall_materials_new
  )] %>%
  ggplot(aes(
    x = reorder(yr_conc, desc(yr_conc)), 
    y = percent, ymin = lwr, ymax = upr
  )) +
  theme_bw() +
  geom_hline(
    yintercept = 50, lwd = .8,
    color = "grey70", lty = "dashed"
  ) +
  geom_point(
    size = 1.5, color = "#377eb8",
    position = position_dodge(width = dodge_width),
  ) +
  geom_errorbar(
    color = "#377eb8",
    position = position_dodge(width = dodge_width),
    lwd = .8, width = .4, alpha = .6
  ) +
  scale_y_continuous(name = "% of Wall Materials") +
  xlab("Year of Construction") +
  theme(
    axis.text.x = element_text(size = 9),
    legend.text = element_text(size = 10)
  ) +
  facet_wrap(~wall_material, ncol = 3) + 
  coord_flip()

pic_yr_region = copy(est_by_yr_region) %>%
  .[, wall_material := factor(
        as.character(wall_material),
        levels = wall_materials_old, 
        labels = wall_materials_new
      )] %>%
  ggplot(aes(
    x = reorder(yr_conc, desc(yr_conc)), 
    y = percent, ymin = lwr, ymax = upr,
    color = region
  )) +
  theme_bw() +
  geom_hline(
    yintercept = 50, lwd = .8,
    color = "grey70", lty = "dashed"
  ) +
  geom_point(
    size = 1.5,
    position = position_dodge(width = dodge_width),
  ) +
  geom_errorbar(
    position = position_dodge(width = dodge_width),
    lwd = .8, width = .4, alpha = .6
  ) +
  scale_color_brewer(name = "Census Region", palette = "Set1") + 
  scale_y_continuous(name = "% of Wall Materials") +
  xlab("Year of Construction") +
  theme(
    axis.text.x = element_text(size = 9),
    legend.text = element_text(size = 10)
  ) +
  facet_wrap(~wall_material, ncol = 3) + 
  coord_flip()

# Create table to present data -----------------------------------------------
pack_by_var = function(dt, var, table){
  # A helper function to pack rows in `table` by levels of variable `var`
  # Inputs:
  #   dt - a data.table, the original dataset to be presented as table
  #   var - a character, the name of the variable to pack rows by
  #   table - a kableExtra table, the table to be formatted
  # Output: the formatted version of `table`
  
  # compute the row span for each level of variable `var`
  level_rowidx = dt %>%
    # create deep copy to avoid altering the original data.table
    copy() %>%
    .[, rowidx := 1:.N] %>%
    .[, .(min = min(rowidx), max = max(rowidx)), by = var] %>%
    setnames(old = var, new = "level")
    # mutate(rowidx = 1:nrow(dt)) %>%
    # group_by(.data[[var]]) %>%
    # summarise(min = min(rowidx), max = max(rowidx), .groups = "drop") %>%
    # rename(level = .data[[var]])
  for (idx in 1:nrow(level_rowidx)){
    row = level_rowidx[idx, ]
    table = table %>%
      kableExtra::pack_rows(
        group_label = row$level, row$min, row$max
      )
  }
  return(table)
}

est_table = copy(est_by_yr_region) %>% 
  .[, pe_ci := sprintf("<div>%4.2f</div> <div>(%4.2f, %4.2f)</div>", 
                       percent, lwr, upr)] %>%
  dcast(yr_conc + wall_material ~ region,
        value.var = "pe_ci") %>%
  .[order(yr_conc, wall_material),]

html_table = est_table[, -"yr_conc"] %>%
  .[, wall_material := factor(
    as.character(wall_material),
    levels = wall_materials_old, 
    labels = wall_materials_new
  )] %>%
  knitr::kable(
    format = 'html', 
    escape = FALSE, 
    align = 'lcccc', 
    col.names = c("Wall Material", names(est_table)[3:6])
  ) %>%
  kableExtra::row_spec(row = 0, align = "c") %>%
  kableExtra::kable_styling('striped', full_width = TRUE) %>%
  pack_by_var(dt = est_table, var = "yr_conc")
