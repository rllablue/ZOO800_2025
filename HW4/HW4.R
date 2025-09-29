##########################
### ZOO800: HOMEWORK 4 ###
##########################

# Authors: Aubrey Wendorff, Jillian Neece, Rebekkah LaBlue, & Victoria Salerno
# Project: Functions
# Due: Septermber 29, 2025


##############
### SET UP ###
#############

library(palmerpenguins)
library(dplyr)
library(magrittr)
library(ggplot2)


###################
### OBJECTIVE 1 ###
###################

# Create binarizing function
Binarize.mass <- function(breakpoint, labels = c("small", "large")) { # flexible labels within function argument as opposed to hard-coded later
  penguins %>%
    mutate(
      body_mass_g_binary = ifelse( # create new binarized mass column defined by following logic
        body_mass_g > breakpoint, # if mass is greater than the breakpoint value, then
        labels[2], # assign penguin to "large" (in labels, 2 indexes large)
        labels[1] # otherwise, assign penguin to "small" (in labels, 1 indexes small)
      )
    )
}

# Apply function
penguins.binary.df <- Binarize.mass(
  breakpoint = mean(penguins$body_mass_g, na.rm = TRUE), # break is specified around mean mass value
  labels = c("small", "large") # match labels
)

# Visualize results (counts)
table(penguins.binary.df$body_mass_g_binary) # shows how many individuals fall into each breakpoint category


###################
### OBJECTIVE 2 ###
###################

# Create categorizing function (size)
Categorize.size <- function(breakpoints, labels = c("small", "medium", "large")) { # flexible label enccoding (will require 3 breaks to match)
  penguins %>%
    mutate(
      body_mass_g_category = cut( # split range of values, define by labels 
        body_mass_g, # data to be categorized
        breaks = breakpoints, # argument for source of bin length/value
        labels = labels, # argument for source of bin names
        include.lowest = TRUE # forces smallest value to be binned even if its exactly equal to value of smallest break (instead of NA)
      )
    )
}

# Apply function
penguins.size.df <- Categorize.size(
  breakpoints = 3,
  labels = c("small", "medium", "large")
)

# Visualize results (counts)
table(penguins.size.df$body_mass_g_category) # shows how many individuals fall into each breakpoint category


###################
### OBJECTIVE 3 ###
###################

# Create categorizing function (species:size)
Categorize.species <- function(breakpoints = c(0.33, 0.67), labels = c("small", "medium", "large")) { # flexible encoding of breaks, labels
  species_list <- penguins$species %>% na.omit() %>% unique() # tidy chr vector of unique species names (Gentoo, Adelie, Chinstrap)
  species_categorized_list <- list() # readies empty list to store results in later
  
  for (sp in species_list) { # automate with for-loop across individuals, sizes, species
   species_df <- filter(penguins, species == sp) # subset species to only sp in for-loop, make small df for sp calculations
   quantile_values <- quantile(species_df$body_mass_g, probs = breakpoints, na.rm = TRUE) # apply quantile breaks named in function call (0.33, 0.67) to mass data
   breakpoint_vector <- c(min(species_df$body_mass_g, na.rm = TRUE),
                          quantile_values,
                          max(species_df$body_mass_g, na.rm = TRUE)) 
                          # define full range of body masses per species (min, max) because quantiles will be different for each species 
                          # ie. we dont want quantiles across all three species
                          # cant calc quantiles if you don't know values of interval min, max
   
   cat("\nBreakpoints for", sp, ":\n") # \n = new line, keeps things tidy, readable
   print(breakpoint_vector)
   
   species_df <- species_df %>% # categorize, store in df species specific body mass using cut
     mutate(
       body_mass_g_category = cut(
         species_df$body_mass_g,
         breaks = breakpoint_vector,
         labels = labels,
         include.lowest = TRUE
       )
     )

   species_categorized_list[[sp]] <- species_df # store dfs per sp in list pre-made above; saves each sp data separately
  }
  
  result_df <- bind_rows(species_categorized_list) # combine into single resutls dataframe
  return(result_df)
}
  
# Apply function
penguins.species.df <- Categorize.species(
  breakpoints = c(0.33, 0.67),
  labels = c("small", "medium", "large")
)

# Visualize results (counts)
table(penguins.species.df$species, penguins.species.df$body_mass_g_category) # shows how many individuals fall into each breakpoint category


###################
### OBJECTIVE 4 ###
###################

# Visualize with boxplot

Categorize.species.plot <- function(breakpoints = c(0.33, 0.67), labels = c("small", "medium", "large")) { # flexible encoding of breaks, labels
  species_list <- penguins$species %>% na.omit() %>% unique() # tidy chr vector of unique species names (Gentoo, Adelie, Chinstrap)
  species_categorized_list <- list() # readies empty list to store results in later
    
  for (sp in species_list) { # automate with for-loop across individuals, sizes, species
    species_df <- filter(penguins, species == sp) # subset species to only sp in for-loop, make small df for sp calculations
    quantile_values <- quantile(species_df$body_mass_g, probs = breakpoints, na.rm = TRUE) # apply quantile breaks named in function call (0.33, 0.67) to mass data
    breakpoint_vector <- c(min(species_df$body_mass_g, na.rm = TRUE),
                            quantile_values,
                            max(species_df$body_mass_g, na.rm = TRUE)) 
    # define full range of body masses per species (min, max) because quantiles will be different for each species 
    # ie. we dont want quantiles across all three species
    # cant calc quantiles if you don't know values of interval min, max
      
    cat("\nBreakpoints for", sp, ":\n") # \n = new line, keeps things tidy, readable
    print(breakpoint_vector)
      
    species_df <- species_df %>% # categorize, store in df species specific body mass using cut
      mutate(
        body_mass_g_category = cut(
          species_df$body_mass_g,
          breaks = breakpoint_vector,
          labels = labels,
          include.lowest = TRUE
        )
      )
      
    species_categorized_list[[sp]] <- species_df # store dfs per sp in list pre-made above; saves each sp data separately
  }
    
  result_df <- bind_rows(species_categorized_list) # combine into single results dataframe
  
  ## New Filter for Plot
  result_df_filtered <- result_df %>% # remove NAs before plotting
    filter(!is.na(body_mass_g) & !is.na(body_mass_g_category))
    
    ## Plot Adjustement
    penguin_plot <- ggplot(result_df_filtered, aes(x = body_mass_g_category, y = body_mass_g, fill = species)) +
      geom_boxplot() + 
      facet_wrap(~species, scales = "free") +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Penguin Body Mass by Species and Size Class",
        x = "Size Class", 
        y = "Body Mass (g)",
        caption = "Figure 1. Size and body mass classes assigned based on species-specific quantiles using the 33rd and 67th percentiles. Data from palmerpenguins package in R.",
        fill = "Species"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", margin = margin(b = 20)),
            axis.title.x = element_text(margin = margin(t = 20)),
            axis.title.y = element_text(margin = margin(r = 20)),
            plot.caption = element_text(hjust = 0.5, size = 10, face = "italic", margin = margin(t = 20)),
            legend.position = "none")
  
    return(list(data = result_df_filtered, plot = penguin_plot))
    }

# Apply function, view plot
Categorize.species.plot()$plot


###################
### OBJECTIVE 5 ###
###################

# Create flexible categorizing function with quantile()
Categorize.species.flex <- function(labels = c("small", "medium", "large")) { # flexible encoding of breaks, labels
  species_list <- penguins$species %>% na.omit() %>% unique() # tidy chr vector of unique species names (Gentoo, Adelie, Chinstrap)
  species_categorized_list <- list() # readies empty list to store results in later
  
  for (sp in species_list) { # automate with for-loop across individuals, sizes, species
    species_df <- filter(penguins, species == sp) # subset species to only sp in for-loop, make small df for sp calculations
    
    ## Quantile Adjustment
    quantile_values <- seq(0, 1, length.out = length(labels) + 1) # quantile distribution min 0, max 1
                                                                   # make number of breakpoints equal to number of label names
                                                                   # +1 to make enough quantile bins (ex. need 4 edges to make 3 bins)
    breakpoint_vector <- quantile(species_df$body_mass_g, probs = quantile_values, na.rm = TRUE) # apply quantiles (bins at certain probabilities) to data of specified length
    
    cat("\nBreakpoints for", sp, ":\n") # \n = new line, keeps things tidy, readable
    print(breakpoint_vector)
    
    species_df <- species_df %>% # categorize, store in df species specific body mass using cut
      mutate(
        body_mass_g_category = cut(
          species_df$body_mass_g,
          breaks = breakpoint_vector,
          labels = labels,
          include.lowest = TRUE
        )
      )
    
    species_categorized_list[[sp]] <- species_df # store dfs per sp in list pre-made above; saves each sp data separately
  }
  
  result_df <- bind_rows(species_categorized_list) # combine into single resutls dataframe
  return(result_df)
}

# Apply function
penguins.species.flex.df <- Categorize.species.flex(
  labels = c("small", "medium", "large")
)

# Visualize results (counts)
table(penguins.species.flex.df$species, penguins.species.flex.df$body_mass_g_category) # shows how many individuals fall into each breakpoint category

