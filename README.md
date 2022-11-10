# Visualization of between-time point cluster transitions

The R function “transition_plot” visualizes microbiota progression throughout sampling time points based on Dirichlet multinomial mixtures (DMM) clusters. DMM clustering (or cluster membership determined by other methods) should be performed prior to using this function.

Dot sizes are relative to the number of samples in that DMM cluster. Transition percentages represent the percentage of samples transitioning from a given DMM cluster to another out of the total number of transitions at that time point.

## Installation and dependancies

```{r}

# install required packages 

requiredPackages <- c("ggplot2", "cowplot", "tidyverse", "magrittr", "conflicted", ggtext")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
  install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(requiredPackages)

```

## Import metadata

The metadata used for plotting should contain at least a dataframe with columns including “pig_ID”, “DMM” and “time” (all as factor).

## Function parameters

- meta dataframe containing the abovementioned variables as factor.

- facet_by (Optional) parameter to set a grouping variable.

- single_edges TRUE/FALSE whether single edges should be plotted.

- cols (Optional) parameter to set DMM cluster colors; should be same length as the number of unique DMM clusters in the metadata.

## Function example 

This example is reproducing the Figure 4B from the manuscript “Impact of intestinal microbiota on growth performance of suckling and weaned piglets”

```{r}

# get example metadata
Fig.4B_data <- readr::read_csv('https://raw.githubusercontent.com/aponsero/transition_plot_function/main/Fig.4B_data.txt')
cols <- c("DMM", "time", "pig_ID")
Fig.4B_data <-Fig.4B_data %<>% mutate_at(cols, factor)

# run function and vizualize plot
transition_plot(Fig.4B_data, ADG_group, single_edges=F) 
```
