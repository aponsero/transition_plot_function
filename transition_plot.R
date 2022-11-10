transition_plot <- function(meta, facet_by, single_edges = F, cols = NULL, data = F) { 
  
  conflict_prefer("filter", "dplyr", quiet = T)
  if(is.null(cols)) { cols <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "BrBG")))(length(levels(meta$DMM))) }

  nodes_full <- meta %>%
    filter(!is.na(DMM)) %>% droplevels 
  
  nodes <- nodes_full %>%
    count(time, DMM, {{facet_by}}, name = "size_DMM") %>%
    arrange(DMM) %>% 
     mutate(DMM = fct_rev(fct_inorder(DMM)))

  DMM_levels <- nodes %>% 
    pull(DMM) %>% 
    levels
  
  scaffold_df <- meta %>% expand(pig_ID, time) 

  meta_edge_prep <- left_join(scaffold_df, meta, by = c("pig_ID", "time"))
  
  meta_edge <- meta_edge_prep %>%
    group_by(pig_ID) %>%
    mutate(DMM_t1=lead(DMM),
           time_t1=lead(time)) %>%
    unite(trans, c("DMM", "DMM_t1"), sep="~") %>%
    unite(time_int, c("time", "time_t1"), sep="~") %>%
    filter(!str_detect(trans, "NA")) %>% ungroup %>% #only time-matched transitions considered
    count(time_int, trans, {{facet_by}}, name = "edge") %>%
    separate(trans, c("DMM", "DMM_t1"), sep = "~") %>%
    separate(time_int, c("time", "time_t1"), sep = "~") %>%
    mutate(across(c(DMM, DMM_t1), ~factor(., levels = DMM_levels))) %>%
    mutate(across(c(time, time_t1), ~factor(., levels = levels(nodes$time)))) %>%
    group_by(time, {{facet_by}})
  
  meta_edge %<>%
    filter(!time==time_t1) %>%
    mutate(total_trans = sum(edge)) %>% ungroup %>% # % transitions to given DMM cluster / total transitions within time window
    mutate(perc = edge/total_trans)
  
  if(!single_edges) { meta_edge %<>% filter(!edge==1) }
  
  facet_by_quo <- enquo(facet_by)
  if(!rlang::quo_is_missing(facet_by_quo)) { 
  message(str_c("Plot is facetted by ", str_c(meta_edge %>% pull({{facet_by}}) %>% levels, collapse = "/"), ".")) }
  message(str_c("Number of transitions is ", meta_edge %>% count({{facet_by}}, wt = edge) %>% pull(n) %>% str_c(., collapse = "/"), " (between time points)."))
  message(str_c("Number of edges plotted is ", meta_edge %>% count({{facet_by}}) %>% pull(n) %>% str_c(., collapse = "/"), " (between time points)."))
  
  edges_gg <- meta_edge %>%
    arrange(perc) %>%
    ggplot(aes(x=time, xend=time_t1, y=DMM, yend=DMM_t1, size=edge, color=perc)) +
    geom_segment() + 
    scale_y_discrete(drop=FALSE) +
    scale_x_discrete(drop=FALSE, position = "top") +
    scale_colour_gradientn(colors = colorRampPalette(RColorBrewer::brewer.pal(6, "BuPu"))(20)[3:20], limits=c(0.02,0.3), oob = scales::squish, name="Transition \nFrequency", breaks=c(0.02, 0.1, 0.2, 0.3), labels=c("<2%", " 10%", " 20%", ">30%")) +
    scale_size_continuous(guide= "none", range=c(0.5,5.5)) + 
    xlab("Sampling time") + ylab("DMM cluster") +
    theme(panel.background = element_blank(), panel.grid.major.x = element_line(color = "grey", size = 0.2,linetype = 2), panel.grid.major.y = element_line(color = "grey", size = 0.2,linetype = 2))
  
  nodes_gg <- nodes %>%
    ggplot(aes(x=time, y=DMM, size=size_DMM, fill=DMM)) +
    geom_point(shape=21) +
    scale_y_discrete(drop=FALSE) +
    scale_fill_manual(values=cols) +
    theme_minimal(base_family=theme_get()$text$family) +
    theme(panel.grid = element_blank(), panel.background = element_blank()) +
    scale_size_continuous(range=c(1.5, 12)) + 
    scale_x_discrete(position = "top", drop = F) +
    xlab(NULL) + ylab(NULL) +
    theme(legend.position = "none", panel.background = element_blank(), strip.text= element_text(color = "transparent"))
  
  facet_by_quo <- enquo(facet_by)
  if(!rlang::quo_is_missing(facet_by_quo)) {
    nodes_gg <- nodes_gg + ggforce::facet_row(vars({{facet_by}}), strip.position = "right") 
    edges_gg <- edges_gg + ggforce::facet_row(vars({{facet_by}}), strip.position = "right") + theme(strip.text.x = element_markdown(), strip.text.y = element_markdown())
  }

  require(cowplot)
  conflict_prefer("align_plots", "cowplot", quiet = T)
  aligned_plots <- align_plots(edges_gg, nodes_gg, align = "hv", axis = "tblr")
  p_comb <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
  
  if(!data) { return(p_comb) }
  if(data) { return(list(p = p_comb, nodes = nodes, edge = meta_edge)) }
}