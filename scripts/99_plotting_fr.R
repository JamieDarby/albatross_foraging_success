
# Plot out the full tracks ------------------------------------------------

# Load in a bathymetric raster to be the plot backdrop
require(marmap)

# Bring in the bathy raster
bathy_rstr <- marmap::getNOAA.bathy(lon1 = -68, lat1 = -67,
                                    lon2 = 0, lat2 = -28,
                                    resolution = 4)

# Plot it out
plot(abs(as.raster(bathy_rstr)))

# Convert to df
bathy_raster_df <- as.data.frame(rasterToPoints(as.raster(bathy_rstr / 1000)))

# NA all positive (land) values
bathy_raster_df$layer[which(bathy_raster_df$layer >= 0)] <- NA

# Plot out the data
waal_bbal_map <-
  plot_grid(
    ggplot() +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.key = element_blank(),
        # legend.box = "vertical",
        legend.text = element_text(size = 11),
        aspect.ratio = 1) +
    coord_cartesian(expand = F) +
    geom_raster(data = bathy_raster_df, aes(x = x, y = y, fill = abs(layer)),
                na.rm = T, alpha = 1) +
    scale_fill_continuous(trans = "reverse", guide = "none",
                          low = "#0f2b3b", high = "#809bab",
                          na.value = "#424242") +
  labs(x = "Longitude", y = "Latitude", colour = "", shape = "", title = "A") +
  scale_colour_viridis_d(begin = 0.9, end = 0, option = "A") +
  geom_path(data = waal_int_bi,
            aes(x = Longitude,
                y = Latitude,
                group = id,
                colour = "Wandering albatross (n = 33)"),
            alpha = 0.8,
            linewidth = 0.7) +
  geom_path(data = bbal_int,
            aes(x = Longitude,
                y = Latitude,
                group = id,
                colour = "Black-browed albatross (n = 48)"),
            alpha = 0.8,
            linewidth = 0.7) +
  geom_point(aes(x = -38.07, y = -54.015, shape = "Bird Island, South Georgia"),
             colour = "#7d0101",
             size = 6, stroke = 2) +
    scale_shape_manual(values = c(10)) +
  scalebar(location = "topright",
           y.min = -60, y.max = -50,
           x.min = -30, x.max = -10,
           dist_unit = "km", dist = 500,
           st.size = 4, height = 0.05,
           st.bottom = T, transform = T,
           st.dist = .1,
           box.color = "black",
           box.fill = c("darkgrey", "white"),
           st.color = "black"))


# Combine activity model plots --------------------------------------------

act_full_plot <-
  plot_grid(bbal_act_plot_c + panel_border(size = 0.5, colour = "black"),
            waal_bi_act_plot_c + panel_border(size = 0.5, colour = "black"), 
            nrow = 1, rel_widths = c(0.5, 1))

# Test it out
act_full_plot

# Save it off
ggsave(act_full_plot, filename = "plots/revised/figure_2.pdf",
       dpi = 500, height = 6, width = 11.6)

# Combine ingestion model plots -------------------------------------------

ing_full_plot <-
  plot_grid(bbal_ing_plot_c + panel_border(size = 0.5, colour = "black"),
            plot_grid(waal_bi_ing_plot_c, waal_bi_ing_plot_d, nrow = 2) +
              panel_border(size = 0.5, colour = "black"),
            nrow = 2, rel_heights = c(1, 1.6))

# Test it out
ing_full_plot

# Save it off
ggsave(ing_full_plot, filename = "plots/revised/figure_3.pdf",
       dpi = 500, height = 11.6, width = 11.6)

# Wind speed distributions ------------------------------------------------

waal_wind <- plot_grid(ggplot(waal_int_bi) +
  geom_histogram(aes(x = wind_sp, fill = "A"),
                 binwidth = 1, boundary = 0, alpha = 0.6,
                 colour = "black") +
  theme_nice() +
    scale_fill_viridis_d(begin = 0, end = 0, option = "A", guide = "none") +
  scale_x_continuous(limits = c(0, 23)) +
  scale_y_continuous(trans = "log", breaks = c(0, 10, 100, 1000)) +
  labs(x = "Wind speed (m/s)", y = "Track points",
       title = "C", fill = "")) + 
  panel_border(size = 0.5, colour = "black")

bbal_wind <- plot_grid(ggplot(bbal_int) +
  geom_histogram(aes(x = wind_sp, fill = "A"),
                 binwidth = 1, boundary = 0, alpha = 0.6,
                 colour = "black") +
  theme_nice() +
    scale_fill_viridis_d(begin = 0.9, end = 0.9, option = "A", guide = "none") +
  scale_x_continuous(limits = c(0, 23)) +
  scale_y_continuous(trans = "log", breaks = c(0, 10, 100, 1000)) +
  labs(x = "Wind speed (m/s)", y = "Track points",
       title = "B", fill = "")) + 
  panel_border(size = 0.5, colour = "black")

# Combine ingestions per time plots ---------------------------------------

total_ing_time_plot <- plot_grid(bbal_ing_time_plot, waal_ing_time_plot,
                                 nrow = 2, rel_heights = c(1, 1))

# Test
total_ing_time_plot

# Save
ggsave(total_ing_time_plot, filename = "plots/total_ing_time_plot.png",
       dpi = 500, width = 8, height = 8)

# Collate figure 1 --------------------------------------------------------

figure_1 <- plot_grid(plot_grid(waal_bbal_map) + 
                         panel_border(size = 0.5, colour = "black"),
                      plot_grid(plot_grid(bbal_wind, waal_wind, nrow = 2),
                                total_ing_time_plot, nrow = 2,
                                rel_heights = c(0.6, 1)),
                      nrow = 1, rel_widths = c(1, 0.85))

# Save
ggsave(figure_1, filename = "plots/figure_1.pdf",
       dpi = 500, width = 11.6, height = 8)  

# Plot out dwell times in different wind conditions -----------------------

dwell_plot <- plot_grid(plot_grid(bbal_dwell_plot) + 
                          panel_border(size = 0.5, colour = "black"),
                        plot_grid(waal_dwell_plot) + 
                          panel_border(size = 0.5, colour = "black"),
                        nrow = 2)

# Save this off
ggsave(dwell_plot, filename = "plots/revised/dwell_plot.png",
       dpi = 500, width = 11.6, height = 8)  
