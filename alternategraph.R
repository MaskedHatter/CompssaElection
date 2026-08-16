President <- as.data.frame(VerifiedVotes_D$president)

colnames(President) <- "candidate"

# 2. Generate the aesthetic chart using your President data frame
ggplot(President, aes(x = fct_infreq(candidate), fill = candidate)) +
  # Create bars by counting the raw categorical data
  geom_bar(width = 0.6, alpha = 0.9, show.legend = FALSE) +
  
  # Add the calculated vote counts to the end of each bar
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5,
            #hjust = -0.2, 
            size = 4.5, 
            fontface = "bold", 
            color = "#333333") +
  
  # Flip horizontally for better readability
  #coord_flip() +
  
  # Apply your custom colors (add more hex codes if you have more than 4 candidates)
  scale_fill_manual(values = c("#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  
  # Strip away default gray backgrounds and grid lines
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),   
    axis.text.y = element_text(face = "bold", color = "black"), 
    axis.title = element_blank(),         
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "#666666", size = 12, margin = margin(b = 20))
  ) +
  
  # Set the context for your Verified Votes
  labs(
    title = "Official Presidential Election Results",
    subtitle = "Total verified votes cast per candidate"
  ) +
  
  # Prevent data labels from being clipped by the edge of the screen
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
