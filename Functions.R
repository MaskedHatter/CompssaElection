VerticalChart <- function(Position, Title)
{
  
  PositionString <- deparse(substitute(Position))
  
  # 2. Generate the aesthetic chart using your Position data frame
  ggplot(Position, aes(x = fct_infreq(candidate), fill = fct_infreq(candidate))) +
    # Create bars by counting the raw categorical data
    geom_bar_rounded(width = 0.6, alpha = 0.9, show.legend = FALSE) +
    
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
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(),   
      axis.text.x = element_text(face = "bold", color = "black"), 
      axis.title = element_blank(),         
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
      plot.subtitle = element_text(color = "#666666", size = 12, margin = margin(b = 20))
    ) +
    
    # Set the context for your Verified Votes
    labs(
      title = Title,
      subtitle = "Total verified votes cast per candidate"
    ) +
    
    # Prevent data labels from being clipped by the edge of the screen
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  ggsave(paste0(address, "results/", PositionString, ".png"))
}


PieChart <- function(Position, Title) {
  
  # 1. Capture the name of the data frame as a string for the file name
  PositionString <- deparse(substitute(Position))
  
  # 2. Generate the pie chart using raw categorical data
  # We assign it to a variable 'p' so we can save it and print it
  p <- ggplot(Position, aes(x = "", fill = candidate)) +
    
    # geom_bar automatically counts the raw rows. width = 1 removes the center hole.
    geom_bar(width = 1, color = "white") +
    
    # NEW: Add the percentage labels
    geom_text(stat = "count", 
              # Calculate count / total, then format as a percentage (e.g., 45%)
              aes(label = scales::percent(after_stat(count / sum(count)), accuracy = 1)), 
              # position_stack puts the label perfectly in the middle of each pie slice
              position = position_stack(vjust = 0.5), 
              size = 5, 
              fontface = "bold", 
              color = "white") + # White text usually pops best on colored pie slices
    
    # Curve the stacked bar chart into a pie chart based on the Y-axis (counts)
    coord_polar(theta = "y", start = 0) +
    
    # Apply your custom colors
    scale_fill_manual(values = c("#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
    
    # NEW: Add the labels argument right inside your manual colors
    scale_fill_manual(
      values = c("#2A9D8F", "#E9C46A", "#F4A261", "#E76F51"),
      labels = scales::label_wrap(15) 
    ) +
    
    # NEW: Tell ggplot to draw the legend items row-by-row so spacing works
    guides(fill = guide_legend(byrow = TRUE)) +
    
    # Use theme_void to remove the background, grid lines, and axes
    theme_void(base_size = 14) +
    
    # Add back some styling for the title and subtitle (hjust = 0.5 centers them)
    theme(
      
      legend.key.size = unit(0.5, "cm"),
      # NEW: Notice the word 'key' is added here
      legend.key.spacing.y = unit(0.5, "cm"),
      legend.text = element_text(face="bold", color="black"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10), hjust = 0.5),
      plot.subtitle = element_text(color = "#666666", size = 12, margin = margin(b = 20), hjust = 0.5)
    ) +
    
    # Set the context for your Verified Votes
    labs(
      title = Title,
      subtitle = "Total verified votes cast per candidate",
      fill = "Candidate" # Capitalizes the legend title
    )
  
  # 3. Save the file
  # I added "_Pie" to the filename so it doesn't overwrite your vertical bar chart!
  ggsave(paste0(address, "results/", PositionString, "_Pie.png"), bg="white")
  
  # 4. Display the chart in RStudio
  print(p)
}

TotalVotersChart <- function(VotesCastTable, RegVotersTable) {
  VotesCast <- nrow(VotesCastTable)
  RegVoters <- nrow(RegVotersTable)
  
  chart_data <- data.frame(
    category = c("Voted", "Remaining"),
    amount = c(VotesCast, RegVoters - VotesCast)
  )
  
  # 3. Build the pie chart using geom_col() since we have exact numbers
  p <- ggplot(chart_data, aes(x = "", y = amount, fill = category)) +
    
    # geom_col uses the exact 'amount' values we provided in the data frame
    geom_col(width = 1, color = "white", key_glyph = "polygon") +
    
    # Add the percentage labels directly to the slices
    geom_text(aes(label = scales::percent(amount / RegVoters, accuracy = 1)), 
              position = position_stack(vjust = 0.5), 
              size = 6, 
              fontface = "bold", 
              color = "white") + 
    
    # Curve the columns into a pie chart
    coord_polar(theta = "y", start = 0) +
    
    # Apply two custom colors for your two categories
    scale_fill_manual(values = c("#E76F51", "#2A9D8F")) +
    
    theme_void(base_size = 14) +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key.spacing.y = unit(0.5, "cm"), # Adjust spacing if needed
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10), hjust = 0.5)
    ) +
    labs(
      title = "Voter Turnout",
      fill = "Status" 
    )
  
  ggsave(paste0(address, "results/VotersTurnout.png"), bg="white")
  
  # 4. Display the chart in RStudio
  print(p)

}

TotalVotersBarChartByDepartment <- function(RawVotesData) {
  
  p <- ggplot(RawVotesData, aes(x = fct_infreq(department), fill = department)) +
    
    # 1. Use the rounded bar, and let it do the counting automatically
    geom_bar_rounded(width = 0.6, alpha = 0.9, show.legend = FALSE) +
    
    # 2. Tell the text layer to also calculate the count
    geom_text(stat = "count", aes(label = after_stat(count)),
              vjust = -0.5,
              size = 4.5, 
              fontface = "bold", 
              color = "#333333") +
    
    scale_fill_manual(values = c("#264653", "#2A9D8F", "#8AB17D", "#E9C46A", "#F4A261", 
                                 "#E76F51", "#9B2226", "#457B9D", "#6D597A")) +
    scale_x_discrete(labels = scales::label_wrap(12)) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(),   
      axis.text.x = element_text(face = "bold", color = "black"), 
      axis.title = element_blank(),         
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
      plot.subtitle = element_text(color = "#666666", size = 12, margin = margin(b = 20))
    ) +
    
    labs(
      title = "Votes Cast by Department",
      subtitle = "Total verified votes cast per department" 
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  
  ggsave(paste0(address, "results/VotesCastbyDepartment.png"), plot = p)
  
  print(p)
  
}


TotalVotersPieChartByDepartment <- function(VotesCastTable, RegVotersTable, Department) {
  
  VotesCast <- nrow(VotesCastTable %>%
                      filter(department == Department))
  RegVoters <- nrow(RegVotersTable %>%
                      filter(department == Department))
  print(RegVoters)
  
  chart_data <- data.frame(
    category = c("Voted", "Remaining"),
    amount = c(VotesCast, RegVoters - VotesCast)
  )
  
  # 3. Build the pie chart using geom_col() since we have exact numbers
  ggplot(chart_data, aes(x = "", y = amount, fill = category)) +
    
    # geom_col uses the exact 'amount' values we provided in the data frame
    geom_col(width = 1, color = "white", key_glyph = "polygon") +
    
    # Add the percentage labels directly to the slices
    geom_text(aes(label = scales::percent(amount / RegVoters, accuracy = 1)), 
              position = position_stack(vjust = 0.5), 
              size = 6, 
              fontface = "bold", 
              color = "white") + 
    
    # Curve the columns into a pie chart
    coord_polar(theta = "y", start = 0) +
    
    # Apply two custom colors for your two categories
    scale_fill_manual(values = c("#E76F51", "#2A9D8F")) +
    
    theme_void(base_size = 14) +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key.spacing.y = unit(0.5, "cm"), # Adjust spacing if needed
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10), hjust = 0.5)
    ) +
    labs(
      title = paste0(Department, " Election Votes Progress"),
      fill = "Status" 
    )
  
}