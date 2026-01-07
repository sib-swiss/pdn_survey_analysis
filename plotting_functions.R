plot_mc_bar <- function(answer_df, qid, question_list, frequency_cutoff = 1) {
  
  # Extract question text
  question_text <- question_list[[qid]]$question
  
  # Get answers for this question (exclude empty responses)
  answers <- answer_df[, qid]
  answers <- answers[!is.na(answers) & answers != ""]
  n_respondents <- length(answers)  # Store number of respondents for percentage calculation
  
  # Handle multiple selections (split by semicolon)
  all_answers <- unlist(strsplit(answers, ";"))
  
  # Count frequencies
  answer_counts <- as.data.frame(table(all_answers)) |>
    rename(answer = all_answers, count = Freq) |>
    arrange(desc(count))
  
  # Apply frequency cutoff
  answer_counts <- answer_counts |>
    mutate(
      answer_display = ifelse(count >= frequency_cutoff, as.character(answer), "Other")
    ) |>
    group_by(answer_display) |>
    summarise(count = sum(count), .groups = "drop") |>
    arrange(desc(count))
  
  # Reorder factor levels for plotting (keep "Other" at the end if present)
  if ("Other" %in% answer_counts$answer_display) {
    other_row <- answer_counts |> filter(answer_display == "Other")
    answer_counts <- answer_counts |> 
      filter(answer_display != "Other") |>
      bind_rows(other_row)
  }
  
  answer_counts$answer_display <- factor(
    answer_counts$answer_display,
    levels = rev(answer_counts$answer_display)
  )
  
  # Calculate percentages
  answer_counts <- answer_counts |>
    mutate(
      percentage = round(count / n_respondents * 100, 0),
      max_count = max(count),
      text_position = ifelse(count < max_count * 0.15, "outside", "inside")
    )
  
  # Create bar chart
  ggplot(answer_counts, aes(x = answer_display, y = count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    # Text inside bars (for long bars)
    geom_text(
      data = answer_counts |> filter(text_position == "inside"),
      aes(label = paste0(count, " (", percentage, "%)")), 
      hjust = 1.05, size = 3.5, color = "white", fontface = "bold"
    ) +
    # Text outside bars (for short bars)
    geom_text(
      data = answer_counts |> filter(text_position == "outside"),
      aes(label = paste0(count, " (", percentage, "%)")), 
      hjust = -0.1, size = 3.5, color = "black"
    ) +
    coord_flip() +
    labs(
      title = str_wrap(question_text, width = 40),
      x = NULL,
      y = "Number of Responses"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 35))
}

# Function to plot multiple barrier questions in one chart
plot_barrier_questions <- function(answer_df, qid_range, question_list, 
                                   grouping_qid = NULL, group1_value = NULL, group2_value = NULL,
                                   significant_label = "Significant Barrier",
                                   not_significant_label = "Not a Significant Barrier",
                                   not_applicable_label = "Not applicable") {
  
  # Check if grouping is requested
  use_grouping <- !is.null(grouping_qid) && !is.null(group1_value) && !is.null(group2_value)
  
  if (use_grouping) {
    # Get grouping variable
    groups <- answer_df[, grouping_qid]
    group1_idx <- which(grepl(group1_value, groups, fixed = TRUE))
    group2_idx <- which(grepl(group2_value, groups, fixed = TRUE))
  }
  
  # Collect data for all questions
  barrier_data <- map_df(qid_range, function(qid) {
    question_text <- question_list[[qid]]$question
    
    if (use_grouping) {
      # Process both groups separately
      process_group <- function(indices, group_name, group_label) {
        answers <- answer_df[indices, qid]
        answers <- answers[!is.na(answers) & answers != ""]
        
        counts <- as.data.frame(table(answers)) |>
          rename(response = answers, count = Freq) |>
          mutate(
            group = group_name,
            group_label = str_wrap(group_label, width = 30),
            question = question_text,
            qid = qid
          )
        return(counts)
      }
      
      data1 <- process_group(group1_idx, "Group 1", group1_value)
      data2 <- process_group(group2_idx, "Group 2", group2_value)
      combined <- bind_rows(data1, data2)
      
      # Ensure all response types exist for both groups
      all_responses <- expand.grid(
        response = c(not_applicable_label, not_significant_label, significant_label),
        group = c("Group 1", "Group 2"),
        stringsAsFactors = FALSE
      )
      
      combined <- all_responses |>
        left_join(combined, by = c("response", "group")) |>
        mutate(
          question = ifelse(is.na(question), question_text, question),
          qid = ifelse(is.na(qid), qid, qid),
          count = ifelse(is.na(count), 0, count),
          group_label = ifelse(group == "Group 1", str_wrap(group1_value, width = 30), str_wrap(group2_value, width = 30))
        )
      
      return(combined)
    } else {
      # No grouping - original behavior
      answers <- answer_df[, qid]
      answers <- answers[!is.na(answers) & answers != ""]
      
      counts <- as.data.frame(table(answers)) |>
        rename(response = answers, count = Freq)
      
      counts$question <- question_text
      counts$qid <- qid
      
      return(counts)
    }
  })
  
  # Calculate percentages
  if (use_grouping) {
    barrier_summary <- barrier_data |>
      group_by(question, qid, group) |>
      mutate(
        total = sum(count),
        percentage = ifelse(total > 0, count / total * 100, 0)
      ) |>
      ungroup()
    
    # Calculate ordering based on average "Significant barrier" percentage
    question_order <- barrier_summary |>
      filter(response == significant_label) |>
      group_by(question) |>
      summarise(avg_sig_pct = mean(percentage, na.rm = TRUE), .groups = "drop") |>
      arrange(avg_sig_pct) |>
      pull(question)
  } else {
    barrier_summary <- barrier_data |>
      group_by(question, qid) |>
      mutate(
        total = sum(count),
        percentage = ifelse(total > 0, count / total * 100, 0)
      ) |>
      ungroup()
    
    # Calculate ordering based on "Significant barrier" percentage
    question_order <- barrier_summary |>
      filter(response == significant_label) |>
      arrange(percentage) |>
      pull(question)
  }
  
  # Create factor with ordering
  barrier_summary$question <- factor(barrier_summary$question, levels = question_order)
  
  # Order response levels for stacking
  barrier_summary$response <- factor(
    barrier_summary$response,
    levels = c(not_applicable_label, not_significant_label, significant_label)
  )
  
  # Filter out any NA values to avoid warnings (but keep 0 percentages)
  barrier_summary <- barrier_summary |>
    filter(!is.na(response) & !is.na(percentage) & !is.na(question))
  
  # Define colors
  color_values <- setNames(
    c("grey80", "lightblue", "firebrick3"),
    c(not_applicable_label, not_significant_label, significant_label)
  )
  
  # Create the plot
  p <- ggplot(barrier_summary, aes(x = percentage, y = question, fill = response))
  
  if (use_grouping) {
    # Grouped version with facets
    p <- p +
      geom_bar(
        aes(group = interaction(question, group)),
        stat = "identity",
        position = position_stack(),
        width = 0.7
      ) +
      facet_grid(. ~ group_label, scales = "free_x") +
      geom_text(
        aes(label = ifelse(percentage > 5, paste0(round(percentage, 0), "%"), ""),
            group = interaction(question, group)),
        position = position_stack(vjust = 0.5),
        size = 2.5,
        color = "white",
        fontface = "bold"
      ) # +
      # labs(
      #   title = "Barriers to Pathogen Data Sharing - Group Comparison",
      #   subtitle = str_wrap(paste0("Comparison between ", group1_value, " and ", group2_value), width = 80)
      # )
  } else {
    # Single plot version
    p <- p +
      geom_bar(stat = "identity", position = "stack", width = 0.8) +
      geom_text(
        aes(label = ifelse(percentage > 5, paste0(round(percentage, 0), "%"), "")),
        position = position_stack(vjust = 0.5),
        size = 3,
        color = "white",
        fontface = "bold"
      ) # +
      # labs(
      #   title = "Barriers to Pathogen Data Sharing",
      #   subtitle = "Questions ordered by frequency of 'Significant barrier' responses"
      # )
  }
  
  # Add common elements
  p +
    scale_fill_manual(
      values = color_values,
      name = NULL,
      labels = function(x) str_wrap(x, width = 15)
    ) +
    labs(
      x = "Percentage of Responses",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.text.y = element_text(size = 10),
      legend.position = "top",
      panel.grid.major.y = element_blank(),
      plot.margin = margin(5, 15, 5, 5)
    ) +
    scale_x_continuous(expand = c(0, 0.02))
}

# Function to compare responses between two groups
plot_mc_comparison <- function(answer_df, qid, question_list, 
                               grouping_qid, group1_value, group2_value,
                               frequency_cutoff = 1, show_percentages = TRUE) {
  
  # Extract question text
  question_text <- question_list[[qid]]$question
  
  # Get grouping variable
  groups <- answer_df[, grouping_qid]
  
  # Filter to only include respondents from the two groups
  group1_idx <- which(grepl(group1_value, groups, fixed = TRUE))
  group2_idx <- which(grepl(group2_value, groups, fixed = TRUE))
  
  # Function to process answers for a group
  process_group <- function(indices, group_name) {
    answers <- answer_df[indices, qid]
    answers <- answers[!is.na(answers) & answers != ""]
    
    if (length(answers) == 0) return(NULL)
    
    # Handle multiple selections
    all_answers <- unlist(strsplit(answers, ";"))
    
    # Count frequencies
    counts <- as.data.frame(table(all_answers)) |>
      rename(answer = all_answers, count = Freq)
    
    counts$group <- group_name
    counts$n_total <- length(indices)
    counts$percentage <- (counts$count / length(answers)) * 100
    
    return(counts)
  }
  
  # Process both groups
  data1 <- process_group(group1_idx, group1_value)
  data2 <- process_group(group2_idx, group2_value)
  
  # Combine data
  combined_data <- bind_rows(data1, data2)
  
  # Apply frequency cutoff (based on total across both groups)
  answer_totals <- combined_data |>
    group_by(answer) |>
    summarise(total_count = sum(count), .groups = "drop") |>
    filter(total_count >= frequency_cutoff)
  
  combined_data <- combined_data |>
    filter(answer %in% answer_totals$answer)
  
  # Ensure all answer-group combinations exist (fill missing with 0)
  all_combinations <- expand.grid(
    answer = unique(combined_data$answer),
    group = c(group1_value, group2_value),
    stringsAsFactors = FALSE
  )
  
  combined_data <- all_combinations |>
    left_join(combined_data, by = c("answer", "group")) |>
    mutate(
      count = ifelse(is.na(count), 0, count),
      percentage = ifelse(is.na(percentage), 0, percentage)
    )
  
  # Order answers by total frequency
  answer_order <- answer_totals |>
    arrange(desc(total_count)) |>
    pull(answer)
  
  combined_data$answer <- factor(combined_data$answer, levels = rev(answer_order))
  
  # Choose y-axis (count or percentage)
  y_var <- if (show_percentages) "percentage" else "count"
  y_label <- if (show_percentages) "Percentage of Responses" else "Number of Responses"
  
  # Create grouped bar chart
  ggplot(combined_data, aes(x = answer, y = .data[[y_var]], fill = group)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(
      aes(label = if (show_percentages) paste0(round(.data[[y_var]], 0), "%") else .data[[y_var]]),
      position = position_dodge(width = 0.7),
      hjust = -0.1,
      size = 3
    ) +
    coord_flip() +
    scale_fill_brewer(
      palette = "Set2", 
      name = NULL,
      labels = function(x) str_wrap(x, width = 25)
    ) +
    labs(
      # title = str_wrap(question_text, width = 60),
      # subtitle = str_wrap(paste0("Comparison between ", group1_value, " and ", group2_value), width = 60),
      x = NULL,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "gray40"),
      axis.text.y = element_text(size = 10),
      legend.position = "top",
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 35))
}