# Simulation + Heatmap script
library(tidyverse)
library(lattice)
library(viridisLite)
library(plotly) # For interactive plots

# Daily & Hourly rates (from data):
vol_data <- read_csv("defi_volume_hour_of_day.csv")

# Hourly rates
hourly_rates <- vol_data %>% 
  group_by(day_of_week) %>% 
  mutate(daily_sum = sum(count)) %>% 
  mutate(hourly_avg = count/daily_sum) 

# Daily
daily_rates <- vol_data %>% 
  group_by(day_of_week) %>% 
  summarise(day_total = sum(count)) %>% 
  mutate(daily_avg = day_total / sum(day_total))

# Assumptions / Inputs
mean_weekly_customers <- 10 # Mean for Poisson distribution random sampling
total_sim <- 100
day_weights <- daily_rates %>% dplyr::select(daily_avg) %>% pull()
order_cost <- 15

# INPUTS (cycles) : cycles per day
trading_cycles <- c(1, 2, 4, 12, 24)

# INPUTS (pools) -- designed to work with 5 pool increments
min_pools <- 10
max_pools <- 100 # 
interval_pools <- 10
pool_number_options <- seq(min_pools, max_pools, interval_pools)

# Variables
results_table <- tibble(order_key = NA, 
                        customer_key = NA, 
                        pool = NA, 
                        hour_order = NA, 
                        day_order = NA, 
                        week_sim= NA)
order_key <- 0
customer_key <- 0
num_sim <- 0

# Probability results matrix:

nrow <- length(pool_number_options)
ncol <- length(trading_cycles)
heatmap_matrix <- matrix(0, nrow = nrow, ncol = ncol)
heatmap_tibble <- tibble(trading_cycle = NA, 
                         pool_number = NA, 
                         saved_orders = NA, 
                         total_orders = NA, 
                         pooled_orders = NA
                         )
l <- 0
h <- 0

# Simulation
for (cycle in trading_cycles){
  l <- l + 1
  print(l)
  h <- 0
  for (pool_number in pool_number_options){
    h <- h + 1
    # Initialize results table for each pair
    results_table <- tibble(order_key = NA, 
                            customer_key = NA, 
                            pool = NA, 
                            hour_order = NA, 
                            day_order = NA, 
                            week_sim= NA)
    
    # Pool distribution (Assumed Uniform)
    pool_weights <- rep(1/pool_number, pool_number)
    
    # For each (cycle, pool_number) pair do:
    customers <- rpois(total_sim, lambda = mean_weekly_customers)
    order_key <- 0
    customer_key <- 0
    num_sim <- 0
    j <- 0
    for (num_cust in customers){
      j <- j+1
      num_sim <- num_sim + 1
      for (i in 1:num_cust){
        customer_key <- customer_key + 1
        # Day & Hour of week
        day_week <- sample(x = 1:7, size = 1, prob = day_weights)
        hourly_weigths <- hourly_rates %>% dplyr::filter(day_of_week == day_week) %>%
          ungroup() %>% 
          dplyr::select(hourly_avg) %>% pull()
        hour_of_day <- sample(x = 0:23, size = 1, prob = hourly_weigths)
        
        # Pool selection
        num_orders <- sample(x = 1:5, size = 1, prob = c(0.8, 0.17, 0.02, 0.008, 0.002))
        selected_pools <- sample(x = 1:pool_number, size = num_orders, replace = FALSE, prob = pool_weights)
        for (pool in selected_pools){
          order_key <- order_key + 1
          new_row <- tibble(order_key = order_key, 
                            customer_key = customer_key, 
                            pool = pool, 
                            hour_order = hour_of_day, 
                            day_order = day_week, 
                            week_sim = num_sim)
          results_table <- results_table %>% bind_rows(new_row)
        }
      }
    }
    # Results processing depending on trading cycle
    
    # Clean and remove NA:
    results_table <- results_table %>% 
      mutate(pool = factor(pool), 
             order_key = factor(order_key)) %>% 
      drop_na()
    
    # Flag for trading cycles:
    if (cycle == 1){
      results_table <- results_table %>% 
        mutate(trading_cycle = 1)
    } else if (cycle == 2){
      results_table <- results_table %>% 
        mutate(trading_cycle = case_when(
          hour_order < 12 ~ 1,
          hour_order >= 12 ~ 2))
    }
    else if (cycle == 4) {
      results_table <- results_table %>% 
        mutate(trading_cycle = case_when(
          hour_order < 6 ~ 1,
          hour_order < 12 ~ 2,
          hour_order < 18 ~ 3, 
          hour_order >= 18 ~ 4))
    } else if (cycle == 12) { # cycle == 12
      results_table <- results_table %>% 
        mutate(trading_cycle = case_when(
          hour_order < 2 ~ 1,
          hour_order < 4 ~ 2,
          hour_order < 6 ~ 3, 
          hour_order < 8 ~ 4,
          hour_order < 10 ~ 5,
          hour_order < 12 ~ 6, 
          hour_order < 14 ~ 7,
          hour_order < 16 ~ 8, 
          hour_order < 18 ~ 9, 
          hour_order < 20 ~ 10, 
          hour_order < 22 ~ 11, 
          hour_order >= 22 ~ 12))
    } else { # cycle == 24
      results_table <- results_table %>% 
        mutate(trading_cycle = hour_order + 1)
    }
    
    # Process data:
    weights_cycle <- rep(0, cycle)
    probs_cycle <- rep(0, cycle)
    for (k in 1:cycle){
      # Filter by trading cycle flag
      results_subset <- results_table %>% 
        filter(trading_cycle == k) 
      
      # Group by to calculate more than two orders from a given pool 
      # in the same trading cycle
      aux_table <- results_subset %>% 
        group_by(trading_cycle, week_sim, day_order, pool) %>% 
        summarise(count_orders = n()) %>%
        dplyr::filter(count_orders >= 2) %>% 
        ungroup()
      
      # Denominator = all orders of a specified trading cycle
      denominator <- nrow(results_subset)
      
      # Numerator = Orders than can be pooled on a specified trading cycle
      if (nrow(aux_table) == 0){
        numerator <- 0
      } else {
        numerator <- aux_table %>% dplyr::select(count_orders) %>% sum()
      }
      
      if (denominator == 0){
        prob <- 0
      } else {
        prob <- numerator/denominator
      }
      
      probs_cycle[k] <- prob
      
      # Weigths = proportion of trades on that trading cycle
      # Will be used for weighted average
      w <- denominator
      weights_cycle[k] <- w
      
    }
    # Weighted average
    normalized_weights <- weights_cycle/sum(weights_cycle)
    weighted_prob <- (normalized_weights %*% probs_cycle)[1]
    
    # Add to heatmap matrix
    heatmap_matrix[h, l] <- weighted_prob
    
    # HEATMAP Tibble
    
    # First: saved orders = total orders that could be pooled - number of unique orders
    aux_hm <-  results_table %>% 
      group_by(trading_cycle, week_sim, day_order, pool) %>% 
      summarise(count_orders = n()) %>% 
      dplyr::filter(count_orders >= 2) %>%
      ungroup()
    
    if (nrow(aux_hm) == 0){
      potential_pooled = 0
    } else {
      potential_pooled <- aux_hm %>% dplyr::select(count_orders) %>% sum()
    }
    
    # Unique orders needed: count rows
    needed_orders <- nrow(aux_hm)
    
    # Saved orders
    saved_orders <- potential_pooled - needed_orders
    total_orders <- nrow(results_table)
    
    # Add row to results
    heatmap_tibble <- heatmap_tibble %>% add_row(trading_cycle = cycle, 
                               pool_number = pool_number, 
                               saved_orders = saved_orders, 
                               total_orders = total_orders, 
                               pooled_orders = potential_pooled)
      
    
  }
}

# Heatmap with geom tile

hm_plot <- heatmap_tibble %>% 
  drop_na() %>% 
  mutate(aux_tc = case_when(trading_cycle == 1 ~ 1, 
                            trading_cycle == 2 ~ 2,
                            trading_cycle == 4 ~ 3, 
                            trading_cycle == 12 ~ 4, 
                            trading_cycle == 24 ~ 5),
         aux_pn = pool_number/10, 
         saved_pct = round(saved_orders/total_orders,4)*100) %>% 
  mutate(text = paste0("Total user orders: ", total_orders,
                       "\n", "Non-poolable user orders: ", total_orders-pooled_orders,
                       "\n", "Poolable user orders: ", pooled_orders,
                       "\n", "Aggregated pool orders: ", pooled_orders-saved_orders,
                       "\n", "Saved orders: ", saved_orders,
                       "\n", "Saved orders %: ", saved_pct,
                       "\n", "Saved orders per week: ", saved_orders/num_sim,
                       "\n", "Cost savings per week (USD): ", saved_orders*order_cost/num_sim)) %>% 
  ggplot(aes(x = aux_tc, y = aux_pn, fill = saved_pct, text = text)) + 
  scale_x_continuous(breaks = 1:5, labels = c("1", "2", "4", "12", "24"), 
                     expand = c(0,0)) +
  scale_y_continuous(breaks = 1:(max_pools/10), labels = sprintf("%.0f", pool_number_options), 
                     expand = c(0,0)) + 
  geom_tile() +
  geom_text(aes(label = paste0(saved_pct,"%"), color = saved_pct)) + 
  theme_light() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        legend.key.height = unit(1.4, "cm")) + 
  guides(color = F) + 
  scale_fill_gradient(low = "white", high = "dodgerblue4")+
  scale_color_gradient(low = "black", high = "white") + 
  labs(fill = "Saved Orders %", x = "Trading Cycles Per Day", 
       y = "Number of Pools Available", title = "Saved orders as a percent of total weekly orders", subtitle = paste0("Active weekly customers: ",mean_weekly_customers," || Simulation weeks: ",num_sim," || Cost per order (USD): ",order_cost))

hm_plot

# Make heatmap interactive

hm_plotly <- ggplotly(hm_plot, tooltip = "text", embed_resources = TRUE, standalone = TRUE)

# Add subtitle to the Plotly layout
hm_plotly %>% 
  layout(
    title = list(text = hm_plot$labels$title),
    xaxis = list(title = hm_plot$labels$x),
    yaxis = list(title = hm_plot$labels$y),
    font = list(family = "Arial", size = 14),
    margin = list(l = 50, r = 50, t = 70, b = 50)
  ) %>% 
  add_annotations(
    text = hm_plot$labels$subtitle,
    font = list(size = 13, color = "darkblue"),
    x = 1, y = 1.04, xref = "paper", yref = "paper", showarrow = FALSE
  )

