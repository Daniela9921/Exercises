# Instala y carga el paquete nycflights13 si no está instalado
if (!requireNamespace("nycflights13", quietly = TRUE)) {
  install.packages("nycflights13")
}

# Carga el paquete nycflights13
library(nycflights13)

# Carga el conjunto de datos flights desde el paquete nycflights13
data("flights")

# Carga el paquete dplyr
library(dplyr)

# Columnas a seleccionar
columns_to_select <- c("time_hour", "air_time")

# Filtra los vuelos según las condiciones dadas
result_filter <- flights %>%
  filter(
    arr_delay >= 120,
    dest %in% c("IAH", "HOU"),
    carrier %in% c("UA", "AA", "DL"),
    month %in% c(7, 8, 9),
    arr_delay > 120,
    dep_delay <= 0,
    dep_delay - arr_delay >= -60,
    between(hour, 0, 6)
  )

# Ordena y selecciona los vuelos según las condiciones dadas
result_arrange_select <- flights %>%
  arrange(
    is.na(arr_delay),
    desc(arr_delay),
    dep_time,
    desc(distance)
  ) %>%
  mutate(
    velocidad = distance / air_time,
    dep_time_minutes = dep_time %/% 100 * 60 + dep_time %% 100,
    sched_dep_time_minutes = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100,
    time_difference = arr_time - dep_time
  ) %>%
  arrange(desc(velocidad)) %>%
  select(matches("TIME", ignore.case = TRUE), everything())

# Comparar air_time con arr_time - dep_time
time_comparison <- result_arrange_select %>%
  select(air_time, time_difference)

# Imprimir los resultados de comparación
print(result_filter)
print(result_arrange_select)
print(time_comparison)

# Scenario 1: A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
scenario1 <- c(rep(-15, 50), rep(15, 50))

# Scenario 2: A flight is always 10 minutes late.
scenario2 <- rep(-10, 100)

# Scenario 3: A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
scenario3 <- c(rep(-30, 50), rep(30, 50))

# Scenario 4: 99% of the time a flight is on time. 1% of the time it’s 2 hours late.
scenario4 <- c(rep(0, 99), rep(-120, 1))

# Calculate mean delay for each scenario
mean_delay <- c(mean(scenario1), mean(scenario2), mean(scenario3), mean(scenario4))

# Calculate median delay for each scenario
median_delay <- c(median(scenario1), median(scenario2), median(scenario3), median(scenario4))

# Grouping and Summarizing
grouped_summary <- result_filter %>%
  group_by(carrier, dest) %>%
  summarize(
    avg_arr_delay = mean(arr_delay),
    max_dep_delay = max(dep_delay, na.rm = TRUE) # Ignoring NA values
  )

# Print the mean and median delay for each scenario
print("Mean Delay for Scenarios:")
print(mean_delay)
print("Median Delay for Scenarios:")
print(median_delay)

# Print grouped summary
print("Grouped Summary:")
print(grouped_summary)