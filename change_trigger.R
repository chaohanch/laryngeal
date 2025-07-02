library(tidyverse)
library(data.table)

# 1. to modify, change the extension of .vmrk to .csv,
# 2. delete all rows above the row starting with "Mk2="
# 3. after the new txt file is saved, delete the " marker.
# 4. replace original code (starting from the line with "Mk2")

filename = "CUE2024_0012_F0"

# read in file
df_0 <- read_csv(paste0("eeg_brainvision/", filename, ".csv"), col_names = FALSE) %>%
  filter(!row_number() %in% 1:9) %>%
  separate(X1, into = c("mk", "type", "description", "value", "latency", "size", "chan")) %>%
  filter(description != "Segment") %>%
  mutate(value = as.numeric(value))


df <- df_0 %>%
  mutate(
    # recode the values
    new_value = case_when(
      # if the current initial is "R" and the next time value is the same as the current time value...
      description=="R" & latency==lead(latency) ~ value*16+lead(value),
      # if the current initial is "R" and the next time value is not the same as the current time value...
      description=="R" & latency!=lead(latency) ~ value*16,
      TRUE ~ value
    ),
  ) %>%
  # remove the rows where the next time value is the same as the current time value, except for the 1st row
  filter( (latency != lag(latency)) | is.na(lag(latency)) ) %>%
  mutate(
    # correct for 39 and 139 (for F0)
    new_value = case_when(new_value==39 ~ 29,
                          new_value==139 ~ 129,
                          TRUE ~ new_value),
    # change the "Response" type to "Stimulus" type
    type = case_when(type=="Response" ~ "Stimulus",
                     TRUE ~ type),
    # change the "R" initial to "S" initial
    description = case_when(description=="R" ~ "S",
                        TRUE ~ description),
    # assign new order, +1 because we start at Mk=2
    new_order = row_number()+1,
    # combine columns to get new code
    new_code = paste0("Mk", new_order, "=", type, ",", description, str_pad(new_value, width = 3, side = "left"), ",", latency, ",", size, ",", chan)
  ) %>%
  # remove other columns
  select(new_code)


# write table
write.table(x = df, file = paste0("eeg_brainvision/", filename, "_recoded.csv"), row.names = FALSE, col.names = FALSE)

