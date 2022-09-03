####
# Data Cleaning
####

## Cleaning chips data set ##

chips <- read.csv('Data/choiceset_matlab.csv') %>% # renaming variables
  rename(
    visitid = X1 ,
    sizemem = X1.1 ,
    speed = X1.2 ,
    branded = X0 ,
    price = X20.31 ,
    out_of_stock = X0.1 ,
    whole_cost = X12.661 ,
    choice = X0.2
  ) %>% 
  filter(out_of_stock == 0) %>% # dropping observations ot of stock
  group_by(visitid) %>% 
  mutate(
    outside_good = ifelse(max(choice) == 0 , 1, 0), # creating outside good
    x_j = ifelse(max(choice) == 1, 1, 0) # creating x_j
  )

 
## Market shares ##

market_shares <- chips %>% 
  group_by(sizemem, speed, branded) %>% 
  summarise(
    market_share = sum(choice) /n() , # Creating market shares
  ) %>% 
  ungroup() %>% 
  mutate(
    type = row_number() , # Creating a variable for type
    outside_share = 1- sum(market_share) , # Creating a v. for the outside share
    ln_ms = log(market_share) , # Creating ln(market_share)
    ln_og = log(outside_share) , # Creating ln(og)
    y = ln_ms - ln_og
  ) %>% 
  select(type, everything()) # Rearranging variables 


## Merging Data ##

# We can now merge market shares data set to the chips data set

chips <- left_join(chips, market_shares,
                   by= c('sizemem'= 'sizemem',
                         'speed' = 'speed',
                         'branded' = 'branded'))

# We may now drop observations from markets without market share

# First, get not bought types
not_bought <- unique(chips[chips$market_share == 0, ]$type) # 41, 42, 45

# Drop observations from that type

chips <- chips %>%
  filter(!type %in% not_bought)


