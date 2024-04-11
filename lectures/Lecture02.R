## --------------------------------------------------
#| label: setup
#| message: false
#| echo: true

# load packages
library(tidyverse)
library(readr)
library(viridis)
# set figure parameters for knitr
knitr::opts_chunk$set(
  fig.align = "center", # center align figures
  fig.asp = 0.618,      # the golden ratio
  fig.retina = 3,       # dpi multiplier for displaying HTML output on retina
  fig.height = 4,        # 4 inches
  dpi = 200             # higher is crisper
)


## --------------------------------------------------
#| message: false
#| fig-width: 4.25
movies <- read_csv("movies.csv") 
movies <- movies %>% filter(budget > 100000000)

ggplot(data = movies, aes(x = gross)) +
  geom_area(stat = "bin",
            fill = "skyblue", 
            alpha = 0.5, 
            color = "blue") +  # Area under the curve
  labs(title = "Gross receipts",
       x = "Gross receipts") +
  theme_classic()


## --------------------------------------------------
#| fig-width: 4.25
ggplot(data = movies, aes(x = gross, fill = rating)) +
  geom_area(stat = "bin",
            alpha = 0.5) +  
  labs(title = "Gross receipts by rating",
       x = "Gross receipts") +
  theme_classic()


## --------------------------------------------------
#| fig-width: 4.25
ggplot(data = movies) +
  geom_density(aes(x = year, color = rating)) +
  labs(title = "Movies with $100mil+ budgets") +
  xlab("Year") + 
  theme_classic()


## --------------------------------------------------
#| fig-width: 4
options(scipen = 9999999)
movies_a <- movies %>% filter(genre == "Animation")
ggplot(data = movies_a) +
  geom_dotplot(aes(x = budget, fill = rating)) +
  labs(title = "Animation Films") +
  xlab("Budget") + 
  theme_classic()


## --------------------------------------------------
#| fig-width: 4
ggplot(data = movies_a) +
  geom_dotplot(aes(x = budget, fill = rating),
               position = "jitter",
               binwidth = 5000000) +
  labs(title = "Animation Films") +
  xlab("Budget") + 
  theme_classic()


## --------------------------------------------------
#| fig-width: 4.25
#| message: false
#| warning: false
ggplot(movies, aes(x = budget, color = rating)) +
  geom_freqpoly(binwidth = 7000000) +
  labs(title = "Frequency Polygon",
       x = "Scores",
       y = "Frequency") +
  theme_minimal()


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false
movies <- read_csv("movies.csv") 
ggplot(movies, aes(x = score)) +
  geom_histogram(color = "blue", fill = "gold") +
  labs(title = "Histogram",
       x = "Scores",
       y = "Frequency") +
  theme_minimal()


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false
movies_b <- movies %>% filter(gross > 750000000)


ggplot(movies_b, aes(x = reorder(genre, genre, function(x)-length(x)))) +
  geom_bar(fill = "green") +
  labs(title = "Bar Plot of Genre with gross > $750mil",
       x = "Genre",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1))  


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false
movies_high <- movies %>% filter(gross > 750000000)


ggplot(movies_high, aes(x = reorder(genre, genre, function(x)-length(x)),
                   y = ..prop..,
                   group = 1)) + # group = 1 sums to 1
  geom_bar(fill = "green") +
  labs(title = "Ordered Bar Plot of Genre",
       x = "Genre",
       y = "Proportion") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12, angle = 30, hjust = 1))  


## --------------------------------------------------
#| fig-width: 4.2
ggplot(movies_high, aes(x = reorder(genre, genre, function(x)-length(x)),
                   y = ..prop..,
                   group = 1)) + # group = 1 sums to 1
  geom_bar(fill = "green") +
  labs(title = "Ordered Bar Plot of Genre",
       x = "Genre",
       y = "Proportion") +
  coord_flip() +
  theme_classic()


## --------------------------------------------------
#| fig-width: 4
movies_tabulated <- movies_high %>%
  count(genre, name = "count")

ggplot(data = movies_tabulated, aes(x="", y=count, fill=genre)) +
  geom_bar(stat="identity", width = 0.75) + # vary width for size of hole
  coord_polar(theta="y") +  # for circular plot
  theme(panel.grid = element_blank(),  
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank()) +
  annotate("text", x = 0, y = 0, label = "") # for the hole



## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false
soybeans <- read_csv("soybeans.csv") 
soybeans <- soybeans %>% select(-2)

ggplot(soybeans, aes(x = NS, y = GY)) +
  geom_point() +
  labs(title = "Scatterplot Soybean Yields",
       x = "Number of Grains Per Plant",
       y = "Grain Yield per Acre") +
  theme_classic() 


## --------------------------------------------------
#| fig-width: 4.2
cls <- kmeans(x = soybeans, centers = 3)
soybeans$cluster <- as.character(cls$cluster)

ggplot() +
  geom_point(data = soybeans, 
             mapping = aes(x = NS,
                           y = GY,
                           color = cluster),
             size = 3)



## --------------------------------------------------
#| fig-width: 4.2
cls <- kmeans(x = soybeans, centers = 3)
soybeans$cluster <- as.character(cls$cluster)

ggplot() +
  geom_point(data = soybeans, aes(x = NS,y = GY, color = cluster), size = 2) +
  geom_point(aes(x = cls$centers[, "NS"], 
                                  y = cls$centers[, "GY"]),
                                  color = "black", size = 4) +
  theme_minimal()


## --------------------------------------------------
#| fig-width: 4.2
ggplot(soybeans, aes(x=NS, y=GY, size = Repetition, color = GY)) +
    geom_point(alpha=0.5) +
    scale_size(range = c(1, 16), name="") +
    scale_color_viridis() +
    theme_light() +
    theme(legend.position = "none")


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false
q <- seq(0.00, 1, by = 0.25)

p <- ggplot(soybeans, aes(x = NS, y = GY)) +
  geom_point() 

p + geom_quantile(quantiles = q) +
  labs(title = "Quantile Reg + Scatterplot Soybean Yields",
       x = "Number of Grains Per Plant",
       y = "Grain Yield per Acre") +
  theme_classic() 


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false
movies_g <- movies %>% filter(gross > 500000000)


ggplot(movies_g, aes(x = budget, y = gross)) +
  geom_point(color = "gold", alpha = 0.3) +
  geom_rug(color = "blue") +
  labs(title = "Plot of budget with gross ",
       x = "Budget",
       y = "Gross") +
  theme_classic() +
  theme(axis.text = element_text(size = 6,  hjust = 1))  


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false


ggplot(movies_g, aes(x = budget, y = gross)) +
  geom_point(color = "gold", alpha = 0.3) +
  geom_rug(color = "blue") +
  geom_smooth() +
  labs(title = "Plot of budget with gross ",
       x = "Budget",
       y = "Gross") +
  theme_classic() +
  theme(axis.text = element_text(size = 6,  hjust = 1))  


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false

ggplot(movies_g, aes(x = budget, y = gross)) +
  geom_point(color = "gold", alpha = 0.3) +
  geom_rug(color = "blue") +
  geom_smooth(method = "lm") +
  labs(title = "Plot of budget with gross ",
       x = "Budget",
       y = "Gross") +
  theme_classic() +
  theme(axis.text = element_text(size = 6,  hjust = 1))  


## --------------------------------------------------
#| fig-width: 4.5
library(maps)

df <- data.frame(x = state.center$x, 
                 y = state.center$y,
                 state = state.name)

p <- ggplot(df, aes(x = x, y = y)) + 
       geom_polygon(data = map_data("state"),
                    color = "black",
                    aes(x = long, y = lat,
                        fill = map_data("state")$region,
                        group = group)) +
       xlab("Longitude X") +
       ylab("Latitude Y") +
       guides(fill = "none") +  # no legend
       theme_classic()
p


## --------------------------------------------------
#| fig-width: 4.5
p + 
  geom_text(aes(label = state), size = 2)


## --------------------------------------------------
#| fig-width: 4.5

p + geom_label(aes(label = state), size = 1.5)


## --------------------------------------------------
#| fig-width: 4.5

library(ggrepel)
p + 
  geom_label_repel(aes(label = state), 
                   size = 1.5, max.overlaps = 25)



## --------------------------------------------------
#| fig-width: 4.2
yelpextract %>% 
  group_by(Cuisine) %>% 
  summarize(mean_stars = mean(stars, na.rm=TRUE)) %>% 
  ggplot() + 
  geom_col(aes(x=Cuisine, y=mean_stars), fill = "lightskyblue") +
  ylab("Average Star Rating") +
  xlab("Cuisine")
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x  = element_text(size = 3)) 


## --------------------------------------------------
#| fig-width: 4.2
data <- for101a %>%
  group_by(Cuisine) %>%
  summarize(mean_stars = mean(stars, na.rm = TRUE)) %>%
  mutate(Cuisine = factor(Cuisine, levels = Cuisine[order(mean_stars)]))  # Order levels using mean_stars

ggplot(data, aes(x=Cuisine, y=mean_stars)) + 
  geom_col(fill = "lightskyblue") +
  ylab("Average Star Rating") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none", axis.text.y  = element_text(size = 10)) 


## --------------------------------------------------
#| echo: true
#| eval: false

## geom_col requires x and y values.
## 
## The heights of the bars represent __values__
## in the data
## 
## The trick to ordering is to utilize factors


## --------------------------------------------------
#| results: hide
#| warning: false
#| message: false
library(tidytuesdayR)
tt_output <- tt_load("2020-11-03") %>%
  suppressMessages()
ikea <- tt_output$ikea %>% filter(category %in% c("Wardrobes", "Sofas & armchairs", "Beds"))


## --------------------------------------------------
#| fig-width: 4.2


ggplot(data = ikea) +
  geom_boxplot(aes(x = category, y = price)) +
  theme_light()


## --------------------------------------------------
#| fig-width: 4.2

ggplot(data = ikea) +
  geom_boxplot(aes(x = category, 
                   y = price,
                   fill = category),
               color = "black") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none")


## --------------------------------------------------
#| fig-width: 4.2

ggplot(data = ikea) +
  geom_boxplot(aes(x = interaction(category, other_colors),
                   y = price,
                   fill = category),
               color = "black") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none")


## --------------------------------------------------
#| fig-width: 4.2

ggplot(data = ikea) +
  geom_boxplot(aes(x = interaction(other_colors, category),
                   y = price,
                   fill = category),
               color = "black") +
  xlab("Colors/Category") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none")


## --------------------------------------------------
#| fig-width: 4.2

ggplot(data = ikea) +
  geom_violin(aes(x = category, y = price)) +
  theme_light()


## --------------------------------------------------
#| fig-width: 4.2

ggplot(data = ikea) +
  geom_violin(aes(x = category, 
                   y = price,
                   fill = category),
               color = "black") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none")


## --------------------------------------------------
#| fig-width: 4.2

ggplot(data = ikea) +
  geom_violin(aes(x = interaction(category, other_colors),
                   y = price,
                   fill = category),
               color = "black") +
  coord_flip() +
  theme_light() +
  theme(legend.position = "none")


## --------------------------------------------------
#| fig-width: 4.2

ggplot(data = ikea) +
  geom_violin(aes(x = interaction(other_colors, category),
                   y = price,
                   fill = category),
               color = "black",
              draw_quantiles =  c(0.10, 0.25, 0.5, 0.75, 0.90)) +
  coord_flip() +
  xlab("Colors/Category") +
  theme_light() +
  theme(legend.position = "none")


## --------------------------------------------------
#| fig-width: 4.2
#| message: false
#| warning: false
stolen <- read_csv("stolen.csv") 



## --------------------------------------------------
#| fig-width: 4.2

ggplot(stolen, aes(x = LON, y = LAT)) +
 geom_point() +
 geom_density_2d()


## --------------------------------------------------
#| fig-width: 4.2

ggplot(stolen, aes(x = LON, y = LAT)) +
 geom_point() +
 geom_density_2d_filled(alpha = 0.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_light() +
  theme(axis.text  = element_text(size = 5),
        axis.title = element_text(size=6,face="bold"), # change axis label size
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.height = unit(0.4, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), #change legend key width
        legend.title = element_text(size=5), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size


## --------------------------------------------------
#| fig-width: 4.2

ggplot(stolen, aes(x = LON, y = LAT)) +
  geom_point() +
  geom_density_2d_filled(alpha = 0.5, color = "black") +
  theme_light() +
  theme(axis.text  = element_text(size = 5),
        axis.title = element_text(size=6,face="bold"), # change axis label size
        legend.key.size = unit(0.4, 'cm'), #change legend key size
        legend.key.height = unit(0.4, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), #change legend key width
        legend.title = element_text(size=5), #change legend title font size
        legend.text = element_text(size=4)) #change legend text font size


## --------------------------------------------------
#| fig-width: 4.2

stolen$month_cat <- month(stolen$`DATE OCC`, 
                          label = TRUE) |> as.character()

stolen <- stolen %>% filter(month_cat %in% c("Jan", "Feb","Mar")) %>% mutate(month = factor(month_cat, levels = c("Jan", "Feb","Mar")))


ggplot(stolen, aes(x = LON, y = LAT)) +
  geom_point() +
  geom_density_2d(aes(color = month)) +
  facet_wrap(~ month) +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 4))



## --------------------------------------------------
#| fig-width: 4.2

movies <- read_csv("movies.csv",
                    show_col_types = FALSE) 

ggplot(data = movies, aes(x = year, y = score, color = votes)) +
  geom_hex(color = "white") +
  scale_fill_viridis_c() +
  theme_minimal()


## --------------------------------------------------
#| fig-width: 4.2

p <- ggplot(movies, aes(x=votes,y=score)) +
  geom_hex() +
  scale_fill_viridis_c() +
  labs(title = "Distribution of Scores by votes",
       x = "votes",
       y = "score",
       fill = "# of movies") +
  theme_bw()

p


## --------------------------------------------------
#| message: false
#| fig-width: 4.25
tsla <- read_csv("TSLA.csv")
ggplot() + # allow for multiple data, aes
  geom_area(data = tsla, 
            aes(x = Date, y = Volume),
            fill = "skyblue", 
            alpha = 0.5, 
            color = "blue") +  # Area under the curve
  labs(title = "geom_area, stock trading volume",
       x = "Date",
       y = "Volume") +
  theme_classic()


## --------------------------------------------------
#| message: false
#| fig-width: 4.25
movies_l <- movies %>% 
  filter(genre %in% c("Action","Animation", "Comedy", "Drama")) %>% 
  group_by(year, genre) %>% 
  mutate(median_budget = median(budget, na.rm = TRUE))
ggplot(data = movies_l) + # allow for multiple  aes
  geom_line(aes(x = year, 
                y = median_budget, 
                color = genre)) +  # line plot
  labs(title = "geom_line example",
       x = "Date",
       y = "budget") +
  theme_classic()


## --------------------------------------------------
#| fig-width: 4.25
movies_t <- movies %>% 
  filter(genre %in% c("Action","Animation", "Comedy", "Drama",
                      "Biography", "Adventure", "Crime", "Horror")) %>%   
  group_by(year, genre) %>% 
  mutate(total_budget = sd(budget, na.rm = TRUE))

ggplot(data = movies_t) + # allow for multiple data, aes
  geom_tile(aes(x = year, y = genre, fill = total_budget)) + 
  labs(title = "geom_tile example",
       x = "year",
       y = "genre") +
  scale_fill_viridis() +
  theme_classic()


## --------------------------------------------------
#| message: false
#| fig-width: 4.25
aapl <- read_csv("AAPL.csv")
aapl2023 <- aapl %>% 
  mutate(Year = year(Date), 
         Month = month(Date),
         Day = yday(Date),
         DOW = wday(Date, label = TRUE)) %>%
  filter(Year > 2020)

ggplot(data = aapl2023) + 
  geom_tile(aes(x = Day, y = DOW, fill = Close),
            height = 1, width = 10,
            color = "white")+ 
  scale_fill_viridis()  +  
  scale_y_discrete(limits=rev, expand = c(0, 4)) +
  facet_wrap(~ Year, ncol = 1) +
  labs(title = "geom_tile, AAPL close 2021-2023",
       x = "",
       y = "") +
  theme_void()

