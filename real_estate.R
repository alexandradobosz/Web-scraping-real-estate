setwd("/Users/aleksandra/Desktop/real estate project")
domiporta <- read.csv("domiporta_scraped.csv", sep = ',')
otodom <- read.csv("otodom_scraped.csv", sep = ',')
adresowo <- read.csv("adresowo_scraped.csv", sep = ',')
nieruchomosci_online <- read.csv("nieruchomosci_online_scraped.csv", sep = ',')

View(adresowo)
View(nieruchomosci_online)
View(domiporta)
View(otodom)

glimpse(adresowo)
glimpse(nieruchomosci_online)
glimpse(domiporta)
glimpse(otodom)
adresowo <- adresowo[adresowo$Rooms != "Bez pośredników", ]

library(dplyr)

adresowo <- adresowo %>%
  mutate(Location = paste(Location, Address))

otodom <- otodom %>% rename(`Total.Price` = Price, `Area` = Area)
domiporta <- domiporta %>% rename( Total.Price= Price, URL = Link, Location = Address)

domiporta <- domiporta %>% select(-c(Land.Area, Title))
adresowo <- adresowo %>% select(-c(Owner.Note, Image, Offer.Type, Property.Type, Address))
nieruchomosci_online <- nieruchomosci_online %>% select(-c(Plot.Area, Description, Image, Property.Type, Title))
otodom <- otodom %>% select(-c(Image, Offer.type, Title))

adresowo %>% merge(adresowo$Location, adresowo$Address)

adresowo <- adresowo[, colnames(domiporta)]
otodom <- otodom[, colnames(domiporta)]
nieruchomosci_online<- nieruchomosci_online[, colnames(domiporta)]


colnames(adresowo)
colnames(otodom)
colnames(nieruchomosci_online)
colnames(domiporta)

# Removing units and whitespace from the columns
domiporta <- domiporta %>%
  mutate(
    Total.Price = str_extract(Total.Price, "^[0-9 ,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Area = str_extract(Area, "^[0-9,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Rooms = str_extract(Rooms, "^[0-9]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Price.per.m. = str_extract(Price.per.m., "^[0-9 ,]+") %>% str_replace_all(" ", "")  # Remove spaces
  )

otodom <- otodom %>%
  mutate(
    Total.Price = str_extract(Total.Price, "^[0-9 ,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Area = str_extract(Area, "^[0-9,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Rooms = str_extract(Rooms, "^[0-9]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Price.per.m. = str_extract(Price.per.m., "^[0-9 ,]+") %>% str_replace_all(" ", "")  # Remove spaces
  )

nieruchomosci_online <- nieruchomosci_online %>%
  mutate(
    Total.Price = str_extract(Total.Price, "^[0-9 ,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Area = str_extract(Area, "^[0-9,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Rooms = str_extract(Rooms, "^[0-9]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Price.per.m. = str_extract(Price.per.m., "^[0-9 ,]+") %>% str_replace_all(" ", "")  # Remove spaces
  )

adresowo <- adresowo %>%
  mutate(
    Total.Price = str_extract(Total.Price, "^[0-9 ,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Area = str_extract(Area, "^[0-9,]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Rooms = str_extract(Rooms, "^[0-9]+") %>% str_replace_all(" ", ""),  # Remove spaces
    Price.per.m. = str_extract(Price.per.m., "^[0-9 ,]+") %>% str_replace_all(" ", "")  # Remove spaces
  )

otodom <- na.omit(otodom)
adresowo <- na.omit(adresowo)
nieruchomosci_online <- na.omit(nieruchomosci_online)
domiporta <- na.omit(domiporta)


merged <- bind_rows(otodom, adresowo, nieruchomosci_online, domiporta)
View(merged)

merged$Total.Price <- as.numeric(merged$Total.Price) 
merged$Price.per.m. <- as.numeric(merged$Price.per.m.) 
merged$Area <- as.numeric(merged$Area) 
merged$Rooms <- as.numeric(merged$Rooms) 



# List of Warsaw districts
districts <- c("Bemowo", "Białołęka", "Bielany", "Mokotów", "Ochota", "Praga Południe", 
               "Praga Północ", "Rembertów", "Śródmieście", "Targówek", "Ursus", "Ursynów", 
               "Wawer", "Wesoła", "Wilanów", "Włochy", "Wola", "Żoliborz")

# Add 'District' column based on 'Location'
merged <- merged %>%
  mutate(
    Location = str_replace_all(Location, "-", " "),
    District = str_extract(Location, paste(districts, collapse = "|"))
  )

merged <- na.omit(merged)


district_count <- table(merged$District)
district_count_sorted <- sort(district_count, decreasing = TRUE)


png("district_distribution.jpg", width = 1400, height = 800)  # Adjust width and height
barplot(district_count_sorted,
        main = "Distribution of Properties by District",
        col = "lightblue",
        xlab = "District",
        ylab = "Number of Properties",
        las = 1.5,  # Rotate x-axis labels for better readability
        cex.names = 0.8)  # Adjust size of the x-axis labels

dev.off()

merged <- merged %>% group_by(District) %>% mutate(DistrictPerMMean = mean(Price.per.m.)) %>% ungroup()

merged_na <- merged[!complete.cases(merged), ]
print(merged_na)


# Calculate mean prices per m² grouped by District
district_means <- aggregate(Price.per.m. ~ District, data = merged, FUN = mean, na.rm = TRUE)

# Sort the means in descending order while keeping District names
district_means_sorted <- district_means[order(district_means$Price.per.m., decreasing = TRUE), ]

# Extract the sorted mean prices and district labels
district_mean_m_sorted <- district_means_sorted$Price.per.m.
district_labels_sorted <- district_means_sorted$District

# Create barplot with proper labels
png("district_mean_m.jpg", width = 1400, height = 800)  # Save plot as image
barplot(district_mean_m_sorted,
        main = "Mean Prices per M² by District",
        col = "lightblue",
        xlab = "District",
        ylab = "Mean M² Price",
        names.arg = district_labels_sorted,  # Add district names as x-axis labels
        las = 2,  # Rotate x-axis labels vertically for better readability
        cex.names = 0.8)  # Adjust size of the x-axis labels
dev.off()

png("district_mean_m.jpg", width = 1400, height = 800)  # Save plot as image
plot(merged$Area, merged$Total.Price, 
     main = "Scatter Plot: Total Price vs. Area", 
     xlab = "Area (m²)", 
     ylab = "Total Price (zł)", 
     pch = 19,  # Filled circles
     col = "purple")

library(ggplot2)

merged <- merged[merged$Total.Price != 2180000000, ]

# Close any open graphics devices (if any)
while (!is.null(dev.list())) dev.off()

ggplot(merged, aes(x = Area, y = Total.Price, color = District, size = Rooms)) +
  geom_point(alpha = 0.7) +
  labs(title = "Property Prices vs. Area (Bubble Size: Rooms)", x = "Area (m²)", y = "Total Price (zł)") +
  theme_minimal()


library(forcats)

# Boxplot for property prices by district with outliers highlighted
ggplot(merged, aes(x = fct_reorder(District, Price.per.m., .fun = median), y = Price.per.m., fill = District)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, notch = TRUE) +
  coord_flip() +  # Flip coordinates for easier district name reading
  scale_y_continuous(labels = scales::comma) +  # Format y-axis numbers with commas for readability
  labs(title = "Boxplot of Property Prices by District in Warsaw",
       x = "District",
       y = "Price per m2 (zł)") +
  theme_minimal() +
  theme(legend.position = "none",  # No legend needed for boxplot
        axis.text.y = element_text(size = 10))  # Adjust y-axis text size

glimpse(merged)
