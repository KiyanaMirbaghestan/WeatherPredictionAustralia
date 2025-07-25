# Option 1: tidytuesdayR R package 
#install.packages("tidytuesdayR")
library(dplyr)
library(tidytuesdayR)
tt <- tidytuesdayR::tt_load('2025-05-20')
head(tt$water_quality)
head(tt$weather)
tt$water_quality
tt$weather
nrow(tt$water_quality)
ncol(tt$water_quality)  
nrow(tt$weather)
ncol(tt$weather)
attach(tt)
sum(is.na(tt$water_quality))
combined<-inner_join(water_quality, weather, by = "date")
colSums(is.na(combined))
round(colSums(is.na(combined)) / nrow(combined) * 100, 2)
sum(is.na(tt$weather))

combined_clean <- na.omit(tt$water_quality)

##Join
library(dplyr)
library(lubridate)
library(janitor)

water_quality <- tt$water_quality |> 
  janitor::clean_names()

combined<-inner_join(water_quality, weather, by = "date")





library(ggplot2)

ggplot(combined, aes(x = precipitation_mm, y = enterococci_cfu_100ml)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  labs(
    title = "آیا بارندگی باعث افزایش باکتری Enterococci در آب می‌شود؟",
    x = "میزان بارندگی (میلی‌متر)",
    y = "تعداد Enterococci (CFU/100ml)"
  ) +
  theme_minimal()


ggplot(combined, aes(x = precipitation_mm, y = enterococci_cfu_100ml)) +
  geom_point(alpha = 0.2, color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "رابطه‌ی بارندگی با میزان آلودگی آب",
    x = "بارندگی (mm)",
    y = "Enterococci (CFU/100ml)"
  ) +
  theme_minimal()
##
model <- lm(enterococci_cfu_100ml ~ precipitation_mm, data = combined)
summary(model)

model <- lm(enterococci_cfu_100ml ~ ., data = combined)
summary(model)


library(dplyr)
library(ggplot2)

# ساختن دسته بندی برای بارش
combined <- combined %>%
  mutate(precipitation_cat = cut(precipitation_mm,
                                 breaks = c(-Inf, 0, 5, 10, 20, 50, Inf),
                                 labels = c("0", "0-5", "5-10", "10-20", "20-50", ">50"),
                                 right = FALSE))

# کشیدن باکس پلات با محور لگاریتمی
ggplot(combined, aes(x = precipitation_cat, y = enterococci_cfu_100ml)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  scale_y_log10() +
  labs(title = "توزیع باکتری Enterococci بر اساس بازه‌های بارش (محور لگاریتمی)",
       x = "میزان بارندگی (میلی‌متر)",
       y = "تعداد Enterococci (CFU/100ml) (log scale)") +
  theme_minimal()





library(dplyr)
library(ggplot2)

# محاسبه مجموع بارندگی هر نقطه شنا
rain_by_site <- combined %>%
  group_by(swim_site) %>%
  summarise(total_precipitation = sum(precipitation_mm, na.rm = TRUE)) %>%
  arrange(desc(total_precipitation))

# انتخاب چند نقطه اول برای نمایش بهتر (مثلا 10 تا)
rain_top10 <- rain_by_site %>% slice(1:10)

library(ggplot2)
library(dplyr)
library(scales)  # برای درصدها

rain_top10 <- rain_by_site %>% slice(1:10)

# اضافه کردن درصد
rain_top10 <- rain_top10 %>%
  mutate(percent = total_precipitation / sum(total_precipitation) * 100,
         label = paste0(swim_site, "\n", round(percent, 1), "%"))

ggplot(rain_top10, aes(x = "", y = total_precipitation, fill = swim_site)) +
  geom_col(color = "black") +
  coord_polar(theta = "y", start = pi / 4) +  # تغییر زاویه شروع
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "میزان کل بارندگی در 10 نقطه شنا برتر") +
  theme_void() +
  theme(legend.position = "none")

ggplot(rain_top10, aes(x = reorder(swim_site, total_precipitation), y = total_precipitation, fill = swim_site)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "میزان کل بارندگی در 10 نقطه شنا برتر", x = "نقطه شنا", y = "کل بارندگی (میلی‌متر)") +
  theme_minimal()




library(dplyr)
library(ggplot2)

# فرض می‌کنیم داده‌ها توی دیتافریم combined هست که شامل ستون‌های:
# date، region، max_temp_C، min_temp_C

# محاسبه دماهای روزانه بر اساس منطقه
temp_summary <- combined %>%
  group_by(region, date) %>%
  summarise(
    max_temp = max(max_temp_C, na.rm = TRUE),
    min_temp = min(min_temp_C, na.rm = TRUE),
    mean_temp = mean((max_temp_C + min_temp_C)/2, na.rm = TRUE)
  ) %>%
  ungroup()

# رسم نمودار سری زمانی دمای بیشینه و کمینه برای یک منطقه خاص (مثلاً "Sydney")
ggplot(temp_summary %>% filter(region == "Sydney"), aes(x = date)) +
  geom_line(aes(y = max_temp, color = "دمای بیشینه")) +
  geom_line(aes(y = min_temp, color = "دمای کمینه")) +
  geom_line(aes(y = mean_temp, color = "دمای میانگین")) +
  labs(
    title = "نمودار سری زمانی دما در منطقه Sydney",
    x = "تاریخ",
    y = "دما (درجه سانتیگراد)",
    color = "متغیر دما"
  ) +
  theme_minimal()


ggplot(temp_summary, aes(x = date)) +
  geom_line(aes(y = max_temp, color = "دمای بیشینه")) +
  geom_line(aes(y = min_temp, color = "دمای کمینه")) +
  facet_wrap(~region) +
  labs(title = "نمودار سری زمانی دما در مناطق مختلف",
       x = "تاریخ",
       y = "دما (درجه سانتیگراد)",
       color = "متغیر دما") +
  theme_minimal()


library(dplyr)

rain_10_2 <- combined %>%
  filter(precipitation_mm == 10.2)
top_sites <- rain_10_2 %>%
  count(swim_site, sort = TRUE) %>%
  slice_head(n = 2) %>%
  pull(swim_site)
selected_sites <- rain_10_2 %>%
  filter(swim_site %in% top_sites)

library(ggplot2)
library(ggplot2)
ggplot(selected_sites, aes(x = date)) +
  geom_line(aes(y = max_temp_C, color = "Max Temp")) +
  geom_line(aes(y = min_temp_C, color = "Min Temp")) +
  facet_wrap(~ swim_site) +
  labs(
    title = "تغییرات دمای روزهایی با بارش 10.2mm برای دو سایت منتخب",
    x = "تاریخ",
    y = "دمای هوا (°C)",
    color = "دمای هوا"
  ) +
  theme_minimal()


model_max <- lm(precipitation_mm ~ max_temp_C, data = combined)
summary(model_max)
model_min <- lm(precipitation_mm ~ min_temp_C, data = combined)
summary(model_min)

library(ggplot2)

ggplot(combined, aes(x = max_temp_C, y = precipitation_mm)) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid", size = 1,
              aes(x = max_temp_C, y = precipitation_mm)) +
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "dashed", size = 1,
              aes(x = min_temp_C, y = precipitation_mm)) +
  labs(
    title = "رابطه دمای هوا با میزان بارش",
    subtitle = "آبی: دمای بیشینه، نارنجی: دمای کمینه",
    x = "دمای هوا (°C)",
    y = "میزان بارش (میلی‌متر)"
  ) +
  theme_minimal()



combined_high_rain <- combined %>%
  filter(precipitation_mm >= 10, !is.na(longitude.x), !is.na(latitude.x))

ggplot(combined_high_rain, aes(x = longitude.x, y = latitude.x)) +
  geom_point(aes(color = precipitation_mm, size = precipitation_mm), alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  scale_size(range = c(3, 12)) +
  labs(
    title = "نقاط با بارش بالای ۱۰ میلی‌متر",
    x = "طول جغرافیایی",
    y = "عرض جغرافیایی",
    color = "بارش (میلی‌متر)",
    size = "بارش (میلی‌متر)"
  ) +
  theme_minimal()



#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("sf")# فقط یک‌بار
library(sf)
library(rnaturalearth)

au <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

ggplot() +
  geom_sf(data = au, fill = "grey90", color = "black") +
  geom_point(data = combined_high_rain,
             aes(x = longitude.y, y = latitude.y,
                 color = precipitation_mm,
                 size = precipitation_mm),
             alpha = 0.8) +
  scale_color_viridis_c(option = "turbo") +
  scale_size(range = c(3, 12)) +
  coord_sf(xlim = c(140, 154), ylim = c(-38, -28), expand = FALSE) +
  labs(
    title = "نقشه نقاط با بارش بالا (۱۰ میلی‌متر به بالا)",
    x = "طول جغرافیایی",
    y = "عرض جغرافیایی",
    color = "بارش (mm)",
    size = "بارش (mm)"
  ) +
  theme_minimal(base_size = 14)


combined_temp <- combined %>%
  filter(!is.na(longitude.y), !is.na(latitude.y), !is.na(max_temp_C))

ggplot(combined_temp, aes(x = longitude.x, y = latitude.x)) +
  geom_point(aes(color = max_temp_C, size = max_temp_C), alpha = 0.7) +
  scale_color_viridis_c(option = "inferno") +
  scale_size(range = c(2, 10)) +
  labs(
    title = "دمای بیشینه در نقاط مختلف جغرافیایی",
    x = "طول جغرافیایی",
    y = "عرض جغرافیایی",
    color = "دمای بیشینه (°C)",
    size = "دمای بیشینه (°C)"
  ) +
  theme_minimal()
