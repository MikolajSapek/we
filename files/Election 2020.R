# Install necessary packages if you haven't already
install.packages("dplyr")
install.packages("ggplot2")
install.packages("sf")  # For working with GeoJSON files
install.packages("readr") # For reading CSV files
install.packages("tidyr")
install.packages("sf")






# Load libraries
library(dplyr)
library(ggplot2)
library(sf)
library(readr)
library(tidyr)
library(sf)
library(scales)
# Set file paths
geojson_path <- "/Users/sapek/Desktop/Final project MDV/polish_provinces.geojson"
round1_path <- "/Users/sapek/Desktop/Final project MDV/results_provinces_round1.csv"
round2_path <- "/Users/sapek/Desktop/Final project MDV/results_provinces_round2.csv"
# Load the first round results
round1_data <- read.csv(round1_path, sep = ";")

# Load the second round results
round2_data <- read.csv(round2_path, sep = ";")

# Load the GeoJSON file for Poland's provinces
geo_data <- st_read(geojson_path)

# Załaduj dane
round1_data <- read.csv(round1_path, sep = ";")
round2_data <- read.csv(round2_path, sep = ";")

# Wybór odpowiednich kolumn z danych z I tury (wszyscy kandydaci)
round1_data_cleaned <- round1_data %>%
  select(
    Powiat,  # Powiat (district)
    Województwo,  # Województwo (province)
    `Liczba.wyborców.uprawnionych.do.głosowania`,  # Liczba wyborców uprawnionych do głosowania
    `Liczba.kart.ważnych`,  # Liczba kart ważnych
    `Liczba.kart.nieważnych`,  # Liczba kart nieważnych
    `Szymon.Franciszek.HOŁOWNIA`,  # Głosy na Szymona Hołownię
    `Andrzej.Sebastian.DUDA`,  # Głosy na Andrzeja Dudę
    `Rafał.Kazimierz.TRZASKOWSKI`  # Głosy na Rafała Trzaskowskiego
  )


# Wybór odpowiednich kolumn z danych z II tury (tylko dwaj najlepsi kandydaci)
round2_data_cleaned <- round2_data %>%
  select(
    Powiat,  # Powiat (district)
    Województwo,  # Województwo (province)
    `Liczba.wyborców.uprawnionych.do.głosowania`,  # Liczba wyborców uprawnionych do głosowania
    `Liczba.kart.ważnych`,  # Liczba kart ważnych
    `Liczba.kart.nieważnych`,  # Liczba kart nieważnych
    `Andrzej.Sebastian.DUDA`,  # Głosy na Andrzeja Dudę
    `Rafał.Kazimierz.TRZASKOWSKI`  # Głosy na Rafała Trzaskowskiego
  )


# Sprawdzanie brakujących danych w oczyszczonych zbiorach
sum(is.na(round1_data_cleaned))  # Brakujące dane w I turze
sum(is.na(round2_data_cleaned))  # Brakujące dane w II turze

round1_data_cleaned <- round1_data_cleaned %>%
  filter(!tolower(Powiat) %in% c("zagranica", "statki"))
round2_data_cleaned <- round2_data_cleaned %>%
  filter(!tolower(Powiat) %in% c("zagranica", "statki"))


# Obliczanie frekwencji w danych (I tura)
round1_data_cleaned <- round1_data_cleaned %>%
  mutate(
    frekwencja_tura1 = ((`Liczba.kart.ważnych` + `Liczba.kart.nieważnych`) / `Liczba.wyborców.uprawnionych.do.głosowania`) * 100  # Obliczanie frekwencji
  )
# Wyświetlanie wyników z obliczoną frekwencją
head(round1_data_cleaned)



# Obliczanie frekwencji w danych (II tura)
round2_data_cleaned <- round2_data_cleaned %>%
  mutate(
    frekwencja_tura2 = ((`Liczba.kart.ważnych` + `Liczba.kart.nieważnych`) / `Liczba.wyborców.uprawnionych.do.głosowania`) * 100  # Obliczanie frekwencji
  )
# Wyświetlanie wyników z obliczoną frekwencją
head(round2_data_cleaned)




# Obliczanie wyników procentowych dla każdego kandydata w I turze
round1_data_cleaned <- round1_data_cleaned %>%
  mutate(
    # Obliczanie procentu dla każdego kandydata
    total_votes = `Szymon.Franciszek.HOŁOWNIA` + `Andrzej.Sebastian.DUDA` + `Rafał.Kazimierz.TRZASKOWSKI`,  # Trzej kandydaci w I turze
    hołownia_percent = (`Szymon.Franciszek.HOŁOWNIA` / total_votes) * 100,
    duda_percent = (`Andrzej.Sebastian.DUDA` / total_votes) * 100,
    trzaskowski_percent = (`Rafał.Kazimierz.TRZASKOWSKI` / total_votes) * 100
  )
# Zaokrąglamy wyniki do dwóch miejsc po przecinku
round1_data_cleaned <- round1_data_cleaned %>%
  mutate(
    hołownia_percent = round(hołownia_percent, 2),
    duda_percent = round(duda_percent, 2),
    trzaskowski_percent = round(trzaskowski_percent, 2)
  )
# Wyświetlanie wyników z obliczonymi procentami
head(round1_data_cleaned)




# Obliczanie wyników procentowych dla każdego kandydata w II turze
round2_data_cleaned <- round2_data_cleaned %>%
  mutate(
    # Obliczanie procentu dla każdego kandydata
    total_votes = `Andrzej.Sebastian.DUDA` + `Rafał.Kazimierz.TRZASKOWSKI`,  # Tylko dwaj kandydaci w II turze
    duda_percent = (`Andrzej.Sebastian.DUDA` / total_votes) * 100,
    trzaskowski_percent = (`Rafał.Kazimierz.TRZASKOWSKI` / total_votes) * 100
  )
# Zaokrąglamy wyniki do dwóch miejsc po przecinku
round2_data_cleaned <- round2_data_cleaned %>%
  mutate(
    duda_percent = round(duda_percent, 2),
    trzaskowski_percent = round(trzaskowski_percent, 2)
  )
# Wyświetlanie wyników z obliczonymi procentami
head(round2_data_cleaned)



turnout_change_table <- round1_data_cleaned %>%
  select(Powiat, Województwo, frekwencja_tura1) %>%  # Dodajemy województwo
  left_join(round2_data_cleaned %>%
              select(Powiat, Województwo, frekwencja_tura2),  # Dodajemy województwo
            by = c("Powiat", "Województwo")) %>%
  mutate(
    Frekwencja_change = frekwencja_tura2 - frekwencja_tura1
  )
# Podgląd tabeli
head(turnout_change_table)



# Opcjonalnie: zapisanie wyników do pliku CSV
write.csv(turnout_change_table, "turnout_change_table.csv", row.names = FALSE)
# Obliczamy średni procentowy wynik Szymona Hołowni w pierwszej turze
mean_hołownia_percent <- round(mean(round1_data_cleaned$hołownia_percent, na.rm = TRUE), 2)
mean_hołownia_percent
# Wyszukiwanie powiatów, w których procentowy wynik Hołowni był wyższy niż średnia, z uwzględnieniem województwa
above_average_counties <- round1_data_cleaned %>%
  filter(hołownia_percent > mean_hołownia_percent) %>%
  select(Powiat, Województwo, hołownia_percent)
# Wyświetlanie wyników
head(above_average_counties)



# Połączenie tabeli z wynikami procentowymi Szymona Hołowni z tabelą z danymi o zmianie frekwencji
comparison_table <- above_average_counties %>%
  left_join(turnout_change_table, by = c("Powiat", "Województwo"))
# Sprawdzenie wyników
head(comparison_table)



# Średnia zmiana frekwencji w powiatach, gdzie Hołownia miał lepszy wynik
mean_turnout_change_above_average <- mean(comparison_table$Frekwencja_change, na.rm = TRUE)
mean_turnout_change_above_average
# Filtrujemy powiaty, gdzie Hołownia miał gorszy wynik niż średnia
below_average_counties <- round1_data_cleaned %>%
filter(hołownia_percent <= mean_hołownia_percent) %>%
select(Powiat, Województwo, hołownia_percent)
# Łączymy te powiaty z tabelą o zmianie frekwencji
comparison_above_avg <- above_average_counties %>%
  left_join(turnout_change_table, by = c("Powiat", "Województwo"))
# Średnia zmiana frekwencji w powiatach, gdzie Hołownia miał gorszy wynik
mean_turnout_change_below_average <- mean(comparison_below_avg$Frekwencja_change, na.rm = TRUE)
mean_turnout_change_below_average




# Porównanie średnich zmian frekwencji
comparison_result <- data.frame(
  Group = c("Above Average Hołownia", "Below Average Hołownia"),
  Mean_Frekwencja_Change = c(mean_turnout_change_above_average, mean_turnout_change_below_average)
)

comparison_result



# Obliczanie procentowego wyniku kandydatów w każdym powiecie i zaokrąglenie do 2 miejsc po przecinku
candidate_percentages <- round1_data_cleaned %>%
  mutate(
    hołownia_percent = round((`Szymon.Franciszek.HOŁOWNIA` / `Liczba.kart.ważnych`) * 100, 2),
    duda_percent = round((`Andrzej.Sebastian.DUDA` / `Liczba.kart.ważnych`) * 100, 2),
    trzaskowski_percent = round((`Rafał.Kazimierz.TRZASKOWSKI` / `Liczba.kart.ważnych`) * 100, 2)
  ) %>%
  select(Powiat, Województwo, hołownia_percent, duda_percent, trzaskowski_percent)

# Podgląd tabeli
head(candidate_percentages)




# Przygotowanie: standaryzacja nazw (jeśli jeszcze nie zrobiono)
round1_data_cleaned <- round1_data_cleaned %>%
  mutate(Powiat = tolower(Powiat), Województwo = tolower(Województwo))

turnout_change_table <- turnout_change_table %>%
  mutate(Powiat = tolower(Powiat), Województwo = tolower(Województwo))

# Połączenie danych
turnout_change_model_data <- round1_data_cleaned %>%
  select(Powiat, Województwo, hołownia_percent) %>%
  inner_join(turnout_change_table, by = c("Powiat", "Województwo"))

mean_hołownia <- mean(turnout_change_model_data$hołownia_percent, na.rm = TRUE)


# Test t-Studenta: czy średnia zmiana frekwencji różni się istotnie między grupami?
t_test_result <- t.test(
  comparison_above_avg$Frekwencja_change,
  comparison_below_avg$Frekwencja_change,
  alternative = "two.sided",  # Test dwustronny
  var.equal = FALSE           # Nie zakładamy równości wariancji
)

# Wyświetlenie wyników
print(t_test_result)




#POTWIERDZE HIPOTEZY NA PRZYKŁADZIE KRZYSZTOFA BOSAKA

# 1. Tworzymy nową tabelę z danymi Bosaka i potrzebnymi zmiennymi
bosak_data <- round1_data %>%
  select(Powiat, Województwo, 
         `Liczba.wyborców.uprawnionych.do.głosowania`, 
         `Liczba.kart.ważnych`, 
         `Liczba.kart.nieważnych`, 
         `Krzysztof.BOSAK`) %>%
  
  # 2. Obliczamy frekwencję (ważne + nieważne) / liczba uprawnionych
  mutate(
    frekwencja_tura1 = ((`Liczba.kart.ważnych` + `Liczba.kart.nieważnych`) / `Liczba.wyborców.uprawnionych.do.głosowania`) * 100,
    total_votes = `Liczba.kart.ważnych`,
    bosak_percent = round((`Krzysztof.BOSAK` / total_votes) * 100, 2)
  )
bosak_data <- bosak_data %>%
  filter(!tolower(Powiat) %in% c("zagranica", "statki"))

# 1. Średnie poparcie Bosaka
mean_bosak_percent <- mean(bosak_data$bosak_percent, na.rm = TRUE)

# 2. Podział powiatów
bosak_above_avg <- bosak_data %>%
  filter(bosak_percent > mean_bosak_percent)

bosak_below_avg <- bosak_data %>%
  filter(bosak_percent <= mean_bosak_percent)

# 3. Połączenie z frekwencją
comparison_bosak_above <- bosak_above_avg %>%
  left_join(turnout_change_table, by = c("Powiat", "Województwo"))

comparison_bosak_below <- bosak_below_avg %>%
  left_join(turnout_change_table, by = c("Powiat", "Województwo"))

# 4. Test t-Studenta
t_test_bosak <- t.test(comparison_bosak_above$Frekwencja_change,
                       comparison_bosak_below$Frekwencja_change)

# 5. Wyniki testu
t_test_bosak



# Model liniowy: wpływ poparcia dla Hołowni na zmianę frekwencji
model <- lm(Frekwencja_change ~ hołownia_percent, data = turnout_change_model_data)

# Podsumowanie modelu
summary(model)



















# Check column names in round1_data
colnames(round1_data)

# Group by Województwo and calculate descriptive statistics for each candidate
descriptive_stats_by_wojewodztwo_round1 <- round1_data %>%
  group_by(Województwo) %>%
  summarise(
    mean_hołownia = mean(`Szymon.Franciszek.HOŁOWNIA`, na.rm = TRUE),
    median_hołownia = median(`Szymon.Franciszek.HOŁOWNIA`, na.rm = TRUE),
    sd_hołownia = sd(`Szymon.Franciszek.HOŁOWNIA`, na.rm = TRUE),
    min_hołownia = min(`Szymon.Franciszek.HOŁOWNIA`, na.rm = TRUE),
    max_hołownia = max(`Szymon.Franciszek.HOŁOWNIA`, na.rm = TRUE),
    
    mean_duda = mean(`Andrzej.Sebastian.DUDA`, na.rm = TRUE),
    median_duda = median(`Andrzej.Sebastian.DUDA`, na.rm = TRUE),
    sd_duda = sd(`Andrzej.Sebastian.DUDA`, na.rm = TRUE),
    min_duda = min(`Andrzej.Sebastian.DUDA`, na.rm = TRUE),
    max_duda = max(`Andrzej.Sebastian.DUDA`, na.rm = TRUE),
    
    mean_trzaskowski = mean(`Rafał.Kazimierz.TRZASKOWSKI`, na.rm = TRUE),
    median_trzaskowski = median(`Rafał.Kazimierz.TRZASKOWSKI`, na.rm = TRUE),
    sd_trzaskowski = sd(`Rafał.Kazimierz.TRZASKOWSKI`, na.rm = TRUE),
    min_trzaskowski = min(`Rafał.Kazimierz.TRZASKOWSKI`, na.rm = TRUE),
    max_trzaskowski = max(`Rafał.Kazimierz.TRZASKOWSKI`, na.rm = TRUE)
  )

# View the summarized statistics
head(descriptive_stats_by_wojewodztwo_round1)
```{r}
library(knitr)
kable(descriptive_stats_by_wojewodztwo_round1, caption = "Descriptive Statistics by Województwo for First Round Candidates")


# Reshape the descriptive statistics dataset to long format for easier plotting
descriptive_stats_long <- descriptive_stats_by_wojewodztwo_round1 %>%
  pivot_longer(cols = c(mean_hołownia, mean_duda, mean_trzaskowski),
               names_to = "Kandydat",
               values_to = "Mean_Vote_Percentage")

# Create a bar plot comparing the mean vote percentages for each candidate in each Województwo
ggplot(descriptive_stats_long, aes(x = Województwo, y = Mean_Vote_Percentage, fill = Kandydat)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  # Position dodge to display bars side by side
  labs(title = "Mean Vote Percentages for Candidates by Województwo (First Round)",
       x = "Province (Województwo)", y = "Mean Vote Percentage") +
  scale_fill_manual(values = c("yellow", "red", "blue")) +  # Assign colors to candidates
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot as a PNG image
ggsave("mean_vote_percentages_plot.png", width = 10, height = 6, dpi = 300)





# Calculate the percentage for each candidate in the first round and round to full percentage
round1_data_cleaned <- round1_data_cleaned %>%
  mutate(
    hołownia_percent = round((`Szymon.Franciszek.HOŁOWNIA` / `Liczba.kart.ważnych`) * 100),
    duda_percent = round((`Andrzej.Sebastian.DUDA` / `Liczba.kart.ważnych`) * 100),
    trzaskowski_percent = round((`Rafał.Kazimierz.TRZASKOWSKI` / `Liczba.kart.ważnych`) * 100)
  )

# Group by Województwo and calculate the mean vote percentage for each candidate
descriptive_stats_by_wojewodztwo_round1 <- round1_data_cleaned %>%
  group_by(Województwo) %>%
  summarise(
    mean_hołownia = mean(hołownia_percent, na.rm = TRUE),
    mean_duda = mean(duda_percent, na.rm = TRUE),
    mean_trzaskowski = mean(trzaskowski_percent, na.rm = TRUE)
  )

# Reshape the descriptive statistics dataset to long format for easier plotting
descriptive_stats_long <- descriptive_stats_by_wojewodztwo_round1 %>%
  pivot_longer(cols = c(mean_hołownia, mean_duda, mean_trzaskowski),
               names_to = "Kandydat",
               values_to = "Mean_Vote_Percentage")

# Create a bar plot comparing the mean vote percentages for each candidate in each Województwo
ggplot(descriptive_stats_long, aes(x = Województwo, y = Mean_Vote_Percentage, fill = Kandydat)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  # Position dodge to display bars side by side
  labs(title = "Mean Vote Percentages for Candidates by Województwo (First Round)",
       x = "Province (Województwo)", y = "Mean Vote Percentage") +
  scale_fill_manual(values = c("yellow", "red", "blue")) +  # Assign colors to candidates
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot as a PNG image
ggsave("mean_vote_percentages_round1.png", width = 10, height = 6, dpi = 300)




# Zaokrąglamy wartości do pełnych procentów
rounded_stats <- descriptive_stats_by_wojewodztwo_round1 %>%
  mutate(
    mean_hołownia = round(mean_hołownia),
    mean_duda = round(mean_duda),
    mean_trzaskowski = round(mean_trzaskowski)
  )

# Przekształcenie danych do formatu long (długiego)
rounded_stats_long <- rounded_stats %>%
  select(Województwo, mean_hołownia, mean_duda, mean_trzaskowski) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "Kandydat", values_to = "Wynik") %>%
  mutate(Kandydat = recode(Kandydat,
                           "mean_hołownia" = "Szymon Hołownia",
                           "mean_duda" = "Andrzej Duda",
                           "mean_trzaskowski" = "Rafał Trzaskowski"))

# Tworzymy wykres
ggplot(rounded_stats_long, aes(x = Województwo, y = Wynik, fill = Kandydat)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Średnie Wyniki Kandydatów w I Turze wg Województw (2020)",
    x = "Województwo",
    y = "Średni wynik (%)"
  ) +
  scale_fill_manual(values = c("yellow", "red", "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# 1. Przygotuj dane – wszystko na małe litery
candidate_percentages <- candidate_percentages %>%
  mutate(
    Powiat = tolower(Powiat),
    Województwo = tolower(Województwo),
    Zwycięzca = case_when(
      hołownia_percent > duda_percent & hołownia_percent > trzaskowski_percent ~ "Hołownia",
      duda_percent > hołownia_percent & duda_percent > trzaskowski_percent ~ "Duda",
      trzaskowski_percent > hołownia_percent & trzaskowski_percent > duda_percent ~ "Trzaskowski"
    )
  )

# 2. Upewnij się, że geo_data ma też małe litery w nazwach powiatów
geo_data <- geo_data %>%
  mutate(powiat = tolower(powiat))

# 3. Połącz dane przestrzenne z wynikami kandydatów
geo_data_merged <- geo_data %>%
  left_join(candidate_percentages, by = c("powiat" = "Powiat"))

# 4. Rysowanie mapy zwycięzców
library(ggplot2)
ggplot(geo_data_merged) +
  geom_sf(aes(fill = Zwycięzca), color = "white", size = 0.1) +
  scale_fill_manual(values = c("Hołownia" = "gold", "Duda" = "red", "Trzaskowski" = "blue")) +
  labs(title = "Zwycięzca I Tury Wyborów Prezydenckich 2020 w Powiatach",
       fill = "Kandydat") +
  theme_minimal()




# 1. Upewniamy się, że kolumny z nazwami powiatów są zapisane małymi literami
candidate_percentages <- candidate_percentages %>%
  mutate(Powiat = tolower(Powiat))

geo_data <- geo_data %>%
  mutate(powiat = tolower(powiat))

# 2. Łączymy dane geograficzne z wynikami Szymona Hołowni
geo_data_hołownia <- geo_data %>%
  left_join(candidate_percentages, by = c("powiat" = "Powiat"))

# 3. Tworzymy mapę – od jasnego do ciemnozłotego w zależności od poparcia
ggplot(geo_data_hołownia) +
  geom_sf(aes(fill = hołownia_percent), color = "white", size = 0.1) +
  scale_fill_gradient(
    low = "#fff8dc", high = "#daa520",  # Jasny → złoty
    name = "Hołownia (%)"
  ) +
  labs(
    title = "Poparcie dla Szymona Hołowni (I tura, 2020)",
    subtitle = "Im ciemniejszy kolor, tym wyższe poparcie w powiecie",
    fill = "Wynik %"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# 1. Przygotuj obie tabele: powiaty na małe litery + oznaczenie grupy
above_avg <- above_average_counties %>%
  mutate(
    powiat = tolower(Powiat),
    grupa = "Powyżej średniej"
  )

below_avg <- below_average_counties %>%
  mutate(
    powiat = tolower(Powiat),
    grupa = "Poniżej średniej"
  )

# 2. Połącz obie tabele w jedną
powiaty_grupa <- bind_rows(above_avg, below_avg)

# 3. Ujednolić też dane przestrzenne
geo_data <- geo_data %>%
  mutate(powiat = tolower(powiat))

# 4. Połączenie danych
geo_hołownia_grupy <- geo_data %>%
  left_join(powiaty_grupa, by = "powiat")

# 5. Mapa
library(ggplot2)
ggplot(geo_hołownia_grupy) +
  geom_sf(aes(fill = grupa), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Powyżej średniej" = "gold", "Poniżej średniej" = "grey80")
  ) +
  labs(
    title = "Podział powiatów według poparcia dla Hołowni (I tura, 2020)",
    subtitle = "Złote – powyżej średniej, Szare – poniżej średniej",
    fill = "Wynik Hołowni"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )





# 1. Oblicz średni wzrost frekwencji
mean_change <- mean(turnout_change_table$Frekwencja_change, na.rm = TRUE)

# 2. Oznacz powiaty jako "Powyżej średniej" / "Poniżej średniej"
turnout_change_table_labeled <- turnout_change_table %>%
  mutate(
    powiat = tolower(Powiat),
    grupa = ifelse(Frekwencja_change > mean_change, "Powyżej średniej", "Poniżej średniej")
  )

# 3. Przygotuj geo_data do połączenia (upewniamy się, że powiaty są małymi literami)
geo_data <- geo_data %>%
  mutate(powiat = tolower(powiat))

# 4. Połącz dane z geometrią
geo_turnout <- geo_data %>%
  left_join(turnout_change_table_labeled, by = "powiat")

# 5. Wygeneruj mapę
ggplot(geo_turnout) +
  geom_sf(aes(fill = grupa), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Powyżej średniej" = "forestgreen", "Poniżej średniej" = "tomato"),
    labels = c("Powyżej średniej" = "Silniejszy wzrost", "Poniżej średniej" = "Słabszy wzrost")
  ) +
  labs(
    title = "Zmiana frekwencji między I a II turą wyborów (2020)",
    subtitle = "Zielony – powiaty z ponadprzeciętnym wzrostem frekwencji",
    fill = "Wzrost frekwencji"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )







# Dane z I tury: tylko wynik Hołowni
round1 <- round1_data_cleaned %>%
  select(Powiat, Województwo, hołownia_percent)

# Dane z II tury: Duda i Trzaskowski
round2 <- round2_data_cleaned %>%
  select(Powiat, Województwo, duda_percent, trzaskowski_percent)

# Połączenie danych
merged_slope <- round1 %>%
  inner_join(round2, by = c("Powiat", "Województwo")) %>%
  pivot_longer(cols = c(hołownia_percent, duda_percent, trzaskowski_percent),
               names_to = "Kandydat", values_to = "Poparcie") %>%
  mutate(
    Kandydat = recode(Kandydat,
                      "hołownia_percent" = "Hołownia (I tura)",
                      "duda_percent" = "Duda (II tura)",
                      "trzaskowski_percent" = "Trzaskowski (II tura)")
  )

# 🔻 Ograniczenie do 100 losowych powiatów
set.seed(123)
sampled_powiaty <- sample(unique(merged_slope$Powiat), 100)

merged_slope_filtered <- merged_slope %>%
  filter(Powiat %in% sampled_powiaty)
ggplot(merged_slope_filtered, aes(x = Kandydat, y = Poparcie, group = Powiat)) +
  geom_line(alpha = 0.4, color = "gray60") +
  geom_point(aes(color = Kandydat), size = 2.5) +
  scale_color_manual(values = c("Hołownia (I tura)" = "gold",
                                "Duda (II tura)" = "red",
                                "Trzaskowski (II tura)" = "blue")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Przepływ poparcia: Hołownia → Duda / Trzaskowski (losowe 100 powiatów)",
    subtitle = "Każda linia to jeden powiat – pokazuje zmianę struktury poparcia między turami",
    x = NULL,
    y = "Poparcie (%)",
    color = "Kandydat"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )





# Dane do wykresu
hołownia_summary <- data.frame(
  Kandydat = "Hołownia",
  Grupa = c("Above Avg", "Below Avg"),
  Frekwencja_change = c(3.87, 4.40)
)

bosak_summary <- data.frame(
  Kandydat = "Bosak",
  Grupa = c("Above Avg", "Below Avg"),
  Frekwencja_change = c(4.03, 4.68)
)

# Łączymy dane
frekwencja_plot_data <- bind_rows(hołownia_summary, bosak_summary)

# Wykres
ggplot(frekwencja_plot_data, aes(x = Kandydat, y = Frekwencja_change, fill = Grupa)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  labs(
    title = "Change in Voter Turnout by Candidate Support (First Round)",
    subtitle = "Comparison between counties with above and below average support",
    x = "Candidate",
    y = "Average Turnout Change (%)",
    fill = "Support Level"
  ) +
  scale_fill_manual(values = c("Above Avg" = "gold", "Below Avg" = "gray50")) +
  theme_minimal()




# 1. Upewnij się, że kolumny z nazwami są małymi literami
bosak_data_above_avg <- bosak_above_avg %>%
  mutate(Powiat = tolower(Powiat),
         Województwo = tolower(Województwo))

geo_data <- geo_data %>%
  mutate(powiat = tolower(powiat))  # Upewnij się, że ta kolumna istnieje

# 2. Łączenie danych geograficznych z powiatami Bosaka
geo_bosak <- geo_data %>%
  left_join(bosak_data_above_avg, by = c("powiat" = "Powiat"))

# 3. Oznaczenie powiatów z wynikiem powyżej średniej
geo_bosak <- geo_bosak %>%
  mutate(
    powyzej_sredniej_bosak = ifelse(!is.na(bosak_percent), TRUE, FALSE)
  )

# 4. Mapa
ggplot(geo_bosak) +
  geom_sf(aes(fill = powyzej_sredniej_bosak), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "grey90"),
    labels = c("TRUE" = "Powyżej średniej", "FALSE" = "Pozostałe")
  ) +
  labs(
    title = "Powiaty z ponadprzeciętnym poparciem Krzysztofa Bosaka (I tura, 2020)",
    subtitle = "Zielony – powiaty z wynikiem powyżej średniej",
    fill = "Wynik Bosaka"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())



# 1. Ujednolicenie nazw i ID dla Hołowni i Bosaka
above_average_counties <- above_average_counties %>%
  mutate(
    Powiat = tolower(Powiat),
    Województwo = tolower(Województwo),
    id = paste(Powiat, Województwo, sep = ", ")
  )

bosak_above_average_counties <- bosak_above_avg %>%
  mutate(
    Powiat = tolower(Powiat),
    Województwo = tolower(Województwo),
    id = paste(Powiat, Województwo, sep = ", ")
  )

# 2. Dodaj kolumnę 'wojewodztwo' do geo_data na podstawie istniejących danych (jeśli nie istnieje)
# Sprawdź czy geo_data ma kolumnę 'wojewodztwo', jeśli nie — musimy ją dołączyć z innej tabeli

if (!"wojewodztwo" %in% colnames(geo_data)) {
  # dołącz województwo z round1_data lub candidate_percentages (tam mamy powiaty i województwa)
  geo_data <- geo_data %>%
    left_join(
      candidate_percentages %>%
        select(Powiat, Województwo) %>%
        mutate(
          Powiat = tolower(Powiat),
          Województwo = tolower(Województwo)
        ),
      by = c("powiat" = "Powiat")
    )
}

# 3. Stwórz ID w geo_data
geo_data <- geo_data %>%
  mutate(
    wojewodztwo = tolower(Województwo),  # to ensure it's lowercase
    id = paste(powiat, wojewodztwo, sep = ", ")
  )

# 4. Tworzymy warstwę koloru
geo_data <- geo_data %>%
  mutate(
    kolor = case_when(
      id %in% above_average_counties$id & id %in% bosak_above_average_counties$id ~ "Obaj",
      id %in% above_average_counties$id ~ "Hołownia",
      id %in% bosak_above_average_counties$id ~ "Bosak",
      TRUE ~ "Żaden"
    )
  )

# 5. Rysowanie mapy
ggplot(geo_data) +
  geom_sf(aes(fill = kolor), color = "white", size = 0.1) +
  scale_fill_manual(values = c(
    "Obaj" = "red",
    "Hołownia" = "gold",
    "Bosak" = "darkgreen",
    "Żaden" = "grey90"
  )) +
  labs(
    title = "Powiaty z ponadprzeciętnym poparciem (I tura, 2020)",
    subtitle = "🟥 Obaj kandydaci  🟨 Hołownia  🟩 Bosak",
    fill = "Poparcie"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Zakładamy, że masz już tabelę turnout_change_model_data z kolumnami:
# hołownia_percent, Frekwencja_change, Województwo, Liczba.kart.ważnych

ggplot(turnout_change_model_data, aes(x = hołownia_percent, y = Frekwencja_change)) +
  geom_point(aes(color = Województwo, size = Liczba.kart.ważnych), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", size = 1) +
  scale_size(range = c(1, 6), name = "Liczba ważnych głosów") +
  labs(
    title = "Zależność między poparciem Hołowni a zmianą frekwencji (2020)",
    subtitle = "Każdy punkt to jeden powiat. Czarna linia – regresja liniowa.",
    x = "Poparcie dla Szymona Hołowni (I tura, %)",
    y = "Zmiana frekwencji (II tura - I tura, p.p.)",
    color = "Województwo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )
