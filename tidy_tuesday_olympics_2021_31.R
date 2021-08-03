library(dplyr)
library(gt)
library(tidyr)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')


olympic_cities <- olympics %>% 
  filter(year>=1960 & season=="Summer") %>% 
  pull(city) %>%  
  unique()


olympic_countries <- tibble(
  city=olympic_cities,
  country=c("Spain","Great Britain","Australia","United States", 
            "China","Brazil","Greece","United States",
            "Mexico","Germany","South Korea","Italy",
            "Canada","Russia","Japan")
)


teams_events <- olympics %>% 
  select(team:medal & !sport) %>% 
  unique() %>% 
  filter(year>=1960 & season=="Summer") %>% 
  left_join(olympic_countries, by="city") %>% 
  mutate(team=case_when(team=="East Germany" ~ "Germany",
                        team=="West Germany" ~ "Germany",
                        team=="Soviet Union" ~ "Russia",
                        TRUE ~ as.character(team)))


host_teams_events <- teams_events %>%  
  filter(team==country)

host_team_attendance <- host_teams_events %>%  
  group_by(city) %>% 
  count()

host_team_medals <- host_teams_events %>% 
  drop_na(medal) %>%  
  group_by(city,medal) %>% 
  count() %>% 
  pivot_wider(names_from=medal, values_from=n, values_fill=0)

host_team_performance <- left_join(host_team_attendance, host_team_medals, by="city") %>% 
  mutate(Total_Medals=sum(Gold,Silver,Bronze),
         Avg_Medal_Per_Event=round((Total_Medals/n),2)) %>% 
  relocate(Bronze, .after = "Silver") %>% 
  rename(Attandance=n) %>% 
  right_join(olympic_countries, host_team_performance, by="city") %>% 
  relocate(country) %>% 
  arrange(country)

away_teams_events <- teams_events %>%  
  anti_join(host_teams_events) %>% 
  filter(team %in% olympic_countries$country)

away_team_attendance <- away_teams_events %>%  
  group_by(team) %>% 
  count()

away_team_medals <- away_teams_events %>% 
  drop_na(medal) %>%  
  group_by(team,medal) %>% 
  count() %>% 
  pivot_wider(names_from=medal, values_from=n, values_fill=0)

away_team_performance <- left_join(away_team_attendance, away_team_medals, by="team") %>% 
  mutate(Total_Medals=sum(Gold,Silver,Bronze),
         Avg_Medal_Per_Event=round((Total_Medals/n),2)) %>% 
  relocate(Bronze, .after = "Silver") %>% 
  rename(Attandance=n) %>% 
  arrange(team)


final_table <- left_join(host_team_performance, away_team_performance, by=c("country"="team")) %>% 
  as_tibble() %>% 
  setNames(c("Country", "City",
             "Attandance_H","Gold_H","Silver_H","Bronze_H","Total_Medals_H","Avg_Medal_Per_Event_H",
             "Attandance_A","Gold_A","Silver_A","Bronze_A","Total_Medals_A","Avg_Medal_Per_Event_A")) %>% 
  mutate(Difference=Avg_Medal_Per_Event_H-Avg_Medal_Per_Event_A) %>% 
  arrange(desc(Difference)) %>% 
  left_join(host_teams_events %>% select(city,year) %>% unique(), by=c("City"="city"))



final_table %>% 
  gt() %>% 
  tab_header(
    title = "1960-2016 Summer Olympics",
    subtitle = "Performance of Host Countries Both Home and Away by Event "
  ) %>%
  cols_merge(
    columns = c("Country", "City","year"),
    pattern="{1}, {2}, {3}",
    hide_columns = c("City","year")
  ) %>% 
  tab_spanner(
    label = "Host Countries' Home Performance",
    columns = c("Attandance_H","Gold_H","Silver_H","Bronze_H","Total_Medals_H","Avg_Medal_Per_Event_H")
  ) %>%
  tab_spanner(
    label = "Host Countries' Away Performance",
    columns = c("Attandance_A","Gold_A","Silver_A","Bronze_A","Total_Medals_A","Avg_Medal_Per_Event_A")
  ) %>% 
  cols_label(
    Gold_H = fontawesome::fa("medal", fill = "orange"),
    Gold_A = fontawesome::fa("medal", fill = "orange"),
    Silver_H = fontawesome::fa("medal", fill = "darkgray"),
    Silver_A = fontawesome::fa("medal", fill = "darkgray"),
    Bronze_H = fontawesome::fa("medal", fill = "#cd7f32"),
    Bronze_A = fontawesome::fa("medal", fill = "#cd7f32"),
    Total_Medals_H =  html("Total<br>Medal"),
    Total_Medals_A = html("Total<br>Medal"),
    Avg_Medal_Per_Event_H = html("Average<br>Medal"),
    Avg_Medal_Per_Event_A = html("Average<br>Medal"),
    Attandance_H = html("Event<br>Attendance"),
    Attandance_A = html("Event<br>Attendance"),
    Difference = html("Difference of<br>Averages") 
  ) %>% 
  cols_align(
    columns = c("Attandance_H","Attandance_A",
                "Total_Medals_H","Avg_Medal_Per_Event_H",
                "Total_Medals_A","Avg_Medal_Per_Event_A",
                "Difference"),
    align ="center"
  ) %>% 
  text_transform(
    locations = cells_body(
      columns = c("Country", "City", "year")
    ),
    fn = function(x){
      Country <- final_table$Country
      City <- final_table$City
      Year <- final_table$year
      glue::glue(
        "<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{Country}</div>
        <div><span style ='font-weight:bold;color:grey;font-size:10px'>IN {City} in {Year}</span></div>"
      )
    }
  ) %>% 
  data_color(
    columns = c("Difference"),
    colors = scales::col_numeric(
      palette = c(
        "red","white", "darkgreen"),
      domain = c(-0.273,0.273))
  ) %>% 
  tab_options(
    data_row.padding = px(3),
    heading.align = "center"
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c("Attandance_A","Difference" )
      )
    )
  ) %>% 
  
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_source_note(
    source_note = "*West Germany and East Germany defined as Germany, Soviet Union as Russia"
  )%>% 
  tab_source_note(
    source_note = "Source: Kaggle / #TidyTuesday 2021-W:31"
  ) %>% 
  gtsave("tidytuesday_2021_31.png")


