
##############################################################

library(dplyr) # this library allows us use tools for working with data frames and data manipulation.
library(stringr) # this library allows us to work with commom string operations
library(shiny) # this library is used to build powerful interactive web applications
library(shinydashboard) # this library is part of shiny and it is used to create dashboards
library(plotly) # librarry used to create interactive plots
library(bigrquery) # library that allows us to talk to Google's 'BigQuery' 

##############################################################

# The following part is the code given by the team of Code for Venezuela, we did not make any change
path = "https://storage.googleapis.com/angostura-public/hult-hackathon-key.json"
bq_auth(path = path)
project_id <- "event-pipeline"
sql <- 'SELECT * from `angostura_dev.eh_health_survey_response`'
df <- query_exec(sql, project_id, use_legacy_sql = FALSE)

# We created lists to group the columns in different categories

# Equipment list
equipment_list<-c('rrt_avail_high_flow_catheters',
                  'rrt_avail_blood_tests_hiv_hvb_hvc_vdr',
                  'rrt_avail_immediate_access_urea_reduction_bun',
                  'nCoV_face_mask_avail',
                  'nCoV_respiratory_isolation_protocol_avail',
                  'nCoV_isolation_area_avail',
                  'operability_lab',
                  'operability_uls',
                  'operability_ct_mri',
                  'operability_xr',
                  'er_avail_defibrillator',
                  'er_avail_ott_intubation',
                  'er_avail_catheter',
                  'er_avail_oxygen_suction',
                  'sx_avail_anesthetic_gases',
                  'sx_avail_ott_intubation',
                  'sx_avail_patient_lingerie_kit',
                  'sx_avail_disposables_mask_gloves_gown',
                  'sx_avail_oxygen_suction',
                  'rrt_hemodialysis_avail_filter',
                  'rrt_hemodialysis_avail_lines',
                  'rrt_hemodialysis_avail_kit_hemodialysis')

# Drug List
drug_list <- c('er_avail_adrenalin',
               'er_avail_atropine',
               'er_avail_dopamine',
               'er_avail_cephalosporins_betalactams',
               'er_avail_aminoglycosides_quinolone',
               'er_avail_vancomycin_clindamycin',
               'er_avail_lidocaine',
               'er_avail_minor_opioids',
               'er_avail_major_opioids',
               'er_avail_iv_fluids',
               'er_avail_diazepam_dph',
               'er_avail_heparin',
               'er_avail_steroids',
               'er_avail_insulin',
               'er_avail_asthma',
               'er_avail_blood_pressure',
               'sx_avail_minor_opioids',
               'sx_avail_major_opioids',
               'sx_avail_anesthetics_iv',
               'sx_avail_relaxants',
               'rrt_hemodialysis_avail_b_complex',
               'rrt_hemodialysis_avail_calcium',
               'rrt_hemodialysis_avail_zemblar',
               'rrt_hemodialysis_avail_iron')

# Functional areas in the hospital
function_list<- c('operability_icu_p',
                  'operability_er',
                  'operability_sx',
                  'rrt_operability',
                  'operability_icu')


# Power equipment List
power_list <- c('power_generator_available',
                'power_outage_days_count')

# Creating a vector to agrupate all the previous lists
full_list <- c(equipment_list, drug_list, function_list, power_list)

# Creating a second data frame choosing only the columns with data that could be converted into numbers
# to do calculations 
df_2 <- df %>% 
  select(c(2,34, 45, 3, 5, 14, 20:33, 35:44, 46:55, 57:63, 65, 77, 79:84, 119))

# Creating a third data frame with only numercial values to do a 
# for loop(function that allows us to iterate in all the values of the data frame )
df_3 <- data.frame(matrix(ncol = ncol(df_2) -3, nrow = nrow(df_2)))

# Renaming the columns to keep the consistency in all the data frames 
colname <- c(colnames(df_2[,4: ncol(df_2)]))
colnames(df_3) <- colname

# Creating a new column: year_weak2 which allows us to generate timeline plots
year_week <- t(data.frame(strsplit(df_2$report_week, "\\s+")))
year<- year_week[, 3]
week<- year_week[, 1]
week2 <- str_pad(week, 2, pad = '0')
year_week2 <- paste(year, week2)


# For loop function to change all the answer into numbers according to the following criteria:

# Todos los días           = 7 
# Funciona todos los días  = 7
# Entre 3 y 5 días         = 4 (the average of 3 and 5)
# Menos de 3 de días       = 2
# Funciona menos de 3 días = 2 
# Entre 1 y 2 días         = 1
# No existe                = 0 
# No operativa             = 0
# Nunca ha existido        = 0
# Hay pero no funciona     = 0 
# Nunca ha habido          = 0
# No hubo                  = 0

for (col in 1:ncol(df_3)) {
  for (row in 1:nrow(df_3)) {
    if (grepl("los", df_2[row, col +3])) {
      df_3[row, col] = 7
    } else if (grepl("3 y 5", df_2[row, col + 3])) {
      df_3[row, col] = 4
    } else if (grepl("hubo", df_2[row, col + 3])) {
      df_3[row, col] = 0
    } else if (grepl("1 y 2", df_2[row, col + 3])) {
      df_3[row, col] = 1
    } else if (grepl("existido", df_2[row, col + 3])) {
      df_3[row, col] = 0
    } else if (grepl("operativa", df_2[row, col + 3])) {
      df_3[row, col] = 0
    } else if (grepl("existe", df_2[row, col + 3])) {
      df_3[row, col] = 0
    } else if (grepl("de 3", df_2[row, col + 3])) {
      df_3[row, col] = 2
    } else if (grepl("pero", df_2[row, col + 3])) {
      df_3[row, col] = 0
    } else if (grepl("habido", df_2[row, col + 3])) {
      df_3[row, col] = 0
    } else {df_3[row, col] = 0}
  }
}

# Adding the 3 columns we deleted before
df_3$year_week <- year_week2
df_3$hospital_code <- df_2$hospital_code
df_3$federal_entity <- df_2$federal_entity


# Creating data frames for every category to generate specific plots per each one 

# Equipment
equipment_df <- df_3 %>%
  select(c(all_of(equipment_list), 54:56))
equipment_df$mean <- rowMeans(equipment_df[1:22], na.rm = TRUE)# Creating a new column with the average of every row
equipment_nation <- equipment_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))

# Drugs
drug_df <- df_3 %>%
  select(c(all_of(drug_list), 54:56))
drug_df$mean <- rowMeans(drug_df[1:24], na.rm = TRUE)# Creating a new column with the average of every row
drug_nation <- drug_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))

# Functional areas in the hospital
function_df <- df_3 %>%
  select(c(all_of(function_list), 54:56))
function_df$mean <- rowMeans(function_df[1:5], na.rm = TRUE)# Creating a new column with the average of every row
function_nation <- function_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))

# Power equipment
power_df <- df_3 %>%
  select(c(all_of(power_list), 54:56))
power_df$mean <- rowMeans(power_df[1:2], na.rm = TRUE)# Creating a new column with the average of every row
power_nation <- power_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))



ax_hide <- list(title = "", zeroline = FALSE, showline = FALSE,
                showticklabels = FALSE, showgrid = FALSE)
ax_tick <- list(dtick = 10)

# Column names
df_4<- df_3
names(df_4) <- c("OPERABILITY ICU", "POWER_GENERATOR", "OPERABILITY ICU P",
                 "HIGH FLOW CATHETERS", "BLOOD TESTS HIV HVB HVC VDR",
                 "UREA REDUCTION BUN", "FACE MASK", "RESPIRATORY ISOLATION PROTOCOL",
                 "EMERGENCY ROOM", "ISOLATION AREA", "SURGICAL PAVILION",
                 "LABORATORY", "ULTRASOUND", "CT-MRI", "X-RAY", "ADRENALINE",
                 "ATROPINE", "DOPAMINE", "CEPHALOSPORINS BETALACTAMS", 
                 "AMINOGLYCOSIDES QUINOLONE", "VANCOMYCIN CLINDAMYCIN",
                 "LIDOCAINE", "ER MINOR OPIOIDS", "ER MAJOR OPIOIDS",
                 "IV FLUIDS", "DIAZEPAM DPH", "HEPARIN", "STEROIDS",
                 "INSULIN", "ASTHMA", "BLOOD PRESSURE", "DEFIBRILLATOR",
                 "ER OTT INTUBATION", "CATHETER", "ER OXYGEN SUCTION",
                 "SX MINOR OPIOIDS", "SX MAJOR OPIOIDS", "ANESTHETIC GASES",
                 "ANESTHETICS IV", "RELAXANTS", "SX OTT INTUBATION", "PATIENT LINGERIE KIT",
                 "DISPOSABLES MASK GLOVES GOWN", "SX OXYGEN SUCTION", "DIALYSIS SERVICES OPERABILITY",
                 "HEMODIALYSIS FILTER", "HEMODIALYSIS LINES", "HEMODIALYSIS KIT",
                 "HEMODIALYSIS IRON", "HEMODIALYSIS B COMPLEX", "HEMODIALYSIS CALCIUM",
                 "HEMODIALYSIS ZEMBLAR", "POWER OUTAGE DAYS COUNT", "YEAR WEEK",
                 "HOSPITAL CODE", "FEDERAL ENTITY")


equipment_df <- df_3 %>%
  select(c(all_of(equipment_list), 54:56))
equipment_df$mean <- rowMeans(equipment_df[1:22], na.rm = TRUE)
equipment_nation <- equipment_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))
drug_df <- df_3 %>%
  select(c(all_of(drug_list), 54:56))
drug_df$mean <- rowMeans(drug_df[1:24], na.rm = TRUE)
drug_nation <- drug_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))
function_df <- df_3 %>%
  select(c(all_of(function_list), 54:56))
function_df$mean <- rowMeans(function_df[1:5], na.rm = TRUE)
function_nation <- function_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))
power_df <- df_3 %>%
  select(c(all_of(power_list), 54:56))
power_df$mean <- rowMeans(power_df[1:2], na.rm = TRUE)
power_nation <- power_df %>%
  group_by(year_week) %>%
  summarize(mean_nation = mean(mean))

drug_df2 <- df_4 %>%
  select(c(54, 16:31, 36:37, 39:40, 49:52, 55:56)) %>%
  arrange(`YEAR WEEK`)

year_week3 <- tail(drug_df2$`YEAR WEEK`, 1)

last_week_drug <- drug_df2 %>%
  filter(`YEAR WEEK` == year_week3) %>%
  select(c(2:25)) %>%
  colMeans(na.rm = T) %>%
  data.frame()
last_week_drug$Medicine <- c(rownames(last_week_drug))

equipment_df2 <- df_4 %>%
  select(c(54, 4:8, 10, 12:15, 32:35, 38, 41:44, 46:48, 55:56)) %>%
  arrange(`YEAR WEEK`)

last_week_equip <- equipment_df2 %>%
  filter(`YEAR WEEK` == year_week3) %>%
  select(c(2:23)) %>%
  colMeans(na.rm = T) %>%
  data.frame()
last_week_equip$Equipment <- c(rownames(last_week_equip))

function_df2 <- df_4 %>%
  select(c(54, 1, 3, 9, 11, 45, 55:56)) %>%
  arrange(`YEAR WEEK`)
last_week_function <- function_df2 %>%
  filter(`YEAR WEEK` == year_week3) %>%
  select(c(2:5)) %>%
  colMeans(na.rm = T) %>%
  data.frame()
last_week_function$Function <- c(rownames(last_week_function))

power_df2 <- df_4 %>%
  select(c(54, 2, 53, 55:56)) %>%
  arrange(`YEAR WEEK`)
last_week_power <- power_df2 %>%
  filter(`YEAR WEEK` == year_week3) %>%
  select(c(2:3)) %>%
  colMeans(na.rm = T) %>%
  data.frame()
last_week_power$Power <- c(rownames(last_week_power))

########### Shinny user interface


ui <- dashboardPage(dashboardHeader(title = "Code 4 Venezuela NGO"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Status", tabName = "status"),
                      menuItem("Overview", tabName = "overview"),
                      menuItem("Medicine", tabName = "medicine"),
                      menuItem("Equipment", tabName = "equipment"),
                      menuItem("Function", tabName = "function2"),
                      menuItem("Power", tabName = "power"))),
                    dashboardBody(tabItems(
                      tabItem(tabName = "status",
                              fluidRow(box(width = 6,
                                  height = 150,
                                  infoBox(width = NULL,
                                  title = "Medicine",
                                  subtitle = textOutput("text1"))),
                                box(width = 6,
                                  height = 150,
                                  infoBox(width = NULL,
                                  title = "Equipment",
                                  subtitle = textOutput("text2")))),
                              fluidRow(box(width = 6,
                                           height = 150,
                                           infoBox(width = NULL,
                                           title = "Function",
                                           subtitle = textOutput("text3"))),
                                       box(width = 6,
                                           height = 150,
                                           infoBox(width = NULL,
                                                   title = "Power",
                                                   subtitle = textOutput("text4"))))),
                      tabItem(tabName = "overview",
                              plotlyOutput("overview")),
                      tabItem(tabName = "medicine",
                              plotlyOutput("medicine")),
                      tabItem(tabName = "equipment",
                              plotlyOutput("equipment")),
                      tabItem(tabName = "function2",
                              plotlyOutput("function2")),
                      tabItem(tabName = "power",
                              plotlyOutput("power"))
                    )
)
)
server <- function(input, output) {
  output$text1 <- renderText(c(round(tail(drug_nation$mean_nation, n=1),2), "Days"))
  output$text2 <- renderText(c(round(tail(equipment_nation$mean_nation, n=1),2), "Days"))
  output$text3 <- renderText(c(round(tail(function_nation$mean_nation, n=1),2), "Days"))
  output$text4 <- renderText(c(round(tail(power_nation$mean_nation, n=1),2), "Days"))
  p1 <- drug_nation %>%
    plot_ly(x = ~year_week, y = ~mean_nation) %>%
    add_lines(name = "Drug") %>%
    layout(xaxis = ax_hide, yaxis = list(title = "National Mean", range = c(0,5)))
  p2 <- equipment_nation %>%
    plot_ly(x = ~year_week, y = ~mean_nation) %>%
    add_lines(name = "Equipment")  %>%
    layout(xaxis = ax_hide, yaxis = list(title = "", range = c(0,5)))
  p3 <- function_nation %>%
    plot_ly(x = ~year_week, y = ~mean_nation) %>%
    add_lines(name = "Function") %>%
    layout(xaxis = ax_tick, yaxis = list(title = "National Mean", range = c(0,5)))
  p4 <- function_nation %>%
    plot_ly(x = ~year_week, y = ~mean_nation) %>%
    add_lines(name = "Power") %>%
    layout(xaxis = ax_tick, yaxis = list(title = "", range = c(0,5)))
  p <- subplot(p1, p2, p3, p4, nrows = 2) %>%
    layout(width = 900, height = 600, title = "Weekly Trend Change")
  output$overview <- renderPlotly(p)
  medicine <- last_week_drug %>%
    plot_ly(y = ~Medicine, x= ~., type = "bar", orientation = "h") %>%
    layout(title = "Medicine Supply Available", height = 500, xaxis = list(title = "Days Supply"))
  output$medicine <- renderPlotly(medicine)
  equipment <- last_week_equip %>%
    plot_ly(y = ~Equipment, x= ~., type = "bar", orientation = "h") %>%
    layout(title = "Equipment Supply Available", height = 500, xaxis = list(title = "Days Supply"))
  output$equipment <- renderPlotly(equipment)
  funct <- last_week_function %>%
    plot_ly(y = ~Function, x= ~., type = "bar", orientation = "h") %>%
    layout(title = "Function Operating", height = 500, xaxis = list(title = "Days In Operation"))
  output$function2 <- renderPlotly(funct)
  power <- last_week_power %>%
    plot_ly(y = ~Power, x = ~., type = "bar", orientation = "h") %>%
    layout(title = "Power supply available", height = 500, xaxis = list(title = "Days Available"))
  output$power <- renderPlotly(power)
}
shinyApp(ui, server)
