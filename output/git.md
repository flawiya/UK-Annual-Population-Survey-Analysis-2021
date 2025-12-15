Descriptive and Inferential Analysis of Transport Habits and Benefit

Status: Evidence from the APS 2021 Flawiya Shirish More

library(tidyverse)  # Data wrangling and plotting
library(gdslStats)  # Advanced statistical functions and estimations
library(scales)     # Data visualization scales and formatting
library(patchwork)  # Combining multiple ggplot2 plots into layouts
library(ggplot2)    # Creating elegant data visualizations
library(moments)    # Calculating skewness and kurtosis
library(survey)     # Survey analysis and chi-square tests
library(devtools)   # Package development and management tools
library(forcats)
library(stringr)
library(gt)

load(APS)

print("Inspecting the Continuous variable: Commute_Time")

[1] "Inspecting the Continuous variable: Commute_Time"

meta_data$Commute_Time$value_labels

                 label value
1            No answer    -8
2       Does not apply    -9
3 0 or works from home     0
4                 180+   180

# Creating a new variable by excluding the invalid values:
APS$Commute_Time_Valid <- replace(APS$Commute_Time, 
                                  which(APS$Commute_Time %in% c(-8, -9, 0)), NA)

 [1] "Does not apply"                   "No answer"                       
 [3] "Car or van"                       "Motorbike or moped"              
 [5] "Bicycle"                          "Bus or coach"                    
 [7] "Taxi"                             "Train"                           
 [9] "Underground, tram, light railway" "Walk"                            
[11] "Other method"                    

# Creating new variable by removing invalid responses:
APS$Commute_Method_Valid <- fct_na_level_to_value(APS$Commute_Method,extra_levels = c("Does not apply", "No answer"))

skewness(APS$Commute_Time_Valid, na.rm = TRUE)

[1] 2.817616

median(APS$Commute_Time_Valid,na.rm=TRUE)

[1] 20

IQR(APS$Commute_Time_Valid, na.rm=TRUE)

[1] 20

ggplot(data = subset(APS, !is.na(Commute_Time_Valid)), 
       aes(x = Commute_Time_Valid)) +
  geom_histogram(binwidth = 5, fill = "#56B4E9", color = "white") +
  labs(
    x = "Commute Time (mins)",
    y = "Count",
    caption = "Figure 1. Distribution of Valid Commute Time"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0.5), 
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

# Find the category with maximum frequency
freq_commute_method <- names(commute_method_list)[which.max(
  commute_method_list)]
print(paste("Most frequent Commute_Method:", freq_commute_method))

[1] "Most frequent Commute_Method: Car or van"

SRDI(APS$Commute_Method_Valid, na.rm = TRUE)

[1] 9.443737

# Creating bins and label for Commute_Time_Valid
time_breaks <- c(0, 15, 30, 60, Inf) # bins
time_labels <- c("0-15 mins", "16-30 mins", "31-60 mins", "61+ mins")

table(APS$Commute_Time_Grouped) # display frequency and labels for Commute_time_Grouped

 0-15 mins 16-30 mins 31-60 mins   61+ mins 
     32051      25907      14542       3684 

APS$Commute_Method_Grouped <- fct_collapse(APS$Commute_Method_Valid,
"Private Transport" = c("Car or van", "Motorbike or moped", "Taxi"), 
"Public Transport" = c("Bus or coach", "Train", "Underground, tram,\nlight railway"), "Active Transport" = 
  c("Bicycle", "Walk"), "Others" = "Other method")

# Display frequency and labels for Commute_Method_Grouped
table(APS$Commute_Method_Grouped)

Private Transport  Active Transport  Public Transport            Others 
            63903             10890              9424               535 

# Create the cross-tabulation data (COLUMN percentages) ---
Cross_tab_c <- tab(APS, Commute_Time_Grouped ~ Commute_Method_Grouped, 
                   measure = "col_pct", na.rm = TRUE)

<style>#oltyqwxwyq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#oltyqwxwyq thead, #oltyqwxwyq tbody, #oltyqwxwyq tfoot, #oltyqwxwyq tr, #oltyqwxwyq td, #oltyqwxwyq th {
  border-style: none;
}
&#10;#oltyqwxwyq p {
  margin: 0;
  padding: 0;
}
&#10;#oltyqwxwyq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#oltyqwxwyq .gt_title {
  color: #333333;
  font-size: 12pt;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#oltyqwxwyq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#oltyqwxwyq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#oltyqwxwyq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#oltyqwxwyq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#oltyqwxwyq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#oltyqwxwyq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#oltyqwxwyq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#oltyqwxwyq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#oltyqwxwyq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#oltyqwxwyq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#oltyqwxwyq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#oltyqwxwyq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#oltyqwxwyq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oltyqwxwyq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#oltyqwxwyq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#oltyqwxwyq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#oltyqwxwyq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oltyqwxwyq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#oltyqwxwyq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oltyqwxwyq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#oltyqwxwyq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oltyqwxwyq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#oltyqwxwyq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oltyqwxwyq .gt_left {
  text-align: left;
}
&#10;#oltyqwxwyq .gt_center {
  text-align: center;
}
&#10;#oltyqwxwyq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#oltyqwxwyq .gt_font_normal {
  font-weight: normal;
}
&#10;#oltyqwxwyq .gt_font_bold {
  font-weight: bold;
}
&#10;#oltyqwxwyq .gt_font_italic {
  font-style: italic;
}
&#10;#oltyqwxwyq .gt_super {
  font-size: 65%;
}
&#10;#oltyqwxwyq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#oltyqwxwyq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#oltyqwxwyq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#oltyqwxwyq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#oltyqwxwyq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#oltyqwxwyq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#oltyqwxwyq .gt_indent_5 {
  text-indent: 25px;
}
&#10;#oltyqwxwyq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#oltyqwxwyq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

Table 1: Commute Time Distribution by Commuting Method

Commuting Time (minutes)

Commuting Method (Column Percent)

Private Transport

Active Transport

Public Transport

Others

0-15 mins

43.58

64.09

6.83

18.97

16-30 mins

36.60

27.61

25.27

13.85

31-60 mins

16.82

7.88

47.02

16.41

61+ mins

2.99

0.42

20.89

50.77

ggplot(
  data = subset(APS, !is.na(Commute_Time_Grouped) & !is.na(
    Commute_Method_Grouped)),
  aes(x = Commute_Time_Grouped, fill = Commute_Method_Grouped)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Private Transport" = "#0072B2",
      "Active Transport" = "#E69F00",
      "Public Transport" = "#56B4E9",
      "Others" = "#999999"
    )
  ) +
  labs(
    x = "Commuting Time (minutes)",
    y = "Count",
    fill = "Commuting Method",
    caption = "Figure 3. Commuting Method by Commuting Time"
  ) +
  theme_minimal() +
  theme(
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 10))
  )

    Pearson's X^2: Rao & Scott adjustment

data:  svychisq(~Commute_Time_Grouped + Commute_Method_Grouped, design = subset(aps_design,     !is.na(Commute_Time_Grouped) & !is.na(Commute_Method_Grouped)))
F = 898.62, ndf = 8.6873e+00, ddf = 6.6175e+05, p-value < 2.2e-16

# Calculate confidence intervals for column percentages using survey design
CI_pct_raw <- ci_pct(Cross_tab_counts, design = aps_design, over = "col")

Figure 1: Commuting Time Distribution within each Transport Method

# Creating unweighted tab
unweighted_tab <- tab(Commute_Time_Grouped ~ Commute_Method_Grouped, 
                      data = APS, 
                      measure = "row_pct", na.rm=TRUE)

<style>#folhgokyzs table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#folhgokyzs thead, #folhgokyzs tbody, #folhgokyzs tfoot, #folhgokyzs tr, #folhgokyzs td, #folhgokyzs th {
  border-style: none;
}
&#10;#folhgokyzs p {
  margin: 0;
  padding: 0;
}
&#10;#folhgokyzs .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#folhgokyzs .gt_title {
  color: #333333;
  font-size: 12pt;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#folhgokyzs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#folhgokyzs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#folhgokyzs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#folhgokyzs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#folhgokyzs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#folhgokyzs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#folhgokyzs .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#folhgokyzs .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#folhgokyzs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#folhgokyzs .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#folhgokyzs .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#folhgokyzs .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#folhgokyzs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#folhgokyzs .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#folhgokyzs .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#folhgokyzs .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#folhgokyzs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#folhgokyzs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#folhgokyzs .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#folhgokyzs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#folhgokyzs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#folhgokyzs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#folhgokyzs .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#folhgokyzs .gt_left {
  text-align: left;
}
&#10;#folhgokyzs .gt_center {
  text-align: center;
}
&#10;#folhgokyzs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#folhgokyzs .gt_font_normal {
  font-weight: normal;
}
&#10;#folhgokyzs .gt_font_bold {
  font-weight: bold;
}
&#10;#folhgokyzs .gt_font_italic {
  font-style: italic;
}
&#10;#folhgokyzs .gt_super {
  font-size: 65%;
}
&#10;#folhgokyzs .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#folhgokyzs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#folhgokyzs .gt_indent_1 {
  text-indent: 5px;
}
&#10;#folhgokyzs .gt_indent_2 {
  text-indent: 10px;
}
&#10;#folhgokyzs .gt_indent_3 {
  text-indent: 15px;
}
&#10;#folhgokyzs .gt_indent_4 {
  text-indent: 20px;
}
&#10;#folhgokyzs .gt_indent_5 {
  text-indent: 25px;
}
&#10;#folhgokyzs .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#folhgokyzs div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

Table 2. Unweighted Row Percentages for Commuting Time by Commuting Method

Commute_Time_Grouped

Commute Method (Percent)

Private Transport

Active Transport

Public Transport

Others

0-15 mins

77.21

20.80

1.88

0.12

16-30 mins

80.22

11.08

8.59

0.10

31-60 mins

65.68

5.63

28.47

0.22

61+ mins

46.17

1.20

49.95

2.69

# Create the weighted table
weighted_tab <- tab(Commute_Time_Grouped ~ Commute_Method_Grouped, 
                    data = APS, 
                    weights = Weight_APS,
                    measure = "row_pct", na.rm=TRUE)

<style>#ccuamgbtcl table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ccuamgbtcl thead, #ccuamgbtcl tbody, #ccuamgbtcl tfoot, #ccuamgbtcl tr, #ccuamgbtcl td, #ccuamgbtcl th {
  border-style: none;
}
&#10;#ccuamgbtcl p {
  margin: 0;
  padding: 0;
}
&#10;#ccuamgbtcl .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ccuamgbtcl .gt_title {
  color: #333333;
  font-size: 12pt;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ccuamgbtcl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ccuamgbtcl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ccuamgbtcl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ccuamgbtcl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ccuamgbtcl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ccuamgbtcl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ccuamgbtcl .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ccuamgbtcl .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ccuamgbtcl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ccuamgbtcl .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ccuamgbtcl .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ccuamgbtcl .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ccuamgbtcl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccuamgbtcl .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ccuamgbtcl .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ccuamgbtcl .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ccuamgbtcl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccuamgbtcl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ccuamgbtcl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccuamgbtcl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ccuamgbtcl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccuamgbtcl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ccuamgbtcl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccuamgbtcl .gt_left {
  text-align: left;
}
&#10;#ccuamgbtcl .gt_center {
  text-align: center;
}
&#10;#ccuamgbtcl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ccuamgbtcl .gt_font_normal {
  font-weight: normal;
}
&#10;#ccuamgbtcl .gt_font_bold {
  font-weight: bold;
}
&#10;#ccuamgbtcl .gt_font_italic {
  font-style: italic;
}
&#10;#ccuamgbtcl .gt_super {
  font-size: 65%;
}
&#10;#ccuamgbtcl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ccuamgbtcl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ccuamgbtcl .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ccuamgbtcl .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ccuamgbtcl .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ccuamgbtcl .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ccuamgbtcl .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ccuamgbtcl .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ccuamgbtcl div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

Table 3. Weighted Row Percentages for Commuting Time by Commuting Method

Commute_Time_Grouped

Commute Method (Percent)

Private Transport

Active Transport

Public Transport

Others

0-15 mins

73.74

23.82

2.35

0.10

16-30 mins

75.18

13.25

11.43

0.15

31-60 mins

58.18

6.64

35.00

0.18

61+ mins

40.81

1.45

55.69

2.05

# Calculate the difference
difference_tab <- weighted_tab - unweighted_tab

<style>#bongcqatpq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#bongcqatpq thead, #bongcqatpq tbody, #bongcqatpq tfoot, #bongcqatpq tr, #bongcqatpq td, #bongcqatpq th {
  border-style: none;
}
&#10;#bongcqatpq p {
  margin: 0;
  padding: 0;
}
&#10;#bongcqatpq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#bongcqatpq .gt_title {
  color: #333333;
  font-size: 12pt;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#bongcqatpq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#bongcqatpq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#bongcqatpq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#bongcqatpq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#bongcqatpq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#bongcqatpq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#bongcqatpq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#bongcqatpq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#bongcqatpq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#bongcqatpq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#bongcqatpq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#bongcqatpq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#bongcqatpq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bongcqatpq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#bongcqatpq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#bongcqatpq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#bongcqatpq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bongcqatpq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#bongcqatpq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bongcqatpq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#bongcqatpq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bongcqatpq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bongcqatpq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bongcqatpq .gt_left {
  text-align: left;
}
&#10;#bongcqatpq .gt_center {
  text-align: center;
}
&#10;#bongcqatpq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#bongcqatpq .gt_font_normal {
  font-weight: normal;
}
&#10;#bongcqatpq .gt_font_bold {
  font-weight: bold;
}
&#10;#bongcqatpq .gt_font_italic {
  font-style: italic;
}
&#10;#bongcqatpq .gt_super {
  font-size: 65%;
}
&#10;#bongcqatpq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#bongcqatpq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#bongcqatpq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#bongcqatpq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#bongcqatpq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#bongcqatpq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#bongcqatpq .gt_indent_5 {
  text-indent: 25px;
}
&#10;#bongcqatpq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#bongcqatpq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

Table 4. Difference in Row Percentages (Weighted - Unweighted)

Commute_Time_Grouped

Difference (Percentage Points)

Private Transport

Active Transport

Public Transport

Others

0-15 mins

−3.48

+3.02

+0.47

−0.02

16-30 mins

−5.05

+2.17

+2.84

+0.04

31-60 mins

−7.49

+1.00

+6.53

−0.04

61+ mins

−5.36

+0.25

+5.74

−0.64

[1] "Frequency and Percentage distribution for Benefit Income"

  Var1   Freq Percent
1   No 248997    99.6
2  Yes   1000     0.4

<img src="Git_files/figure-commonmark/fig-benefit-facet-1.png"
id="fig-benefit-facet" />

Figure 2

