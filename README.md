# Road Traffic Accident Severity – Shiny Dashboard

This repository contains an interactive **R Shiny dashboard** analyzing road traffic accident data with a focus on injury severity and demographic factors.

## 🔗 Live Shiny App
[Open the RTA Shiny Dashboard](https://sabashahbaz.shinyapps.io/rta_shiny_app/)

## 📊 Dataset
- File: `rta_dataset.csv`
- The dataset includes information on:
  - Accident severity
  - Driver gender
  - Area of accident (Urban / Rural)
  - Day of week
  - Type of vehicle

## 🖥️ Running Locally
1. Clone the repository  
2. Open `app.R` in RStudio  
3. Make sure `rta_dataset.csv` is in the same folder  
4. Run the app using:

```r
shiny::runApp()
