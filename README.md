# Road Traffic Accidents Analysis

This project analyzes road traffic accidents data.

## Description

The project uses data from the `RoadTrafficAccidentLocations_GPS` and `pers_accidents.px` datasets to visualize and analyze road traffic accidents. The analysis focuses on accidents that occurred in the vicinity of Geneva, Switzerland.

## Dependencies

The project uses the following R packages:

- leaflet: for creating interactive maps
- dplyr: for data manipulation
- ggplot2: for creating plots
- (my fork of) pxR: for reading .px files
- janitor: for cleaning data

## Usage

To run the analysis, open the `RoadKills.r` script in R and run the code. The script will generate a leaflet map and a ggplot plot of the accident locations, and perform some data cleaning and manipulation on the `pers_accidents.px` dataset.

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)