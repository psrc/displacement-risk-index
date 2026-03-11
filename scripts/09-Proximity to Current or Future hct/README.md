## Details about the indicator
Percent of tract area within ¼ mile of 2035 BRT and ½ mile of other 2035 high capacity transit stops/stations. These values are based on current population (2023 base year) and are based on total population (not households). 

## Information about generating the data
To generate the data set that will be used in the datagen and vis scripts, please run Python script []. Follow instructions below.
1. Pull script down to your local machine
2. Update the file paths in the config.toml file if necessary
   - The file paths in the config.toml file are from the Network drive. Sometimes these files may be slow to load so it can be helpful to copy them locally and run the code with local file paths. Specifically, the transit_stops.csv can be slow to load.
4. Update the output path so that it doesn't overwrite the current data. Just renaming the CSV should do.
5. Click "run python file" the play button in the top right corner, it should run the script. Run it in the summary environment
6. The script will run and it will produce a csv of output data for percentage of area in a census tract within 1/4 mile of BRT and 1/2 mile of other high capacity transit. 
