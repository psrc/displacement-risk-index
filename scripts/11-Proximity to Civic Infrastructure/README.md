## Details about the indicator
The distance (miles) to schools and parks by census tract. These values are based on current population (2023 base year) and are weighted by the total population (not households). 

## Information about generating the data
To generate the data set that will be used in the datagen and vis scripts, please run Python script 11-ProximityToCivicInfrastructure_Parks.py for distance to parks or 11-ProximityToCivicInfrastructure_Schools.py for distance to schools. Follow instructions below.

1. Pull script down to your local machine
2. Update the file paths in the config.toml file if necessary
3. The file paths in the config.toml file are from the Network drive. Sometimes these files may be slow to load so it can be helpful to copy them locally and run the code with local file paths. Specifically, the parcels_urbanism.csv can be slow to load.
4. Update the output path so that it doesn't overwrite the current data. Just renaming the CSV should do.
5. Click "run python file" the play button in the top right corner, it should run the script. Run it in the summary environment
6. The script will run and it will produce a csv of output data with the average distance in miles to schools or parks by census tract wieghted by total population. 
7. The testing folders have the python jupyter notebook which was used to test and write the scripts. You can walk through the process there, although it is not very well cleaned up. 
