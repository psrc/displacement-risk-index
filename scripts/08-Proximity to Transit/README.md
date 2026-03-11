## Details about the indicator
Percent of population within ¼ mile to frequent or high capacity transit by census tract. These values are based on current population (2023 base year) and are weighted by the total population (not households). 

## Information about generating the data
To generate the data set that will be used in the datagen and vis scripts, please run Python script 08_ProximityToTransit_Script.py. Follow instructions below.
1. Pull script down to your local machine
2. Update the file paths in the config.toml file if necessary
  - The file paths in the config.toml file are from the Network drive. Sometimes these files may be slow to load so it can be helpful to copy them locally and run the code with local file paths. Specifically, the parcels_urbanism.csv can be slow to load. 
4. Update the output path so that it doesn't overwrite the current data. Just renaming the CSV should do.
5. Click "run python file" the play button in the top right corner, it should run the script. Run it in the summary environment
6. The script will run and it will produce a csv of output data and an html map showing census tracts highlighted depending on the % of the population          within 1/4 mile of high capacity transit stations


