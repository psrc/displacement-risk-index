## Details about the indicator
Percent of population within ¼ mile to frequent or high capacity transit by census tract. These values are based on current population (2023 base year) and are weighted by the total population (not households). 

## Information about generating the data
To generate the data set that will be used in the datagen and vis scripts, please run Python script []. Follow instructions below.
	1. Pull script down to your local machine
	2. Update the file paths in the config.toml file if necessary
	3. Update the output path so that it doesn't overwrite the current data. Just renaming the CSV should do. 
	4.  Click "run python file" the play button in the top right corner, it should run the script. Run it in the summary environment
	5. The script will run and it will produce a csv of output data and an html map showing census tracts highlighted depending on the % of the population          within 1/4 mile of high capacity transit stations


