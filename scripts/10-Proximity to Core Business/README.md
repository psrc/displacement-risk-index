## Details about the indicator
The distance (miles) to three core business types (supermarket, restaurant, and pharmacy) by census tract. These values are based on current population (2023 base year) and are weighted by the total population (not households). 

## Information about generating the data
This will include details about the background processses used to create the data sets for each of the business types. The notes are currently saved in a word document in the network project folder.

The process was adjusted slightly in 4/2026 due to an error which was found in the data sets - 

From Michael: "I looked into the issue about negative distances for the displacement risk index, and found a -1 was being used in the code as a sentinel value when the API failed to return a true distance. Unfortunately, those values got averaged by tract, resulting in artificially low estimates even in cases where the average wasn't negative. I looked at Suzanne's code, and she had included a default to the maximum search distance (10km) for those cases.
 
Thankfully, the API results were saved as intermediates, so I regenerated tract averages without needing to send all the API calls again, and replaced the old files on the Y:\ drive. I've also adjusted the code to preclude this in the future.
 
I'll mention, that the 10km is returned both for API failures as well as when no results get returned, so it's not perfect. Also, by definition that max search distance is an undercount when no amenity was found. Still, the current results will be much better, as well as being more directly comparable to the 2021 method it's meant to replicate."

## Exploration
For the 2026 update, there was additional exploration into alternative data sources. This process is documented here (README.md)
