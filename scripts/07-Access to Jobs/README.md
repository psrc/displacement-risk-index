## Details about the indicator
This measure represents the average number of jobs that are accessible within a certain time threshold from a home location. Two measures are created to represent variations by personal vehicle and transit.

a) Average number of jobs within **30 min auto drive** for each census tract

b) Average number of jobs within **45 minutes transit ride** for each census tract

For auto trips, each pair of origin-destination TAZs is filtered to select only the zone pairs within a 30-minute auto trip, in the peak AM period (7 to 8 AM), for single-occupant vehicles (SOV), of income class 2 (out of 4 income classes, representing an average commuter type). These eligible zone pairs are joined to parcel level household and employment totals, which can be aggregated to any level of geography as a **weighted average based on number of households** in a parcel. For a given household, the measure represents the total number of potential jobs a person could access from their home location, based on the road network conditions in the morning peak period. When we aggregate this measure to represent larger areas like Census Tracts, the number represents an average that could reasonably be expected across that larger area. Weighting by total households in parcels helps account for locations within larger zones with higher populations that have greater or less accessibility than surrounding areas.

The same approach is used for transit trips, except that a threshold of 45 minutes is used, since this includes the walk- or drive-access time, initial wait time, transfer time, and in-vehicle travel times. Many observed transit commutes are over 30 minutes when considering all these components. Additionally, jobs accessible via intrazonal trips (those starting and ending in the same TAZ) were removed from this calculation, assuming transit would not be used for such short trips and would be accessed by walking or other means.

The script that generates these data sets is part of standard Soundcast summaries and can be found in the [Soundcast repo](https://github.com/psrc/soundcast/blob/master/scripts/summarize/standard/job_accessibility.py)



## Information about generating the data
The data sets for this indicator are produced automatically from a standard Soundcast run - developed for the 2020 census tracts and based on the 2023 network and 2023 jobs/population values. 

The raw outputs (`auto_jobs_access.csv` and `transit_jobs_access.csv`) are saved to the model project folder (N:\rtp_2026_2050\final_runs\sc_base_year_2023_final\soundcast\outputs\access). To avoid unintentional edits to the model output data, we copy/paste the CSV files to the network project folder within the indicator\data subfolder. 

In the `auto_jobs_access.csv` and `transit_jobs_access.csv` files there are three columns of data. (The first unnamed column is an index that can be ignored.) The “geography” column is the name of the aggregated area and “value” is the weighted average number of jobs available. The “geography_group” indicates which geographic aggregation was used (e.g., county, Census Tracts). 

For the displacement index, the `DataGen_07_AccessToJobs.R` script transforms these raw CSV files into data sets that can be used for the displacement risk index project- isolating the 2020 Census Tract geographies and the corresponding average number of jobs -> `07_a_AccesstoJobs.csv` and `07_b_AccesstoJobs.csv`.

## Change for 2026 update
The 2021 update used a standard model output based on the projected future 2030 network and 2030 jobs/population numbers. For the 2026 update, we are changing to the ‘current’ base year (2023) network and jobs/population numbers - this is still a standard model output, but it reflects the 'current' conditions instead of future conditions. 
