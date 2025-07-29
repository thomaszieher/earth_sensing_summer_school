#####################################################
##                                                 ##
##      ######      #########  ##          ##      ##
##      #######     ########   ##          ##      ##
##      ##    ##    ##         ##          ##      ##
##      ##   ##     ##         ##          ##      ##
##      ######      ######     ##          ##      ##
##      #######     #####      ##          ##      ##
##      ##    ##    ##         ##    ##    ##      ##
##      ##     ##   ##         ##   ####   ##      ##
##      ##     ##   ##         ##  ##  ##  ##      ##
##      ########    ##          ####    ####       ##
##      #######     ##           ##      ##        ##
##                                                 ##
#####################################################
##                                                 ##
##  Author: Thomas Zieher                          ##
##  Institute: Nat Haz, 6.3                        ##
##  Date: 2025-05-28                               ##
##  License: GNU GPL V.3 or later                  ##
##                                                 ##
#####################################################
##  Description: Script for downloading ERA5 time series
##  Environment: -
#####################################################

import cdsapi

##limited data length for download, hence download time series per year and variable
years=["2019","2020","2021","2022","2023","2024"]
variables=["10m_u_component_of_wind","10m_v_component_of_wind","2m_temperature","2m_dewpoint_temperature",
           "total_precipitation","maximum_2m_temperature_since_previous_post_processing",
           "minimum_2m_temperature_since_previous_post_processing","surface_solar_radiation_downwards"]

#log=open("log.txt","w")
for year in years:
    for variable in variables:
        #log.write(f"{year}\t{variable}\n")

        dataset="reanalysis-era5-single-levels"
        request={
            "product_type": ["reanalysis"],
            "variable": [
                variable
            ],
            "year": [
                year
            ],
            "month": [
                "01", "02", "03","04", "05", "06","07", "08", "09","10", "11", "12"
            ],
            "day": [
                "01", "02", "03","04", "05", "06","07", "08", "09",
                "10", "11", "12","13", "14", "15","16", "17", "18",
                "19", "20", "21","22", "23", "24","25", "26", "27",
                "28", "29", "30","31"
            ],
            "time": [
                "00:00", "01:00", "02:00","03:00", "04:00", "05:00",
                "06:00", "07:00", "08:00","09:00", "10:00", "11:00",
                "12:00", "13:00", "14:00","15:00", "16:00", "17:00",
                "18:00", "19:00", "20:00","21:00", "22:00", "23:00"
            ],
            "data_format": "netcdf",
            "download_format": "unarchived",
            "area": [46.449, 12.215, 46.450, 12.216]
        }

        client=cdsapi.Client()
        target=f"./data/era5/{year}_{variable}.nc"
        client.retrieve(dataset,request,target)

#log.close()
