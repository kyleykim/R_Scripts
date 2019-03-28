#!/usr/bin/env python
# -*- coding: utf-8 -*-

# pip install bs4
from bs4 import BeautifulSoup

# pip install agate
import csv

# pip install requests
import requests
import datetime
import logging

logger = logging.getLogger("root")
logging.basicConfig(
    format = "\033[1;36m%(levelname)s: %(filename)s (def %(funcName)s %(lineno)s): \033[1;37m %(message)s",
    level=logging.INFO
)

class ScrapeAtpData(object):
    # Setup
    def __init__(self):
        # Output file name
        self.file_name = 'wildfires.csv'
        # Format date time for ??
        self.file_date_time = datetime.datetime.now().strftime('%Y_%m_%d_%H%M')
        # Set base URL
        self.url = 'http://cdfdata.fire.ca.gov/incidents/incidents_archived?archive_year=2018&pc=5000'
        # Set URL prefix
        self.urlprefix = 'http://cdfdata.fire.ca.gov/incidents/incidents_archived?archive_year='
        # Set URL suffix
        self.urlsuffix = '&pc=5000'
        # An empty dataframe
        self.list_of_data = []
        # Create a vector for year range that will feed into URL string
        self.year_list = range(2003, 2019, 1)

    # Go to the URLS    
    def make_request_to_page(self):
        # Forloop to loop through my year_list
        for year in self.year_list:
            # Concatenate my URL path
            url = "%s%s%s" % (self.urlprefix, year, self.urlsuffix)
            # Acccess the URLS
            response = requests.get(url)
            # Use BeautifulSoup function to parse the page
            soup = BeautifulSoup(response.content, "html.parser")
            # ??
            data = self.process_table_data(soup, year)
        # ??
        self.write_csv(self.list_of_data)

    # Create a dataframe
    def process_table_data(self, soup, year):
        try:
            # Find all HTML tables with class "incident_table"
            tables = soup.find_all('table', class_='incident_table')
            # Table forloop
            for table in tables:
                # create a data dictionary
                data_dict = {}
                # Find all the tr elements in the tables
                rows = table.find_all("tr")
                # Row forloop
                for row in rows[0:]:
                    # ??
                    self.tablerow_to_dicts(row, data_dict)
                # 
                self.list_of_data.append(data_dict)
        except Exception, exception:
            error_output = "%s" % (exception)
            logger.error(error_output)
            return False
    
    def tablerow_to_dicts(self, row, dict):
        targets = row.find_all("td")
        if len(targets) == 1:
            pass
        else:        
            key = self.string_to_dict_key(targets[0].text.encode("utf-8"))
            value = targets[1].text.encode("utf-8")
            dict[key] = value.strip("&nbsp;")
            return dict

    def string_to_dict_key(self, string):
        """
        converts a string_to_dict_key
        """
        if string is not None:
            return string.strip().lower().replace(":", "").replace(" ", "_").replace("_-_", "_").replace("/", "_")
        else:
            return False

    def write_csv(self, list_of_data):
        fieldnames = [
            'name',
            'administrative_unit',
            'last_update',
            'county',
            'status_notes',
            'location',
            'date_started',
        ]

        with open(self.file_name, "wb") as csv_file:
            csv_output = csv.DictWriter(csv_file, fieldnames=fieldnames, extrasaction='ignore')
            csv_output.writeheader()
            for d in list_of_data: 
                print (d)
                csv_output.writerow(d)

if __name__ == '__main__':
    task_run = ScrapeAtpData()
    task_run.make_request_to_page()
