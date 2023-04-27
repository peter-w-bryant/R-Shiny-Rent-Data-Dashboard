import json
import os
import csv
import time
from selenium_scraper import SeleniumScraper
import sys
import string

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CSV_FILE = os.path.join(BASE_DIR,'cali_rentdata.csv')
RENT_DATA_BASE_URL = "https://www.rentdata.org/states/california/"
download_dir_path = os.path.join(BASE_DIR, 'resources')
years = range(2006, 2023)

def rent_data_to_csv():
    # create a new csv file or overwrite existing file
    with open(CSV_FILE, 'w', newline='') as csvfile:
        csvwriter = csv.writer(csvfile)
        csvwriter.writerow(['Year', 'County', '0BR', '1BR', '2BR', '3BR', '4BR'])
        with SeleniumScraper(download_dir_path) as scraper:
            for year in years:
                year_url = RENT_DATA_BASE_URL + str(year)
                scraper.driver.get(year_url) # Go to the url
                time.sleep(2) # Wait for page to load
                table_body_xpath = '//*[@id="state_table"]/tbody'
                i = 1
                while(True):
                    table_row_xpath = table_body_xpath + f'/tr[{i}]'
                    try:
                        row = scraper.find_element(table_row_xpath)
                        try:
                            row_list = row.text.split()
                            row_list.remove('County')
                            row_list.remove('Metro')
                        except ValueError as e: # if either 'County' or 'Metro' is not in row_list
                            pass
                        # for each element in row_list, only keep the numbers
                        for j in range(1, len(row_list)):
                            row_list[j] = row_list[j].replace(',', '').replace('$', '')

                        # if the second element is a digit, the the first element is county name and the second element is the 0 BR
                        if row_list[1].isdigit():
                            row_list = [year, row_list[0], row_list[1], row_list[2], row_list[3], row_list[4], row_list[5]]
                        # if the second element is not a digit, then the first element is county name and the second element is part of the county name
                        else:
                            row_list = [year, f"{row_list[0]} {row_list[1]}", row_list[2], row_list[3], row_list[4], row_list[5], row_list[6]]
                        print(row_list)
                        csvwriter.writerow(row_list)
                        i += 1
                    except Exception as e:
                        print(e)
                        break

if __name__ == '__main__':
    start = time.time()
    rent_data_to_csv()
    end = time.time()
    print(f'Elapsed time: {end - start}')