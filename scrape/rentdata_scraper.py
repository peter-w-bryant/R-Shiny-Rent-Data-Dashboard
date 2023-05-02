import json
import os
import csv
import time
from selenium_scraper import SeleniumScraper
import sys
import string

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
csv_dir_path = os.path.join(BASE_DIR, 'csv_files')
download_dir_path = os.path.join(BASE_DIR, 'resources')
years = range(2006, 2022)
#years = [2006]

def rent_data_to_csv(state_name):
    CSV_FILE = os.path.join(csv_dir_path, f'{state_name}_rentdata.csv')
    RENT_DATA_BASE_URL = f"https://www.rentdata.org/states/{state_name}/"
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
                            if 'County' in row_list:
                                row_list.remove('County')
                            if 'Metro' in row_list:
                                row_list.remove('Metro')
                            if 'town' in row_list:
                                row_list.remove('town')
                            if 'Town' in row_list:
                                row_list.remove('Town')
                            if 'city' in row_list:
                                row_list.remove('city')
                            if 'Parish' in row_list:
                                row_list.remove('Parish')
                        except ValueError as e: 
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
                        
                        csvwriter.writerow(row_list)
                        i += 1
                    except Exception as e:
                        print(e)
                        break
    
def merge_all_csv():
    # create a new csv file or overwrite existing file
    with open('rentdata.csv', 'w', newline='') as csvfile:
        csvwriter = csv.writer(csvfile)
        csvwriter.writerow(['State', 'Year', 'County', '0BR', '1BR', '2BR', '3BR', '4BR'])
        for state in state_names:
            CSV_FILE = os.path.join(csv_dir_path, f'{state}_rentdata.csv')
            with open(CSV_FILE, 'r') as csv_file:
                csvreader = csv.reader(csv_file)
                next(csvreader)
                for row in csvreader:
                    if "-" in state:
                        state = state.replace("-", " ")
                    if state == "Massachusetts":
                        #replace " town" or " city" in county name with ""
                        row[1] = row[1].replace("town", "").replace("city", "").strip()
                       # print(row)
                    csvwriter.writerow([state] + row)
                    
if __name__ == '__main__':
    start = time.time()
    state_names = ["Alaska", "Alabama", "Arkansas", "Arizona",\
                   "California", "Colorado", "Connecticut", "district-of-columbia",\
                   "Delaware", "Florida", "Georgia", "Iowa", "Idaho",\
                   "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts",\
                   "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", "Mississippi",\
                   "Montana", "North-Carolina", "North-Dakota", "Nebraska", "New-Hampshire",\
                   "New-Jersey", "New-Mexico", "Nevada", "New-York", "Ohio", "Oklahoma", "Oregon",\
                   "Pennsylvania", "Rhode-Island", "South-Carolina", "South-Dakota",\
                   "Tennessee", "Texas", "Utah", "Virginia", "Vermont", "Washington",\
                   "Wisconsin", "West-Virginia", "Wyoming"]
    
    # state_names = ["Massachusetts"]

    # state_names = ["Vermont", "Maine", "New-Hampshire", "Louisiana"]
    # for state in state_names:
    #     rent_data_to_csv(state)

    merge_all_csv()
    end = time.time()
    print(f'Elapsed time: {end - start}')