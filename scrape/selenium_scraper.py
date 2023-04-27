import os

# Selenium imports
from selenium import webdriver                                                  # Webdriver
from selenium.webdriver.common.by import By                                     # Find elements by
from selenium.webdriver.chrome.service import Service                           # Chrome service
from selenium.webdriver.support.ui import WebDriverWait                         # Wait for elements to load
from selenium.webdriver.support import expected_conditions as EC                # Expected conditions
from selenium.common.exceptions import TimeoutException, NoSuchElementException # Misc. exceptions

import time

# Selenium: Path to WebDriver
path_to_webdriver = os.path.join(os.getcwd(), 'chromedriver.exe')

class SeleniumScraper:
    """
    Selenium scraper class
    """ 
    def __init__(self, path_to_local_dir):
        self.service = Service(path_to_webdriver)
        self.service.start()
        self.options = webdriver.ChromeOptions() # Create a new Chrome session
        # self.options.headless = True           # TODO: Not working in HEADLESS mode

        # Set the download directory to the current working directory
        prefs = {'download.default_directory': path_to_local_dir}
        self.options.add_experimental_option('prefs', prefs)

        # Ignore SSL certificate errors
        self.options.add_argument('--ignore-certificate-errors')
        self.options.add_argument('--ignore-ssl-errors')
        self.options.add_argument('--ignore-certificate-errors-spki-list')
        self.options.add_argument('log-level=3')

        self.driver = webdriver.Chrome(service=self.service, options=self.options) # Init Chrome driver
        self.target_json = os.path.join(os.getcwd(), 'repo_scrapers' ,'u4g.json')

    def __enter__(self):
        self.driver = webdriver.Chrome(service=self.service, options=self.options) # Init Chrome driver
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.driver.quit()
        self.service.stop()

    def find_element(self, xpath):
        """
        Find element by xpath
        """
        #print(f"Path to WebDriver: {path_to_webdriver}")
        #print (f"Finding element by xpath: {xpath}")
        return self.driver.find_element(By.XPATH, xpath)