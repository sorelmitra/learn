import os
import random
import time

from selenium.webdriver.common.keys import Keys

from site_base import SiteBase


class SitePassportsRo(SiteBase):
    app_name = 'EPASAPOARTE'
    last_county = None

    def open(self, path=None):
        url = f"{self.config[f'{self.app_name}.URL']}{'' if path is None else '/' + path}"
        print("Open URL", url)
        self.driver.get(url)
        self.wait_for_element_with_text('Instructiuni')

    def check_availability(self, counties, months, year, ignore_list):
        self.select_document_type()
        found = False
        n = len(counties)
        for i in range(0, n):
            county = counties[i]
            self.select_county(county)
            self.last_county = county
            if self.check_month_availability(county, months, year, ignore_list):
                found = True
            time.sleep(1)
            if i < n - 1:
                self.click_element_with_text(county)
        if found:
            os.system('say "I have found some of the months you\'re looking for!"')
        else:
            os.system('say "Still looking..."')

    def select_document_type(self):
        document = 'Tipul de document'
        self.click_ancestor_of_element_with_text(document, 1)
        if self.last_county is not None:
            self.select_typeable_list_item(document)
            time.sleep(2)
        self.select_typeable_list_item('pasaport simplu electronic')
        time.sleep(0.3)
        self.send_action_keys(Keys.ENTER)

    def select_county(self, county):
        self.click_element_with_text(self.last_county or 'Toate')
        time.sleep(1)
        self.click_descendant_of_sibling_of_ancestor_of_element_with_text(county, 'div', 'button', ancestor_count=2, index=0, sibling_index=1)
        time.sleep(1)

    def check_month_availability(self, county, months, year, ignore_list):
        found = False
        for month in months:
            for combination in self.get_month_combinations(month, year):
                try:
                    if self.find_element_with_text(combination) is not None:
                        ignored = False
                        for ignore_item in ignore_list:
                            if county == ignore_item[0] and month == ignore_item[1]:
                                ignored = True
                        print(f'Found {combination} in {county}{", ignored" if ignored else ""}')
                        if not ignored:
                            found = True
                except IndexError:
                    pass
        return found

    def get_month_combinations(self, month, year):
        month_dict = {
            'March': ['m', 'ma', 'mar', 'mart', 'martie'],
            'April': ['a', 'ap', 'apr', 'april', 'aprilie'],
            'June': ['iun']
        }
        months = month_dict[month]
        combinations = []
        for m in months:
            combinations.append(f'{m} {year}')
        return combinations


if __name__ == '__main__':
    e = SitePassportsRo()
    e.open()
    while True:
        e.check_availability(['Valcea', 'Sibiu', 'Gorj', 'Arges'], ['March', 'April'], '2022', [
            ['Gorj', 'April']
        ])
        # time.sleep(random.randint(5, 6))
        time.sleep(random.randint(5 * 60, 10 * 60))
