import time

from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np

url = "https://www.transfermarkt.com/laliga/startseite/wettbewerb/L1"
options = webdriver.ChromeOptions()
path = r'C:\Users\pawel.szydlik\chromedriver.exe'

driver = webdriver.Chrome(path, options=options)
driver.get(url)
page_source = driver.page_source
soup = BeautifulSoup(page_source, 'html.parser')
table_data = soup.find_all('table', class_='items')
# table_final = table_from_soup(table_data[0])

html_club_name = []
html_club_no = []
for tr in table_data[0].find_all('tr')[2:]:
    for a in tr.find_all('a'):
        try:
            html_club_name.append(a['href'].split('/')[1])
            html_club_no.append(a['href'].split('/')[4])
        except:
            next

html_club_name = list(set(html_club_name))
html_club_no = list(set(html_club_no))
driver.quit()


def table_from_soup_players(html_table):
    headers = []
    for i in html_table.find_all('th'):
        title = i.text
        headers.append(title)

    df = pd.DataFrame(columns=headers)
    for j in html_table.find_all('tr')[1:]:
        row_data = j.find_all('td')
        row = [tr.text for tr in row_data]

        row_data_name = j.find_all('a')
        row_name = [tr.text for tr in row_data_name]

        row = row[:2] + row[5:]

        try:
            row[1] = next(s for s in row_name if s)
            length = len(df)
            df.loc[length] = row
        except:
            next

    return df


def get_players_last_10_years(html_name_of_club, html_no_of_club):
    driver = webdriver.Chrome(path, options=options)
    for season in range(10, 22):
        try:
            url = "https://www.transfermarkt.com/" + html_name_of_club + "/startseite/verein/" + html_no_of_club + "?saison_id=20" + str(
                season)
            time.sleep(8)
            driver.get(url)
            page_source = driver.page_source
            soup = BeautifulSoup(page_source, 'html.parser')
            table_data = soup.find_all('table', class_='items')
            team_name = soup.find_all('h1')[0].text
            team_name = team_name.replace("\n", "")
            table_final_new = table_from_soup_players(table_data[0])
            table_final_new["season"] = int("20" + str(season))

            if season == 10:
                table_final = table_final_new
            else:
                table_final = table_final.append(table_final_new, ignore_index=True)
        except:
            next

    try:
        table_final = table_final.iloc[:, :-1]
        table_final["Market value"] = table_final["Market value"].str.replace("\xa0", "")
        table_final["Date of birth / Age"].str.split("(", 1)
        table_final[["Date of birth", "Age"]] = pd.DataFrame(
            table_final["Date of birth / Age"].str.split("(", 1).tolist(),
            columns=['Date of birth', 'Age'])
        table_final["Age"] = table_final["Age"].str.replace(")", "")
        table_final.drop(columns=["Date of birth / Age", "Nat.", "Current club"], inplace=True)
        table_final["Market value"] = table_final["Market value"].str.replace("€", "")

        table_final['Unit'] = [1000000 if 'm' in x else 1000 for x in table_final['Market value']]
        table_final["Market value"] = table_final["Market value"].str.replace("Th.", "").str.replace("m", ""). \
            str.replace("-", "")
        table_final.replace("", np.nan, inplace=True)
        table_final["Market value"] = table_final["Market value"].astype('float')
        table_final["Price"] = table_final["Market value"] * table_final['Unit']
        table_final["Price"] = table_final["Price"].astype('Int64')
        table_final["player"] = table_final["player"].str.replace("\u200e", "")
        table_final["player"] = table_final["player"].str.replace("\u0107", "")
        table_final["player"] = table_final["player"].str.replace("\u0160", "")
        table_final["player"] = table_final["player"].str.replace("\u2020", "")
        table_final.to_csv("player_values/" + team_name + ".csv", encoding="utf-8", sep=";")

        driver.quit()
        return table_final
    except:
        next


for number in html_club_no:
    get_players_last_10_years('newcastle-united', number)
