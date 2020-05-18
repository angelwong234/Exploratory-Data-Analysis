import pandas as pd
import csv

#
negative=list()
positive=list()
with open("train.ft.txt", "r",encoding='UTF-8') as file:
    for line in file:
        review= line.split(": ")
        if "__label__2" in review[0]:
            title= review[0].split("__label__2 ")[1]
            sentiment="positive"
            positive.append([sentiment, title])
        else:
            title = review[0].split("__label__1 ")[1]
            sentiment = "negative"
            negative.append([sentiment, title])


print(negative)
# print(negative)
with open('amazon_titles.csv', 'w',newline="") as wr:
    wr_file = csv.writer(wr)
    wr_file.writerows(positive)
    wr_file.writerows(negative)

