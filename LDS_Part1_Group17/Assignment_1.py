"""
@ Assignment 1: split of the tennis table
@authors: Martina Trigilia, Gianni Andreozzi
"""

import csv
import math
from datetime import datetime


# return the year of birth of a player
def get_year_of_birth(tourney_date, player_age):
    if (float(tourney_date) != -1.0) and (float(player_age) != -1.0):
        date_format = "%Y%m%d"
        tourney_date = datetime.strptime(tourney_date, date_format)
        end_of_the_year = datetime.strptime(str(tourney_date.year) + "1231", date_format)
        fractal = ((end_of_the_year - tourney_date).days * (1 / 365))
        year_of_birth = tourney_date.year - int(float(player_age))
        part = "0." + str((player_age.partition('.')[2]))
        if 1 - float(part) < fractal:
            year_of_birth -= 1
        if year_of_birth >= 2020:
            print(tourney_date)
            print(player_age)
        return year_of_birth
    return -1


# return the gender of a player
def get_sex(player_name, sex_players):
    if player_name in sex_players["male"]:
        sex = "Male"
    else:
        sex = "Female"
    return sex


# return a string which indicates the quarter
def get_quarter(month):
    if str(month) in ['1', '2', '3']:
        return 'Q1'
    elif str(month) in ['4', '5', '6']:
        return 'Q2'
    elif str(month) in ['7', '8', '9']:
        return 'Q3'
    else:
        return 'Q4'


# return a dictionary that maps genders to its players
def map_player(male_db, female_db):
    sex_players = {"male": [], "female": []}
    for row in male_db:
        row_new = row["name"] + ' ' + row["surname"]
        sex_players["male"].append(row_new)
    for row in female_db:
        row_new = row["name"] + ' ' + row["surname"]
        sex_players["female"].append(row_new)
    return sex_players


# create the structure of Date table
def create_date_structure(file_open):
    # this list contains all the attributes needed to create the header
    date_col = ['date_id', 'day', 'month', 'year', 'quarter']
    # open the output file
    file_out = open(file_open, mode="w", newline='')
    file_writer = csv.writer(file_out)
    # insert the header
    file_writer.writerow(date_col)
    # create an empty set to store all the ids, in order to avoid duplicates
    tourney_date_ids = set()
    return file_writer, tourney_date_ids


# create the structure of Match table
def create_match_structure(file_open, tennis_header):
    # this list contains all the attributes needed to create the header
    match_cols = ['match_id', 'tourney_id', 'winner_id', 'loser_id', 'score', 'best_of', 'round', 'minutes',
                  'w_ace', 'w_df', 'w_svpt', 'w_1stIn', 'w_1stWon', 'w_2ndWon', 'w_SvGms', 'w_bpSaved',
                  'w_bpFaced', 'l_ace', 'l_df', 'l_svpt', 'l_1stIn', 'l_1stWon', 'l_2ndWon', 'l_SvGms',
                  'l_bpSaved', 'l_bpFaced', 'winner_rank', 'winner_rank_points', 'loser_rank', 'loser_rank_points']
    # in this list we put all attributes that we have to take from tennis
    match_from_tennis = [x for x in match_cols if x in tennis_header]
    # open the output file
    file_out = open(file_open, mode='w', newline='')
    file_writer = csv.writer(file_out)
    # insert the header
    file_writer.writerow(match_cols)
    return file_writer, match_from_tennis


# create the structure of Tournament table
def create_tournament_structure(file_open, tennis_header):
    # this list contains all the attributes needed to create the header
    tournament_cols = ['tourney_id', 'date_id', 'tourney_name', 'surface', 'draw_size',
                       'tourney_level', 'tourney_spectators', 'tourney_revenue']
    # this list contains all the attributes that needs to be taken from tennis
    tournament_from_tennis = [x for x in tournament_cols if x in tennis_header]
    tournament_from_tennis.insert(1, 'tourney_date')
    # open the output file
    file_out = open(file_open, mode="w", newline='')
    file_writer = csv.writer(file_out)
    # insert the header
    file_writer.writerow(tournament_cols)
    # create an empty set to store all the ids, in order to avoid duplicates
    tourney_ids = set()
    return file_writer, tournament_from_tennis, tourney_ids


# create the structure of Player table
def create_player_structure(file_open):
    # this list contains all the attributes needed to create the header
    player_cols = ['player_id', 'country_id', 'name', 'sex', 'hand', 'ht', 'byear_of_birth']
    # those two lists contains all the attributes that needs to be taken from tennis, winner and loser
    player_winner_from_tennis = ['winner_id', 'winner_ioc', 'winner_name', 'winner_hand', 'winner_ht']
    player_loser_from_tennis = ['loser_id', 'loser_ioc', 'loser_name', 'loser_hand', 'loser_ht']
    # open the output file
    file_out = open(file_open, mode='w', newline='')
    # open the files to get the gender info about players
    female_In = open("dataset/female_players.csv", mode='r')
    male_In = open("dataset/male_players.csv", mode='r')
    male_db = csv.DictReader(male_In, delimiter=',')
    female_db = csv.DictReader(female_In, delimiter=',')
    # dictionary about gender
    sex_players = map_player(male_db, female_db)
    file_writer = csv.writer(file_out)
    # we insert the header
    file_writer.writerow(player_cols)
    # we create an empty set to store all the ids, in order to avoid duplicates
    players_ids = set()
    return file_writer, player_winner_from_tennis, player_loser_from_tennis, players_ids, sex_players


# create Geography table
def create_geo(file_open):
    geo_cols = ['country_code', 'continent', 'language']
    # open two files to get info about language and continent
    country_language_In = open("dataset/country_list.csv", mode='r')
    country_In = open("dataset/countries.csv", mode='r')
    file_lan_reader = csv.DictReader(country_language_In, delimiter=',')
    file_country_reader = csv.DictReader(country_In, delimiter=',')
    # open the output file
    file_out = open(file_open, mode="w", newline='')
    file_writer = csv.writer(file_out)
    # create a dict which maps country name to its language
    lan_country = {rows['country_name']: rows['lang_name'] for rows in file_lan_reader}
    # insert the header
    file_writer.writerow(geo_cols)
    for row in file_country_reader:
        # create the row with IOC and continent info
        row_geo = [row.get(key) for key in geo_cols if key != 'language']
        # if there is info about language of a country add it to the row
        if row['country_name'] in lan_country.keys():
            row_geo.insert(2, lan_country.get(row['country_name']))
            # write the row into the output file
            file_writer.writerow(row_geo)


# insert a row into Match table
def create_match_row(row, match_from_tennis, file_out):
    # to create the row, we first generate match_id from match_num and tourney_id
    match_id = row.get('match_num') + row.get('tourney_id')
    #  then we get the rest of the columns from tennis
    row_match = [row.get(key) for key in match_from_tennis]
    #  insert the match_id in the row
    row_match.insert(0, match_id)
    # insert the row into the table
    file_out.writerow(row_match)


# insert a row into Tournament table
def create_tournament_row(row, tournament_tennis, tourney_ids, file_out):
    # write the row just in the case we have not yet insert this id into the table
    if row.get('tourney_id') not in tourney_ids:
        # get columns from tennis
        row_tournament = [row.get(key) for key in tournament_tennis]
        # insert the row into the table
        file_out.writerow(row_tournament)
        # insert the id into the set
        tourney_ids.add(row.get('tourney_id'))


# insert a row into Player table
def create_player_row(row, tennis_player, players_ids, sex_players, file_out, player_id, player_name, player_age):
    # write the row just in the case we have not yet insert this id into the table
    if player_id not in players_ids:
        # get columns from tennis
        row_player = [row.get(key) for key in tennis_player]
        # get gender of the player
        row_player.insert(3, get_sex(player_name, sex_players))
        # get year of birth of the player
        row_player.append(get_year_of_birth(row["tourney_date"], player_age))
        # insert the row into the table
        file_out.writerow(row_player)
        # insert the id into the set
        players_ids.add(player_id)


# insert a row into Date table
def create_date_row(row, tourney_date_ids, file_date):
    # write the row just in the case we have not yet insert this id into the table
    if row.get('tourney_date') not in tourney_date_ids:
        # get date_id from tennis
        row_date_id = row.get('tourney_date')
        # convert it into datetime and create the row
        datetime_object = datetime.strptime(row_date_id, "%Y%m%d")
        row_date = [row_date_id, datetime_object.day, datetime_object.month, datetime_object.year,
                    get_quarter(datetime_object.month)]
        # insert the row into the table
        file_date.writerow(row_date)
        # insert the id into the set
        tourney_date_ids.add(row.get('tourney_date'))


# execute the split of tennis
def create_tables(tennis_db, output_file):
    tennis_header = tennis_db.fieldnames
    print("*** Create Geography Table ***")
    create_geo(output_file['geography'])

    print("*** Create Match Structure ***")
    file_match, match_from_tennis = create_match_structure(output_file['match'], tennis_header)

    print("*** Create Tourney Table Structure ***")
    file_tournament, tournament_from_tennis, tourney_ids = create_tournament_structure(output_file['tournament'],
                                                                                       tennis_header)
    print("*** Create Player Table Structure ***")
    file_player, player_winner_from_tennis, player_loser_from_tennis, players_ids, sex_players = \
        create_player_structure(output_file['player'])

    print("*** Create Date Table Structure ***")
    file_date, tourney_date_ids = create_date_structure(output_file['date'])
    print("*** Insert row in each table ***")

    for row in tennis_db:
        create_match_row(row, match_from_tennis, file_match)

        create_tournament_row(row, tournament_from_tennis, tourney_ids, file_tournament)

        create_date_row(row, tourney_date_ids, file_date)

        create_player_row(row, player_winner_from_tennis, players_ids, sex_players, file_player, row["winner_id"]
                          , row["winner_name"], row["winner_age"])
        create_player_row(row, player_loser_from_tennis, players_ids, sex_players, file_player, row["loser_id"],
                          row["loser_name"], row["loser_age"])

    print("*** Match Table created ***")
    print("*** Tourney Table created ***")
    print("*** Date Table created ***")
    print("*** Player Table created ***")

def main():
    output_file = {'match': 'dataset/match.csv', 'tournament': 'dataset/tournament.csv', 'date': 'dataset/date.csv',
                   'player': 'dataset/player.csv', 'geography': 'dataset/geography.csv'}

    tennis_In = open("dataset/tennis.csv", mode='r')
    tennis_db = csv.DictReader(tennis_In, delimiter=',')
    print("*** CREATE ALL TABLES ***")
    create_tables(tennis_db, output_file)


if __name__ == "__main__":
    main()
