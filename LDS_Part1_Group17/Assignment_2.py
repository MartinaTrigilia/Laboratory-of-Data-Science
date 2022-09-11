"""
@ Assignment 2: populate the database
@authors: Martina Trigilia, Gianni Andreozzi
"""

import pyodbc
import csv

output_file_dict = {
    'match':
        ['dataset/match.csv',
         "INSERT INTO Match2(match_id, tourney_id, winner_id, loser_id, score, best_of, round, minutes,\
        w_ace, w_df,w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_SvGms, w_bpSaved,w_bpFaced, l_ace, l_df,\
        l_svpt, l_1stIn, l_1stWon,l_2ndWon, l_SvGms,l_bpSaved, l_bpFaced, winner_rank, winner_rank_points,\
         loser_rank, loser_rank_points) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"],

    'tournament':
        ['dataset/tournament.csv', "INSERT INTO Tournament(tourney_id, date_id, tourney_name, surface, draw_size ,\
                                   tourney_level,tourney_spectators,tourney_revenue) VALUES(?,?,?,?,?,?,?,?)"],

    'date':
        ['dataset/date.csv', "INSERT INTO Date(date_id,day, month, year, quarter) VALUES(?,?,?,?,?)"],

    'player':
        ['dataset/player.csv',
         "INSERT INTO Player(player_id, country_id, name, sex, hand, ht, byear_of_birth) VALUES(?,?,?,?,?,?,?)"],

    'geography':
        ['dataset/geography.csv',
         "INSERT INTO Geography(country_ioc, continent, language)VALUES(?,?,?)"]
}


def player_to_db(fileIn, sql, cursor, cn):
    file_out = open(fileIn, mode='r')
    fileIn = csv.DictReader(file_out, delimiter=',')
    for row in fileIn:
        cursor.execute(sql, row['player_id'], row['country_id'], row['name'], row['sex'], row['hand'], int(float(row['ht'])),
                       row['byear_of_birth'])
    cn.commit()
    print("Player Table - UPLOADED ON SERVER")


def date_to_db(fileIn, sql, cursor, cn):
    file_out = open(fileIn, mode='r')
    fileIn = csv.DictReader(file_out, delimiter=',')
    for row in fileIn:
        cursor.execute(sql, row['date_id'], row['day'], row['month'], row['year'], row['quarter'])
    cn.commit()
    print("Date Table - UPLOADED ON SERVER")


def geo_to_db(fileIn, sql, cursor, cn):
    file_out = open(fileIn, mode='r')
    fileIn = csv.DictReader(file_out, delimiter=',')
    for row in fileIn:
        cursor.execute(sql, row['country_code'], row['continent'], row['language'])
    cn.commit()
    print("Geography Table - UPLOADED ON SERVER")


def tournament_to_db(fileIn, sql, cursor, cn):
    file_out = open(fileIn, mode='r')
    fileIn = csv.DictReader(file_out, delimiter=',')
    for row in fileIn:
        cursor.execute(sql, row['tourney_id'], row['date_id'], row['tourney_name'], row['surface'], row['draw_size'],
                       row['tourney_level'], row['tourney_spectators'], row['tourney_revenue'])
    cn.commit()
    print("Tournament Table - UPLOADED ON SERVER")


def match_to_db(fileIn, sql, cursor, cn):
    file_out = open(fileIn, mode='r')
    fileIn = list(csv.DictReader(file_out, delimiter=','))
    cc = 0
    for _ in fileIn:
        cc += 1
    k = cc / 5
    i = 0
    print(cc)
    for idx, row in enumerate(fileIn, 1):
        cursor.execute(sql, row['match_id'], row['tourney_id'], int(float(row['winner_id'])),
                       int(float(row['loser_id'])), row['score'],
                       int((row['best_of'])), row['round'], int(float(row['minutes'])),
                       int(float(row['w_ace'])), int(float(row['w_df'])), int(float(row['w_svpt'])),
                       int(float(row['w_1stIn'])), int(float(row['w_1stWon'])), int(float(row['w_2ndWon'])),
                       int(float(row['w_SvGms'])), int(float(row['w_bpSaved'])),
                       int(float(row['w_bpFaced'])), int(float(row['l_ace'])), int(float(row['l_df'])),
                       int(float(row['l_svpt'])), int(float(row['l_1stIn'])), int(float(row['l_1stWon'])),
                       int(float(row['l_2ndWon'])), int(float(row['l_SvGms'])),
                       int(float(row['l_bpSaved'])), int(float(row['l_bpFaced'])),
                       int(float(row['winner_rank'])), int(float(row['winner_rank_points'])),
                       int(float(row['loser_rank'])), int(float(row['loser_rank_points'])) )

        if ((idx % k == 0) & (idx != 0)) or idx == cc:
            print("commit on Match Table")
            cn.commit()
    print("Match Table - UPLOADED ON SERVER")



# connect to Group_17_DB
def main():
    server = 'lds.di.unipi.it'
    database = 'Group_17_DB'
    username = 'Group_17'
    password = '1CUBUGUW'
    connectionString = 'DRIVER={ODBC Driver 17 for SQL Server};SERVER=' + server + ';DATABASE=' + database + ';UID=' \
                       + username + ';PWD=' + password
    cn = pyodbc.connect(connectionString)
    cursor = cn.cursor()

    date_to_db(output_file_dict.get('date')[0], output_file_dict.get('date')[1], cursor, cn)

    geo_to_db(output_file_dict.get('geography')[0], output_file_dict.get('geography')[1], cursor, cn)

    player_to_db(output_file_dict.get('player')[0], output_file_dict.get('player')[1], cursor, cn)

    tournament_to_db(output_file_dict.get('tournament')[0], output_file_dict.get('tournament')[1], cursor, cn)

    match_to_db(output_file_dict.get('match')[0], output_file_dict.get('match')[1], cursor, cn)

    cursor.close()
    cn.close()


if __name__ == "__main__":
    main()
