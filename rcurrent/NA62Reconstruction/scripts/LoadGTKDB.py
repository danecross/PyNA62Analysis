import sys
import sqlite3
import os
from subprocess import call
import tempfile

"""
    This is a semi-automated tool to load the GTK DB
                *** VERSION ALPHA ***
"""

"""
    Settings:
    1. Run validity range
        Syntax: correction_set:(from run ,to run)
    2. Path to the data files
"""

# SQL connection
g_db_conn = None

run_mapping = {6291: [(6250, 6353)],  # 100 V
               6467: [(6354, 6501)],  # 100 V
               6525: [(6502, 6587)],  # 100 V
               6614: [(6588, 6626)],  # 100 V
               # 6632: [(6627,6658)], # 100 V
               # 6670: [(6659,6678)], # 100 V
               6670: [(6627, 6705)],  # 100 V
               # 6683: [(6679,6705)], # 100 V
               6761: [(6706, 6784)],  # 100 V
               6871: [(6785, 6790), (6796, 6897)],  # 150 V
               6792: [(6791, 6792)],  # 250 V (Note: Bias was set to 200 V for run 6791  / correction not available)
               6899: [(6793, 6795), (6898, 6912)]}  # 150/300/300 V

data_path = '/eos/na62/user/m/mperrint/Na62/Ana/TimeCalibration_{run}/Corrections/'
time_walk = 'time-walk_gtk{station}-chip{chip}'
time_walk_binning = 'time-walk_binning'
t_zero = 't-zero_gtk{station}-chip{chip}'


def get_file(eos_full_path):
    """
        Get a copy of a file stored on EOS
        The content is stored in a tmp. file
        WARNING: Is the user's responsibility to delete the tmp. file
    """

    tmp_file = tempfile.NamedTemporaryFile(delete=False)
    tmp_file.close()
    call(['eos', 'cp', eos_full_path, tmp_file.name])
    return tmp_file.name


def get_run_map():
    """
        Read the current run map
    """
    global g_db_conn
    cur = g_db_conn.cursor()
    for row in cur.execute('SELECT run_id,set_id FROM runs'):
        print row['run_id'], row['set_id']


def set_run_map(runs_list, set_id):
    """
        Create the map between the run_id and the set_id
    """
    global g_db_conn
    cur = g_db_conn.cursor()
    for run in runs_list:
        cur.execute('INSERT INTO runs (run_id,set_id) VALUES (?,?)', (run, set_id))
    g_db_conn.commit()


def create_tables():
    """
        Creates (if not exists):
         * runs: for the mapping between the runs and the parameter set,
         * time_walk_binning: for the time-walk correction binning,
         * time_walk_gtk{0,2}_chip{0,9}: for the time-walk corrections,
         * t_zero_gtk{0,2}_chip{0,9}: for the t zero corrections,

    """
    global g_db_conn
    cur = g_db_conn.cursor()
    print 'Creating tables ...'
    cur.execute("CREATE TABLE IF NOT EXISTS runs (run_id INTEGER NOT NULL, set_id INTEGER NOT NULL, PRIMARY KEY(run_id));")
    cur.execute("CREATE TABLE IF NOT EXISTS time_walk_binning (set_id INTEGER NOT NULL, bin_id INTEGER NOT NULL, bin_low_edge REAL NOT NULL, PRIMARY KEY(set_id, bin_id));")
    for station in range(3):

        for chip in range(10):
            statement = "CREATE TABLE IF NOT EXISTS time_walk_gtk{0}_chip{1} (set_id INTEGER NOT NULL, bin_id INTEGER NOT NULL, time_walk REAL NOT NULL, PRIMARY KEY(set_id, bin_id));".format(str(station), str(chip))
            cur.execute(statement)
            statement = "CREATE TABLE IF NOT EXISTS t_zero_gtk{0}_chip{1} (set_id INTEGER NOT NULL, pixel_id INTEGER NOT NULL, t_zero REAL NOT NULL, PRIMARY KEY(set_id, pixel_id));".format(str(station), str(chip))
            cur.execute(statement)

    g_db_conn.commit()


def load_time_walk_binning(input_file, set_id):
    """
        Load the time-walk binning.
        Binning follows the ROOT scheme
        - bin_id 0: first bin with low-edge 0 included
        - bin_id N: last bin with low-edge N+1 excluded

        That is, for N bins, we need to store N+1 edge. An extra bin (N+1) is used to represent the last edge.
    """
    global g_db_conn
    cur = g_db_conn.cursor()
    with open(input_file, 'r') as fh:
        for bin_id, line in enumerate(fh):
            bin_edge = line.strip()
            statement = 'INSERT INTO time_walk_binning (set_id, bin_id, bin_low_edge) VALUES (?,?,?)'
            cur.execute(statement, (set_id, bin_id, bin_edge))

    g_db_conn.commit()


def load_time_walk(input_file, station, chip, set_id):
    """
        Load the time-walk corrections
        Each line corresponds to a time-over-threshold bin, see above for the bin definition
    """
    global g_db_conn
    cur = g_db_conn.cursor()
    with open(input_file, 'r') as fh:
        for bin_id, line in enumerate(fh):
            time_walk = line.strip()
            statement = 'INSERT INTO time_walk_gtk' + str(station) + '_chip' + str(chip) + '(set_id, bin_id, time_walk) VALUES (?,?,?)'
            cur.execute(statement, (set_id, bin_id, time_walk))

    g_db_conn.commit()


def load_t_zero(input_file, station, chip, set_id):
    """
        Load the t0 corrections
        The file format is 'pixel_id t_zero'
    """
    global g_db_conn
    cur = g_db_conn.cursor()
    with open(input_file, 'r') as fh:
        for line in fh:
            toks = line.strip().split(' ')
            pixel_id = toks[0]
            t_zero = toks[1]
            statement = 'INSERT INTO t_zero_gtk' + str(station) + '_chip' + str(chip) + '(set_id, pixel_id, t_zero) VALUES (?,?,?)'
            cur.execute(statement, (set_id, pixel_id, t_zero))

    g_db_conn.commit()


def copy_parameters(source_set_id, target_set_id):
    """
        Copy all the (keys,value) with set_id "source_set_id"
        The new copy will have the set_it "target_set_id"
        This is a weak point! Need to find a better architecture
    """

    global g_db_conn
    cur = g_db_conn.cursor()
    statement = 'INSERT INTO parameters (set_id,key,value) SELECT ?,key,value FROM parameters WHERE set_id = ?'
    cur.execute(statement, (target_set_id, source_set_id))
    g_db_conn.commit()


def main():
    global g_db_conn
    if(len(sys.argv) != 2):
        print 'usage: ' + sys.argv[0] + ' config_db'
        return -1

    config_db = sys.argv[1]

    g_db_conn = sqlite3.connect(config_db)
    g_db_conn.row_factory = sqlite3.Row

    # create_tables()

    # get_run_map()

    for set_id in run_mapping:
        runs = []
        for run_range in run_mapping[set_id]:
            runs += list(range(run_range[0], run_range[1] + 1))
        set_run_map(runs, set_id)

    if(False):
        for run in [6632, 6683]:
            print '==== Loading correction Set ' + str(run) + ' ===='

            time_walk_binning_path = data_path.format(run=run) + time_walk_binning
            time_walk_binning_file = get_file(time_walk_binning_path)
            load_time_walk_binning(time_walk_binning_file, run)
            os.unlink(time_walk_binning_file)

            for station in range(3):
                for chip in range(10):
                    time_walk_path = data_path.format(run=run) + time_walk.format(station=station, chip=chip)

                    time_walk_file = get_file(time_walk_path)
                    load_time_walk(time_walk_file, station, chip, run)
                    os.unlink(time_walk_file)

                    t_zero_path = data_path.format(run=run) + t_zero.format(station=station, chip=chip)

                    t_zero_file = get_file(t_zero_path)
                    load_t_zero(t_zero_file, station, chip, run)
                    os.unlink(t_zero_file)

    g_db_conn.close()


if __name__ == '__main__':
    sys.exit(main())
