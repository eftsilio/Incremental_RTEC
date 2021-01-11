from os import sys, path
from multiprocessing import Process
from os import listdir
from os.path import isfile, join, exists
import argparse
import subprocess, os, time
from time import sleep
import timeit
import shutil
import csv
import math
from datetime import datetime


convert = lambda text: int(text) if text.isdigit() else text
alphanum_key = lambda key: [ convert(c) for c in re.split('([0-9]+)', key) ]

examples = {'b': 'maritime_brest', 'i': 'maritime_imis', 'f': 'fleet_management'}
times = {'b': [1443650401, 1459461590], 'i': [1451606401, 1454284790], 'f': [1530399600, 1533705300]}


def main(args):

	if not os.path.exists(args['results_dir']):
		os.makedirs(args['results_dir'])

	for d in args['delays']:
		if not os.path.exists(args['results_dir']+d):
			os.makedirs(args['results_dir']+d)

	# Run experiments

	print('Running experiments...')
	dstart = datetime.now()

	run_from_file(args)

	dend = datetime.now()
	dexp = dend - dstart
	print('System time elapsed for experiments...')
	print(dexp)

	print('Done!')


def run_from_file(args):

	start = args['start_time']

	end = args['end_time']

	step = args['step']
    

	for w in args['window']:
        
		wm = w
	  
		processes = []
		
		for nd, d in enumerate(args['delays']):
			
			inc_RTEC_stats_file = os.path.join(args['results_dir']+d, 'stats-wm=%s-step=%s' % (wm, step))

			inc_RTEC_patterns_file = os.path.join(args['results_dir']+d, 'CEs-wm=%s-step=%s' % (wm, step))

			inc_RTEC_command = getCommand(args['data'][nd], str(start), str(wm), str(step), str(end),
				inc_RTEC_stats_file, inc_RTEC_patterns_file)

			inc_RTEC_prolog_args = [['yap', '-s', '0', '-h', '0', '-t', '0', '-l', args['prologFile'], '-g', inc_RTEC_command]]

			print("New Incremental RTEC process... ")
			print(inc_RTEC_prolog_args)

			sleep(1)
			p_inc_RTEC = Process(target=run_prolog, args=inc_RTEC_prolog_args)
			processes.append(p_inc_RTEC)

		for p in processes:
			p.start()
		for p in processes:
			p.join()


def run_prolog(prolog_args):
	print("\nInvoking YAP Prolog. Wait...\n")
	sleep(1)
	subprocess.call(prolog_args)
    

def getCommand(datafile, start, wm, step, end, statsFile, patternsFile):
	dfstr = '[\'' + datafile + '\']'
	return 'performFullER(%s,\'%s\',\'%s\',%s,%s,%s,%s),halt.' % (dfstr, statsFile, patternsFile, str(start), str(wm),
		str(step), str(end))


if __name__ == "__main__":

	apps = ['b', 'i', 'f']
	delays = set(['5', '10', '20', '40', '80'])
	args = {}

	print('To run experiments you have first to select one of the following applications:\n'
	'1. For Brest maritime monitoring, please type b.\n'
	'2. For Imis maritime monitoring, please type i.\n'
	'3. For Fleet management, please type f.\n')

	while True:

		args['app'] = input()

		if args['app'].lower() not in apps:
			print('Your choice is not correct. Please try again.\n')
			continue

		topDir = os.getcwd()
		example = examples[args['app']]
		inc_RTEC_dir = topDir + '/examples/' + example + '/data/'
		inc_RTEC_data = inc_RTEC_dir + 'dynamic_data/'
		inc_RTEC_data_files = sorted([inc_RTEC_data+f for f in listdir(inc_RTEC_data) if f[-4:] == '.csv'], key=alphanum_key)
		args['results_dir'] = inc_RTEC_dir + 'results/'
		args['prologFile'] = inc_RTEC_dir + 'loadFiles.prolog'
		args['start_time'] = times[args['app']][0]
		args['end_time'] = times[args['app']][1]

		if not inc_RTEC_data_files:
			print('You have first to download the dataset(s) of the application.\n')
			sys.exit()

		if args['app'] != 'i':
			print('Please select the percentage of the delayed events. The available choices are 5, 10, 20, 40, 80.\n'
			'If you want to run experiments with more than one delay percentage, please seperate your choices with space.\n'
			'For example, type: 5 10 40\n'
			'If you want to run experiments with all the available delays, just type a.\n')

			while True:
				dc = input().split(' ')
				answer = list(set(dc) - delays)
				if dc == 'a' or 'a' in dc:
					args['delays'] = sorted(delays, key=alphanum_key)
					args['data'] = inc_RTEC_data_files
					break
				elif answer:
					print('Some or all of your choices are not correct. Please try again.\n')
					continue
				else:
					args['delays'] = sorted(dc, key=alphanum_key)
					args['data'] = []
					for d in args['delays']:
						args['data'] = args['data'] + [f for f in inc_RTEC_data_files if d in f]
					break

		else:
			args['data'] = inc_RTEC_data_files

		print('Please type the size of the window in seconds. If you want to run experiments with more than one window sizes, please seperate them with space.\n'
		'For example, type: 3600 7200 14400\n')

		while True:
			w = set(input().split(' '))
			try:
				w = sorted([int(i) for i in w])
				args['window'] = w
				break
			except ValueError:
				print('The window size must be a number. Please try again.\n')
				continue

		print('Please type the size of the sliding step in seconds. The sliding step must be smaller or equal to the smaller window.\n'
		'For example, type: 3600\n')

		while True:
			step = input()
			try:
				step = int(step)
				if step <= args['window'][0]:
					args['step'] = int(step)
					break
				else:
					print('The sliding step must be smaller or equal to the smaller window. Please try again.\n')
				continue
			except ValueError:
				print('The sliding step must be a number. Please try again.\n')
				continue

		if args['data']:
			break

	print("\nParameters set:")
	print(args)
	print("\n")
	main(args)

