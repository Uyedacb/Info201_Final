with open('LLCP2016.ASC_','r') as f:
	lines = f.read().split('\n')
	
	#print lines[0:2]
	
	colnames = {
		'_STATE' : (1, 2),
		'IMONTH': (19,2),
		'IDAY': (21,2),
		'IYEAR' : (23,4),
		'ADDEPEV2' : (113,1),
		'SEX' :(120,1),
		'MENTHLTH' : (93,2)
		}
		
	states = {
		'01':'Alabama',
		'02':'Alaska',
		'04':'Arizona',
		'05':'Arkansas',
		'06':'California',
		'08':'Colorado',
		'09':'Connecticut',
		'10':'Delaware',
		'11':'District of Columbia',
		'12':'Florida',
		'13':'Georgia',
		'15':'Hawaii',
		'16':'Idaho',
		'17':'Illinois',
		'18':'Indiana',
		'19':'Iowa',
		'20':'Kansas',
		'21':'Kentucky',
		'22':'Louisiana',
		'23':'Maine',
		'24':'Maryland',
		'25':'Massachusetts',
		'26':'Michigan',
		'27':'Minnesota',
		'28':'Mississippi',
		'29':'Missouri',
		'30':'Montana',
		'31':'Nebraska',
		'32':'Nevada',
		'33':'New Hampshire',
		'34':'New Jersey',
		'35':'New Mexico',
		'36':'New York',
		'37':'North Carolina',
		'38':'North Dakota',
		'39':'Ohio',
		'40':'Oklahoma',
		'41':'Oregon',
		'42':'Pennsylvania',
		'44':'Rhode Island',
		'45':'South Carolina',
		'46':'South Dakota',
		'47':'Tennessee',
		'48':'Texas',
		'49':'Utah',
		'50':'Vermont',
		'51':'Virginia',
		'53':'Washington',
		'54':'West Virginia',
		'55':'Wisconsin',
		'56':'Wyoming',
		'66':'Guam',
		'72':'Puerto Rico',
		'78':'Virgin Islands',
	}
	array = []
	array.append(','.join(colnames.keys()))
	for line in lines:
		entries = []
		for cat in colnames:
			if cat == '_STATE':
				cstate = line[colnames[cat][0] - 1 : colnames[cat][0] - 1 + colnames[cat][1]]
				t = states[cstate]
				entries.append(t)
			else:
				entries.append(line[colnames[cat][0] - 1 : colnames[cat][0] - 1 + colnames[cat][1]])
		array.append(','.join(entries))
	result = '\n'.join(array)
	with open('stripped_data.csv','w') as g:
		g.write(result)