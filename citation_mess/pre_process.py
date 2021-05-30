#!/usr/local/bin/python

# Pre-Processing Python Script

# Modules
from sys import argv
import re

# Inputs
filename = raw_input("Original bib file: ")
new_file_name = raw_input("New bib file: ")

#filename = '/Users/baobaozhang/Dropbox/jpr-bst-file/example_latex/bad_bib.bib'
#new_file_name = '/Users/baobaozhang/Dropbox/jpr-bst-file/example_latex/good_bib.bib'
# States Dictionary
states = {
        'AK': 'Alaska',
        'AL': 'Alabama',
        'AR': 'Arkansas',
        'AS': 'American Samoa',
        'AZ': 'Arizona',
        'CA': 'California',
        'CO': 'Colorado',
        'CT': 'Connecticut',
        'DC': 'District of Columbia',
        'DE': 'Delaware',
        'FL': 'Florida',
        'GA': 'Georgia',
        'GU': 'Guam',
        'HI': 'Hawaii',
        'IA': 'Iowa',
        'ID': 'Idaho',
        'IL': 'Illinois',
        'IN': 'Indiana',
        'KS': 'Kansas',
        'KY': 'Kentucky',
        'LA': 'Louisiana',
        'MA': 'Massachusetts',
        'MD': 'Maryland',
        'ME': 'Maine',
        'MI': 'Michigan',
        'MN': 'Minnesota',
        'MO': 'Missouri',
        'MP': 'Northern Mariana Islands',
        'MS': 'Mississippi',
        'MT': 'Montana',
        'NA': 'National',
        'NC': 'North Carolina',
        'ND': 'North Dakota',
        'NE': 'Nebraska',
        'NH': 'New Hampshire',
        'NJ': 'New Jersey',
        'NM': 'New Mexico',
        'NV': 'Nevada',
        'NY': 'New York',
        'OH': 'Ohio',
        'OK': 'Oklahoma',
        'OR': 'Oregon',
        'PA': 'Pennsylvania',
        'PR': 'Puerto Rico',
        'RI': 'Rhode Island',
        'SC': 'South Carolina',
        'SD': 'South Dakota',
        'TN': 'Tennessee',
        'TX': 'Texas',
        'UT': 'Utah',
        'VA': 'Virginia',
        'VI': 'Virgin Islands',
        'VT': 'Vermont',
        'WA': 'Washington',
        'WI': 'Wisconsin',
        'WV': 'West Virginia',
        'WY': 'Wyoming'
}

# New Dictionary 
states_dic = dict(zip(states.values(), states.keys()))

# Find and Replace State Names  
def multipleReplace(text, wordDict):
    """
    take a text and replace words that match the key in a dictionary
    with the associated value, return the changed text
    """
    for key in wordDict:
        text = text.replace(".","")
        text = text.replace(key, wordDict[key])
    return text

# Capitalize Function
def uppercase(matchobj):
    temp = matchobj.group(0).upper()
    return '{'+temp+'}'

def capitalize(s):
    return re.sub('^((?i)[a-z])|[\.|\?|\!|\:]\s*((?i)[a-z])|\s+((?i)[a-z])(?=\.)', uppercase, s)


# Go Through Each Line and Fine and Replace Problems
with open(new_file_name, 'w') as n_file:
        with open(filename) as f:
            for line in f:
                if 'author =' in str.lower(line) or 'editor =' in str.lower(line) or 'author=' in str.lower(line) or 'editor=' in str.lower(line):
                    temp = line.replace(".", "");
                    m = re.search("[A-Z]\s[A-Z]\s", temp)
                    if m:
                        found = m.group(0);
                        n_file.write(re.sub(found, "".join(found.split())+' ', temp))
                    else:
                        n_file.write(temp);
                elif 'title =' in str.lower(line) or 'title=' in str.lower(line):
                    b_index = line.find('{');
                    n_file.write(line[0:b_index+1]+capitalize(line[(b_index+1):len(line)]));
                elif 'address =' in str.lower(line) or 'address=' in str.lower(line):
                        if 'New York, ' in line or '{New York' in line:
                            n_file.write('address = {New York},\n');
                        else:
                            if "," in line:
                                line_part = line.partition(",");
                                n_file.write("".join(line_part[0:2])+multipleReplace(line_part[2], states_dic));
                            else:
                                n_file.write(line);
                else:
                    n_file.write(line);




