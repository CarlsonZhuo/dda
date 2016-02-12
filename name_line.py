'''
This simple python script name all the line in the cobol codes.
'''

def format_num(num):
    output_str = '00'
    assert(num)
    output_str = str(num) + output_str
    if num / 10 == 0:
        # num is at most 9
        output_str = '000' + output_str
    elif num / 100 == 0:
        # num is at most 99
        output_str = '00' + output_str
    elif num / 1000 == 0:
        # num is at most 999
        output_str = '0' + output_str
    elif num / 10000 == 0:
        # num is at most 9999
        output_str = '' + output_str
    assert(len(output_str) == 6)
    return output_str


cobol_code_file = 'dda1.cob'
current_line = 1
for line in open(cobol_code_file, 'rw'):
    line = format_num(current_line) + line[6:]
    current_line += 1
    print line,


