" Author: Johannes Wienke <languitar@semipol.de>
" Description: alex for text files

call ale#linter#Define('text', {
\   'name': 'alex',
\   'executable': 'alex',
\   'command': 'alex %t -t',
\   'output_stream': 'stderr',
\   'callback': 'ale#handlers#alex#Handle',
\})
