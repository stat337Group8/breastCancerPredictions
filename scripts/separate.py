import pandas as pd
import argparse
import json

parser = argparse.ArgumentParser(description='separate big csv into smaller ones with targeted columns')
parser.add_argument('-i', '--input', help='input csv file to extract from', required=True)
parser.add_argument('-d', '--data', help='json file with headers in one list and new labels in another; must use headers and labels as key', required=True)
parser.add_argument('-n', '--number', help='header number', required=False, default=0)
parser.add_argument('-o', '--output', help='output file', required=True)
parser.add_argument('-l', '--labelCol', help='column name to make index; must match the new label title', required=False, default=None)
parser.add_argument('-r', '--remove', help='row numbers to remove', nargs='*', type=int, required=False, default=None)

if __name__ == '__main__':
    args = parser.parse_args()
    df = pd.read_csv(args.input, header=int(args.number))
   
    if args.remove != None:
        df = df.drop(list(args.remove))

    with open(args.data, 'r') as dataFile:
        dictionary = json.load(dataFile)
   
    headers = list(dictionary['headers'])
    headers = [header.replace('\n', '') for header in headers]
    labels = list(dictionary['labels'])
    labels = [label.replace('\n', '') for label in labels]
    
    x = df[headers]
    x = x.rename(columns={head: label for head, label in zip(headers, labels)})
    if args.labelCol != None:
        x = x.set_index(args.labelCol)

    with open(args.output, 'w') as file:
        x.to_csv(file)
        print(f'File saved at {args.output}')
