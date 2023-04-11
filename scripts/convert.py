import pandas as pd
import argparse

parser = argparse.ArgumentParser(description="convert excel to csv")
parser.add_argument('-i', '--input', help='input excel file path', required=True)
parser.add_argument('-o', '--output', help='output csv file path', required=False, default='./output.csv')
parser.add_argument('-c', '--columnHeader', help='input the start of the actual data without header', required=False, default=0)

def convert(inputFilePath, outputFilePath, header):
    
    if header == 0:
        df = pd.read_excel(inputFilePath)
    else:
        df = pd.read_excel(inputFilePath, header=[i for i in range(header)])

    df.to_csv(outputFilePath, header=True, index=None)

if __name__ == '__main__':
    args = parser.parse_args()
    convert(args.input, args.output, int(args.columnHeader))
