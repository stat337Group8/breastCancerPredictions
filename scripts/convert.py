import pandas as pd
import argparse

parser = argparse.ArgumentParser(description="convert excel to csv")
parser.add_argument('-i', '--input', help='input excel file path', required=True)
parser.add_argument('-o', '--output', help='output csv file path', required=False, default='./output.csv')

def convert(inputFilePath, outputFilePath):
    
    df = pd.read_excel(inputFilePath)

    df.to_csv(outputFilePath, header=True, index=None)

if __name__ == '__main__':
    args = parser.parse_args()
    convert(args.input, args.output)
