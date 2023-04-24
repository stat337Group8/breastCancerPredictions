import pandas as pd
import argparse

parser = argparse.ArgumentParser(description='remove data that is missing')
parser.add_argument('-i', '--input', help='input file path', required=True)
parser.add_argument('-o', '--output', help='output file path', required=False, default='./image.csv')

if __name__ == '__main__':
    args = parser.parse_args()
    
