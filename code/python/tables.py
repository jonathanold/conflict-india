# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from tabula import read_pdf
df = read_pdf("p3.pdf", pages=(2,3), area=(100,0,100,0))
df

from tabula import convert_into
convert_into("p3.pdf", "output2.csv", output_format="csv", pages=2)
