import math

COUNTRY_CONSTANT = "NGA"

def ascii_val(c): return ord(c)
def name_freq(s): return sum(ord(c) for c in s if c.isalpha()) % 100

def pythagoras_mod(a,b):
    if b==0: return 0,1,1
    sin = a % b
    cos = b % a if a!=0 else 1
    tan = a % cos if cos!=0 else 1
    return sin,cos,tan

def predict(query):
    words = query.split()
    remainder = 0
    print(f"Input: {query}")

    for i in range(0,len(words)-1,2):
        a = ascii_val(words[i][0])
        b = ascii_val(words[i+1][0])
        sin,cos,tan = pythagoras_mod(a,b)
        print(f"Chunk: {words[i]} {words[i+1]} -> sin={sin} cos={cos} tan={tan}")
        remainder = (remainder + sin + cos + tan) % 97

    begin, end = ascii_val(query[0]), ascii_val(query[-1])
    remainder = (remainder + begin % end) % 97

    pred = ""
    cycle = 0
    while remainder!=0 and cycle<10:
        div = (cycle+2)
        remainder = remainder % div
        pred += chr(97 + remainder % 26)
        cycle+=1

    print(f"Prediction: {pred}")
    return pred

predict("I going to the market")