"""Rebuild the embedded data in index.html from stitched agg.csv files.

Usage:  python3 build_results_page.py index.html out.html --study12 agg12.csv [--study3 agg3.csv]
Any study whose agg file is not given keeps its current embedded data.
"""
import argparse, json, re
import pandas as pd

METHOD = {'gold': 'gold', 'CC': 'CC', 'IPW-nm': 'MAR (IPW)',
          'MICE-std': 'MAR (MICE)', 'MICE-int': 'MAR (MICE)',
          'mia-tmle': 'MIA-T', 'mia-pkg-ice': 'MIA-I'}
ORDER = list(dict.fromkeys(METHOD.values()))

def rows(path):
    df = pd.read_csv(path)
    df = df[df.method.isin(METHOD)].copy()
    df['m'] = df.method.map(METHOD)
    df['mo'] = df.m.map(ORDER.index)
    df = df.sort_values(['dag_name', 'W_dim', 'N', 'mo'])
    out = []
    for _, r in df.iterrows():
        g = lambda c: None if pd.isna(r[c]) else round(float(r[c]), 2) + 0.0
        out.append({'dag': r.dag_name, 'N': int(r.N), 'wdim': int(r.W_dim), 'method': r.m,
                    'reps': int(r.reps),
                    'bB': g('BhatBias'), 'rB': g('BhatRMSE'), 'cB': g('BhatCover'), 'wB': g('BhatWidth'),
                    'bI': g('IntBias'),  'rI': g('IntRMSE'),  'cI': g('IntCover'),  'wI': g('IntWidth')})
    return out

def embed(html, name, data):
    body = "[\n" + ",\n".join("  " + json.dumps(d) for d in data) + "\n]"
    new, n = re.subn(rf"const {name} = \[.*?\];\n", lambda m: f"const {name} = {body};\n", html, flags=re.S)
    assert n == 1, name
    return new

ap = argparse.ArgumentParser()
ap.add_argument('src'); ap.add_argument('dst')
ap.add_argument('--study12'); ap.add_argument('--study3')
a = ap.parse_args()
html = open(a.src).read()
if a.study12: html = embed(html, 'DATA1', rows(a.study12))
if a.study3:  html = embed(html, 'DATA3', rows(a.study3))
open(a.dst, 'w').write(html)
