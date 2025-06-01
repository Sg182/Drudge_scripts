#!/usr/bin/env python
"""
quasi_pair_bcs.py

Normal‐ordering Pᵢ† Pⱼ with respect to the BCS (seniority-zero) state,
using ReducedBCSDrudge—plus a patch to avoid the SymPy EnumSymbs pickle bug.
"""

# ─────────────────────────────────────────────────────────────────────────────
# A) Patch SymPy’s EnumSymbs before importing drudge
# ─────────────────────────────────────────────────────────────────────────────

"""
quasi_pair_bcs.py
"""

import os, sys
import re

# ────────────────────────────────────────────────────────────────
# 0) Pin PySpark to **this** exact Python for driver & workers
# ────────────────────────────────────────────────────────────────
os.environ["PYSPARK_DRIVER_PYTHON"] = sys.executable
os.environ["PYSPARK_PYTHON"]        = sys.executable


#!/usr/bin/env python
# normal_pairing.py

from pyspark import SparkContext
from sympy import IndexedBase, symbols, conjugate,Rational
from drudge.bcs import ReducedBCSDrudge


# ── 1) Spark + your BCS‐drudge ─────────────────────────────────────────────
ctx = SparkContext('local[*]', 'bcs_normal')
dr  = ReducedBCSDrudge(ctx)
dr.full_simplify = True

# ── 2) Bogoliubov amplitudes & indices ────────────────────────────────────
u, v = IndexedBase('u'), IndexedBase('v')
i, j = symbols('i j')
H000 = IndexedBase(r'H^{000}')
h = IndexedBase('h')
V = IndexedBase('V')
W = IndexedBase('W')

P    = dr.names.P      # pair‐annihilation Vec
Pdag = dr.names.Pdag   # pair‐creation     Vec
N    = dr.names.N      # number           Vec



P_i_dag = (u[i]*v[i]+u[i]**2*Pdag[i]
     - u[i]*v[i]*N[i]
     - v[i]**2*P[i])

P_j_dag = (
        u[j]*v[j]
      + u[j]**2 * Pdag[j]
      - u[j]*v[j] * N[j]
      - v[j]**2 * P[j]
    )
P_i = (
        conjugate(u[i])*conjugate(v[i])
      + conjugate(u[i])**2 * P[i]
      - conjugate(u[i])*conjugate(v[i]) * N[i]
      - conjugate(v[i])**2 * Pdag[i]
    )
P_j = (conjugate(u[j])*conjugate(v[j])
      + conjugate(u[j])**2 * P[j]
      - conjugate(u[j])*conjugate(v[j]) * N[j]
      - conjugate(v[j])**2 * Pdag[j])
#hc = P_j_dag * P_i
N_i = 2*conjugate(v[i])*v[i] +(conjugate(u[i])*u[i] - conjugate(v[i])*v[i])*N[i] +2*u[i]*conjugate(v[i])*Pdag[i]\
            +2*conjugate(u[i])*v[i]*P[i]
N_j = 2*conjugate(v[j])*v[j] +(conjugate(u[j])*u[j] - conjugate(v[j])*v[j])*N[j] +2*u[j]*conjugate(v[j])*Pdag[j]\
            +2*conjugate(u[j])*v[j]*P[j]
# ── 3) Build the quasi‐spin operator Pᵢ† Pⱼ ──────────────────────────────────
expr = h[i]*N_i + V[i,j]*P_i_dag*P_j+ Rational(1,4)*W[i,j]*N_i*N_j

print("")

# ── 4) Einstein‐sum, normal‐order, simplify ────────────────────────────────
tensor = H000+dr.einst(expr)


normal = tensor.normal_order().simplify().merge()
latex_str = normal.latex()
print(normal.latex())
# ── 5) Emit HTML report ───────────────────────────────────────────────────
pieces, lines, curr = re.split(r'( \+ | - )', latex_str), [], ""
max_len = 300
for piece in pieces:
    if len(curr) + len(piece) > max_len and curr:
            lines.append(curr)
            curr = piece.lstrip()
    else:
            curr += piece
if curr:
        lines.append(curr)

    # 13) Write out bcs_pairing_norm_2.tex
with open("bcs_pairing_generic_2.tex", "w") as f:
    f.write(r"\documentclass{article}" + "\n")
    f.write(r"\usepackage{amsmath}" + "\n")
    f.write(r"\begin{document}" + "\n\n")
    f.write(r"\begin{equation}" + "\n")
    f.write(r"  \begin{split}" + "\n")
    for ln in lines:
        f.write("    " + ln + r" \\" + "\n")
    f.write(r"  \end{split}" + "\n")
    f.write(r"\end{equation}" + "\n\n")
    f.write(r"\end{document}" + "\n")

    print("Wrote normal‐ordered LaTeX to bcs_pairing_norm_2.tex")


ctx.stop()


