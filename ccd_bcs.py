#!/usr/bin/env python3
"""
quasi_pair_bcs_ccsd.py

Derive CCSD (T1+T2) equations for the BCS pairing Hamiltonian
in the quasiparticle basis using ReducedBCSDrudge.
"""
from pyspark import SparkContext
import sympy as sp
from sympy import IndexedBase, simplify, symbols, conjugate, Rational,Symbol
from drudge.bcs import ReducedBCSDrudge
from drudge import FockDrudge,Stopwatch
import re

# ----------------------------------------------------------------------------
# 1) Start Spark & BCS quasiparticle drudge
# ----------------------------------------------------------------------------
ctx = SparkContext('local[*]', 'bcs_ccsd')
dr  = ReducedBCSDrudge(ctx)
dr.full_simplify = True

stopwatch = Stopwatch()
# ----------------------------------------------------------------------------
# 2) Define indices and amplitudes
# ----------------------------------------------------------------------------
p,q = symbols('p q')
u, v = IndexedBase('u'), IndexedBase('v')

# Quasiparticle operators from drudge
P, Pdag, N = dr.names.P, dr.names.Pdag, dr.names.N
P_i_dag = (u[p]*v[p]+u[p]**2*Pdag[p]
     - u[p]*v[p]*N[p]
     - v[p]**2*P[p])

P_j_dag = (
        u[q]*v[q]
      + u[q]**2 * Pdag[q]
      - u[q]*v[q] * N[q]
      - v[q]**2 * P[q]
    )
P_i = (
        conjugate(u[p])*conjugate(v[p])
      + conjugate(u[p])**2 * P[p]
      - conjugate(u[p])*conjugate(v[p]) * N[p]
      - conjugate(v[p])**2 * Pdag[p]
    )
P_j = ((u[q])*(v[q])
      + (u[q])**2 * P[q]
      - (u[q])*(v[q]) * N[q]
      - (v[q])**2 * Pdag[q])
#hc = P_j_dag * P_i
N_i = 2*(v[p])*v[p] +((u[p])*u[p] - (v[p])*v[p])*N[p] +2*u[p]*(v[p])*Pdag[p]\
            +2*(u[p])*v[p]*P[p]
N_j = 2*(v[q])*v[q] +((u[q])*u[q] - (v[q])*v[q])*N[q] +2*u[q]*(v[q])*Pdag[q]\
            +2*(u[q])*v[q]*P[q]
 
# ----------------------------------------------------------------------------
# 3) Build and normal-order the pairing Hamiltonian
# ----------------------------------------------------------------------------
eps, lam, G = IndexedBase(r'\epsilon'), Symbol(r'\lambda'), Symbol('G')
expr = (eps[p] - lam)*N_i - G * P_i_dag * P_j
Hbar = dr.einst(expr).normal_order().simplify()

# ----------------------------------------------------------------------------
# 4) Define CCSD cluster operators T1 and T2
#    T1 := t1[] P†_p
#    T2 := 1/2 t2[p,q] P†_p P†_q
# ----------------------------------------------------------------------------
t = IndexedBase('t')
cluster = dr.einst(
    #t[p] * Pdag[p] +
    Rational(1, 2) * t[p,q] *Pdag[p]*Pdag[q]) 

cluster = cluster.simplify()
cluster.cache()
'''dr.set_name(t1, t2)
T1 = dr.define_einst(t1[p], Pdag[p])
T2 = dr.define_einst(t2[p,q], Pdag[p] * Pdag[q]/ Rational(2))
T = T1 + T2'''

# ----------------------------------------------------------------------------
# 5) Similarity-transformed Hamiltonian: Hbar = e^{-T} H e^{T}
#    via nested commutators up to fourth order
# ----------------------------------------------------------------------------
for order in range(4):
    curr = (Hbar | cluster).simplify() * Rational(1, order + 1)
    stopwatch.tock('Commutator order {}'.format(order + 1), curr)
    Hbar += curr
    continue

'''c0 = Hbar
c1 = dr.simplify(c0 | T)
c2 = dr.simplify(c1 | T)
c3 = dr.simplify(c2 | T)
c4 = dr.simplify(c3 | T)
Hbar = dr.simplify(c0 + c1 + c2/2 + c3/6 + c4/24)'''

# ----------------------------------------------------------------------------
# 6) Derive CCSD equations using eval_vev:
#     a) Energy:   E = <0| Hbar |0>
#     b) Singles:  0 = <0| P_i Hbar |0> → s1_eqn
#     c) Doubles:  0 = <0| P_j P_i Hbar |0> → s2_eqn
# ----------------------------------------------------------------------------



E_eqn  = Hbar.eval_vev().simplify()
stopwatch.tock('Energy equation', E_eqn)

r,s = symbols('r s')
#s1_eqn = (P[p] * Hbar).eval_fermi_vev().simplify()
s2_eqn = (P[r] * P[s] * Hbar).eval_vev().simplify()  # do normal-ordering of PrPsH_bar # we need physical vacuum
print(E_eqn.latex())
print("")
print('The amplitude equation is below')
print(s2_eqn.latex())
# ----------------------------------------------------------------------------
# 7) Export CCSD equations to LaTeX
# ----------------------------------------------------------------------------
latex_E  = E_eqn.latex()
#latex_t1 = s1_eqn.latex()
latex_t2 = s2_eqn.latex()
latex_parts = []
latex_parts.append(r"\begin{equation}" + E_eqn.latex()  + r"\end{equation}")
#latex_parts.append(r"\paragraph{Singles (T1): }\begin{equation}"  + s1_eqn.latex() + r"\end{equation}")
latex_parts.append(r"\paragraph{Doubles (T2): }\begin{equation}"  + s2_eqn.latex() + r"\end{equation}")

latex_str = " "+" ".join(latex_parts)

# 8) Break into ~300-char lines at + or - boundaries
pieces = re.split(r'( \\+ | - )', latex_str)
lines, curr, max_len = [], "", 300
for piece in pieces:
    if curr and len(curr) + len(piece) > max_len:
        lines.append(curr)
        curr = piece.lstrip()
    else:
        curr += piece
if curr:
    lines.append(curr)

# 9) Write out LaTeX split environment
with open("ccd_bcs_eqns.tex", "w") as f:
    f.write(r"\documentclass{article}" + "\n")
    f.write(r"\usepackage{amsmath}" + "\n\n")
    f.write(r"\begin{document}" + "\n")
    f.write(r"\begin{equation}" + "\n")
    f.write(r"  \begin{split}" + "\n")
    for ln in lines:
        # each line ends with a literal " \\"
        f.write("    " + ln + r" \\" + "\n")
    f.write(r"  \end{split}" + "\n")
    f.write(r"\end{equation}" + "\n\n")
    f.write(r"\end{document}" + "\n")

print("Wrote normal‐ordered CCSD equations to ccsd_bcs_eqns.tex")
ctx.stop()