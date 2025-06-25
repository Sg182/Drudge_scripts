#!/usr/bin/env python3
"""
quasi_pair_bcs_ccsd.py

Derive CCSDT (T1+T2+T3) equations for the BCS pairing Hamiltonian
in the quasiparticle basis using ReducedBCSDrudge.
"""


from pyspark import SparkContext
 

from sympy import *

from drudge import *


import re

# ----------------------------------------------------------------------------
# 1) Start Spark & BCS quasiparticle drudge
# ----------------------------------------------------------------------------
ctx = SparkContext('local[*]', 'bcs_ccsd')
dr  = ReducedBCSDrudge(ctx)
dr.full_simplify = True
 
#===================================================================================================================
# 2) Define indices and amplitudes
#===================================================================================================================
p,q,r = symbols('p q r')
u, v = IndexedBase('u'), IndexedBase('v')
E0 = Symbol(r'E_0')
H11, H20 , H02 = IndexedBase(r'H^{11}'), IndexedBase(r'H^{20}'), IndexedBase(r'H^{02}')
H04, H40 = IndexedBase(r'H^{04}'), IndexedBase(r'H^{40}')
H22, Hb22 = IndexedBase(r'H^{22}'), IndexedBase(r'\tilde{H}^{22}')
H31, H13 = IndexedBase(r'H^{31}'), IndexedBase(r'H^{13}')
#===================================================================================================================
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
#===================================================================================================================s
# ----------------------------------------------------------------------------
# 3) Build and normal-order the pairing Hamiltonian
# ----------------------------------------------------------------------------
eps, lam, G = IndexedBase(r'\epsilon'), Symbol(r'\lambda'), Symbol('G')
expr = E0+H11[p]*N[p] + H02[p]*Pdag[p] +H20[p]*P[p]+H22[p,q]*N[p]*N[q]+Hb22[p,q]*Pdag[p]*P[q]\
    +H40[p,q]*P[p]*P[q]+H04[p,q]*Pdag[p]*Pdag[q]+H13[p,q]*Pdag[p]*N[q]+H31[p,q]*N[q]*P[p]
Hbar =  dr.einst(expr).normal_order().simplify().merge()
print("The Hbar is the following:")
print(Hbar.latex())
#====================================================================================================================
# 4) Define CCSD cluster operators T1 and T2
#    T1 := t1[a,i] P†_a P_i
#    T2 := 1/2 t2[a,b,i,j] P†_a P†_b P_j P_i

t = IndexedBase('t')

cluster = dr.einst(
    t[p] * Pdag[p] +
    Rational(1, 2) * t[p,q] *Pdag[p]*Pdag[q] + Rational(1,8)*t[p,q,r]*Pdag[p]*Pdag[q]*Pdag[r]) 

 
dr.set_symm(t, Perm([1, 0]), valence=2)   # Enforce t[p,q] = t[q,p]
print("Perm works!")

#====================================================================================================================
# 5) Similarity-transformed Hamiltonian: Hbar = e^{-T} H e^{T}
#    via nested commutators up to fourth order


curr = Hbar
for order in range(4):
    curr = (curr | cluster).simplify() * Rational(1, order + 1)
    Hbar += curr

    continue

#====================================================================================================================
# 6) Derive CCSDT equations using eval_vev:
#     a) Energy:   E = <0| Hbar |0>
#     b) Singles:  0 = <0| P_i Hbar |0> → s1_eqn
#     c) Doubles:  0 = <0| P_j P_i Hbar |0> → s2_eqn
#.    d) Triples:  0 = <0| P_iPjP_r Hbar|0> -> s3_eqn

E_eqn  = Hbar.normal_order().eval_vev().simplify()
s1_eqn = (P[p] * Hbar).normal_order().eval_vev().merge().simplify()
s2_eqn = (P[p] * P[q] * Hbar).normal_order().eval_vev().merge().simplify()
s3_eqn = (P[p] * P[q] *P[r]* Hbar).normal_order().eval_vev().merge().simplify()

s1_eqn = s1_eqn.subst(t[q,q],0).simplify().cache()
s1_eqn = s1_eqn.subst(t[p,p],0).simplify().cache()
s1_eqn = s1_eqn.subst(H40[p,p],0).simplify().cache()
s1_eqn = s1_eqn.subst(H04[p,p],0).simplify().cache()
s1_eqn = s1_eqn.subst(H40[q,q],0).simplify().cache()
s1_eqn = s1_eqn.subst(H04[q,q],0).simplify().cache()
s2_eqn =  s2_eqn.subst(H40[p,p],0).simplify().cache()
s2_eqn =  s2_eqn.subst(H40[q,q],0).simplify().cache()
s2_eqn =  s2_eqn.subst(H04[q,q],0).simplify().cache()
s2_eqn =  s2_eqn.subst(H04[p,p],0).simplify().cache()
s3_eqn =  s3_eqn.subst(H40[p,p],0).simplify().cache()
s3_eqn =  s3_eqn.subst(H04[q,q],0).simplify().cache()
s3_eqn =  s3_eqn.subst(H04[p,p],0).simplify().cache()
s3_eqn =  s3_eqn.subst(H40[q,q],0).simplify().cache()


print(E_eqn.latex())
print('=========================================================================================')
#===================================================================================================================
# 7) Export CCSD equations to LaTeX
# ----------------------------------------------------------------------------
latex_E  = E_eqn.latex()
#latex_t1 = s1_eqn.latex()
latex_t2 = s2_eqn.latex()
print(latex_t2)
latex_parts = []
latex_parts.append(r"\paragraph{Energy: }\begin{equation}" + E_eqn.latex()  + r"\end{equation}")
latex_parts.append(r"\paragraph{Singles (T1): }\begin{equation}"  + s1_eqn.latex() + r"\end{equation}")
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
with open("CCSDT.tex", "w") as f:
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

with dr.report('rCCSDT.html', 'BCS CCSD Theory') as rep:
    rep.add('Pairing Hamiltonian in quasiparticle basis',            expr)
    rep.add(r"Similarity‐transformed $\\bar{H}$",     Hbar)
    rep.add(r"The cluster operator $\T$", cluster)
    rep.add(r"Energy equation $E = \\langle0|\\bar H|0\\rangle$", E_eqn)
    rep.add(r"Singles (T\\textsubscript{1}) residual",      s1_eqn)
    rep.add('Doubles (T\\textsubscript{2}) residual',      s2_eqn)
    rep.add('Triples (T\\textsubscript{3}) residual',      s3_eqn)
ctx.stop()
