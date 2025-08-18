#!/usr/bin/env python3
"""
quasi_pair_bcs_ccsd.py

Derive CCSD (T1+T2) equations for the BCS pairing Hamiltonian
in the quasiparticle basis using ReducedBCSDrudge. This I am calculating for 3-body Hamiltonian, got by SFS transformation.
"""

import os,sys
import re as regex


os.environ["PYSPARK_DRIVER_PYTHON"] = sys.executable
os.environ["PYSPARK_PYTHON"]        = sys.executable

from pyspark import SparkContext

from sympy import *

from drudge import *
print("It works!")


# ----------------------------------------------------------------------------
# 1) Start Spark & BCS quasiparticle drudge
# ----------------------------------------------------------------------------
ctx = SparkContext('local[*]', 'bcs_normal')
dr  = ReducedBCSDrudge(ctx)
dr.full_simplify = True
print("It works!")
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
P_j = (conjugate(u[q])*conjugate(v[q])
      + conjugate(u[q])**2 * P[q]
      - conjugate(u[q])*conjugate(v[q]) * N[q]
      - conjugate(v[q])**2 * Pdag[q])
#hc = P_j_dag * P_i
N_i = 2*(v[p])*v[p] +((u[p])*u[p] - (v[p])*v[p])*N[p] +2*u[p]*(v[p])*Pdag[p]\
            +2*(u[p])*v[p]*P[p]
N_j = 2*(v[q])*v[q] +((u[q])*u[q] - (v[q])*v[q])*N[q] +2*u[q]*(v[q])*Pdag[q]\
            +2*(u[q])*v[q]*P[q]

N_ip1 = 2*conjugate(v[p+1])*v[p+1] +(conjugate(u[p+1])*u[p+1] - conjugate(v[p+1])*v[p+1])*N[p+1] +2*u[p+1]*conjugate(v[p+1])*Pdag[p+1]\
            +2*conjugate(u[p+1])*v[p+1]*P[p+1]
N_im1 = 2*conjugate(v[p-1])*v[p-1] +(conjugate(u[p-1])*u[p-1] - conjugate(v[p-1])*v[p-1])*N[p-1] +2*u[p-1]*conjugate(v[p-1])*Pdag[p-1]\
            +2*conjugate(u[p-1])*v[p-1]*P[p-1]
#===================================================================================================================s
# ----------------------------------------------------------------------------
# 3) Build and normal-order the pairing Hamiltonian
# ----------------------------------------------------------------------------
eps, lam, G = IndexedBase(r'\epsilon'), Symbol(r'\lambda'), Symbol(r'\Delta')
expr = Rational(1,4)*(P_i_dag + P_i) - Rational(1,2)*(N_im1 - 1)*(P_i_dag + P_i)*(N_ip1 - 1) + Rational(1,4)*G*(N_im1 - 1)*(N_ip1 - 1)
Hbar =  dr.einst(expr).normal_order().simplify().merge()
print(Hbar.latex())


# Build top-level pieces split only on +/– that are NOT inside braces/parens/brackets

# --- helpers ---------------------------------------------------------------

def split_top_level(s: str):
    """Split at top-level + / - (depth 0), keeping the operator with the next term."""
    terms = []
    brace = paren = brack = 0
    curr = []
    i, n = 0, len(s)

    while i < n:
        # treat \left( / \right) as single tokens so their '(' and ')' don't confuse depth
        if s.startswith(r'\left(', i):
            paren += 1
            curr.append(r'\left(')
            i += len(r'\left(')
            continue
        if s.startswith(r'\right)', i):
            paren -= 1 if paren > 0 else 0
            curr.append(r'\right)')
            i += len(r'\right)')
            continue

        ch = s[i]
        if ch == '{':   brace += 1
        elif ch == '}': brace -= 1 if brace > 0 else 0
        elif ch == '(': paren += 1
        elif ch == ')': paren -= 1 if paren > 0 else 0
        elif ch == '[': brack += 1
        elif ch == ']': brack -= 1 if brack > 0 else 0

        if brace == paren == brack == 0 and ch in '+-':
            if curr:
                terms.append(''.join(curr).strip())
                curr = []
            # keep the operator (with spaces) attached to next term
            curr.append(f' {ch} ')
            i += 1
            continue

        curr.append(ch)
        i += 1

    if curr:
        terms.append(''.join(curr).strip())
    return [t for t in terms if t]


def split_inside_parens(term: str):
    """
    If term starts with (...) or \left( ... \right), break the INSIDE at top-level +/-
    and rewrap as \left(\begin{aligned}[t] ... \end{aligned}\right).
    """
    # preserve leading spaces (for nice indentation when writing)
    lead_len = len(term) - len(term.lstrip())
    lead, s = term[:lead_len], term[lead_len:]

    if not (s.startswith(r'\left(') or s.startswith('(')):
        return term  # nothing to do

    # find the matching closing delimiter
    j = len(r'\left(') if s.startswith(r'\left(') else 1
    depthL = 1 if s.startswith(r'\left(') else 0  # \left ... \right nesting
    depthP = 1 if s.startswith('(') else 0        # bare ( ... ) nesting
    start_inside = j
    end_after = None

    while j < len(s):
        if s.startswith(r'\left(', j):
            depthL += 1
            j += len(r'\left(')
            continue
        if s.startswith(r'\right)', j):
            if depthL > 0:
                depthL -= 1
                j += len(r'\right)')
                if depthL == 0 and depthP == 0:
                    end_after = j  # index just after the closing token
                    break
                continue
        ch = s[j]
        if ch == '(':
            depthP += 1
            j += 1
            continue
        if ch == ')':
            if depthP > 0:
                depthP -= 1
                j += 1
                if depthL == 0 and depthP == 0:
                    end_after = j
                    break
                continue
        j += 1

    if end_after is None:
        # unmatched: leave unchanged
        return term

    # compute the inside slice (exclude the closing token)
    if s[end_after - len(r'\right)'):end_after] == r'\right)':
        inside_end = end_after - len(r'\right)')
    else:
        inside_end = end_after - 1

    inside = s[start_inside:inside_end]
    rest   = s[end_after:]

    inner_terms = split_top_level(inside)
    if len(inner_terms) <= 1:
        return term  # nothing to wrap

    # Always use stretchable delimiters for multiline content
    inner = []
    inner.append(r'\left(')
    inner.append(r'\begin{aligned}[t]')
    for k, it in enumerate(inner_terms):
        inner.append('  & ' + it + (r' \\' if k < len(inner_terms) - 1 else ''))
    inner.append(r'\end{aligned}')
    inner.append(r'\right)')

    return lead + '\n'.join(inner) + rest


# --- main ------------------------------------------------------------------

raw = Hbar.latex().strip()

outer_terms = split_top_level(raw)
wrapped_terms = [split_inside_parens(t) for t in outer_terms]

out = "bcs_Normal-ordered_3_body.tex"
with open(out, "w") as f:
    f.write(r"\documentclass{article}" + "\n")
    f.write(r"\usepackage[a4paper,margin=1.8cm]{geometry}" + "\n")
    f.write(r"\usepackage{amsmath,amssymb,mathtools}" + "\n")
    f.write(r"\allowdisplaybreaks[3]" + "\n")
    f.write(r"\setlength{\jot}{3pt}" + "\n")
    f.write(r"\begin{document}" + "\n\n")
    f.write(r"\begin{equation}" + "\n")
    f.write(r"\begin{aligned}" + "\n")
    for k, term in enumerate(wrapped_terms):
        f.write("  & " + term + (r" \\" if k < len(wrapped_terms) - 1 else "") + "\n")
    f.write(r"\end{aligned}" + "\n")
    f.write(r"\end{equation}" + "\n\n")
    f.write(r"\end{document}" + "\n")

print(f"Wrote normal-ordered LaTeX to {out}")




ctx.stop()
#====================================================================================================================
# 4) Define CCSD cluster operators T1 and T2
#    T1 := t1[a,i] P†_a P_i
#    T2 := 1/2 t2[a,b,i,j] P†_a P†_b P_j P_i

r"""t = IndexedBase('t')

cluster = dr.einst(
    t[p] * Pdag[p] +
    Rational(1, 2) * t[p,q] *Pdag[p]*Pdag[q]) 

 
dr.set_symm(t, Perm([1, 0]), valence=2)   # Enforce t[p,q] = t[q,p]
print("Perm works!")

#====================================================================================================================
# 5) Similarity-transformed Hamiltonian: Hbar = e^{-T} H e^{T}
#    via nested commutators up to fourth order


curr = Hbar
for order in range(4):
    curr = (curr | cluster).simplify() * Rational(1, order + 1)
    #stopwatch.tock('Commutator order {}'.format(order + 1), curr)
    Hbar += curr

    continue

 
# ----------------------------------------------------------------------------
# 6) Derive CCSD equations using eval_vev:
#     a) Energy:   E = <0| Hbar |0>
#     b) Singles:  0 = <0| P_i Hbar |0> → s1_eqn
#     c) Doubles:  0 = <0| P_j P_i Hbar |0> → s2_eqn
# ----------------------------------------------------------------------------



E_eqn  = Hbar.normal_order().eval_vev().simplify()
s1_eqn = (P[p] * Hbar).normal_order().eval_vev().merge().simplify()
s2_eqn = (P[p] * P[q] * Hbar).normal_order().eval_vev().merge().simplify()
 

s1_eqn = s1_eqn.subst(t[q,q],0).simplify().cache()
s1_eqn = s1_eqn.subst(t[p,p],0).simplify().cache()
s1_eqn = s1_eqn.subst(H40[p,p],0).simplify().cache()
s1_eqn = s1_eqn.subst(H04[p,p],0).simplify().cache()
s1_eqn = s1_eqn.subst(H40[q,q],0).simplify().cache()
s1_eqn = s1_eqn.subst(H04[q,q],0).simplify().cache()
s2_eqn = s2_eqn.subst(t[p,p],0).simplify.cache()
s2_eqn = s2_eqn.subst(t[q,q],0).simplify.cache()
s2_eqn =  s2_eqn.subst(H40[p,p],0).simplify().cache()
s2_eqn =  s2_eqn.subst(H40[q,q],0).simplify().cache()
s2_eqn =  s2_eqn.subst(H04[q,q],0).simplify().cache()
s2_eqn =  s2_eqn.subst(H04[p,p],0).simplify().cache()






 
print(E_eqn.latex())
print('-------------------------------')
# ----------------------------------------------------------------------------
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
with open("CCSD.tex", "w") as f:
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

with dr.report('rCCSD.html', 'BCS CCSD Theory') as rep:
    rep.add('Pairing Hamiltonian in quasiparticle basis',            expr)
    rep.add(r"Similarity‐transformed $\\bar{H}$",     Hbar)
    rep.add(r"The cluster operator $\T$", cluster)
    rep.add(r"Energy equation $E = \\langle0|\\bar H|0\\rangle$", E_eqn)
    rep.add(r"Singles (T\\textsubscript{1}) residual",      s1_eqn)
    rep.add('Doubles (T\\textsubscript{2}) residual',      s2_eqn)
    """

 