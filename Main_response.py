import numpy as np
import os
import sys
import pickle
import pandas as pd
from parameter import *
from pbcs import Pairing, XXZSquare, XXZ, J1J2Square, J1J2XXZ, PBCS, PairingChannel, XXZtriangular, XXZhoneycomb, XXZPerturbed
# from Hub2Pair_complex import hub2pair_complex
from Hub2Pair import hub2pair
from SpSq_integral import spsq_integral
from NBCSCC_X3 import nbcscc
from copy import deepcopy
import SpSq_tools

ccsd_spsq = SpSq_tools.ccsdspsq.ccsd_spsq
ccsd_spsq_grad = SpSq_tools.ccsdspsqgrad.ccsd_spsq_grad

#==================Functions for Correlation function===================================
def calc_corr_1d_from_spsq(SpSq, is_pbc=True, tol=1.0e-10):
    """
    Python version of CalcCorr1D_FromSpSq from the old Fortran.
    Returns Corr with shape (NAO,)
    """
    nao = SpSq.shape[0]
    Corr = np.zeros(nao, dtype=float)

    for r in range(nao):
        sumv = 0.0 + 0.0j
        count = 0

        if is_pbc:
            for p in range(nao):
                q = (p + r) % nao
                sumv += SpSq[p, q]
                count += 1
        else:
            for p in range(nao - r):
                q = p + r
                sumv += SpSq[p, q]
                count += 1

        if count > 0:
            avg = sumv / count
            if abs(avg.imag) > tol:
                print(f"Warning: Im(Corr) at r = {r}, value = {avg.imag}")
            Corr[r] = avg.real

    return Corr


def calc_corr_2d_from_spsq(SpSq, nx, ny, is_pbc=True, tol=1.0e-10):
    """
    Python version of CalcCorr2D_FromSpSq from the old Fortran.
    Returns Corr with shape (nx, ny)
    """
    Corr = np.zeros((nx, ny), dtype=float)

    for dx in range(nx):
        for dy in range(ny):
            sumv = 0.0 + 0.0j
            count = 0

            for x1 in range(nx):
                for y1 in range(ny):
                    x2 = x1 + dx
                    y2 = y1 + dy

                    if is_pbc:
                        x2 %= nx
                        y2 %= ny
                    else:
                        if x2 >= nx or y2 >= ny:
                            continue

                    p = x1 * ny + y1
                    q = x2 * ny + y2
                    termv = SpSq[p,q]
                    sumv += termv
                    count += 1

            if count > 0:
                avg = sumv / count
                if abs(avg.imag) > tol:
                    print(f"Warning: Im(Corr) at dx,dy=({dx},{dy}), value = {avg.imag}")
                Corr[dx, dy] = avg.real

    return Corr

#=====================================================================================================================
Lambda = 0.0

H1 = np.arange(NAO) + 1
is_pbc = True
# ------------------------------------------------------------------
# x = 0 run: needed for analytic response <Sp.Sq> using T,Z at x0
x0 = 0.0
# -----------------------------------------------------------------
p = 1
q = 2

# ham = Pairing(NAO, nelec, H1, J, Lambda)
#ham = XXZ(NAO, J, periodic=is_pbc)
ham = XXZPerturbed(NAO, J,x0,p,q, periodic=is_pbc)
#ham = XXZSquare(nx, ny, J, periodic=is_pbc)
# ham = J1J2Square(nx, ny, J, periodic=True)
# ham = XXZtriangular(J, nx, ny, periodic=True)
# ham = J1J2XXZ(NAO, 1.0, J, periodic=False)

pairing = PairingChannel(ham)
pairing.ngrid = 32

if os.path.exists("UV.p"):
    u, v = pickle.load(open("UV.p", "rb"))
    U, V, info = pairing.run(x0=u, y0=v, init_guess='perturbed', is_from_complex=False)
else:
    U, V, info = pairing.run(init_guess='perturbed', is_from_complex=False)

pickle.dump([U, V], open("UV.p", "wb"))

if pairing.ngrid == 1:
    print("Enuc =", ham.Enuc)
    particle = np.dot(V, V) * 2
    print(f"particle number after opt is {particle}")
    print("E(BCS) =", info['obj_val'] + ham.Enuc)
    print("Eta = ", V / U)
else:
    print("Enuc =", ham.Enuc)
    print("E(AGP) =", info['obj_val'] + ham.Enuc)
    print("Eta = ", V / U)

# sanity
assert np.allclose(U * U + V * V, 1.0, atol=1e-14)

print("normalization",U**2 + V**2)

S00, S20p, S20q, S11p, S11q, S02p, S02q, \
S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04 = spsq_integral(U, V, NAO) #Integrals for SpSq

S40_use = S40.copy()
S04_use = S04.copy()
for k in range(NAO):
    S40_use[k, k] = 0
    S04_use[k, k] = 0



H00, H20, H11, H02, H40, H31, H22, Hb22, H13, H04 = hub2pair( U, V,ham.h_diag, ham.v, ham.w,   #GETS YOU THE INTEGRALS FOR H = H0 + xH1_pq
    p, q, x0,
    S00, S20p, S20q, S11p, S11q, S02p, S02q,
    S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04,NAO
)
'''
h_test = 1.0e-3

Hp = hub2pair(
    U, V, ham.h_diag, ham.v, ham.w,
    p, q, +h_test,
    S00, S20p, S20q, S11p, S11q, S02p, S02q,
    S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04,
    NAO
)

Hm = hub2pair(
    U, V, ham.h_diag, ham.v, ham.w,
    p, q, -h_test,
    S00, S20p, S20q, S11p, S11q, S02p, S02q,
    S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04,
    NAO
)

# unpack
H00p, H20p, H11p, H02p, H40p, H31p, H22p, Hb22p, H13p, H04p = Hp
H00m, H20m, H11m, H02m, H40m, H31m, H22m, Hb22m, H13m, H04m = Hm

# if your convention enforces zero diagonal in H40/H04, do it before differencing
for i in range(NAO):
    H40p[i, i] = 0
    H04p[i, i] = 0
    H40m[i, i] = 0
    H04m[i, i] = 0

dH00  = (H00p  - H00m ) / (2*h_test)
dH20  = (H20p  - H20m ) / (2*h_test)
dH11  = (H11p  - H11m ) / (2*h_test)
dH02  = (H02p  - H02m ) / (2*h_test)
dH40  = (H40p  - H40m ) / (2*h_test)
dH31  = (H31p  - H31m ) / (2*h_test)
dH22  = (H22p  - H22m ) / (2*h_test)
dHb22 = (Hb22p - Hb22m) / (2*h_test)
dH13  = (H13p  - H13m ) / (2*h_test)
dH04  = (H04p  - H04m ) / (2*h_test)

print("================ hub2pair vs S-tensor check ================")
print("pair (p,q) =", p, q)
print("dH00               =", dH00)
print("S00[p-1,q-1]       =", S00[p-1, q-1])
print("diff H00           =", dH00 - S00[p-1, q-1])

print("dH20[p-1], S20p    =", dH20[p-1], S20p[p-1, q-1])
print("dH20[q-1], S20q    =", dH20[q-1], S20q[p-1, q-1])

print("dH02[p-1], S02p    =", dH02[p-1], S02p[p-1, q-1])
print("dH02[q-1], S02q    =", dH02[q-1], S02q[p-1, q-1])

print("dH11[p-1], S11p    =", dH11[p-1], S11p[p-1, q-1])
print("dH11[q-1], S11q    =", dH11[q-1], S11q[p-1, q-1])

print("dH40[p-1,q-1], S40 =", dH40[p-1, q-1], S40[p-1, q-1])
print("dH04[p-1,q-1], S04 =", dH04[p-1, q-1], S04[p-1, q-1])

print("dH31[p-1,q-1], S31pq =", dH31[p-1, q-1], S31pq[p-1, q-1])
print("dH31[q-1,p-1], S31qp =", dH31[q-1, p-1], S31qp[p-1, q-1])

print("dH13[p-1,q-1], S13pq =", dH13[p-1, q-1], S13pq[p-1, q-1])
print("dH13[q-1,p-1], S13qp =", dH13[q-1, p-1], S13qp[p-1, q-1])
print("dH40 pair-sum vs S40 =",
      dH40[p-1, q-1] + dH40[q-1, p-1], S40[p-1, q-1])

print("dH04 pair-sum vs S04 =",
      dH04[p-1, q-1] + dH04[q-1, p-1], S04[p-1, q-1])
print("dH22 pair-sum vs S22NN =",
      dH22[p-1, q-1] + dH22[q-1, p-1], S22NN[p-1, q-1])

print("dHb22[p-1,q-1] vs St22pq =",
      dHb22[p-1, q-1], St22pq[p-1, q-1])

print("dHb22[q-1,p-1] vs St22qp =",
      dHb22[q-1, p-1], St22qp[p-1, q-1])'''
print("============================================================")
ESCF = info['obj_val']
Lam = info['mult_g'][0]

print("The perturbation x =",x0)
for k in range(NAO):
    H04[k,k] = 0
    H40[k,k] = 0
    # H13[p,p] = 0
    # H31[p,p] = 0

print("H00 is ", H00)

# ============================================================
# Flags for Fortran
# ============================================================
ipbc = 1 if is_pbc else 0

# Set icorr = 1 only when you want the correlation function
# Set icorr = 0 for energy-only calculation
icorr = 1

# ============================================================
# Call Fortran CCSD
# New interface:
# nbcscc(U, V, J, Lambda, H00, H11, H20, H02, H31, H13,
#        H22, Hb22, H40, H04, NAO, NOcc, ngrid, BroyVec,
#        nx, ny, ipbc, icorr)
# Returns:
#   ECC, Corr
# ============================================================

ECC,T1,T2,Z1,Z2 = nbcscc(U, V, J, Lambda,H00, H11, H20, H02, H31, H13,
    H22, Hb22, H40, H04,NOcc, ngrid, BroyVec,NAO
)

#ECC, Corr = nbcscc(
#    U, V, J, Lambda,
#    H00, H11, H20, H02, H31, H13,
#    H22, Hb22, H40, H04,
#    S00, S20p, S20q, S11p, S11q, S02p, S02q,
#    S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04,
#    NOcc, ngrid, BroyVec, nx, ny, ipbc, icorr,NAO
#)

print("CC at point = ", J)
print("CC at perturbation x = ", x0)
print("E(CCSD) without Enuc:", ECC)

ECC = ECC + ham.Enuc
print("E(CCSD) = ", ECC)

# ============================================================
# Build SpSq / Corr only if correlation functions are needed
# ============================================================
if icorr == 1:

    print("=======================The Analytic Gradient for unrelaxed L(x)======================================")
    # Analytic response matrix <Sp.Sq> #ccsd_spsq calculates the analytic gradient for response (in other words for BCS_x=0)
    SpSq = ccsd_spsq(
        T1, T2, Z1, Z2,
        S00, S20p, S20q, S11p, S11q, S02p, S02q,
        S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04,
        NAO
    )
    
    analytic_grad = SpSq[p-1, q-1]
    print(f"Analytic <S_{p}.S_{q}> =", analytic_grad)
    print(f"Analytic <S_{q}.S_{p}> =", SpSq[q-1,p-1])


    print("======================================================================================================")
    # Build Corr from SpSq
    if 'nx' in globals() and 'ny' in globals() and nx * ny == NAO and ny > 1:
        Corr = calc_corr_2d_from_spsq(SpSq, nx, ny, is_pbc=is_pbc)
        print("Corr shape =", Corr.shape)
        print("Correlation function Corr(dx,dy):")
        print(pd.DataFrame(Corr))
    else:
        Corr = calc_corr_1d_from_spsq(SpSq, is_pbc=is_pbc)
        print("Corr shape =", Corr.shape)
        print("Correlation function Corr(r):")
        print(pd.DataFrame({"r": np.arange(len(Corr)), "Corr": Corr}))

    np.save("Corr.npy", Corr)

# ------------------------------------------------------------------
# Numerical derivative: fixed BCS/reference for Response check, Solve CC at each x
# 6-point central stencil uses x0 ± h, x0 ± 2h, x0 ± 3h
# ------------------------------------------------------------------
h_fd = 1.0e-2
offsets = [-3, -2, -1, 1, 2, 3]
E_of_x = {}

print("======================= Numerical gradient from 6-point stencil =======================")
for m in offsets:
    x_fd = x0 + m * h_fd

    H00_fd, H20_fd, H11_fd, H02_fd, H40_fd, H31_fd, H22_fd, Hb22_fd, H13_fd, H04_fd = hub2pair(
        U, V,
        ham.h_diag, ham.v, ham.w, p, q, x_fd,
        S00, S20p, S20q, S11p, S11q, S02p, S02q,
        S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04,
        NAO
    )

    for i in range(NAO):
        H04_fd[i, i] = 0
        H40_fd[i, i] = 0
    

    ECC_fd, _, _, _, _ = nbcscc(
        U, V, J, Lambda,
        H00_fd, H11_fd, H20_fd, H02_fd, H31_fd, H13_fd,
        H22_fd, Hb22_fd, H40_fd, H04_fd,
        NOcc, ngrid, BroyVec, NAO
    )

    # Enuc is independent of x here, so you can use ECC_fd directly.
    E_of_x[m] = ECC_fd
    print(f"x = {x_fd:+.6e}   ECC = {ECC_fd}")

num_grad = (
    -E_of_x[-3] + 9.0 * E_of_x[-2] - 45.0 * E_of_x[-1]
    + 45.0 * E_of_x[1] - 9.0 * E_of_x[2] + E_of_x[3]
) / (60.0 * h_fd)
print("=============================FINAL GRADIENT SUMMARY================================================")
print("Numerical gradient (6-point stencil) =", num_grad)
print("Analytic gradient                    =", analytic_grad)
print("Absolute error                       =", abs(num_grad - analytic_grad))
print("Relative error                       =", abs(num_grad - analytic_grad) / max(1.0e-14, abs(analytic_grad)))
