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

Lambda = 0.0

H1 = np.arange(NAO) + 1
is_pbc = True
x = 0
p = 1
q = 2
# ham = Pairing(NAO, nelec, H1, J, Lambda)
ham = XXZ(NAO, J, periodic=is_pbc)
#ham = XXZPerturbed(NAO, J,x,p,q, periodic=is_pbc)
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

H00, H20, H11, H02, H40, H31, H22, Hb22, H13, H04 = hub2pair(
    U, V, ham.h_diag, ham.v, ham.w, NAO
)

S00, S20p, S20q, S11p, S11q, S02p, S02q, \
S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04 = spsq_integral(U, V, NAO)

ESCF = info['obj_val']
Lam = info['mult_g'][0]

print("The perturbation x =",x)
for p in range(NAO):
    H04[p, p] = 0
    H40[p, p] = 0
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

#ECC = nbcscc(
#    U, V, J, Lambda,
#    H00, H11, H20, H02, H31, H13,
#    H22, Hb22, H40, H04,
#    NOcc, ngrid, BroyVec,
#    NAO
#)

ECC, Corr = nbcscc(
    U, V, J, Lambda,
    H00, H11, H20, H02, H31, H13,
    H22, Hb22, H40, H04,
    S00, S20p, S20q, S11p, S11q, S02p, S02q,
    S40, S31pq, S31qp, S22NN, St22pq, St22qp, S13pq, S13qp, S04,
    NOcc, ngrid, BroyVec, nx, ny, ipbc, icorr,NAO
)

print("CC at point = ", J)
print("E(CCSD) without Enuc:", ECC)

ECC = ECC + ham.Enuc
print("E(CCSD) = ", ECC)

# ============================================================
# Correlation output, only meaningful if icorr == 1
# Corr should be shape (nx, ny)
# Corr[dx, dy] corresponds to displacement (dx, dy)
# ============================================================
if icorr == 1:
    print("Corr shape =", Corr.shape)
    print("Correlation function Corr(dx,dy):")
    print(pd.DataFrame(Corr))

    # Save for later plotting/analysis if needed
    np.save("Corr.npy", Corr)
