import numpy as np

class SFS_1D_XXZ:
    def __init__(self, nmo, delta, periodic=True):
        self.nmo = nmo
        self.delta = delta
        self.periodic = periodic

        # h100, h001: on-site source terms (complex dtype0)

        self.h100 = np.zeros(nmo, dtype=np.complex128)
        self.h001 = np.zeros(nmo, dtype=np.complex128)
        # h010: diagonal one-body shift from Δ term
        self.h010 = (-0.5 * delta) * np.ones(nmo, dtype=np.complex128)
        if not periodic and nmo >= 2:
            # ends have half the bonds in OBC
            self.h010[0]  = (-0.25 * delta)
            self.h010[-1] = (-0.25 * delta)

        # h110: hopping (nearest neighbor)
        self.h110 = np.zeros((nmo, nmo), dtype=np.complex128)
        if periodic:
            p = np.arange(nmo)
            self.h110[p, (p + 1) % nmo] += 0.25
            self.h110[p, (p - 1) % nmo] += 0.25
        else:
            if nmo >= 2:
                p = np.arange(1, nmo - 1)
                self.h110[p, p + 1] += 0.25
                self.h110[p, p - 1] += 0.25

        # h011: usually the Hermitian partner of h110
        

        self.h011 = np.zeros((nmo, nmo), dtype=np.complex128)
        if periodic:

            p = np.arange(nmo)
            self.h011[(p - 1) % nmo, p] += 0.25   # N_{p-1} P_p
            self.h011[(p + 1) % nmo, p] += 0.25   # P_p N_{p+1}
        else:
            if nmo >= 2:
                p = np.arange(1, nmo - 1)
                self.h011[p - 1, p] += 0.25
                self.h011[p + 1, p] += 0.25

        self.h011 = self.h110.T


        # h120: three-index block
        self.h120 = np.zeros((nmo, nmo, nmo), dtype=np.complex128)
        if periodic:
            for p in range(nmo):
                q = (p - 1) % nmo
                r = (p + 1) % nmo
                self.h120[p, q, r] += -0.125
                self.h120[p, r, q] += -0.125
        else:
            for p in range(1, nmo - 1):
                q = p - 1
                r = p + 1
                self.h120[p, q, r] += -0.125
                self.h120[p, r, q] += -0.125

        # h021
        self.h021 = np.zeros((nmo, nmo, nmo), dtype=np.complex128)
        if periodic:
            for p in range(nmo):
                r = (p - 1) % nmo
                q = (p + 1) % nmo
                self.h021[r, q, p] += -0.125
                self.h021[q, r, p] += -0.125
        else:
            for p in range(1, nmo - 1):
                r = p - 1
                q = p + 1
                self.h021[r, q, p] += -0.125
                self.h021[q, r, p] += -0.125

        # h020 (two-site, next-nearest in your scheme)
        self.h020 = np.zeros((nmo, nmo), dtype=np.complex128)
        if periodic:
            p = np.arange(nmo)
            q = (p + 2) % nmo
            self.h020[p, q] += 0.125 * delta
            self.h020[q, p] += 0.125 * delta
        else:
            if nmo >= 3:
                p = np.arange(nmo - 2)
                q = p + 2
                self.h020[p, q] += 0.125 * delta
                self.h020[q, p] += 0.125 * delta

        # scalar h000 (constant shift)
        self.h000 = (delta * 0.25 * (nmo if periodic else (nmo - 1))) + 0.0j


        #print("h020 ",float(self.h020))
