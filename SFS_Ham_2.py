import numpy as np

class SFS_1D_XXZ:
    def __init__(self, nmo, delta):
        """
        .
        0-based ↔ 1-based:
            site 0 ↔ p = 1 (special)
            site i ↔ p = i+1
        """
        self.nmo = nmo
        self.delta = delta

        # --- h100, h001: keep the boundary P_1† and P_1 coming from N_0 * P_1(†)
        self.h100 = np.zeros(nmo, dtype=np.complex128)
        self.h001 = np.zeros(nmo, dtype=np.complex128)
        if nmo >= 1:
            self.h100[0] = 0.50   # (1/4)*N_0 with N_0 = 1
            self.h001[0] = 0.50

        # --- h010: one-body N_p
        self.h010 = np.zeros(nmo, dtype=np.complex128)
        #if nmo >= 1:
        #    self.h010[0] = -0.25 * delta
        #if nmo >= 2:
        #    self.h010[1] = -0.25 * delta
        if nmo >= 3:
            self.h010[0] = -0.25 * delta
            for i in range(2, nmo - 2):
                self.h010[i] = -0.5 * delta
            #self.h010[nmo - 1] = -0.25 * delta
            #self.h010[nmo - 2] = -0.25 * delta        # site N-1
            self.h010[nmo - 1] = -0.25 * delta

        # --- h110: P†_p N_q, interior only for N_0 = 1
        self.h110 = np.zeros((nmo, nmo), dtype=np.complex128)
        self.h110[0, 1] = -0.25
        for p in range(1, nmo - 1):     # p = 1 .. nmo-2
            self.h110[p, p - 1] += 0.25
            self.h110[p, p + 1] += 0.25

        # --- h011: N_p P_q, interior only
        self.h011 = np.zeros((nmo, nmo), dtype=np.complex128)
        self.h011[1,0] = -0.25
        for p in range(1, nmo - 1):
            self.h011[p - 1, p] += 0.25
            self.h011[p + 1, p] += 0.25

        # --- h120: P† N N (halved)
        self.h120 = np.zeros((nmo, nmo, nmo), dtype=np.complex128)
        for p in range(1, nmo - 1):
            q = p - 1
            r = p + 1
            self.h120[p, q, r] += -0.125
            self.h120[p, r, q] += -0.125

        # --- h021: N N P (halved)
        self.h021 = np.zeros((nmo, nmo, nmo), dtype=np.complex128)
        for p in range(1, nmo - 1):
            left = p - 1
            right = p + 1
            self.h021[left, right, p] += -0.125
            self.h021[right, left, p] += -0.125

        # --- h020: N N next-nearest (halved so (i,j)+(j,i)=Δ/4)
        self.h020 = np.zeros((nmo, nmo), dtype=np.complex128)
        for p in range(1, nmo - 1):
            i = p - 1
            j = p + 1
            self.h020[i, j] += 0.125 * delta
            self.h020[j, i] += 0.125 * delta

        
        self.h000 = 0.25 * delta * (nmo - 3) + 0.0j

