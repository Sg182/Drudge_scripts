import numpy as np

def build_A_pq(U, v, h, V, W, p, q, x):
    """
    Build the matrix A^(pq) with elements

        A[t, s] = dF_t^(pq) / dtheta_s

    for a fixed pair (p, q), using the expanded formula provided.

    Notes
    -----
    This version assumes your analytic formula is already valid for complex
    u and v, i.e. it uses products like u*u and v*v exactly as written,
    with NO complex conjugation inserted automatically.
    """
    
    u = np.asarray(U, dtype=np.complex128)
    v = np.asarray(v, dtype=np.complex128)
    h = np.asarray(h, dtype=np.complex128)
    V = np.asarray(V, dtype=np.complex128)
    W = np.asarray(W, dtype=np.complex128)
    x = np.complex128(x)

    n = len(U)

    if v.shape != (n,):
        raise ValueError("u and v must have shape (n,)")
    if h.shape != (n,):
        raise ValueError("h must have shape (n,)")
    if V.shape != (n, n):
        raise ValueError("V must have shape (n, n)")
    if W.shape != (n, n):
        raise ValueError("W must have shape (n, n)")
    if not (0 <= p < n and 0 <= q < n):
        raise ValueError("p and q must be valid 0-based indices")

    u2 = u * u
    v2 = v * v
    u4 = u2 * u2
    v4 = v2 * v2

    A = np.zeros((n, n), dtype=np.complex128)

    delta_pq = 1.0 if p == q else 0.0

    for t in range(n):
        diag_sum_t = 0.0 + 0.0j
        for p0 in range(n):
            diag_sum_t += (
                4.0 * V[p0, t] * u[p0] * u[t] * v[p0] * v[t]
                + 4.0 * V[t, p0] * u[p0] * u[t] * v[p0] * v[t]
                - 4.0 * W[p0, t] * u2[t] * v2[p0]
                + 4.0 * W[p0, t] * v2[p0] * v2[t]
            )

        for s in range(n):
            delta_ts = 1.0 if t == s else 0.0
            delta_pt = 1.0 if p == t else 0.0
            delta_ps = 1.0 if p == s else 0.0
            delta_qt = 1.0 if q == t else 0.0
            delta_qs = 1.0 if q == s else 0.0

            val = 0.0 + 0.0j

            # First big summed diagonal piece
            val -= delta_ts * diag_sum_t

            # x-dependent terms
            val +=  2.0 * x * delta_pq * delta_pt * delta_ps * u4[p]
            val += -2.0 * x * delta_pq * delta_pt * delta_ps * v4[p]

            val +=  2.0 * x * delta_pt * delta_ps * u2[p] * v2[q]
            val += -1.0 * x * delta_pt * delta_ps * u2[p]
            val += -4.0 * x * delta_pt * delta_ps * u[p] * u[q] * v[p] * v[q]
            val += -2.0 * x * delta_pt * delta_ps * v2[p] * v2[q]
            val +=  1.0 * x * delta_pt * delta_ps * v2[p]

            val +=  1.0 * x * delta_pt * delta_qs * u2[p] * u2[q]
            val += -1.0 * x * delta_pt * delta_qs * u2[p] * v2[q]
            val +=  4.0 * x * delta_pt * delta_qs * u[p] * u[q] * v[p] * v[q]
            val += -1.0 * x * delta_pt * delta_qs * u2[q] * v2[p]
            val +=  1.0 * x * delta_pt * delta_qs * v2[p] * v2[q]

            val +=  1.0 * x * delta_ps * delta_qt * u2[p] * u2[q]
            val += -1.0 * x * delta_ps * delta_qt * u2[p] * v2[q]
            val +=  4.0 * x * delta_ps * delta_qt * u[p] * u[q] * v[p] * v[q]
            val += -1.0 * x * delta_ps * delta_qt * u2[q] * v2[p]
            val +=  1.0 * x * delta_ps * delta_qt * v2[p] * v2[q]

            val += -4.0 * x * delta_qt * delta_qs * u[p] * u[q] * v[p] * v[q]
            val +=  2.0 * x * delta_qt * delta_qs * u2[q] * v2[p]
            val += -1.0 * x * delta_qt * delta_qs * u2[q]
            val += -2.0 * x * delta_qt * delta_qs * v2[p] * v2[q]
            val +=  1.0 * x * delta_qt * delta_qs * v2[q]

            # Remaining terms
            val += 12.0 * delta_ts * V[t, t] * u2[t] * v2[t]
            val += -4.0 * delta_ts * V[t, t] * v4[t]
            val +=  4.0 * delta_ts * h[t] * u2[t]
            val += -4.0 * delta_ts * h[t] * v2[t]

            val +=  V[t, s] * u2[t] * u2[s]
            val += -V[t, s] * u2[t] * v2[s]
            val += -V[t, s] * u2[s] * v2[t]
            val +=  V[t, s] * v2[t] * v2[s]

            val +=  V[s, t] * u2[t] * u2[s]
            val += -V[s, t] * u2[t] * v2[s]
            val += -V[s, t] * u2[s] * v2[t]
            val +=  V[s, t] * v2[t] * v2[s]

            val +=  8.0 * W[t, s] * u[t] * u[s] * v[t] * v[s]

            A[t, s] = val

    return A


def build_B_pq(U, V, p, q):
    """
    Build the vector B^(pq) with elements

        B[t] = F_t^(1)(pq)

    using complex u and v directly, with the formula as written.
    """

    u = np.asarray(U, dtype=np.complex128)
    v = np.asarray(V, dtype=np.complex128)

    n = len(U)

    if v.shape != (n,):
        raise ValueError("u and v must have shape (n,)")
    if not (0 <= p < n and 0 <= q < n):
        raise ValueError("p and q must be valid 0-based indices")

    B = np.zeros(n, dtype=np.complex128)

    delta_pq = 1.0 if p == q else 0.0

    for t in range(n):
        delta_pt = 1.0 if p == t else 0.0
        delta_qt = 1.0 if q == t else 0.0

        val = 0.0 + 0.0j

        val += 2.0 * delta_pq * delta_pt * u[p]**3 * v[p]
        val += 2.0 * delta_pq * delta_pt * u[p] * v[p]**3

        val += delta_pt * u[p]**2 * u[q] * v[q]
        val += 2.0 * delta_pt * u[p] * v[p] * v[q]**2
        val += -delta_pt * u[p] * v[p]
        val += -delta_pt * u[q] * v[p]**2 * v[q]

        val += delta_qt * u[p] * u[q]**2 * v[p]
        val += -delta_qt * u[p] * v[p] * v[q]**2
        val += 2.0 * delta_qt * u[q] * v[p]**2 * v[q]
        val += -delta_qt * u[q] * v[q]

        B[t] = val

    return B


def solve_theta1_pq(u, v, h, V, W, p, q, x, check_symmetry=True):
    """
    Solve

        A^(pq) theta1^(pq) = -B^(pq)

    for complex A and B.
    """
    A = build_A_pq(u, v, h, V, W, p, q, x)
    B = build_B_pq(u, v, p, q)

    if check_symmetry:
        # For complex formulas like this, check complex symmetry, not Hermiticity
        asym = np.max(np.abs(A - A.T))
        print(f"(p,q)=({p},{q}) max complex asymmetry in A = {asym:.6e}")

        herm = np.max(np.abs(A - A.conj().T))
        print(f"(p,q)=({p},{q}) max non-Hermitian part in A = {herm:.6e}")

    theta1 = -np.linalg.solve(A, B)
    return theta1, A, B


def compute_ccsd_energy_partials_uv(U, Vocc, T1, T2, h, Vmat, Wmat, symmetrize_t2=False):
    """
    Compute dE/du_r and dE/dv_r for the latest formulas.

    Conventions
    -----------
    - Uses products exactly as written, with no automatic complex conjugation.
    - Full sums over all indices.
    - If T2 is stored only in one triangle, set symmetrize_t2=True.
    """
    u = np.asarray(U, dtype=np.complex128)
    v = np.asarray(Vocc, dtype=np.complex128)
    t1 = np.asarray(T1, dtype=np.complex128)
    t2 = np.asarray(T2, dtype=np.complex128)
    h = np.asarray(h, dtype=np.complex128)
    Vmat = np.asarray(Vmat, dtype=np.complex128)
    Wmat = np.asarray(Wmat, dtype=np.complex128)

    n = len(u)

    if v.shape != (n,):
        raise ValueError("U and Vocc must have shape (n,)")
    if t1.shape != (n,):
        raise ValueError("T1 must have shape (n,)")
    if t2.shape != (n, n):
        raise ValueError("T2 must have shape (n, n)")
    if h.shape != (n,):
        raise ValueError("h must have shape (n,)")
    if Vmat.shape != (n, n):
        raise ValueError("Vmat must have shape (n, n)")
    if Wmat.shape != (n, n):
        raise ValueError("Wmat must have shape (n, n)")

    if symmetrize_t2:
        t2 = 0.5 * (t2 + t2.T)

    dE0_du = np.zeros(n, dtype=np.complex128)
    dE0_dv = np.zeros(n, dtype=np.complex128)
    dX_du = np.zeros(n, dtype=np.complex128)
    dX_dv = np.zeros(n, dtype=np.complex128)
    dY_du = np.zeros(n, dtype=np.complex128)
    dY_dv = np.zeros(n, dtype=np.complex128)

    for r in range(n):
        # ====================================================
        # dE0 / du_r
        # ∂E0/∂u_r = Σ_p [V_{p,r} u_p v_p v_r + V_{r,p} u_p v_p v_r]
        # ====================================================
        val = 0.0 + 0.0j
        for p in range(n):
            val += Vmat[p, r] * u[p] * v[p] * v[r]
            val += Vmat[r, p] * u[p] * v[p] * v[r]
        dE0_du[r] = val

        # ====================================================
        # dE0 / dv_r
        # ∂E0/∂v_r = Σ_p [V_{p,r} u_p u_r v_p + V_{r,p} u_p u_r v_p + 4 W_{p,r} v_p^2 v_r]
        #            + [4 V_{r,r} v_r^3 + 4 h_r v_r]
        # ====================================================
        val = 0.0 + 0.0j
        for p in range(n):
            val += Vmat[p, r] * u[p] * u[r] * v[p]
            val += Vmat[r, p] * u[p] * u[r] * v[p]
            val += 4.0 * Wmat[p, r] * v[p]**2 * v[r]
        val += 4.0 * Vmat[r, r] * v[r]**3
        val += 4.0 * h[r] * v[r]
        dE0_dv[r] = val

        # ====================================================
        # dX / du_r
        # ====================================================
        val = 0.0 + 0.0j
        for p in range(n):
            val +=  2.0 * Wmat[p, r] * t1[p] * t1[r] * u[p] * v[p] * v[r]
            val +=  2.0 * Wmat[p, r] * t2[p, r] * u[p] * v[p] * v[r]
            val += -2.0 * Vmat[r, p] * t1[p] * t1[r] * u[r] * v[p]**2
            val += -2.0 * Vmat[r, p] * t2[p, r] * u[r] * v[p]**2
        dX_du[r] = val

        # ====================================================
        # dX / dv_r
        # ====================================================
        val = 0.0 + 0.0j
        for p in range(n):
            val +=  2.0 * Wmat[p, r] * t1[p] * t1[r] * u[p] * u[r] * v[p]
            val +=  2.0 * Wmat[p, r] * t2[p, r] * u[p] * u[r] * v[p]
            val += -2.0 * Vmat[p, r] * t1[p] * t1[r] * u[p]**2 * v[r]
            val += -2.0 * Vmat[p, r] * t2[p, r] * u[p]**2 * v[r]
        dX_dv[r] = val

        # ====================================================
        # dY / du_r
        # ∂Y/∂u_r = Σ_q [2 δ_pr V_{q,p} t_p u_p u_q v_q + 2 δ_pr W_{q,p} t_p v_p v_q^2]
        #         + [2 δ_pr V_{p,p} t_p v_p^3 + 2 δ_pr h_p t_p v_p
        #            - V_{p,r} t_p v_p^2 v_r + V_{r,p} t_p u_p^2 v_r]
        #
        # After applying δ_pr, p = r in the delta terms.
        # ====================================================
        val = 0.0 + 0.0j

        # delta_pr terms (p = r), summed over q
        for q in range(n):
            val += 2.0 * Vmat[q, r] * t1[r] * u[r] * u[q] * v[q]
            val += 2.0 * Wmat[q, r] * t1[r] * v[r] * v[q]**2

        val += 2.0 * Vmat[r, r] * t1[r] * v[r]**3
        val += 2.0 * h[r] * t1[r] * v[r]

        # non-delta pieces, summed over p
        for p in range(n):
            val += -Vmat[p, r] * t1[p] * v[p]**2 * v[r]
            val +=  Vmat[r, p] * t1[p] * u[p]**2 * v[r]

        dY_du[r] = val

        # ====================================================
        # dY / dv_r
        # ∂Y/∂v_r = - Σ_q [2 δ_pr V_{p,q} t_p u_q v_p v_q - 2 δ_pr W_{q,p} t_p u_p v_q^2]
        #         + [6 δ_pr V_{p,p} t_p u_p v_p^2 + 2 δ_pr h_p t_p u_p
        #            - V_{p,r} t_p u_r v_p^2 + V_{r,p} t_p u_p^2 u_r
        #            + 4 W_{p,r} t_p u_p v_p v_r]
        #
        # After applying δ_pr, p = r in the delta terms.
        # ====================================================
        val = 0.0 + 0.0j

        # delta_pr terms (p = r), summed over q
        for q in range(n):
            val += -2.0 * Vmat[r, q] * t1[r] * u[q] * v[r] * v[q]
            val +=  2.0 * Wmat[q, r] * t1[r] * u[r] * v[q]**2

        val += 6.0 * Vmat[r, r] * t1[r] * u[r] * v[r]**2
        val += 2.0 * h[r] * t1[r] * u[r]

        # non-delta pieces, summed over p
        for p in range(n):
            val += -Vmat[p, r] * t1[p] * u[r] * v[p]**2
            val +=  Vmat[r, p] * t1[p] * u[p]**2 * u[r]
            val +=  4.0 * Wmat[p, r] * t1[p] * u[p] * v[p] * v[r]

        dY_dv[r] = val

    dE_du = dE0_du + dX_du + dY_du
    dE_dv = dE0_dv + dX_dv + dY_dv

    return {
        "dE0_du": dE0_du,
        "dE0_dv": dE0_dv,
        "dX_du": dX_du,
        "dX_dv": dX_dv,
        "dY_du": dY_du,
        "dY_dv": dY_dv,
        "dE_du": dE_du,
        "dE_dv": dE_dv,
    }


def compute_g_theta_ccsd(U, Vocc, T1, T2, h, Vmat, Wmat, symmetrize_t2=False):
    u = np.asarray(U, dtype=np.complex128)
    v = np.asarray(Vocc, dtype=np.complex128)

    dat = compute_ccsd_energy_partials_uv(
        u, v, T1, T2, h, Vmat, Wmat, symmetrize_t2=symmetrize_t2
    )

    g_theta = -v * dat["dE_du"] + u * dat["dE_dv"]
    dat["g_theta"] = g_theta
    return dat


def compute_orbital_relaxation_term(U, Vocc, T1, T2, h, Vmat, Wmat,
                                    p, q, x=0.0,
                                    check_symmetry=True,
                                    symmetrize_t2=False):
    """
    Convenience wrapper that computes the explicit orbital-relaxation term

        sum_r g_theta[r] * theta1[r]

    for a given pair (p, q).

    Returns a dictionary containing theta1, g_theta, the contraction, and
    intermediate derivatives.

    Important
    ---------
    This uses a plain sum, not np.vdot, to stay consistent with the
    no-conjugation convention used in your formulas.
    """
    theta1, A, B = solve_theta1_pq(
        U, Vocc, h, Vmat, Wmat, p, q, x,
        check_symmetry=check_symmetry
    )

    gdat = compute_g_theta_ccsd(
        U, Vocc, T1, T2, h, Vmat, Wmat,
        symmetrize_t2=symmetrize_t2
    )

    orbital_relaxation = np.sum(gdat["g_theta"] * theta1)

    out = {
        "theta1": theta1,
        "A": A,
        "B": B,
        "g_theta": gdat["g_theta"],
        "orbital_relaxation": orbital_relaxation,
    }
    out.update(gdat)
    return out