Module CCLamCCSD
Use Precision
Use Constants

Contains

Subroutine CCSD_Z(L1,L2,z1,z2,T1,T2,NAO,H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: T1(NAO), T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: z1(NAO), z2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: L1(NAO), L2(NAO,NAO)
    Integer                          :: p, q, r, s, i, j, k, l

    
    complex(kind=pr) , dimension(:, :), allocatable :: tau0

    complex(kind=pr) , dimension(:, :), allocatable :: tau1

    complex(kind=pr) , dimension(:, :), allocatable :: tau2

    complex(kind=pr) , dimension(:), allocatable :: tau3

    complex(kind=pr) , dimension(:, :), allocatable :: tau4

    complex(kind=pr) , dimension(:, :), allocatable :: tau5

    complex(kind=pr) , dimension(:), allocatable :: tau6

    complex(kind=pr) , dimension(:), allocatable :: tau7

    complex(kind=pr) , dimension(:), allocatable :: tau8

    complex(kind=pr) , dimension(:), allocatable :: tau9

    complex(kind=pr) , dimension(:), allocatable :: tau10

    complex(kind=pr) , dimension(:), allocatable :: tau11

    complex(kind=pr) , dimension(:), allocatable :: tau12

    complex(kind=pr) , dimension(:), allocatable :: tau13

    complex(kind=pr) , dimension(:, :), allocatable :: tau14

    complex(kind=pr) , dimension(:, :), allocatable :: tau15

    complex(kind=pr) , dimension(:), allocatable :: tau16

    complex(kind=pr) , dimension(:, :), allocatable :: tau17

    complex(kind=pr) , dimension(:, :), allocatable :: tau18

    complex(kind=pr) , dimension(:, :), allocatable :: tau19

    complex(kind=pr) , dimension(:, :), allocatable :: tau20

    complex(kind=pr) , dimension(:), allocatable :: tau21

    !$omp parallel default(shared)

    allocate(tau0(NAO, NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau0(p, q) = tau0(p, q) - ( &
                H31(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau0(p, q) = tau0(p, q) + ( &
                2 * t1(p) * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau2(NAO, NAO))
    !$omp single
    tau2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau2(p, q) = tau2(p, q) + ( &
                2 * t2(q, p) * tau0(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    L2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) - ( &
                2 * z1(p) * tau0(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) - ( &
                2 * z1(q) * tau0(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau0)

    allocate(tau1(NAO, NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) + ( &
                H22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) + ( &
                H22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau2(p, q) = tau2(p, q) + ( &
                2 * t1(p) * tau1(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau1)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau2(p, q) = tau2(p, q) + ( &
                H13(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau2(p, q) = tau2(p, q) - ( &
                t1(p)**2 * H31(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau2(p, q) = tau2(p, q) + ( &
                    H31(q, r) * t2(p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp single
    L1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            L1(p) = L1(p) + ( &
                2 * tau2(q, p) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau2)

    allocate(tau3(NAO))
    !$omp single
    tau3 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3(p) = tau3(p) + ( &
                t1(q) * H40(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau6(NAO))
    !$omp single
    tau6 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            2 * tau3(p) * z1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau12(NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) + ( &
            2 * tau3(p)&
        )
    
    end do
    !$omp end do

    allocate(tau16(NAO))
    !$omp single
    tau16 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            2 * tau3(p)&
        )
    
    end do
    !$omp end do

    allocate(tau21(NAO))
    !$omp single
    tau21 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            2 * t1(p) * tau3(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau3)

    allocate(tau4(NAO, NAO))
    !$omp single
    tau4 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau4(p, q) = tau4(p, q) + ( &
                    H40(p, r) * t2(r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau5(NAO, NAO))
    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                2 * tau4(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau14(NAO, NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                2 * tau4(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau4)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                HT22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                2 * t1(p) * H31(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) - ( &
                2 * t1(p)**2 * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau6(p) = tau6(p) + ( &
                tau5(q, p) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau5)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            H20(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            2 * z1(p) * H31(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        L1(p) = L1(p) - ( &
            2 * t1(p) * tau6(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau6)

    allocate(tau7(NAO))
    !$omp single
    tau7 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau7(p) = tau7(p) + ( &
                t2(p, q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau8(NAO))
    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau8(p) = tau8(p) + ( &
            2 * t1(p) * tau7(p)&
        )
    
    end do
    !$omp end do

    allocate(tau13(NAO))
    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) + ( &
            tau7(p)&
        )
    
    end do
    !$omp end do

    allocate(tau17(NAO, NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau17(p, q) = tau17(p, q) - ( &
                4 * tau7(p) * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau8(p) = tau8(p) - ( &
            t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau8(p) = tau8(p) + ( &
            t1(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau8(p) = tau8(p) - ( &
                z1(q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            L1(p) = L1(p) - ( &
                2 * tau8(q) * H40(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau8)

    allocate(tau9(NAO))
    !$omp single
    tau9 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau9(p) = tau9(p) + ( &
                t1(q) * H31(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau11(NAO))
    !$omp single
    tau11 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            tau9(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            tau9(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau9)

    allocate(tau10(NAO))
    !$omp single
    tau10 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p) = tau10(p) + ( &
                H40(p, q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) - ( &
            2 * tau10(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            2 * tau10(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau10)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            H11(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            2 * H22(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        L1(p) = L1(p) + ( &
            2 * tau11(p) * z1(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau11)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) + ( &
            H20(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) + ( &
            2 * H31(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        L1(p) = L1(p) - ( &
            2 * tau12(p) * tau7(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau12)

    deallocate(tau7)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            L1(p) = L1(p) + ( &
                2 * tau13(q) * H31(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau13)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                HT22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                2 * t1(q) * H31(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) - ( &
                2 * t1(q)**2 * H40(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau15(NAO, NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau15(p, q) = tau15(p, q) + ( &
                    tau14(p, r) * z2(r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau14)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau17(p, q) = tau17(p, q) + ( &
                tau15(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau15)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            H20(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau17(p, q) = tau17(p, q) + ( &
                tau16(p) * z1(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau16)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) + ( &
                tau17(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) + ( &
                tau17(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau17)

    allocate(tau18(NAO, NAO))
    !$omp single
    tau18 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau18(p, q) = tau18(p, q) + ( &
                H22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau18(p, q) = tau18(p, q) - ( &
                t1(q) * H31(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau20(NAO, NAO))
    !$omp single
    tau20 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau20(p, q) = tau20(p, q) + ( &
                tau18(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau20(p, q) = tau20(p, q) + ( &
                tau18(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau18)

    allocate(tau19(NAO, NAO))
    !$omp single
    tau19 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau19(p, q) = tau19(p, q) + ( &
                t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau19(p, q) = tau19(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau20(p, q) = tau20(p, q) + ( &
                2 * H40(q, p) * tau19(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau19)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) + ( &
                4 * tau20(q, p) * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau20)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            H20(p) * t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            H11(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            2 * t1(p) * H31(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            2 * H22(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) - ( &
                2 * tau21(p) * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) - ( &
                2 * tau21(q) * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau21)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            L1(p) = L1(p) + ( &
                z1(q) * HT22(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        L1(p) = L1(p) + ( &
            H20(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            L2(p, q) = L2(p, q) + ( &
                2 * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp end parallel
    End Subroutine CCSD_Z
End Module CCLamCCSD