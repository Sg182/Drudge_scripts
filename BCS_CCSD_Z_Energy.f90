Module CC_Z_Test
Use Precision
Use Constants

!Here we will us optimized Z and T
    Subroutine CCSD_Z_Ene(E,z1,z2,T1,T2,NAO,H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: T1(NAO), T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: z1(NAO), z2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    !Complex (Kind=pr), Intent(Out)   :: L1(NAO), L2(NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: E
    Integer                          :: p, q, r, s, i, j, k, l

    complex(kind=pr) , dimension(:, :), allocatable :: tau0

    complex(kind=pr) , dimension(:, :), allocatable :: tau1

    complex(kind=pr) , dimension(:), allocatable :: tau2

    complex(kind=pr) , dimension(:), allocatable :: tau3

    complex(kind=pr) , dimension(:), allocatable :: tau4

    complex(kind=pr) , dimension(:), allocatable :: tau5

    complex(kind=pr) , dimension(:), allocatable :: tau6

    complex(kind=pr) , dimension(:, :), allocatable :: tau7

    complex(kind=pr) , dimension(:, :), allocatable :: tau8

    complex(kind=pr) , dimension(:, :), allocatable :: tau9

    complex(kind=pr) , dimension(:, :), allocatable :: tau10

    complex(kind=pr) , dimension(:, :), allocatable :: tau11

    complex(kind=pr) , dimension(:), allocatable :: tau12

    complex(kind=pr) , dimension(:), allocatable :: tau13

    complex(kind=pr) , dimension(:), allocatable :: tau14

    !$omp parallel default(shared)

    allocate(tau0(NAO, NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau0(p, q) = tau0(p, q) + ( &
                H31(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau0(p, q) = tau0(p, q) - ( &
                t1(p) * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau1(NAO, NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) - ( &
                t1(q) * tau0(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau0)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) + ( &
                H22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau11(NAO, NAO))
    !$omp single
    tau11 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                4 * t2(q, p) * tau1(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau1)

    allocate(tau2(NAO))
    !$omp single
    tau2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p) = tau2(p) + ( &
                t1(q) * H31(p, q)&
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
    
        tau6(p) = tau6(p) - ( &
            tau2(p)&
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
            2 * t1(p) * tau2(p)&
        )
    
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
                H40(p, q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            2 * tau3(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) - ( &
            4 * t1(p) * tau3(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau3)

    allocate(tau4(NAO))
    !$omp single
    tau4 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p) = tau4(p) + ( &
                t1(q) * H40(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau5(NAO))
    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau5(p) = tau5(p) + ( &
            2 * tau4(p)&
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
            2 * tau4(p)&
        )
    
    end do
    !$omp end do

    allocate(tau14(NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau14(p) = tau14(p) + ( &
            tau4(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau4)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau5(p) = tau5(p) + ( &
            H20(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau5(p) = tau5(p) + ( &
            2 * H31(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            t1(p) * tau5(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) - ( &
            t1(p)**2 * tau5(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau5)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) - ( &
            H11(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) - ( &
            2 * H22(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) - ( &
                2 * tau6(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau6)

    allocate(tau7(NAO, NAO))
    !$omp single
    tau7 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau7(p, q) = tau7(p, q) + ( &
                    H40(r, p) * t2(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau8(NAO, NAO))
    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                2 * tau7(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau10(NAO, NAO))
    !$omp single
    tau10 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10(p, q) = tau10(p, q) + ( &
                tau7(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau7)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                HT22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                2 * t1(p) * H31(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                t1(p)**2 * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) - ( &
                t1(q)**2 * tau8(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau8)

    allocate(tau9(NAO, NAO))
    !$omp single
    tau9 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                H13(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                2 * t1(p) * H22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau9(p, q) = tau9(p, q) + ( &
                    H31(q, r) * t2(p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                2 * t1(q) * tau9(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau9)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10(p, q) = tau10(p, q) + ( &
                HT22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau11(p, q) = tau11(p, q) + ( &
                    t2(r, p) * tau10(r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau10)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                H04(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                2 * t2(q, p)**2 * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    E = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:E)
    
    do q=1, NAO
        do p=1, NAO
            E = E + ( &
                tau11(p, q) * z2(p, q)&
            )
        end do
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
        do q=1, NAO
            tau13(p) = tau13(p) + ( &
                tau12(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau12)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) + ( &
            H02(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) + ( &
            2 * H11(p) * t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) + ( &
            4 * t1(p) * H22(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p) = tau13(p) + ( &
                t1(q) * HT22(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p) = tau13(p) + ( &
                2 * H31(p, q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static) reduction(+:E)
    
    do p=1, NAO
        E = E + ( &
            tau13(p) * z1(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau13)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau14(p) = tau14(p) + ( &
            H20(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static) reduction(+:E)
    
    do p=1, NAO
        E = E + ( &
            t1(p) * tau14(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau14)

    !$omp do schedule(static) reduction(+:E)
    
    do q=1, NAO
        do p=1, NAO
            E = E + ( &
                H40(q, p) * t2(q, p)&
            )
        end do
    end do
    
    !$omp end do

    !$omp end parallel
    End Subroutine CCSD_Z_Ene

End Module CC_Z_Test
