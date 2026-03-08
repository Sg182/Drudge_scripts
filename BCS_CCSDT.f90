Module CCResCCSDT
  Use Precision
  Use Constants

  Contains
    
    Subroutine CCSDT(Ene,Res1,Res2,Res3,T1,T2,T3,NAO, &
        H20,H11,H02,H40,H31,H22,HT22,H13,H04)

    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: T1(NAO)
    Complex (Kind=pr), Intent(In)    :: T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: T3(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: Ene,Res1(NAO),Res2(NAO,NAO),Res3(NAO,NAO,NAO)

    Integer                          :: p, q, r, s, i, j, k, l
    complex(kind=pr) , dimension(:), allocatable :: tau0

    complex(kind=pr) , dimension(:), allocatable :: tau1

    complex(kind=pr) , dimension(:), allocatable :: tau2

    complex(kind=pr) , dimension(:), allocatable :: tau3

    complex(kind=pr) , dimension(:), allocatable :: tau4

    complex(kind=pr) , dimension(:), allocatable :: tau5

    complex(kind=pr) , dimension(:, :), allocatable :: tau6

    complex(kind=pr) , dimension(:, :), allocatable :: tau7

    complex(kind=pr) , dimension(:, :), allocatable :: tau8

    complex(kind=pr) , dimension(:, :), allocatable :: tau9

    complex(kind=pr) , dimension(:, :), allocatable :: tau10

    complex(kind=pr) , dimension(:, :), allocatable :: tau11

    complex(kind=pr) , dimension(:, :), allocatable :: tau12

    complex(kind=pr) , dimension(:, :), allocatable :: tau13

    complex(kind=pr) , dimension(:, :), allocatable :: tau14

    complex(kind=pr) , dimension(:, :), allocatable :: tau15

    complex(kind=pr) , dimension(:), allocatable :: tau16

    complex(kind=pr) , dimension(:), allocatable :: tau17

    complex(kind=pr) , dimension(:, :), allocatable :: tau18

    complex(kind=pr) , dimension(:, :), allocatable :: tau19

    complex(kind=pr) , dimension(:, :), allocatable :: tau20

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau21

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau22

    complex(kind=pr) , dimension(:, :), allocatable :: tau23

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau24

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau25

    complex(kind=pr) , dimension(:, :), allocatable :: tau26

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau27

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau28

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau29

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau30

    complex(kind=pr) , dimension(:, :), allocatable :: tau31

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau32

    complex(kind=pr) , dimension(:, :), allocatable :: tau33

    complex(kind=pr) , dimension(:, :), allocatable :: tau34

    complex(kind=pr) , dimension(:, :), allocatable :: tau35

    complex(kind=pr) , dimension(:, :), allocatable :: tau36

    complex(kind=pr) , dimension(:, :), allocatable :: tau37

    complex(kind=pr) , dimension(:, :), allocatable :: tau38

    !$omp parallel default(shared)

    allocate(tau0(NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                t1(q) * H40(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau1(NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) + ( &
            tau0(p)&
        )
    
    end do
    !$omp end do

    allocate(tau5(NAO))
    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau5(p) = tau5(p) + ( &
            2 * tau0(p)&
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
            2 * tau0(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    Res1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) - ( &
            2 * t1(p)**2 * tau0(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau0)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) + ( &
            H20(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    Ene = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:Ene)
    
    do p=1, NAO
        Ene = Ene + ( &
            t1(p) * tau1(p)&
        )
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

    allocate(tau4(NAO))
    !$omp single
    tau4 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) + ( &
            tau2(p)&
        )
    
    end do
    !$omp end do

    allocate(tau17(NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) - ( &
            tau2(p)&
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
    
        tau4(p) = tau4(p) - ( &
            2 * tau3(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) + ( &
            2 * tau3(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau3)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) + ( &
            H11(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) + ( &
            2 * H22(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            2 * t1(p) * tau4(p)&
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
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                tau5(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    Res2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    tau5(r) * t3(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau5)

    allocate(tau6(NAO, NAO))
    !$omp single
    tau6 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau6(p, q) = tau6(p, q) + ( &
                    H31(p, r) * t3(q, r, p)&
                )
            end do
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
    
            tau11(p, q) = tau11(p, q) - ( &
                tau6(p, q)&
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

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                t1(p)**2 * tau7(p, q)&
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
                2 * tau7(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau23(NAO, NAO))
    !$omp single
    tau23 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau23(p, q) = tau23(p, q) + ( &
                2 * tau7(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau33(NAO, NAO))
    !$omp single
    tau33 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) + ( &
                2 * tau7(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau7)

    allocate(tau8(NAO, NAO))
    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau8(p, q) = tau8(p, q) + ( &
                    H31(p, r) * t2(q, r)&
                )
            end do
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
    
            tau10(p, q) = tau10(p, q) - ( &
                tau8(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau34(NAO, NAO))
    !$omp single
    tau34 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau34(p, q) = tau34(p, q) - ( &
                tau8(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau35(NAO, NAO))
    !$omp single
    tau35 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau35(p, q) = tau35(p, q) - ( &
                tau8(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau36(NAO, NAO))
    !$omp single
    tau36 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau36(p, q) = tau36(p, q) - ( &
                tau8(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau37(NAO, NAO))
    !$omp single
    tau37 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau37(p, q) = tau37(p, q) - ( &
                tau8(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau38(NAO, NAO))
    !$omp single
    tau38 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau38(p, q) = tau38(p, q) - ( &
                tau8(p, q)&
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
            do r=1, NAO
                tau9(p, q) = tau9(p, q) + ( &
                    H40(p, r) * t3(q, r, p)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau10(p, q) = tau10(p, q) + ( &
                2 * tau9(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau11(p, q) = tau11(p, q) + ( &
                t1(p) * tau10(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau10)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau11(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau11(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau11)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau34(p, q) = tau34(p, q) + ( &
                2 * tau9(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau35(p, q) = tau35(p, q) + ( &
                2 * tau9(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau36(p, q) = tau36(p, q) + ( &
                2 * tau9(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau37(p, q) = tau37(p, q) + ( &
                2 * tau9(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau38(p, q) = tau38(p, q) + ( &
                2 * tau9(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau9)

    allocate(tau12(NAO, NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) - ( &
                H31(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) + ( &
                2 * t1(p) * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau13(NAO, NAO))
    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                t1(q) * tau12(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) - ( &
                t1(p) * H31(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                H22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                H22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t2(q, p) * tau13(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau13)

    allocate(tau14(NAO, NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                2 * t2(q, p)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                t1(p)**2 * t1(q)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * H40(q, p) * tau14(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau14)

    allocate(tau15(NAO, NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau15(p, q) = tau15(p, q) + ( &
                H22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau15(p, q) = tau15(p, q) + ( &
                H22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t1(p) * t1(q) * tau15(p, q)&
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
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            2 * H31(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) + ( &
            t1(p) * tau16(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau34(p, q) = tau34(p, q) + ( &
                tau16(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau35(p, q) = tau35(p, q) + ( &
                tau16(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau36(p, q) = tau36(p, q) + ( &
                tau16(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau16)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) - ( &
            H11(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) - ( &
            2 * H22(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau17(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau17(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    Res3 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau17(p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau17(q) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau17(r) * t3(r, p, q)&
                )
    
            end do
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
                Hb22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau18(p, q) = tau18(p, q) + ( &
                2 * t1(p) * H31(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                t1(p)**2 * tau18(q, p)&
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
                H13(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau19(p, q) = tau19(p, q) - ( &
                t1(p)**2 * H31(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(p) * tau19(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau19)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau20(p, q) = tau20(p, q) + ( &
                Hb22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    t2(r, p) * tau20(r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau20)

    allocate(tau21(NAO, NAO, NAO))
    !$omp single
    tau21 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau21(p, q, r) = tau21(p, q, r) + ( &
                        H31(p, s) * t3(q, s, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    allocate(tau25(NAO, NAO, NAO))
    !$omp single
    tau25 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau25(p, q, r) = tau25(p, q, r) + ( &
                    2 * t1(p) * tau21(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau21)

    allocate(tau22(NAO, NAO, NAO))
    !$omp single
    tau22 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau22(p, q, r) = tau22(p, q, r) + ( &
                        H40(s, p) * t3(q, s, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau25(p, q, r) = tau25(p, q, r) - ( &
                    2 * t1(p)**2 * tau22(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau22)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau23(p, q) = tau23(p, q) + ( &
                Hb22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau24(NAO, NAO, NAO))
    !$omp single
    tau24 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau24(p, q, r) = tau24(p, q, r) + ( &
                        tau23(p, s) * t3(s, q, r)&
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau25(p, q, r) = tau25(p, q, r) + ( &
                    tau24(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau24)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau25(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau25(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau25(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau25)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau36(p, q) = tau36(p, q) + ( &
                t1(q) * tau23(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau38(p, q) = tau38(p, q) + ( &
                t1(p) * tau23(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau23)

    allocate(tau26(NAO, NAO))
    !$omp single
    tau26 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau26(p, q) = tau26(p, q) + ( &
                2 * H40(q, p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau26(p, q) = tau26(p, q) + ( &
                H22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau26(p, q) = tau26(p, q) + ( &
                H22(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau27(NAO, NAO, NAO))
    !$omp single
    tau27 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau27(p, q, r) = tau27(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau27(p, q, r) = tau27(p, q, r) + ( &
                    t1(q) * t2(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau27(p, q, r) = tau27(p, q, r) + ( &
                    t1(r) * t2(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * tau26(q, p) * tau27(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau27)

    allocate(tau28(NAO, NAO, NAO))
    !$omp single
    tau28 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau28(p, q, r) = tau28(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau28(p, q, r) = tau28(p, q, r) + ( &
                    t1(p) * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau28(p, q, r) = tau28(p, q, r) + ( &
                    t1(q) * t2(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * tau26(r, p) * tau28(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau28)

    allocate(tau29(NAO, NAO, NAO))
    !$omp single
    tau29 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau29(p, q, r) = tau29(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau29(p, q, r) = tau29(p, q, r) + ( &
                    t1(p) * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau29(p, q, r) = tau29(p, q, r) + ( &
                    t1(r) * t2(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * tau26(r, q) * tau29(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau26)

    deallocate(tau29)

    allocate(tau30(NAO, NAO, NAO))
    !$omp single
    tau30 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau30(p, q, r) = tau30(p, q, r) + ( &
                    2 * t1(p) * t3(r, p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau30(p, q, r) = tau30(p, q, r) + ( &
                    t1(p)**2 * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau12(p, q) * tau30(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau12(p, r) * tau30(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau12(q, r) * tau30(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau12)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * H31(q, p) * tau30(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * H31(r, p) * tau30(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * H31(r, q) * tau30(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau30)

    allocate(tau31(NAO, NAO))
    !$omp single
    tau31 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) + ( &
                t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau32(NAO, NAO, NAO))
    !$omp single
    tau32 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau32(p, q, r) = tau32(p, q, r) + ( &
                    H31(p, q) * t2(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau32(p, q, r) = tau32(p, q, r) + ( &
                    H31(q, p) * t2(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    4 * tau31(q, p) * tau32(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    4 * tau31(r, p) * tau32(p, r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    4 * tau31(r, q) * tau32(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau32)

    deallocate(tau31)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) + ( &
                Hb22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau33(p, q) = tau33(p, q) - ( &
                2 * t1(p)**2 * H40(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau34(p, q) = tau34(p, q) + ( &
                t1(q) * tau33(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau35(p, q) = tau35(p, q) + ( &
                t1(p) * tau33(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau37(p, q) = tau37(p, q) + ( &
                t1(p) * tau33(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau33)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau34(p, q) = tau34(p, q) - ( &
                H13(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, q) * tau34(p, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau34)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau35(p, q) = tau35(p, q) - ( &
                H13(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, q) * tau35(r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau35)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau36(p, q) = tau36(p, q) - ( &
                H13(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, p) * tau36(q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau36)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau37(p, q) = tau37(p, q) - ( &
                H13(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, p) * tau37(r, q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau37)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau38(p, q) = tau38(p, q) - ( &
                H13(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(q, p) * tau38(q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(q, p) * tau38(p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau38)

    !$omp do schedule(static) reduction(+:Ene)
    
    do q=1, NAO
        do p=1, NAO
            Ene = Ene + ( &
                H40(q, p) * t2(q, p)&
            )
        end do
    end do
    
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) - ( &
            t1(p)**2 * H20(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) - ( &
            2 * t1(p)**2 * H31(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            H02(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    H40(r, q) * t3(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                t1(q) * Hb22(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                2 * H31(p, q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    Hb22(p, r) * t2(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * H04(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                t1(q)**2 * Hb22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(q) * H13(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp end parallel

    END SUBROUTINE CCSDT
END SUBROUTINE CCResCCSDT
