Module CCSDSpSq
    Use Precision
    Use Constants

    Contains

Subroutine CCSD_SpSq(SpSq,U,V,z1,z2,T1,T2,NAO,H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: T1(NAO), T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: z1(NAO), z2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: U(NAO), V(NAO)
    Complex (Kind=pr), Intent(Out)   :: SpSq(NAO,NAO) 
    Integer                          :: p, q, r, s, i, j, k, l

    Complex (Kind=pr)                :: tmp
    complex(kind=pr) , dimension(NAO) :: tau0
    complex(kind=pr) , dimension(NAO, NAO) :: tau1
    complex(kind=pr) , dimension(NAO) :: tau2
    complex(kind=pr) , dimension(NAO) :: tau3
    complex(kind=pr) , dimension(NAO, NAO) :: tau4
    complex(kind=pr) , dimension(NAO, NAO) :: tau5
    complex(kind=pr) , dimension(NAO, NAO) :: tau6
    complex(kind=pr) , dimension(NAO) :: tau7
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau8
    complex(kind=pr) , dimension(NAO) :: tau9
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau10
    complex(kind=pr) , dimension(NAO, NAO) :: tau11
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau12
    complex(kind=pr) , dimension(NAO, NAO) :: tau13
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau14
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau15
    complex(kind=pr) , dimension(NAO) :: tau16
    complex(kind=pr) , dimension(NAO) :: tau17
    complex(kind=pr) , dimension(NAO) :: tau18
    complex(kind=pr) , dimension(NAO, NAO) :: tau19
    complex(kind=pr) , dimension(NAO, NAO) :: tau20
    complex(kind=pr) , dimension(NAO) :: tau21
    complex(kind=pr) , dimension(NAO) :: tau22
    complex(kind=pr) , dimension(NAO, NAO) :: tau23
    complex(kind=pr) , dimension(NAO, NAO) :: tau24
    complex(kind=pr) , dimension(NAO, NAO) :: tau25
    complex(kind=pr) , dimension(NAO, NAO) :: tau26
    complex(kind=pr) , dimension(NAO, NAO) :: tau27
    complex(kind=pr) , dimension(NAO, NAO) :: tau28
    complex(kind=pr) , dimension(NAO, NAO) :: tau29
    complex(kind=pr) , dimension(NAO, NAO) :: tau30
    complex(kind=pr) , dimension(NAO, NAO) :: tau31
    complex(kind=pr) , dimension(NAO, NAO) :: tau32
    complex(kind=pr) , dimension(NAO, NAO) :: tau33
    complex(kind=pr) , dimension(NAO, NAO) :: tau34
    complex(kind=pr) , dimension(NAO, NAO) :: tau35
    complex(kind=pr) , dimension(NAO, NAO) :: tau36
    complex(kind=pr) , dimension(NAO, NAO) :: tau37
    complex(kind=pr) , dimension(NAO) :: tau38
    complex(kind=pr) , dimension(NAO) :: tau39
    complex(kind=pr) , dimension(NAO) :: tau40
    complex(kind=pr) , dimension(NAO) :: tau41
    complex(kind=pr) , dimension(NAO, NAO) :: tau42
    complex(kind=pr) , dimension(NAO) :: tau43
    complex(kind=pr) , dimension(NAO) :: tau44
    complex(kind=pr) , dimension(NAO) :: tau45
    complex(kind=pr) , dimension(NAO) :: tau46
    complex(kind=pr) , dimension(NAO, NAO) :: tau47
    complex(kind=pr) , dimension(NAO) :: tau48
    complex(kind=pr) , dimension(NAO) :: tau49
    complex(kind=pr) , dimension(NAO) :: tau50
    complex(kind=pr) , dimension(NAO) :: tau51
    complex(kind=pr) , dimension(NAO) :: tau52
  


    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau0(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = tau1(p, q) - ( &
                u(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = tau1(p, q) + ( &
                2 * t1(p) * u(p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau2(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p) = tau2(p) + ( &
                t2(q, p) * tau1(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau3(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau3(p) = tau3(p) + ( &
            t1(p) * u(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau3(p) = tau3(p) - ( &
            tau2(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) + ( &
                v(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) + ( &
                tau0(p) * u(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) - ( &
                tau0(p) * v(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau4(p, q) = tau4(p, q) + ( &
                tau3(p) * v(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau5(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau5(p, q) = tau5(p, q) + ( &
                    t2(r, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau6(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau6(p, q) = tau6(p, q) + ( &
                z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau6(p, q) = tau6(p, q) - ( &
                z2(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau6(p, q) = tau6(p, q) + ( &
                tau5(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau7(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau7(p) = tau7(p) + ( &
            u(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau7(p) = tau7(p) - ( &
            v(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau8(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau8(p, q, r) = tau8(p, q, r) + ( &
                    u(p) * u(q) * v(p) * tau6(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau8(p, q, r) = tau8(p, q, r) - ( &
                    t1(q) * tau7(p) * u(q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau9(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau9(p) = tau9(p) + ( &
            u(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau9(p) = tau9(p) - ( &
            u(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau10(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau10(p, q, r) = tau10(p, q, r) + ( &
                    t1(r) * tau7(r) * t2(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau10(p, q, r) = tau10(p, q, r) + ( &
                    tau9(r) * v(r) * t2(p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = tau11(p, q) + ( &
                2 * t1(p) * u(q) * v(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = tau11(p, q) - ( &
                t1(p)**2 * u(q)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12(p, q, r) = tau12(p, q, r) - ( &
                    z2(q, r) * v(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12(p, q, r) = tau12(p, q, r) + ( &
                    tau5(r, q) * u(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12(p, q, r) = tau12(p, q, r) + ( &
                    tau11(r, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) + ( &
                t1(p)**2 * v(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = tau13(p, q) + ( &
                2 * t1(p) * u(q) * v(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau14(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau14(p, q, r) = tau14(p, q, r) + ( &
                    z2(q, r) * u(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau14(p, q, r) = tau14(p, q, r) - ( &
                    tau5(r, q) * v(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau14(p, q, r) = tau14(p, q, r) + ( &
                    tau13(r, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau15(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau15(p, q, r) = tau15(p, q, r) + ( &
                    z2(q, r) * v(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau15(p, q, r) = tau15(p, q, r) + ( &
                    z2(q, r) * t1(r)**2 * u(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau16(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau16(p) = tau16(p) + ( &
            t1(p)**2 * u(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau16(p) = tau16(p) + ( &
            2 * t1(p) * u(p) * v(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau17(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau17(p) = tau17(p) - ( &
            u(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau17(p) = tau17(p) + ( &
            2 * t1(p) * u(p) * v(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p) = tau18(p) + ( &
                z1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) - ( &
                t1(p) * tau18(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) + ( &
                2 * t1(p) * t1(q) * tau0(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) + ( &
                2 * t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) + ( &
                t1(p)**2 * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) + ( &
                4 * t1(p) * t1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau21(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau21(p) = tau21(p) + ( &
            t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau21(p) = tau21(p) + ( &
            tau0(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = tau22(p) + ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = tau22(p) - ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) - ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) - ( &
                z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                t1(p) * z1(q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                tau19(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                tau19(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) - ( &
                tau20(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                2 * tau21(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                2 * tau21(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) - ( &
                t1(q) * tau22(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = tau24(p, q) - ( &
                2 * u(p) * u(q) * v(p) * v(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = tau24(p, q) + ( &
                u(p)**2 * v(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = tau24(p, q) + ( &
                u(q)**2 * v(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p, q) = tau25(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p, q) = tau25(p, q) + ( &
                t1(p) * t1(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau26(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau26(p, q) = tau26(p, q) + ( &
                4 * u(p) * u(q) * v(p) * v(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau26(p, q) = tau26(p, q) + ( &
                tau7(p) * tau7(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau27(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau27(p, q) = tau27(p, q) + ( &
                2 * t1(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau27(p, q) = tau27(p, q) - ( &
                t1(p) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau28(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau28(p, q) = tau28(p, q) + ( &
                tau27(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau28(p, q) = tau28(p, q) - ( &
                tau27(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau29(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau29(p, q) = tau29(p, q) + ( &
                t1(p) * u(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau29(p, q) = tau29(p, q) - ( &
                u(q) * tau28(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                u(p) * v(q) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                u(p) * v(q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31(p, q) = tau31(p, q) + ( &
                u(p)**2 * u(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31(p, q) = tau31(p, q) + ( &
                v(p)**2 * v(q)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) - ( &
                2 * tau25(q, p) * tau26(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) - ( &
                2 * tau7(p) * v(q) * tau29(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) - ( &
                2 * tau7(q) * v(p) * tau29(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) + ( &
                2 * u(p) * v(q) * tau30(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) + ( &
                tau31(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) + ( &
                tau31(q, p) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) + ( &
                2 * t1(p) * tau7(q) * u(p) * v(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p, q) = tau32(p, q) + ( &
                2 * t1(q) * tau7(p) * u(q) * v(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau33(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau33(p, q) = tau33(p, q) - ( &
                t1(p) * tau0(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau33(p, q) = tau33(p, q) + ( &
                t1(q) * tau0(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau33(p, q) = tau33(p, q) + ( &
                z1(p) * tau25(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau33(p, q) = tau33(p, q) - ( &
                z1(q) * tau25(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p, q) = tau34(p, q) - ( &
                tau7(q) * u(p) * v(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p, q) = tau34(p, q) + ( &
                tau7(p) * u(q) * v(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = tau35(p, q) + ( &
                t1(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p, q) = tau35(p, q) + ( &
                t1(q) * z1(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) - ( &
                u(p) * u(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) - ( &
                u(p) * u(q) * tau35(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) + ( &
                2 * tau21(p) * u(p) * u(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) + ( &
                2 * tau21(q) * u(p) * u(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p, q) = tau37(p, q) - ( &
                2 * tau0(q) * tau7(q) * u(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p, q) = tau37(p, q) + ( &
                tau7(q) * u(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau38(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau38(p) = tau38(p) + ( &
            z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau38(p) = tau38(p) - ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau38(p) = tau38(p) + ( &
            tau18(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau39(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau39(p) = tau39(p) + ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau39(p) = tau39(p) + ( &
            z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau39(p) = tau39(p) + ( &
            tau18(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau40(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau40(p) = tau40(p) + ( &
            u(p) * v(p)**3 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau40(p) = tau40(p) + ( &
            v(p) * u(p)**3 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau41(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau41(p) = tau41(p) + ( &
            v(p)**4 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau41(p) = tau41(p) - ( &
            u(p)**4 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau41(p) = tau41(p) + ( &
            2 * t1(p) * tau40(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau42(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau42(p, q) = tau42(p, q) + ( &
                tau41(p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau42(p, q) = tau42(p, q) - ( &
                tau40(p) * z1(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau43(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43(p) = tau43(p) + ( &
            u(p)**4 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43(p) = tau43(p) - ( &
            v(p)**4 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau44(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau44(p) = tau44(p) + ( &
            u(p) * v(p)**3 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau44(p) = tau44(p) + ( &
            v(p) * u(p)**3 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau44(p) = tau44(p) + ( &
            tau43(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau45(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau45(p) = tau45(p) + ( &
            z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau45(p) = tau45(p) - ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = tau46(p) + ( &
            v(p)**4 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = tau46(p) + ( &
            u(p)**2 * v(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau46(p) = tau46(p) - ( &
                t2(q, p) * tau42(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = tau46(p) + ( &
            t1(p) * tau44(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = tau46(p) + ( &
            tau40(p) * tau45(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau47(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau47(p, q) = tau47(p, q) - ( &
                v(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau47(p, q) = tau47(p, q) + ( &
                2 * t1(p) * v(p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau48(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau48(p) = tau48(p) + ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau48(p) = tau48(p) + ( &
            z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau48(p) = tau48(p) - ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau49(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau49(p) = tau49(p) - ( &
                t2(q, p) * tau47(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau49(p) = tau49(p) + ( &
            tau48(p) * v(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau50(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau50(p) = tau50(p) - ( &
            tau2(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau50(p) = tau50(p) + ( &
            tau48(p) * u(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau51(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau51(p) = tau51(p) + ( &
            v(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau51(p) = tau51(p) - ( &
            v(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau52(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau52(p) = tau52(p) + ( &
            tau51(p) * u(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau52(p) = tau52(p) + ( &
            t1(p) * tau7(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                1.0_pr / 4.0_pr &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                v(p)**2 * v(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau4(p, q) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau4(q, p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    v(p) * t2(q, r) * tau8(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    u(p) * v(p) * z2(q, r) * tau10(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) - ( &
                    t2(q, r) * tau12(q, r, p) * v(p)**2 / 2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    t2(q, r) * tau14(q, r, p) * u(p)**2 / 2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    t2(p, r) * tau15(p, r, q) * v(q)**2 / 2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    tau16(q) * t2(p, r) * z2(q, r) * v(p)**2 / 2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) - ( &
                    tau17(q) * t2(p, r) * z2(q, r) * u(p)**2 / 2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau23(q, p) * tau24(q, p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau32(q, p) * z2(q, p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau33(q, p) * tau34(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                v(p) * v(q) * tau36(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau31(q, p) * tau35(q, p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                t1(q) * v(p) * tau37(p, q) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau38(q) * tau7(q) * u(p) * v(p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau7(p) * u(q) * v(q) * z1(p) * t1(p)**2 / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                t1(p) * tau0(p) * tau7(p) * u(q) * v(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau39(p) * tau7(p) * u(q) * v(q) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                deltaf(p, q) * tau46(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau49(p) * u(p) * v(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau50(q) * v(q) * v(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau21(p) * tau7(p) * v(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau21(q) * tau7(q) * v(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau52(p) * z1(p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau52(q) * z1(q) / 2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel
    do p = 1, NAO
	SpSq(p,p) = 0.75_pr
    do q = p+1, NAO
       tmp = 0.5_pr * (SpSq(p,q) + (SpSq(q,p)))
       SpSq(p,q) = tmp
       SpSq(q,p) = (tmp)
    end do
    end do
End Subroutine CCSD_SpSq

pure double precision function deltaf(p, q)
   implicit none
   integer, intent(in) :: p, q

   if (p == q) then
    deltaf = 1.0d0
   else
    deltaf = 0.0d0
   end if

   end function deltaf

End Module CCSDSpSq
