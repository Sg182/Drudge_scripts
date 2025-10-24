    Module CCRes 
    Use Precision
    Use Constants

    Contains


    Subroutine CCSD_SFS(Ene,Res1,Res2,T1,T2,NAO,  &
        H001,H100,H010,H101,H020,H200,H002,H110,H011, &
        H030,H111,H120,H210,H021,H012,H201,H102,H003,H300)   !Don't forget to add H000 with the energy
    Implicit None
    Integer,           Intent(In)    :: NAO 
    Complex (Kind=pr), Intent(In)    :: T1(NAO)
    Complex (Kind=pr), Intent(In)    :: T2(NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: H001(NAO), H100(NAO), H010(NAO)
    Complex(Kind=pr), Intent(out)  :: H101(NAO,NAO), H200(NAO,NAO), H002(NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: H110(NAO,NAO), H011(NAO,NAO),H020(NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: H030(NAO,NAO,NAO), H111(NAO,NAO,NAO), H120(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: H210(NAO,NAO,NAO), H021(NAO,NAO,NAO), H012(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: H003(NAO,NAO,NAO), H300(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: H201(NAO,NAO,NAO), H102(NAO,NAO,NAO)

    Complex (Kind=pr), Intent(Out)   :: Ene, Res1(NAO), Res2(NAO,NAO)
    Integer                          :: p, q, r, s, i, j, k, l
    
    complex (kind=pr) , dimension(:), allocatable :: tau0

    complex (kind=pr) , dimension(:), allocatable :: tau1

    complex (kind=pr) , dimension(:), allocatable :: tau2

    complex (kind=pr) , dimension(:, :), allocatable :: tau3

    complex (kind=pr) , dimension(:), allocatable :: tau4

    complex (kind=pr) , dimension(:), allocatable :: tau5

    complex (kind=pr) , dimension(:, :), allocatable :: tau6

    complex (kind=pr) , dimension(:, :), allocatable :: tau7

    complex (kind=pr) , dimension(:, :), allocatable :: tau8

    complex (kind=pr) , dimension(:, :), allocatable :: tau9

    complex (kind=pr) , dimension(:), allocatable :: tau10

    complex (kind=pr) , dimension(:, :), allocatable :: tau11

    complex (kind=pr) , dimension(:, :), allocatable :: tau12

    complex (kind=pr) , dimension(:), allocatable :: tau13

    complex (kind=pr) , dimension(:, :), allocatable :: tau14

    complex (kind=pr) , dimension(:), allocatable :: tau15

    complex (kind=pr) , dimension(:), allocatable :: tau16

    complex (kind=pr) , dimension(:), allocatable :: tau17

    complex (kind=pr) , dimension(:, :), allocatable :: tau18

    complex (kind=pr) , dimension(:), allocatable :: tau19

    complex (kind=pr) , dimension(:), allocatable :: tau20

    complex (kind=pr) , dimension(:), allocatable :: tau21

    complex (kind=pr) , dimension(:, :), allocatable :: tau22

    complex (kind=pr) , dimension(:, :), allocatable :: tau23

    complex (kind=pr) , dimension(:, :), allocatable :: tau24

    complex (kind=pr) , dimension(:, :), allocatable :: tau25

    complex (kind=pr) , dimension(:, :), allocatable :: tau26

    complex (kind=pr) , dimension(:, :), allocatable :: tau27

    complex (kind=pr) , dimension(:, :), allocatable :: tau28

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau29

    complex (kind=pr) , dimension(:, :), allocatable :: tau30

    complex (kind=pr) , dimension(:, :), allocatable :: tau31

    complex (kind=pr) , dimension(:, :), allocatable :: tau32

    complex (kind=pr) , dimension(:, :), allocatable :: tau33

    complex (kind=pr) , dimension(:), allocatable :: tau34

    complex (kind=pr) , dimension(:), allocatable :: tau35

    complex (kind=pr) , dimension(:, :), allocatable :: tau36

    complex (kind=pr) , dimension(:, :), allocatable :: tau37

    complex (kind=pr) , dimension(:, :), allocatable :: tau38

    complex (kind=pr) , dimension(:, :), allocatable :: tau39

    complex (kind=pr) , dimension(:, :), allocatable :: tau40

    complex (kind=pr) , dimension(:, :), allocatable :: tau41

    complex (kind=pr) , dimension(:, :), allocatable :: tau42

    complex (kind=pr) , dimension(:, :), allocatable :: tau43

    complex (kind=pr) , dimension(:, :), allocatable :: tau44

    complex (kind=pr) , dimension(:, :), allocatable :: tau45

    complex (kind=pr) , dimension(:, :), allocatable :: tau46

    complex (kind=pr) , dimension(:, :), allocatable :: tau47

    complex (kind=pr) , dimension(:, :), allocatable :: tau48

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau49

    complex (kind=pr) , dimension(:, :), allocatable :: tau50

    complex (kind=pr) , dimension(:, :), allocatable :: tau51

    complex (kind=pr) , dimension(:), allocatable :: tau52

    complex (kind=pr) , dimension(:), allocatable :: tau53

    complex (kind=pr) , dimension(:), allocatable :: tau54

    complex (kind=pr) , dimension(:), allocatable :: tau55

    complex (kind=pr) , dimension(:, :), allocatable :: tau56

    complex (kind=pr) , dimension(:, :), allocatable :: tau57

    complex (kind=pr) , dimension(:, :), allocatable :: tau58

    complex (kind=pr) , dimension(:, :), allocatable :: tau59

    complex (kind=pr) , dimension(:, :), allocatable :: tau60

    complex (kind=pr) , dimension(:, :), allocatable :: tau61

    complex (kind=pr) , dimension(:, :), allocatable :: tau62

    complex (kind=pr) , dimension(:, :), allocatable :: tau63

    complex (kind=pr) , dimension(:, :), allocatable :: tau64

    complex (kind=pr) , dimension(:, :), allocatable :: tau65

    !$omp parallel default(shared)

    allocate(tau0(NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                t1(q) * H002(p, q)&
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
            tau0(p)&
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
            2 * tau0(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau0)

    allocate(tau1(NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p) = tau1(p) + ( &
                t2(q, p) * H003(p, p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) - ( &
            6 * tau1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau20(NAO))
    !$omp single
    tau20 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) - ( &
            18 * tau1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            6 * tau1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau53(NAO))
    !$omp single
    tau53 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau53(p) = tau53(p) - ( &
            18 * tau1(p)&
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
                t1(q)**2 * H003(q, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) - ( &
            3 * tau2(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) - ( &
            3 * tau2(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            3 * tau2(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau53(p) = tau53(p) - ( &
            3 * tau2(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau2)

    allocate(tau3(NAO, NAO))
    !$omp single
    tau3 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau3(p, q) = tau3(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau3(p, q) = tau3(p, q) + ( &
                3 * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau4(p) = tau4(p) + ( &
                    tau3(q, r) * H003(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau3)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    Ene = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:Ene)
    
    do p=1, NAO
        Ene = Ene + ( &
            t1(p) * tau4(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau4)

    allocate(tau5(NAO))
    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau5(p) = tau5(p) + ( &
                t1(q) * H003(p, p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) - ( &
            6 * t1(p) * tau5(p)&
        )
    
    end do
    !$omp end do

    allocate(tau54(NAO))
    !$omp single
    tau54 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau54(p) = tau54(p) + ( &
            3 * tau5(p)&
        )
    
    end do
    !$omp end do

    allocate(tau63(NAO, NAO))
    !$omp single
    tau63 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau63(p, q) = tau63(p, q) - ( &
                6 * tau5(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    Res1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            6 * t1(p)**3 * tau5(p)&
        )
    
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
                    t1(r) * H012(p, q, r)&
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
                2 * tau6(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau25(NAO, NAO))
    !$omp single
    tau25 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau25(p, q) = tau25(p, q) + ( &
                2 * tau6(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau44(NAO, NAO))
    !$omp single
    tau44 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) - ( &
                2 * t1(q) * tau6(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau60(NAO, NAO))
    !$omp single
    tau60 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) - ( &
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
                    t2(p, r) * H003(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                3 * tau7(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau25(p, q) = tau25(p, q) - ( &
                6 * tau7(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau34(NAO))
    !$omp single
    tau34 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p) = tau34(p) + ( &
                t1(q) * tau7(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau35(NAO))
    !$omp single
    tau35 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau35(p) = tau35(p) - ( &
            6 * tau34(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau34)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) + ( &
                6 * t1(p) * tau7(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau60(p, q) = tau60(p, q) + ( &
                3 * tau7(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau7)

    !$omp single
    Res2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t1(q)**2 * t1(p) * tau60(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t1(p)**2 * t1(q) * tau60(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau60)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau8(p, q) = tau8(p, q) - ( &
                2 * t1(q) * H012(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                2 * t2(q, p) * tau8(p, q)&
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
                t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau13(NAO))
    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau13(p) = tau13(p) + ( &
                    tau9(q, r) * H012(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau16(NAO))
    !$omp single
    tau16 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            tau13(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau35(p) = tau35(p) + ( &
            tau13(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau13)

    allocate(tau41(NAO, NAO))
    !$omp single
    tau41 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                2 * tau35(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau35)

    allocate(tau17(NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau17(p) = tau17(p) + ( &
                    tau9(q, r) * H003(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            3 * tau17(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            3 * tau17(p)&
        )
    
    end do
    !$omp end do

    allocate(tau27(NAO, NAO))
    !$omp single
    tau27 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau27(p, q) = tau27(p, q) - ( &
                3 * tau17(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau17)

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    tau9(q, r) * H102(p, q, r)&
                )
            end do
        end do
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
                t1(q)**2 * H012(p, q, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) - ( &
            tau10(p)&
        )
    
    end do
    !$omp end do

    allocate(tau55(NAO))
    !$omp single
    tau55 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) - ( &
            tau10(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau10)

    allocate(tau11(NAO, NAO))
    !$omp single
    tau11 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau11(p, q) = tau11(p, q) + ( &
                    t1(r) * H003(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau12(NAO, NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) + ( &
                3 * tau11(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau45(NAO, NAO))
    !$omp single
    tau45 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau45(p, q) = tau45(p, q) + ( &
                3 * tau11(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau62(NAO, NAO))
    !$omp single
    tau62 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                3 * tau11(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau11)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau12(p, q) = tau12(p, q) - ( &
                3 * t1(p) * H003(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p) = tau16(p) - ( &
                2 * t2(q, p) * tau12(q, p)&
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
            do r=1, NAO
                tau33(p, q) = tau33(p, q) + ( &
                    t2(r, p) * tau12(r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau12)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) - ( &
                2 * t1(p)**2 * tau33(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau33)

    allocate(tau14(NAO, NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau15(NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15(p) = tau15(p) + ( &
                t1(q) * tau14(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau14)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            tau15(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) + ( &
            tau15(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau15)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            H010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            2 * H020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau16(p) = tau16(p) + ( &
            4 * H030(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            2 * t1(p) * tau16(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau16)

    allocate(tau18(NAO, NAO))
    !$omp single
    tau18 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau18(p, q) = tau18(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau18(p, q) = tau18(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau19(NAO))
    !$omp single
    tau19 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p) = tau19(p) + ( &
                t1(q) * tau18(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau18)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            2 * tau19(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau53(p) = tau53(p) + ( &
            2 * tau19(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau19)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            2 * H011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            4 * H021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) - ( &
            t1(p)**2 * tau20(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau20)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                tau21(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau21)

    allocate(tau22(NAO, NAO))
    !$omp single
    tau22 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau22(p, q) = tau22(p, q) + ( &
                    t1(r) * H201(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                tau22(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau22)

    allocate(tau23(NAO, NAO))
    !$omp single
    tau23 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau23(p, q) = tau23(p, q) + ( &
                    t2(q, r) * H003(p, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                6 * t1(p)**3 * tau23(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau23)

    allocate(tau24(NAO, NAO))
    !$omp single
    tau24 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau24(p, q) = tau24(p, q) + ( &
                    t2(p, r) * H021(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau27(p, q) = tau27(p, q) + ( &
                4 * tau24(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau24)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau25(p, q) = tau25(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau25(p, q) = tau25(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau25(p, q) = tau25(p, q) + ( &
                6 * t2(p, q) * H003(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau25(p, q) = tau25(p, q) - ( &
                2 * t1(q) * H012(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau26(NAO, NAO))
    !$omp single
    tau26 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau26(p, q) = tau26(p, q) + ( &
                    t2(r, p) * tau25(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau25)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau27(p, q) = tau27(p, q) + ( &
                tau26(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau26)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                2 * t1(p) * tau27(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau27)

    allocate(tau28(NAO, NAO))
    !$omp single
    tau28 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau28(p, q) = tau28(p, q) + ( &
                    t2(p, r) * H012(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau31(NAO, NAO))
    !$omp single
    tau31 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) - ( &
                2 * tau28(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau40(NAO, NAO))
    !$omp single
    tau40 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau40(p, q) = tau40(p, q) + ( &
                2 * tau28(p, q)&
            )
    
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
    
                tau29(p, q, r) = tau29(p, q, r) - ( &
                    H102(p, r, q)&
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
                    3 * t2(p, q) * H003(q, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau30(NAO, NAO))
    !$omp single
    tau30 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau30(p, q) = tau30(p, q) + ( &
                    t1(r) * tau29(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau29)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) + ( &
                tau30(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau30)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau31(p, q) = tau31(p, q) + ( &
                t1(q) * H102(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau32(NAO, NAO))
    !$omp single
    tau32 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau32(p, q) = tau32(p, q) + ( &
                    t2(r, p) * tau31(q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau31)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) - ( &
                2 * tau32(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau32)

    allocate(tau36(NAO, NAO))
    !$omp single
    tau36 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau36(p, q) = tau36(p, q) + ( &
                    t2(q, r) * H102(p, q, r)&
                )
            end do
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
    
            tau38(p, q) = tau38(p, q) + ( &
                2 * tau36(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau36)

    allocate(tau37(NAO, NAO))
    !$omp single
    tau37 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau37(p, q) = tau37(p, q) + ( &
                    t1(r) * H111(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau38(p, q) = tau38(p, q) - ( &
                tau37(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau37)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) - ( &
                2 * t1(q) * tau38(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau38)

    allocate(tau39(NAO, NAO))
    !$omp single
    tau39 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau39(p, q) = tau39(p, q) + ( &
                    t1(r) * H102(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau40(p, q) = tau40(p, q) + ( &
                tau39(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau39)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) - ( &
                2 * t1(q)**2 * tau40(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau40)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                2 * t1(q)**3 * H102(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                tau41(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                tau41(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau41)

    allocate(tau42(NAO, NAO))
    !$omp single
    tau42 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau42(p, q) = tau42(p, q) + ( &
                    t1(r) * H021(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau47(NAO, NAO))
    !$omp single
    tau47 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                2 * tau42(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau56(NAO, NAO))
    !$omp single
    tau56 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) + ( &
                tau42(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau42)

    allocate(tau43(NAO, NAO))
    !$omp single
    tau43 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau43(p, q) = tau43(p, q) + ( &
                    t2(q, r) * H012(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) - ( &
                2 * tau43(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau59(NAO, NAO))
    !$omp single
    tau59 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau59(p, q) = tau59(p, q) + ( &
                tau43(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau59(p, q) = tau59(p, q) + ( &
                tau43(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau43)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                8 * t1(p) * t1(q) * tau59(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau59)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) + ( &
                6 * H030(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) + ( &
                3 * t1(q)**2 * H012(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                tau44(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                tau44(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau44)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau45(p, q) = tau45(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau45(p, q) = tau45(p, q) + ( &
                2 * H012(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau45(p, q) = tau45(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                2 * t1(p) * t1(q) * tau45(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau61(NAO, NAO))
    !$omp single
    tau61 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) - ( &
                2 * t1(p)**2 * tau45(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t2(q, p)**2 * tau45(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau45)

    allocate(tau46(NAO, NAO))
    !$omp single
    tau46 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau46(p, q) = tau46(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau46(p, q) = tau46(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau46(p, q) = tau46(p, q) + ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau46(p, q) = tau46(p, q) + ( &
                9 * t1(p)**2 * H003(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) - ( &
                t1(q) * tau46(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) - ( &
                t1(p) * tau46(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau46)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                2 * H020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t2(q, p) * tau47(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau47)

    allocate(tau48(NAO, NAO))
    !$omp single
    tau48 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau48(p, q) = tau48(p, q) + ( &
                6 * t2(q, p)**2 * t1(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau48(p, q) = tau48(p, q) + ( &
                t1(p)**3 * t1(q)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                6 * tau48(q, p) * H003(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                6 * tau48(p, q) * H003(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau48)

    allocate(tau49(NAO, NAO, NAO))
    !$omp single
    tau49 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau49(p, q, r) = tau49(p, q, r) - ( &
                    H111(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau49(p, q, r) = tau49(p, q, r) + ( &
                    2 * t2(p, r) * H012(q, r, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau50(NAO, NAO))
    !$omp single
    tau50 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau50(p, q) = tau50(p, q) + ( &
                    t2(r, p) * tau49(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau49)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau50(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau50(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau50)

    allocate(tau51(NAO, NAO))
    !$omp single
    tau51 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau51(p, q) = tau51(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau51(p, q) = tau51(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau51(p, q) = tau51(p, q) - ( &
                3 * t1(p) * H003(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau52(NAO))
    !$omp single
    tau52 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52(p) = tau52(p) + ( &
                t2(q, p) * tau51(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau51)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) - ( &
            2 * tau52(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau52)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau53(p) = tau53(p) + ( &
            H001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau53(p) = tau53(p) + ( &
            2 * H011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau53(p) = tau53(p) + ( &
            4 * H021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) - ( &
            t1(p) * tau53(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau53)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau54(p) = tau54(p) + ( &
            2 * H012(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) + ( &
            3 * t1(p)**2 * tau54(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau54)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) + ( &
            H010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) + ( &
            2 * H020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau55(p) = tau55(p) + ( &
            4 * H030(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau55(p) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau55(q) * t2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau55)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) + ( &
                H020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) + ( &
                3 * H030(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau56(p, q) = tau56(p, q) + ( &
                3 * H030(q, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau58(NAO, NAO))
    !$omp single
    tau58 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) + ( &
                4 * t1(p) * tau56(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau56)

    allocate(tau57(NAO, NAO))
    !$omp single
    tau57 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) - ( &
                t1(p)**2 * tau57(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) + ( &
                2 * t1(p) * tau57(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau57)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) + ( &
                H110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) + ( &
                2 * H120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau58(p, q) = tau58(p, q) + ( &
                2 * t1(p)**3 * H012(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(p) * tau58(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau58)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) + ( &
                H101(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau61(p, q) = tau61(p, q) + ( &
                2 * H111(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                t1(p)**2 * tau61(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau61)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau63(p, q) = tau63(p, q) + ( &
                    2 * t2(r, q) * tau62(r, p)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau62)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau63(p, q) = tau63(p, q) + ( &
                H101(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    t2(r, q) * tau63(r, p)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau63)

    allocate(tau64(NAO, NAO))
    !$omp single
    tau64 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                H110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                2 * H120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                2 * t1(p)**3 * H012(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(q) * tau64(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau64)

    allocate(tau65(NAO, NAO))
    !$omp single
    tau65 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                H101(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                2 * H111(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                t1(q)**2 * tau65(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau65)

    !$omp do schedule(static) reduction(+:Ene)
    
    do q=1, NAO
        do p=1, NAO
            Ene = Ene + ( &
                H002(q, p) * t2(q, p)&
            )
        end do
    end do
    
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                t1(q) * H101(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) - ( &
                t1(q)**2 * H102(p, q, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                6 * t2(p, q)**2 * H003(q, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            4 * t1(p)**3 * H012(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            H100(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    H101(q, r) * t2(p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * H200(q, p)&
            )
    
        end do
    end do
    !$omp end do

    Call Symm2(Res2,NAO)
    Return
    End Subroutine CCSD_SFS


    Subroutine Symm2(T2,NAO)
      Implicit None
      Integer,           Intent(In)    :: NAO 
      Complex (Kind=pr), Intent(InOut) :: T2(NAO,NAO)
      Complex (Kind=pr), Allocatable   :: X2(:,:)
      Integer   :: I, J
      Integer   :: IAlloc
! Allocate necessary space
      Allocate(X2(NAO,NAO), Stat=IAlloc)
! Symmetrize
      Do I = 1, NAO
      Do J = 1, NAO 
        X2(I,J) = T2(I,J) + T2(J,I)
        If(I.eq.J) X2(I,J) = Zero
      EndDo
      EndDo
      T2 = X2
! Deallocate
      Deallocate(X2, Stat=IAlloc)
      Return
      End Subroutine Symm2

    End Module
    !$omp end parallel
