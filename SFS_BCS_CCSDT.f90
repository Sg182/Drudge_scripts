Module CCRes_SFS 
    Use Precision
    Use Constants

    Contains


    Subroutine CCSDT_SFS(Ene,Res1,Res2,Res3,T1,T2,T3,NAO,  &
        H001,H100,H010,H101,H020,H200,H002,H110,H011, &
        H030,H111,H120,H210,H021,H012,H201,H102,H003,H300)   !Don't forget to add H000 with the energy
    Implicit None
    Integer,           Intent(In)    :: NAO 
    Complex (Kind=pr), Intent(In)    :: T1(NAO)
    Complex (Kind=pr), Intent(In)    :: T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: T3(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H001(NAO), H100(NAO), H010(NAO)
    Complex(Kind=pr), Intent(In)  :: H101(NAO,NAO), H200(NAO,NAO), H002(NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H110(NAO,NAO), H011(NAO,NAO),H020(NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H030(NAO,NAO,NAO), H111(NAO,NAO,NAO), H120(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H210(NAO,NAO,NAO), H021(NAO,NAO,NAO), H012(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H003(NAO,NAO,NAO), H300(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(In)  :: H201(NAO,NAO,NAO), H102(NAO,NAO,NAO)

    Complex (Kind=pr), Intent(Out)   :: Ene, Res1(NAO), Res2(NAO,NAO), Res3(NAO,NAO,NAO)
    Integer                          :: p, q, r, s, i, j, k, l
    
    complex(kind=pr) , dimension(:), allocatable :: tau0

    complex(kind=pr) , dimension(:, :), allocatable :: tau1

    complex(kind=pr) , dimension(:), allocatable :: tau2

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau3

    complex(kind=pr) , dimension(:, :), allocatable :: tau4

    complex(kind=pr) , dimension(:, :), allocatable :: tau5

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau6

    complex(kind=pr) , dimension(:, :), allocatable :: tau7

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau8

    complex(kind=pr) , dimension(:, :), allocatable :: tau9

    complex(kind=pr) , dimension(:), allocatable :: tau10

    complex(kind=pr) , dimension(:), allocatable :: tau11

    complex(kind=pr) , dimension(:), allocatable :: tau12

    complex(kind=pr) , dimension(:), allocatable :: tau13

    complex(kind=pr) , dimension(:, :), allocatable :: tau14

    complex(kind=pr) , dimension(:), allocatable :: tau15

    complex(kind=pr) , dimension(:, :), allocatable :: tau16

    complex(kind=pr) , dimension(:), allocatable :: tau17

    complex(kind=pr) , dimension(:), allocatable :: tau18

    complex(kind=pr) , dimension(:), allocatable :: tau19

    complex(kind=pr) , dimension(:), allocatable :: tau20

    complex(kind=pr) , dimension(:, :), allocatable :: tau21

    complex(kind=pr) , dimension(:), allocatable :: tau22

    complex(kind=pr) , dimension(:), allocatable :: tau23

    complex(kind=pr) , dimension(:), allocatable :: tau24

    complex(kind=pr) , dimension(:), allocatable :: tau25

    complex(kind=pr) , dimension(:), allocatable :: tau26

    complex(kind=pr) , dimension(:, :), allocatable :: tau27

    complex(kind=pr) , dimension(:, :), allocatable :: tau28

    complex(kind=pr) , dimension(:, :), allocatable :: tau29

    complex(kind=pr) , dimension(:, :), allocatable :: tau30

    complex(kind=pr) , dimension(:, :), allocatable :: tau31

    complex(kind=pr) , dimension(:, :), allocatable :: tau32

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau33

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau34

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau35

    complex(kind=pr) , dimension(:, :), allocatable :: tau36

    complex(kind=pr) , dimension(:, :), allocatable :: tau37

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau38

    complex(kind=pr) , dimension(:, :), allocatable :: tau39

    complex(kind=pr) , dimension(:, :), allocatable :: tau40

    complex(kind=pr) , dimension(:, :), allocatable :: tau41

    complex(kind=pr) , dimension(:, :), allocatable :: tau42

    complex(kind=pr) , dimension(:, :), allocatable :: tau43

    complex(kind=pr) , dimension(:, :), allocatable :: tau44

    complex(kind=pr) , dimension(:, :), allocatable :: tau45

    complex(kind=pr) , dimension(:, :), allocatable :: tau46

    complex(kind=pr) , dimension(:, :), allocatable :: tau47

    complex(kind=pr) , dimension(:, :), allocatable :: tau48

    complex(kind=pr) , dimension(:, :), allocatable :: tau49

    complex(kind=pr) , dimension(:, :), allocatable :: tau50

    complex(kind=pr) , dimension(:, :), allocatable :: tau51

    complex(kind=pr) , dimension(:, :), allocatable :: tau52

    complex(kind=pr) , dimension(:, :), allocatable :: tau53

    complex(kind=pr) , dimension(:, :), allocatable :: tau54

    complex(kind=pr) , dimension(:, :), allocatable :: tau55

    complex(kind=pr) , dimension(:, :), allocatable :: tau56

    complex(kind=pr) , dimension(:, :), allocatable :: tau57

    complex(kind=pr) , dimension(:), allocatable :: tau58

    complex(kind=pr) , dimension(:), allocatable :: tau59

    complex(kind=pr) , dimension(:, :), allocatable :: tau60

    complex(kind=pr) , dimension(:, :), allocatable :: tau61

    complex(kind=pr) , dimension(:, :), allocatable :: tau62

    complex(kind=pr) , dimension(:, :), allocatable :: tau63

    complex(kind=pr) , dimension(:, :), allocatable :: tau64

    complex(kind=pr) , dimension(:, :), allocatable :: tau65

    complex(kind=pr) , dimension(:, :), allocatable :: tau66

    complex(kind=pr) , dimension(:, :), allocatable :: tau67

    complex(kind=pr) , dimension(:, :), allocatable :: tau68

    complex(kind=pr) , dimension(:, :), allocatable :: tau69

    complex(kind=pr) , dimension(:, :), allocatable :: tau70

    complex(kind=pr) , dimension(:, :), allocatable :: tau71

    complex(kind=pr) , dimension(:, :), allocatable :: tau72

    complex(kind=pr) , dimension(:, :), allocatable :: tau73

    complex(kind=pr) , dimension(:, :), allocatable :: tau74

    complex(kind=pr) , dimension(:, :), allocatable :: tau75

    complex(kind=pr) , dimension(:, :), allocatable :: tau76

    complex(kind=pr) , dimension(:, :), allocatable :: tau77

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau78

    complex(kind=pr) , dimension(:, :), allocatable :: tau79

    complex(kind=pr) , dimension(:), allocatable :: tau80

    complex(kind=pr) , dimension(:), allocatable :: tau81

    complex(kind=pr) , dimension(:), allocatable :: tau82

    complex(kind=pr) , dimension(:, :), allocatable :: tau83

    complex(kind=pr) , dimension(:, :), allocatable :: tau84

    complex(kind=pr) , dimension(:, :), allocatable :: tau85

    complex(kind=pr) , dimension(:, :), allocatable :: tau86

    complex(kind=pr) , dimension(:, :), allocatable :: tau87

    complex(kind=pr) , dimension(:, :), allocatable :: tau88

    complex(kind=pr) , dimension(:, :), allocatable :: tau89

    complex(kind=pr) , dimension(:, :), allocatable :: tau90

    complex(kind=pr) , dimension(:, :), allocatable :: tau91

!!$omp parallel default(shared)

    allocate(tau0(NAO))
!!$omp single
    tau0 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                t1(q) * H002(p, q)&
            )
        end do
    end do
!!$omp end do

    allocate(tau2(NAO))
!!$omp single
    tau2 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau2(p) = tau2(p) + ( &
            tau0(p)&
        )
    
    end do
!!$omp end do

    allocate(tau24(NAO))
!!$omp single
    tau24 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau24(p) = tau24(p) + ( &
            2 * tau0(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau0)

    allocate(tau1(NAO, NAO))
!!$omp single
    tau1 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau1(p, q) = tau1(p, q) + ( &
                3 * t2(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau2(p) = tau2(p) + ( &
                    tau1(q, r) * H003(q, p, r)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau1)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau2(p) = tau2(p) + ( &
            H001(p)&
        )
    
    end do
!!$omp end do

!!$omp single
    Ene = 0.0
!!$omp end single

!!$omp do schedule(static) reduction(+:Ene)
    
    do p=1, NAO
        Ene = Ene + ( &
            t1(p) * tau2(p)&
        )
    end do
    
!!$omp end do

    deallocate(tau2)

    allocate(tau3(NAO, NAO, NAO))
!!$omp single
    tau3 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau3(p, q, r) = tau3(p, q, r) + ( &
                    6 * t1(p) * t2(r, q)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau3(p, q, r) = tau3(p, q, r) + ( &
                    t3(q, p, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau3(p, q, r) = tau3(p, q, r) + ( &
                    t3(q, r, p)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau3(p, q, r) = tau3(p, q, r) + ( &
                    t3(r, q, p)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp single
    Res1 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    2 * H012(p, q, r) * tau3(r, q, p) / 3&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau3)

    allocate(tau4(NAO, NAO))
!!$omp single
    tau4 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau4(p, q) = tau4(p, q) + ( &
                    t2(p, r) * H003(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau5(NAO, NAO))
!!$omp single
    tau5 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                3 * tau4(p, q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau41(NAO, NAO))
!!$omp single
    tau41 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) - ( &
                6 * tau4(p, q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau58(NAO))
!!$omp single
    tau58 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau58(p) = tau58(p) + ( &
                t1(q) * tau4(p, q)&
            )
        end do
    end do
!!$omp end do

    allocate(tau59(NAO))
!!$omp single
    tau59 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            6 * tau58(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau58)

    allocate(tau71(NAO, NAO))
!!$omp single
    tau71 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) + ( &
                6 * t1(p) * tau4(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau76(NAO, NAO))
!!$omp single
    tau76 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) - ( &
                6 * tau4(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp single
    Res2 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                12 * t1(q)**2 * t1(p) * tau4(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau4)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) - ( &
                H011(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) + ( &
                2 * t1(q) * H012(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau5(p, q) = tau5(p, q) - ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) - ( &
                2 * t2(q, p) * tau5(p, q)&
            )
        end do
    end do
!!$omp end do

    deallocate(tau5)

    allocate(tau6(NAO, NAO, NAO))
!!$omp single
    tau6 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau6(p, q, r) = tau6(p, q, r) + ( &
                    t3(p, q, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau6(p, q, r) = tau6(p, q, r) + ( &
                    t3(p, r, q)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau6(p, q, r) = tau6(p, q, r) + ( &
                    t3(q, p, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    H002(q, r) * tau6(q, p, r) / 3&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau6)

    allocate(tau7(NAO, NAO))
!!$omp single
    tau7 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau7(p, q) = tau7(p, q) + ( &
                    t1(r) * H003(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau14(NAO, NAO))
!!$omp single
    tau14 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                3 * tau7(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau72(NAO, NAO))
!!$omp single
    tau72 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau72(p, q) = tau72(p, q) + ( &
                3 * tau7(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau89(NAO, NAO))
!!$omp single
    tau89 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau89(p, q) = tau89(p, q) + ( &
                3 * tau7(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau8(NAO, NAO, NAO))
!!$omp single
    tau8 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau8(p, q, r) = tau8(p, q, r) + ( &
                    t3(p, q, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau8(p, q, r) = tau8(p, q, r) + ( &
                    t3(q, p, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau8(p, q, r) = tau8(p, q, r) + ( &
                    t3(q, r, p)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    tau7(q, r) * tau8(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau8)

    deallocate(tau7)

    allocate(tau9(NAO, NAO))
!!$omp single
    tau9 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                t2(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                t1(p) * t1(q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau15(NAO))
!!$omp single
    tau15 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau15(p) = tau15(p) + ( &
                    tau9(q, r) * H012(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau18(NAO))
!!$omp single
    tau18 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) - ( &
            tau15(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            tau15(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau15)

    allocate(tau20(NAO))
!!$omp single
    tau20 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau20(p) = tau20(p) + ( &
                    tau9(q, r) * H003(q, p, r)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau23(NAO))
!!$omp single
    tau23 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) + ( &
            3 * tau20(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau24(p) = tau24(p) + ( &
            3 * tau20(p)&
        )
    
    end do
!!$omp end do

    allocate(tau43(NAO, NAO))
!!$omp single
    tau43 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau43(p, q) = tau43(p, q) + ( &
                9 * tau20(p) * t2(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau20)

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                Res1(p) = Res1(p) + ( &
                    tau9(q, r) * H102(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau9)

    allocate(tau10(NAO))
!!$omp single
    tau10 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p) = tau10(p) + ( &
                t1(q)**2 * H012(p, q, q)&
            )
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            tau10(p)&
        )
    
    end do
!!$omp end do

    allocate(tau82(NAO))
!!$omp single
    tau82 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) - ( &
            tau10(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau10)

    allocate(tau11(NAO))
!!$omp single
    tau11 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau11(p) = tau11(p) + ( &
                    H003(p, q, r) * t3(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            tau11(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            tau11(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau11)

    allocate(tau12(NAO))
!!$omp single
    tau12 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau12(p) = tau12(p) + ( &
                    H003(p, q, r) * t3(r, p, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            tau12(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            tau12(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau12)

    allocate(tau13(NAO))
!!$omp single
    tau13 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau13(p) = tau13(p) + ( &
                    H003(p, q, r) * t3(r, q, p)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            tau13(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            tau13(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau13)

    allocate(tau65(NAO, NAO))
!!$omp single
    tau65 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                12 * tau59(p) * t2(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau59)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau14(p, q) = tau14(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p) = tau18(p) + ( &
                2 * t2(q, p) * tau14(q, p)&
            )
        end do
    end do
!!$omp end do

    allocate(tau46(NAO, NAO))
!!$omp single
    tau46 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau46(p, q) = tau46(p, q) + ( &
                    t2(r, p) * tau14(r, q)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau47(NAO, NAO))
!!$omp single
    tau47 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) - ( &
                2 * tau46(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau46)

    allocate(tau16(NAO, NAO))
!!$omp single
    tau16 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau16(p, q) = tau16(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau16(p, q) = tau16(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau17(NAO))
!!$omp single
    tau17 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau17(p) = tau17(p) + ( &
                t1(q) * tau16(p, q)&
            )
        end do
    end do
!!$omp end do

    deallocate(tau16)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) - ( &
            tau17(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) + ( &
            tau17(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau17)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) - ( &
            H010(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) - ( &
            2 * H020(p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) - ( &
            4 * H030(p, p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) - ( &
            2 * t1(p) * tau18(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau18)

    allocate(tau19(NAO))
!!$omp single
    tau19 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p) = tau19(p) + ( &
                t2(p, q) * H003(p, p, q)&
            )
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) - ( &
            18 * tau19(p)&
        )
    
    end do
!!$omp end do

    allocate(tau81(NAO))
!!$omp single
    tau81 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau81(p) = tau81(p) - ( &
            18 * tau19(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau19)

    allocate(tau21(NAO, NAO))
!!$omp single
    tau21 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau21(p, q) = tau21(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau21(p, q) = tau21(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau22(NAO))
!!$omp single
    tau22 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau22(p) = tau22(p) + ( &
                t1(q) * tau21(q, p)&
            )
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) + ( &
            2 * tau22(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau81(p) = tau81(p) + ( &
            2 * tau22(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau22)

    allocate(tau80(NAO))
!!$omp single
    tau80 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau80(p) = tau80(p) + ( &
                t2(q, p) * tau21(q, p)&
            )
        end do
    end do
!!$omp end do

    deallocate(tau21)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) - ( &
            2 * tau80(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau80)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) + ( &
            H001(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) + ( &
            2 * H011(p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) + ( &
            4 * H021(p, p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) - ( &
            t1(p)**2 * tau23(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau23)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau24(p) = tau24(p) + ( &
            H001(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                tau24(q) * t2(q, p)&
            )
        end do
    end do
!!$omp end do

    allocate(tau25(NAO))
!!$omp single
    tau25 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p) = tau25(p) + ( &
                t1(q) * H003(p, p, q)&
            )
        end do
    end do
!!$omp end do

    allocate(tau26(NAO))
!!$omp single
    tau26 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do p=1, NAO
    
        tau26(p) = tau26(p) + ( &
            3 * tau25(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau25)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau26(p) = tau26(p) + ( &
            2 * H012(p, p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) + ( &
            3 * t1(p)**2 * tau26(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            2 * t1(p)**3 * tau26(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau26)

    allocate(tau27(NAO, NAO))
!!$omp single
    tau27 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau27(p, q) = tau27(p, q) + ( &
                    t1(r) * H012(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                2 * tau27(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) - ( &
                2 * t1(q) * tau27(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                2 * tau27(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                4 * t1(q)**2 * t1(p) * tau27(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau27)

    allocate(tau28(NAO, NAO))
!!$omp single
    tau28 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau28(p, q) = tau28(p, q) + ( &
                    H021(q, p, r) * t3(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                8 * tau28(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau28)

    allocate(tau29(NAO, NAO))
!!$omp single
    tau29 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau29(p, q) = tau29(p, q) + ( &
                    H021(q, p, r) * t3(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                8 * tau29(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau29)

    allocate(tau30(NAO, NAO))
!!$omp single
    tau30 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau30(p, q) = tau30(p, q) + ( &
                    H021(q, p, r) * t3(r, p, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                8 * tau30(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau30)

    allocate(tau31(NAO, NAO))
!!$omp single
    tau31 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau31(p, q) = tau31(p, q) + ( &
                    t1(r) * H201(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                6 * tau31(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau31)

    allocate(tau32(NAO, NAO))
!!$omp single
    tau32 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau32(p, q) = tau32(p, q) + ( &
                    t2(q, r) * H003(p, p, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                36 * t1(p)**3 * tau32(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau32)

    allocate(tau33(NAO, NAO, NAO))
!!$omp single
    tau33 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau33(p, q, r) = tau33(p, q, r) + ( &
                        t2(r, s) * H003(p, s, q)&
                    )
                end do
            end do
        end do
    end do
!!$omp end do

    allocate(tau34(NAO, NAO, NAO))
!!$omp single
    tau34 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau34(p, q, r) = tau34(p, q, r) + ( &
                    3 * tau33(q, p, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

    deallocate(tau33)

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau34(p, q, r) = tau34(p, q, r) + ( &
                    H102(r, q, p)&
                )
    
            end do
        end do
    end do
!!$omp end do

    allocate(tau35(NAO, NAO, NAO))
!!$omp single
    tau35 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau35(p, q, r) = tau35(p, q, r) + ( &
                    t3(p, q, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau35(p, q, r) = tau35(p, q, r) + ( &
                    t3(p, r, q)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau35(p, q, r) = tau35(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
!!$omp end do

    allocate(tau36(NAO, NAO))
!!$omp single
    tau36 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau36(p, q) = tau36(p, q) + ( &
                        tau34(r, s, p) * tau35(s, r, q)&
                    )
                end do
            end do
        end do
    end do
!!$omp end do

    deallocate(tau34)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                2 * tau36(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau36)

    allocate(tau40(NAO, NAO))
!!$omp single
    tau40 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau40(p, q) = tau40(p, q) + ( &
                        H012(p, r, s) * tau35(s, r, q)&
                    )
                end do
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau43(p, q) = tau43(p, q) - ( &
                tau40(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau40)

    allocate(tau45(NAO, NAO))
!!$omp single
    tau45 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau45(p, q) = tau45(p, q) + ( &
                        H003(r, p, s) * tau35(s, r, q)&
                    )
                end do
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) - ( &
                tau45(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau45)

    allocate(tau48(NAO, NAO))
!!$omp single
    tau48 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau48(p, q) = tau48(p, q) + ( &
                    tau24(r) * tau35(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau24)

    deallocate(tau35)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                tau48(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau48)

    allocate(tau37(NAO, NAO))
!!$omp single
    tau37 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau37(p, q) = tau37(p, q) + ( &
                    t2(p, r) * H021(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau43(p, q) = tau43(p, q) - ( &
                12 * tau37(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau37)

    allocate(tau38(NAO, NAO, NAO))
!!$omp single
    tau38 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t3(p, q, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t3(p, r, q)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t3(q, p, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t3(q, r, p)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t3(r, p, q)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t3(r, q, p)&
                )
    
            end do
        end do
    end do
!!$omp end do

    allocate(tau39(NAO, NAO))
!!$omp single
    tau39 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau39(p, q) = tau39(p, q) + ( &
                    tau14(r, p) * tau38(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau14)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau43(p, q) = tau43(p, q) + ( &
                tau39(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau39)

    allocate(tau44(NAO, NAO))
!!$omp single
    tau44 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau44(p, q) = tau44(p, q) + ( &
                    H003(p, p, r) * tau38(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau47(p, q) = tau47(p, q) + ( &
                3 * tau44(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau44)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                6 * t1(p)**2 * tau47(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau47)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau41(p, q) = tau41(p, q) - ( &
                2 * t1(q) * H012(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau42(NAO, NAO))
!!$omp single
    tau42 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau42(p, q) = tau42(p, q) + ( &
                    t2(r, p) * tau41(q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau43(p, q) = tau43(p, q) - ( &
                3 * tau42(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau42)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                4 * t1(p) * tau43(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau43)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    tau41(p, r) * tau38(p, q, r) / 3&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    tau41(q, r) * tau38(p, q, r) / 3&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau41)

    deallocate(tau38)

    allocate(tau49(NAO, NAO))
!!$omp single
    tau49 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau49(p, q) = tau49(p, q) + ( &
                    H012(p, q, r) * t3(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau57(NAO, NAO))
!!$omp single
    tau57 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * tau49(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau49)

    allocate(tau50(NAO, NAO))
!!$omp single
    tau50 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau50(p, q) = tau50(p, q) + ( &
                    H012(p, q, r) * t3(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * tau50(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau50)

    allocate(tau51(NAO, NAO))
!!$omp single
    tau51 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau51(p, q) = tau51(p, q) + ( &
                    H012(p, q, r) * t3(q, p, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * tau51(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau51)

    allocate(tau52(NAO, NAO))
!!$omp single
    tau52 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau52(p, q) = tau52(p, q) + ( &
                    H012(p, q, r) * t3(q, r, p)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * tau52(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau52)

    allocate(tau53(NAO, NAO))
!!$omp single
    tau53 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau53(p, q) = tau53(p, q) + ( &
                    H012(p, q, r) * t3(r, p, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * tau53(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau53)

    allocate(tau54(NAO, NAO))
!!$omp single
    tau54 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau54(p, q) = tau54(p, q) + ( &
                    H012(p, q, r) * t3(r, q, p)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                2 * tau54(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau54)

    allocate(tau55(NAO, NAO))
!!$omp single
    tau55 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau55(p, q) = tau55(p, q) + ( &
                    t2(q, r) * H102(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) + ( &
                6 * tau55(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau55)

    allocate(tau56(NAO, NAO))
!!$omp single
    tau56 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau56(p, q) = tau56(p, q) + ( &
                    t1(r) * H111(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau57(p, q) = tau57(p, q) - ( &
                3 * tau56(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau56)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                4 * t1(q) * tau57(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau57)

    allocate(tau60(NAO, NAO))
!!$omp single
    tau60 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau60(p, q) = tau60(p, q) + ( &
                    t1(r) * H102(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau62(NAO, NAO))
!!$omp single
    tau62 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                tau60(p, q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau64(NAO, NAO))
!!$omp single
    tau64 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                tau60(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau60)

    allocate(tau61(NAO, NAO))
!!$omp single
    tau61 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau61(p, q) = tau61(p, q) + ( &
                    t2(p, r) * H012(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                2 * tau61(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                2 * tau61(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau61)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) + ( &
                12 * t1(q)**2 * tau64(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau64)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) - ( &
                t1(q) * H102(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau63(NAO, NAO))
!!$omp single
    tau63 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau63(p, q) = tau63(p, q) + ( &
                    t2(r, p) * tau62(q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau62)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                12 * tau63(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau63)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau65(p, q) = tau65(p, q) - ( &
                12 * t1(q)**3 * H102(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                tau65(p, q) / 6&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                tau65(q, p) / 6&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau65)

    allocate(tau66(NAO, NAO))
!!$omp single
    tau66 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau66(p, q) = tau66(p, q) + ( &
                    t1(r) * H021(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

    allocate(tau75(NAO, NAO))
!!$omp single
    tau75 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                2 * tau66(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau83(NAO, NAO))
!!$omp single
    tau83 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau83(p, q) = tau83(p, q) + ( &
                tau66(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau66)

    allocate(tau67(NAO, NAO))
!!$omp single
    tau67 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau67(p, q) = tau67(p, q) + ( &
                    H003(q, r, p) * t3(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) + ( &
                tau67(p, q)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau85(NAO, NAO))
!!$omp single
    tau85 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau85(p, q) = tau85(p, q) + ( &
                tau67(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau67)

    allocate(tau68(NAO, NAO))
!!$omp single
    tau68 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau68(p, q) = tau68(p, q) + ( &
                    H003(q, r, p) * t3(p, r, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) + ( &
                tau68(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau85(p, q) = tau85(p, q) + ( &
                tau68(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau68)

    allocate(tau69(NAO, NAO))
!!$omp single
    tau69 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau69(p, q) = tau69(p, q) + ( &
                    H003(q, r, p) * t3(r, p, q)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) + ( &
                tau69(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau85(p, q) = tau85(p, q) + ( &
                tau69(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau69)

    allocate(tau70(NAO, NAO))
!!$omp single
    tau70 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau70(p, q) = tau70(p, q) + ( &
                    t2(q, r) * H012(p, q, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) - ( &
                2 * tau70(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau85(p, q) = tau85(p, q) - ( &
                2 * tau70(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau70)

    allocate(tau86(NAO, NAO))
!!$omp single
    tau86 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau86(p, q) = tau86(p, q) + ( &
                tau85(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau86(p, q) = tau86(p, q) + ( &
                tau85(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau85)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t1(p) * t1(q) * tau86(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau86)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) + ( &
                6 * H030(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau71(p, q) = tau71(p, q) + ( &
                3 * t1(q)**2 * H012(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                tau71(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                tau71(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau71)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau72(p, q) = tau72(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau72(p, q) = tau72(p, q) + ( &
                2 * H012(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau72(p, q) = tau72(p, q) + ( &
                2 * H012(q, q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                2 * t1(p) * t1(q) * tau72(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau73(NAO, NAO))
!!$omp single
    tau73 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau73(p, q) = tau73(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau73(p, q) = tau73(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau73(p, q) = tau73(p, q) + ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau73(p, q) = tau73(p, q) + ( &
                9 * t1(p)**2 * H003(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) - ( &
                t1(q) * tau73(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau73)

    allocate(tau74(NAO, NAO))
!!$omp single
    tau74 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau74(p, q) = tau74(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau74(p, q) = tau74(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau74(p, q) = tau74(p, q) + ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) - ( &
                t1(p) * tau74(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau84(NAO, NAO))
!!$omp single
    tau84 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau84(p, q) = tau84(p, q) - ( &
                t1(p)**2 * tau74(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau74)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                2 * H020(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                4 * t2(q, p) * tau75(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau75)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                H011(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                2 * H021(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                4 * H021(q, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * t1(p)**2 * t1(q) * tau76(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau76)

    allocate(tau77(NAO, NAO))
!!$omp single
    tau77 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau77(p, q) = tau77(p, q) + ( &
                6 * t2(q, p)**2 * t1(p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau77(p, q) = tau77(p, q) + ( &
                t1(p)**3 * t1(q)**2&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                6 * tau77(q, p) * H003(q, q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau77)

    allocate(tau78(NAO, NAO, NAO))
!!$omp single
    tau78 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau78(p, q, r) = tau78(p, q, r) - ( &
                    H111(p, q, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau78(p, q, r) = tau78(p, q, r) + ( &
                    2 * t2(p, r) * H012(q, r, r)&
                )
    
            end do
        end do
    end do
!!$omp end do

    allocate(tau79(NAO, NAO))
!!$omp single
    tau79 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau79(p, q) = tau79(p, q) + ( &
                    t2(r, p) * tau78(q, p, r)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau78)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau79(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau79(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau79)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau81(p) = tau81(p) + ( &
            H001(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau81(p) = tau81(p) + ( &
            2 * H011(p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau81(p) = tau81(p) + ( &
            4 * H021(p, p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) - ( &
            t1(p) * tau81(p)&
        )
    
    end do
!!$omp end do

    deallocate(tau81)

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) + ( &
            H010(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) + ( &
            2 * H020(p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) + ( &
            4 * H030(p, p, p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau82(p) * t2(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau82(q) * t2(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau82)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau83(p, q) = tau83(p, q) + ( &
                H020(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau83(p, q) = tau83(p, q) + ( &
                3 * H030(p, p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau83(p, q) = tau83(p, q) + ( &
                3 * H030(q, q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau84(p, q) = tau84(p, q) + ( &
                4 * t1(p) * tau83(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau83)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau84(p, q) = tau84(p, q) + ( &
                H110(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau84(p, q) = tau84(p, q) + ( &
                2 * H120(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau84(p, q) = tau84(p, q) + ( &
                2 * t1(p)**3 * H012(q, p, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(p) * tau84(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau84)

    allocate(tau87(NAO, NAO))
!!$omp single
    tau87 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau87(p, q) = tau87(p, q) + ( &
                2 * t2(q, p)**2&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau87(p, q) = tau87(p, q) + ( &
                t1(p)**2 * t1(q)**2&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau72(q, p) * tau87(q, p)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau72)

    deallocate(tau87)

    allocate(tau88(NAO, NAO))
!!$omp single
    tau88 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau88(p, q) = tau88(p, q) + ( &
                H110(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau88(p, q) = tau88(p, q) + ( &
                2 * H120(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau88(p, q) = tau88(p, q) + ( &
                2 * t1(p)**3 * H012(q, p, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(q) * tau88(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau88)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau89(p, q) = tau89(p, q) + ( &
                H002(q, p)&
            )
    
        end do
    end do
!!$omp end do

    allocate(tau90(NAO, NAO))
!!$omp single
    tau90 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau90(p, q) = tau90(p, q) + ( &
                    2 * t2(r, q) * tau89(r, p)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau89)

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau90(p, q) = tau90(p, q) + ( &
                H101(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    t2(r, q) * tau90(r, p)&
                )
            end do
        end do
    end do
!!$omp end do

    deallocate(tau90)

    allocate(tau91(NAO, NAO))
!!$omp single
    tau91 = 0.0
!!$omp end single

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau91(p, q) = tau91(p, q) + ( &
                H101(p, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau91(p, q) = tau91(p, q) + ( &
                2 * H111(p, q, q)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                t1(p)**2 * tau91(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) - ( &
                t1(q)**2 * tau91(p, q)&
            )
    
        end do
    end do
!!$omp end do

    deallocate(tau91)

!!$omp do schedule(static) reduction(+:Ene)
    
    do q=1, NAO
        do p=1, NAO
            Ene = Ene + ( &
                H002(q, p) * t2(q, p)&
            )
        end do
    end do
    
!!$omp end do

!!$omp do schedule(static) reduction(+:Ene)
    
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                Ene = Ene + ( &
                    H003(r, p, q) * t3(q, r, p)&
                )
            end do
        end do
    end do
    
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                t1(q) * H101(p, q)&
            )
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) - ( &
                t1(q)**2 * H102(p, q, q)&
            )
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do p=1, NAO
    
        Res1(p) = Res1(p) + ( &
            H100(p)&
        )
    
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    H101(q, r) * t2(p, r)&
                )
            end do
        end do
    end do
!!$omp end do

!!$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            Res2(p, q) = Res2(p, q) + ( &
                2 * H200(q, p)&
            )
    
        end do
    end do
!!$omp end do

!!$omp end parallel
    End Subroutine CCSDT_SFS
End Module CCRes_SFS