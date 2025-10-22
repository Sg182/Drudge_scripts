    Subroutine Hub2Pair(u,v,H010,H001,H100,H020,H110,H011,H120,H021,NAO, &
               W000,W001,W100,W010,W101,W020,W200,W002,W110,W011, &
               W030,W111,W120,W210,W021,W012,W201,W102,W003,W300)
    Use Precision
    Implicit None
    Integer                        :: NAO
    Complex(Kind=pr), Intent(in)   :: u(NAO), v(NAO)
    Complex(Kind=pr), Intent(in)   :: H010(NAO),H001(NAO), H100(NAO)
    Complex(Kind=pr), Intent(in)   :: H020(NAO,NAO), H110(NAO,NAO), H011(NAO,NAO)
    Complex(Kind=pr), Intent(in)   :: H120(NAO,NAO,NAO),H021(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: W000
    Complex(Kind=pr), Intent(out)  :: W001(NAO), W100(NAO), W010(NAO)
    Complex(Kind=pr), Intent(out)  :: W101(NAO,NAO), W200(NAO,NAO), W002(NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: W110(NAO,NAO), W011(NAO,NAO),W020(NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: W030(NAO,NAO,NAO), W111(NAO,NAO,NAO), W120(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: W210(NAO,NAO,NAO), W021(NAO,NAO,NAO), W012(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: W003(NAO,NAO,NAO), W300(NAO,NAO,NAO)
    Complex(Kind=pr), Intent(out)  :: W201(NAO,NAO,NAO), W102(NAO,NAO,NAO)

    complex (kind=pr) , dimension(:), allocatable :: tau0

    complex (kind=pr) , dimension(:), allocatable :: tau1

    complex (kind=pr) , dimension(:), allocatable :: tau2

    complex (kind=pr) , dimension(:), allocatable :: tau3

    complex (kind=pr) , dimension(:), allocatable :: tau4

    complex (kind=pr) , dimension(:), allocatable :: tau5

    complex (kind=pr) , dimension(:), allocatable :: tau6

    complex (kind=pr) , dimension(:), allocatable :: tau7

    complex (kind=pr) , dimension(:), allocatable :: tau8

    complex (kind=pr) , dimension(:, :), allocatable :: tau9

    complex (kind=pr) , dimension(:), allocatable :: tau10

    complex (kind=pr) , dimension(:), allocatable :: tau11

    complex (kind=pr) , dimension(:), allocatable :: tau12

    complex (kind=pr) , dimension(:, :), allocatable :: tau13

    complex (kind=pr) , dimension(:), allocatable :: tau14

    complex (kind=pr) , dimension(:), allocatable :: tau15

    complex (kind=pr) , dimension(:, :), allocatable :: tau16

    complex (kind=pr) , dimension(:, :), allocatable :: tau17

    complex (kind=pr) , dimension(:), allocatable :: tau18

    complex (kind=pr) , dimension(:), allocatable :: tau19

    complex (kind=pr) , dimension(:), allocatable :: tau20

    complex (kind=pr) , dimension(:), allocatable :: tau21

    complex (kind=pr) , dimension(:), allocatable :: tau22

    complex (kind=pr) , dimension(:), allocatable :: tau23

    complex (kind=pr) , dimension(:), allocatable :: tau24

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau25

    complex (kind=pr) , dimension(:), allocatable :: tau26

    complex (kind=pr) , dimension(:), allocatable :: tau27

    complex (kind=pr) , dimension(:), allocatable :: tau28

    complex (kind=pr) , dimension(:, :), allocatable :: tau29

    complex (kind=pr) , dimension(:), allocatable :: tau30

    complex (kind=pr) , dimension(:), allocatable :: tau31

    complex (kind=pr) , dimension(:), allocatable :: tau32

    complex (kind=pr) , dimension(:), allocatable :: tau33

    complex (kind=pr) , dimension(:), allocatable :: tau34

    complex (kind=pr) , dimension(:, :), allocatable :: tau35

    complex (kind=pr) , dimension(:), allocatable :: tau36

    complex (kind=pr) , dimension(:), allocatable :: tau37

    complex (kind=pr) , dimension(:), allocatable :: tau38

    complex (kind=pr) , dimension(:), allocatable :: tau39

    complex (kind=pr) , dimension(:), allocatable :: tau40

    complex (kind=pr) , dimension(:), allocatable :: tau41

    complex (kind=pr) , dimension(:), allocatable :: tau42

    complex (kind=pr) , dimension(:), allocatable :: tau43

    complex (kind=pr) , dimension(:, :), allocatable :: tau44

    complex (kind=pr) , dimension(:), allocatable :: tau45

    complex (kind=pr) , dimension(:), allocatable :: tau46

    complex (kind=pr) , dimension(:), allocatable :: tau47

    complex (kind=pr) , dimension(:, :), allocatable :: tau48

    complex (kind=pr) , dimension(:), allocatable :: tau49

    complex (kind=pr) , dimension(:), allocatable :: tau50

    complex (kind=pr) , dimension(:), allocatable :: tau51

    complex (kind=pr) , dimension(:), allocatable :: tau52

    complex (kind=pr) , dimension(:), allocatable :: tau53

    complex (kind=pr) , dimension(:), allocatable :: tau54

    complex (kind=pr) , dimension(:), allocatable :: tau55

    complex (kind=pr) , dimension(:), allocatable :: tau56

    complex (kind=pr) , dimension(:), allocatable :: tau57

    complex (kind=pr) , dimension(:), allocatable :: tau58

    complex (kind=pr) , dimension(:), allocatable :: tau59

    complex (kind=pr) , dimension(:), allocatable :: tau60

    complex (kind=pr) , dimension(:, :), allocatable :: tau61

    complex (kind=pr) , dimension(:, :), allocatable :: tau62

    complex (kind=pr) , dimension(:), allocatable :: tau63

    complex (kind=pr) , dimension(:, :), allocatable :: tau64

    complex (kind=pr) , dimension(:), allocatable :: tau65

    complex (kind=pr) , dimension(:), allocatable :: tau66

    complex (kind=pr) , dimension(:, :), allocatable :: tau67

    complex (kind=pr) , dimension(:, :), allocatable :: tau68

    complex (kind=pr) , dimension(:, :), allocatable :: tau69

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau70

    complex (kind=pr) , dimension(:, :), allocatable :: tau71

    complex (kind=pr) , dimension(:, :), allocatable :: tau72

    complex (kind=pr) , dimension(:, :), allocatable :: tau73

    complex (kind=pr) , dimension(:, :), allocatable :: tau74

    complex (kind=pr) , dimension(:, :), allocatable :: tau75

    complex (kind=pr) , dimension(:, :), allocatable :: tau76

    complex (kind=pr) , dimension(:, :), allocatable :: tau77

    complex (kind=pr) , dimension(:, :), allocatable :: tau78

    complex (kind=pr) , dimension(:, :), allocatable :: tau79

    complex (kind=pr) , dimension(:, :), allocatable :: tau80

    complex (kind=pr) , dimension(:, :), allocatable :: tau81

    complex (kind=pr) , dimension(:, :), allocatable :: tau82

    complex (kind=pr) , dimension(:, :), allocatable :: tau83

    complex (kind=pr) , dimension(:), allocatable :: tau84

    complex (kind=pr) , dimension(:), allocatable :: tau85

    complex (kind=pr) , dimension(:, :), allocatable :: tau86

    complex (kind=pr) , dimension(:, :), allocatable :: tau87

    complex (kind=pr) , dimension(:), allocatable :: tau88

    complex (kind=pr) , dimension(:, :), allocatable :: tau89

    complex (kind=pr) , dimension(:, :), allocatable :: tau90

    complex (kind=pr) , dimension(:, :), allocatable :: tau91

    complex (kind=pr) , dimension(:, :), allocatable :: tau92

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau93

    complex (kind=pr) , dimension(:, :), allocatable :: tau94

    complex (kind=pr) , dimension(:, :), allocatable :: tau95

    complex (kind=pr) , dimension(:, :), allocatable :: tau96

    complex (kind=pr) , dimension(:, :), allocatable :: tau97

    complex (kind=pr) , dimension(:, :), allocatable :: tau98

    complex (kind=pr) , dimension(:, :), allocatable :: tau99

    complex (kind=pr) , dimension(:), allocatable :: tau100

    complex (kind=pr) , dimension(:, :), allocatable :: tau101

    complex (kind=pr) , dimension(:, :), allocatable :: tau102

    complex (kind=pr) , dimension(:, :), allocatable :: tau103

    complex (kind=pr) , dimension(:, :), allocatable :: tau104

    complex (kind=pr) , dimension(:, :), allocatable :: tau105

    complex (kind=pr) , dimension(:, :), allocatable :: tau106

    complex (kind=pr) , dimension(:, :), allocatable :: tau107

    complex (kind=pr) , dimension(:, :), allocatable :: tau108

    complex (kind=pr) , dimension(:, :), allocatable :: tau109

    complex (kind=pr) , dimension(:, :), allocatable :: tau110

    complex (kind=pr) , dimension(:, :), allocatable :: tau111

    complex (kind=pr) , dimension(:), allocatable :: tau112

    complex (kind=pr) , dimension(:, :), allocatable :: tau113

    complex (kind=pr) , dimension(:), allocatable :: tau114

    complex (kind=pr) , dimension(:), allocatable :: tau115

    complex (kind=pr) , dimension(:, :), allocatable :: tau116

    complex (kind=pr) , dimension(:, :), allocatable :: tau117

    complex (kind=pr) , dimension(:, :), allocatable :: tau118

    complex (kind=pr) , dimension(:, :), allocatable :: tau119

    complex (kind=pr) , dimension(:, :), allocatable :: tau120

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau121

    complex (kind=pr) , dimension(:, :), allocatable :: tau122

    complex (kind=pr) , dimension(:, :), allocatable :: tau123

    complex (kind=pr) , dimension(:, :), allocatable :: tau124

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau125

    complex (kind=pr) , dimension(:, :, :), allocatable :: tau126

    !$omp parallel default(shared)

    allocate(tau0(NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                u(q) * v(q) * h011(p, q)&
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
            tau0(p) * v(p)&
        )
    
    end do
    !$omp end do

    allocate(tau63(NAO))
    !$omp single
    tau63 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            tau0(p)&
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
                u(q) * v(q) * h110(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            tau1(p) * v(p)&
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
                v(q)**2 * h020(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            4 * tau2(p) * v(p)&
        )
    
    end do
    !$omp end do

    allocate(tau31(NAO))
    !$omp single
    tau31 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau31(p) = tau31(p) + ( &
            4 * tau2(p)&
        )
    
    end do
    !$omp end do

    allocate(tau65(NAO))
    !$omp single
    tau65 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau65(p) = tau65(p) + ( &
            4 * tau2(p)&
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
                u(q)**3 * v(q) * h021(q, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            2 * tau3(p) * v(p)&
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
                u(q)**3 * v(q) * h120(p, q, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            2 * tau4(p) * v(p)&
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
                v(q)**3 * u(q) * h021(q, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) - ( &
            2 * tau5(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau5)

    allocate(tau6(NAO))
    !$omp single
    tau6 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau6(p) = tau6(p) + ( &
                    v(q)**2 * u(r) * v(r) * h021(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            4 * tau6(p) * v(p)&
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
        do r=1, NAO
            do q=1, NAO
                tau7(p) = tau7(p) + ( &
                    v(q)**2 * u(r) * v(r) * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            2 * tau7(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau7)

    allocate(tau8(NAO))
    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau8(p) = tau8(p) + ( &
                    v(q)**2 * u(r) * v(r) * h120(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            2 * tau8(p) * v(p)&
        )
    
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
                h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                h120(q, p, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau9(p, q) = tau9(p, q) + ( &
                2 * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau10(NAO))
    !$omp single
    tau10 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p) = tau10(p) + ( &
                u(q) * v(q) * tau9(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau9)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau10(p) = tau10(p) + ( &
            h020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) - ( &
            2 * v(p)**3 * tau10(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau10)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau11(p) = tau11(p) + ( &
            h010(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    W000 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:W000)
    
    do p=1, NAO
        W000 = W000 + ( &
            2 * tau11(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau11)

    allocate(tau12(NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) + ( &
            v(p)**6&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) + ( &
            u(p)**4 * v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) - ( &
            4 * u(p)**2 * v(p)**4&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static) reduction(+:W000)
    
    do p=1, NAO
        W000 = W000 - ( &
            4 * tau12(p) * h021(p, p, p)&
        )
    end do
    
    !$omp end do

    deallocate(tau12)

    allocate(tau13(NAO, NAO))
    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                2 * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                2 * h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau13(p, q) = tau13(p, q) + ( &
                2 * h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau14(NAO))
    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau14(p) = tau14(p) + ( &
                v(q)**2 * tau13(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau13)

    allocate(tau15(NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            2 * tau14(p)&
        )
    
    end do
    !$omp end do

    allocate(tau33(NAO))
    !$omp single
    tau33 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau33(p) = tau33(p) + ( &
            2 * tau14(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau14)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            h011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            2 * h110(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15(p) = tau15(p) - ( &
                2 * u(q)**2 * h120(q, p, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static) reduction(+:W000)
    
    do p=1, NAO
        W000 = W000 - ( &
            2 * u(p)**2 * v(p)**2 * tau15(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau15)

    allocate(tau16(NAO, NAO))
    !$omp single
    tau16 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau16(p, q) = tau16(p, q) + ( &
                    v(r)**2 * h021(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau17(NAO, NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau17(p, q) = tau17(p, q) + ( &
                2 * tau16(q, p)&
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
    
            tau35(p, q) = tau35(p, q) + ( &
                2 * tau16(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau64(NAO, NAO))
    !$omp single
    tau64 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                8 * tau16(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau16)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau17(p, q) = tau17(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau17(p, q) = tau17(p, q) - ( &
                    2 * u(r)**2 * h120(r, q, p)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau18(NAO))
    !$omp single
    tau18 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p) = tau18(p) + ( &
                2 * v(q)**2 * tau17(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau17)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            h001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            8 * u(p)**4 * h120(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p) = tau18(p) - ( &
                8 * v(q)**4 * h021(q, p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p) = tau18(p) - ( &
                2 * u(q)**2 * h110(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static) reduction(+:W000)
    
    do p=1, NAO
        W000 = W000 - ( &
            v(p)**2 * tau18(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau18)

    allocate(tau19(NAO))
    !$omp single
    tau19 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19(p) = tau19(p) + ( &
            h011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau19(p) = tau19(p) + ( &
            8 * u(p)**2 * h120(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static) reduction(+:W000)
    
    do p=1, NAO
        W000 = W000 + ( &
            2 * v(p)**4 * tau19(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau19)

    allocate(tau20(NAO))
    !$omp single
    tau20 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau20(p) = tau20(p) + ( &
                    v(q)**2 * v(r)**2 * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau59(NAO))
    !$omp single
    tau59 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            8 * tau20(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    W100 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            4*NAO * u(p)**2 * tau20(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    W001 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            4*NAO * v(p)**2 * tau20(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau20)

    allocate(tau21(NAO))
    !$omp single
    tau21 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau21(p) = tau21(p) + ( &
                v(q)**2 * h110(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            3 * tau21(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            2*NAO * u(p)**2 * tau21(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            2*NAO * v(p)**2 * tau21(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau21)

    allocate(tau22(NAO))
    !$omp single
    tau22 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau22(p) = tau22(p) + ( &
                v(q)**2 * h021(p, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            12 * v(p)**3 * tau22(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            8*NAO * v(p)**4 * tau22(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau22)

    allocate(tau23(NAO))
    !$omp single
    tau23 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p) = tau23(p) + ( &
                u(q)**2 * v(q)**2 * h120(p, q, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            8 * tau23(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            4*NAO * u(p)**2 * tau23(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            4*NAO * v(p)**2 * tau23(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau23)

    allocate(tau24(NAO))
    !$omp single
    tau24 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p) = tau24(p) + ( &
                u(q) * v(q) * h021(p, p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau32(NAO))
    !$omp single
    tau32 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau32(p) = tau32(p) + ( &
            2 * u(p)**3 * tau24(p)&
        )
    
    end do
    !$omp end do

    allocate(tau40(NAO))
    !$omp single
    tau40 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau40(p) = tau40(p) + ( &
            tau24(p) * u(p)&
        )
    
    end do
    !$omp end do

    allocate(tau60(NAO))
    !$omp single
    tau60 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau60(p) = tau60(p) + ( &
            2 * tau24(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau24)

    allocate(tau25(NAO, NAO, NAO))
    !$omp single
    tau25 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau25(p, q, r) = tau25(p, q, r) + ( &
                    2 * h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau25(p, q, r) = tau25(p, q, r) + ( &
                    h120(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau25(p, q, r) = tau25(p, q, r) + ( &
                    h120(q, r, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau26(NAO))
    !$omp single
    tau26 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau26(p) = tau26(p) + ( &
                    v(r)**2 * u(q) * v(q) * tau25(r, p, q)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau25)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau31(p) = tau31(p) + ( &
            2 * tau26(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau26)

    allocate(tau27(NAO))
    !$omp single
    tau27 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau27(p) = tau27(p) + ( &
            v(p)**3 * u(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau27(p) = tau27(p) - ( &
            u(p)**3 * v(p)&
        )
    
    end do
    !$omp end do

    allocate(tau28(NAO))
    !$omp single
    tau28 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau28(p) = tau28(p) + ( &
                tau27(q) * h120(p, q, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau31(p) = tau31(p) - ( &
            2 * tau28(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau28)

    !$omp single
    W002 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W002(p, q) = W002(p, q) - ( &
                4*NAO**2 * u(q)**2 * tau27(p) * h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau27)

    allocate(tau29(NAO, NAO))
    !$omp single
    tau29 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau29(p, q) = tau29(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau29(p, q) = tau29(p, q) + ( &
                h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau30(NAO))
    !$omp single
    tau30 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p) = tau30(p) + ( &
                u(q) * v(q) * tau29(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau29)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau31(p) = tau31(p) + ( &
            tau30(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau30)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau31(p) = tau31(p) + ( &
            h010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau32(p) = tau32(p) + ( &
            tau31(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau31)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau32(p) = tau32(p) + ( &
            2 * u(p)**3 * h020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            2*NAO * tau32(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) + ( &
            2*NAO * tau32(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau32)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau33(p) = tau33(p) + ( &
            h011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau33(p) = tau33(p) + ( &
            2 * h110(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau36(NAO))
    !$omp single
    tau36 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau36(p) = tau36(p) + ( &
            2 * u(p)**2 * tau33(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau33)

    allocate(tau34(NAO))
    !$omp single
    tau34 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau34(p) = tau34(p) + ( &
            h021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau34(p) = tau34(p) + ( &
            2 * h120(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau36(p) = tau36(p) + ( &
            4 * u(p)**4 * tau34(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau34)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau35(p, q) = tau35(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p) = tau36(p) + ( &
                2 * v(q)**2 * tau35(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau35)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau36(p) = tau36(p) + ( &
            h001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) - ( &
            NAO * v(p)**2 * tau36(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau36)

    allocate(tau37(NAO))
    !$omp single
    tau37 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p) = tau37(p) + ( &
                u(q) * v(q) * h120(p, p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau40(p) = tau40(p) + ( &
            tau37(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau37)

    allocate(tau38(NAO))
    !$omp single
    tau38 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau38(p) = tau38(p) + ( &
                u(q) * v(q) * h120(p, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau40(p) = tau40(p) + ( &
            tau38(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau38)

    allocate(tau39(NAO))
    !$omp single
    tau39 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau39(p) = tau39(p) + ( &
                u(q) * v(q) * h021(p, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau40(p) = tau40(p) + ( &
            2 * tau39(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau39)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau40(p) = tau40(p) + ( &
            u(p) * h020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) - ( &
            4*NAO * v(p)**3 * tau40(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            4*NAO * v(p)**3 * tau40(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau40)

    allocate(tau41(NAO))
    !$omp single
    tau41 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau41(p) = tau41(p) - ( &
            v(p)**6&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau41(p) = tau41(p) + ( &
            4 * u(p)**2 * v(p)**4&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            4*NAO * tau41(p) * h021(p, p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau41)

    allocate(tau42(NAO))
    !$omp single
    tau42 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau42(p) = tau42(p) + ( &
                    u(q)**2 * v(r)**2 * h021(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) + ( &
            4*NAO * v(p)**2 * tau42(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau42)

    allocate(tau43(NAO))
    !$omp single
    tau43 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau43(p) = tau43(p) + ( &
                u(q)**2 * h011(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) + ( &
            2*NAO * v(p)**2 * tau43(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau43)

    allocate(tau44(NAO, NAO))
    !$omp single
    tau44 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) + ( &
                h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) + ( &
                h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau44(p, q) = tau44(p, q) + ( &
                4 * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau45(NAO))
    !$omp single
    tau45 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau45(p) = tau45(p) + ( &
                2 * v(q)**2 * tau44(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau68(NAO, NAO))
    !$omp single
    tau68 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau68(p, q) = tau68(p, q) + ( &
                2 * u(p)**2 * tau44(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau44)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau45(p) = tau45(p) + ( &
            h110(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau45(p) = tau45(p) + ( &
            2 * h011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau45(p) = tau45(p) - ( &
                2 * u(q)**2 * h021(p, p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau47(NAO))
    !$omp single
    tau47 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau47(p) = tau47(p) + ( &
            2 * v(p)**2 * tau45(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau45)

    allocate(tau46(NAO))
    !$omp single
    tau46 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau46(p) = tau46(p) + ( &
            h021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau46(p) = tau46(p) + ( &
            h120(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau47(p) = tau47(p) - ( &
            16 * v(p)**4 * tau46(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau46)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau47(p) = tau47(p) - ( &
            h001(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            NAO * u(p)**2 * tau47(p)&
        )
    
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
                h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau48(p, q) = tau48(p, q) + ( &
                h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau49(NAO))
    !$omp single
    tau49 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau49(p) = tau49(p) + ( &
                2 * v(q)**2 * tau48(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau103(NAO, NAO))
    !$omp single
    tau103 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau103(p, q) = tau103(p, q) - ( &
                4 * u(q)**2 * v(p)**2 * tau48(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W002(p, q) = W002(p, q) + ( &
                4*NAO**2 * v(p)**4 * u(q) * v(q) * tau48(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau49(p) = tau49(p) + ( &
            h110(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) + ( &
            2*NAO * v(p)**4 * tau49(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau49)

    allocate(tau50(NAO))
    !$omp single
    tau50 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau50(p) = tau50(p) + ( &
            v(p)**6&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau50(p) = tau50(p) + ( &
            u(p)**4 * v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            4*NAO * tau50(p) * h120(p, p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau50)

    allocate(tau51(NAO))
    !$omp single
    tau51 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51(p) = tau51(p) + ( &
                u(q)**2 * h110(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            tau51(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau51)

    allocate(tau52(NAO))
    !$omp single
    tau52 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52(p) = tau52(p) + ( &
                u(q)**2 * h021(p, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            4 * v(p)**3 * tau52(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau52)

    allocate(tau53(NAO))
    !$omp single
    tau53 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p) = tau53(p) + ( &
                u(q)**2 * h120(p, p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            2 * v(p)**3 * tau53(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau53)

    allocate(tau54(NAO))
    !$omp single
    tau54 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54(p) = tau54(p) + ( &
                u(q)**2 * h120(p, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            2 * v(p)**3 * tau54(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau54)

    allocate(tau55(NAO))
    !$omp single
    tau55 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55(p) = tau55(p) + ( &
                v(q)**2 * h120(p, p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            6 * v(p)**3 * tau55(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau55)

    allocate(tau56(NAO))
    !$omp single
    tau56 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56(p) = tau56(p) + ( &
                v(q)**2 * h120(p, q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            6 * v(p)**3 * tau56(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau56)

    allocate(tau57(NAO))
    !$omp single
    tau57 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau57(p) = tau57(p) + ( &
                    u(q)**2 * v(r)**2 * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            2 * tau57(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau57)

    allocate(tau58(NAO))
    !$omp single
    tau58 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau58(p) = tau58(p) + ( &
                    u(q)**2 * v(r)**2 * h120(p, r, q)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            2 * tau58(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau58)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            h001(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            h100(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            4 * v(p)**5 * h021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) + ( &
            4 * v(p)**5 * h120(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            2 * v(p)**3 * h011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau59(p) = tau59(p) - ( &
            2 * v(p)**3 * h110(p, p)&
        )
    
    end do
    !$omp end do

    !$omp single
    W010 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        W010(p) = W010(p) - ( &
            NAO * tau59(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau59)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau60(p) = tau60(p) + ( &
            h020(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) - ( &
            4 * v(p)**2 * tau60(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau60)

    allocate(tau61(NAO, NAO))
    !$omp single
    tau61 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau61(p, q) = tau61(p, q) + ( &
                    u(r) * v(r) * h021(p, q, r)&
                )
            end do
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
                tau61(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau62(p, q) = tau62(p, q) + ( &
                h020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p) = tau63(p) + ( &
                4 * v(q)**2 * tau62(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    W020 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) + ( &
                NAO**2 * u(p)**2 * u(q)**2 * tau62(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau62)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            h010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W010(p) = W010(p) + ( &
            NAO * u(p)**2 * tau63(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau63)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau64(p, q) = tau64(p, q) + ( &
                3 * h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p) = tau65(p) + ( &
                u(q) * v(q) * tau64(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau64)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau65(p) = tau65(p) + ( &
            h010(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W010(p) = W010(p) - ( &
            NAO * v(p)**2 * tau65(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau65)

    allocate(tau66(NAO))
    !$omp single
    tau66 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau66(p) = tau66(p) + ( &
            v(p)**3 * h021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau66(p) = tau66(p) + ( &
            v(p)**3 * h120(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W010(p) = W010(p) + ( &
            4*NAO * u(p)**3 * tau66(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau66)

    allocate(tau67(NAO, NAO))
    !$omp single
    tau67 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau67(p, q) = tau67(p, q) + ( &
                    v(r)**2 * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau68(p, q) = tau68(p, q) + ( &
                2 * tau67(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau77(NAO, NAO))
    !$omp single
    tau77 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau77(p, q) = tau77(p, q) + ( &
                2 * tau67(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau108(NAO, NAO))
    !$omp single
    tau108 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau108(p, q) = tau108(p, q) + ( &
                2 * tau67(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau67)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau68(p, q) = tau68(p, q) + ( &
                h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau69(NAO, NAO))
    !$omp single
    tau69 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau69(p, q) = tau69(p, q) + ( &
                u(q) * tau68(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau68)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau69(p, q) = tau69(p, q) + ( &
                2 * u(q)**3 * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W002(p, q) = W002(p, q) - ( &
                2*NAO**2 * v(p)**2 * v(q) * tau69(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau69)

    allocate(tau70(NAO, NAO, NAO))
    !$omp single
    tau70 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau70(p, q, r) = tau70(p, q, r) + ( &
                    h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau70(p, q, r) = tau70(p, q, r) + ( &
                    h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau71(NAO, NAO))
    !$omp single
    tau71 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau71(p, q) = tau71(p, q) + ( &
                    u(r) * v(r) * tau70(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau70)

    allocate(tau72(NAO, NAO))
    !$omp single
    tau72 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau72(p, q) = tau72(p, q) + ( &
                tau71(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau71)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau72(p, q) = tau72(p, q) + ( &
                h020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W002(p, q) = W002(p, q) + ( &
                4*NAO**2 * u(p) * u(q) * v(p) * v(q) * tau72(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    W200 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W200(p, q) = W200(p, q) + ( &
                4*NAO**2 * u(p) * u(q) * v(p) * v(q) * tau72(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    W101 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) + ( &
                8*NAO**2 * u(p) * u(q) * v(p) * v(q) * tau72(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau72)

    allocate(tau73(NAO, NAO))
    !$omp single
    tau73 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau73(p, q) = tau73(p, q) + ( &
                    u(r)**2 * h021(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau75(NAO, NAO))
    !$omp single
    tau75 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) + ( &
                2 * u(p) * v(p) * tau73(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau107(NAO, NAO))
    !$omp single
    tau107 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) - ( &
                4 * tau73(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau110(NAO, NAO))
    !$omp single
    tau110 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau110(p, q) = tau110(p, q) + ( &
                4 * v(q)**2 * tau73(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau73)

    allocate(tau74(NAO, NAO))
    !$omp single
    tau74 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau74(p, q) = tau74(p, q) + ( &
                    u(r) * v(r) * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau75(p, q) = tau75(p, q) - ( &
                v(p)**2 * tau74(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W002(p, q) = W002(p, q) + ( &
                4*NAO**2 * v(q)**2 * tau75(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau75)

    allocate(tau82(NAO, NAO))
    !$omp single
    tau82 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau82(p, q) = tau82(p, q) + ( &
                v(q)**2 * tau74(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau76(NAO, NAO))
    !$omp single
    tau76 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau76(p, q) = tau76(p, q) + ( &
                h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W200(p, q) = W200(p, q) - ( &
                8*NAO**2 * u(p)**2 * v(p)**2 * u(q) * v(q) * tau76(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau76)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau77(p, q) = tau77(p, q) + ( &
                h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau78(NAO, NAO))
    !$omp single
    tau78 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau78(p, q) = tau78(p, q) + ( &
                v(q) * tau77(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau103(p, q) = tau103(p, q) + ( &
                u(q)**2 * tau77(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau78(p, q) = tau78(p, q) - ( &
                2 * v(q)**3 * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W200(p, q) = W200(p, q) + ( &
                2*NAO**2 * u(p)**2 * u(q) * tau78(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau78)

    allocate(tau79(NAO, NAO))
    !$omp single
    tau79 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau79(p, q) = tau79(p, q) + ( &
                    v(r)**2 * h021(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau80(NAO, NAO))
    !$omp single
    tau80 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau80(p, q) = tau80(p, q) + ( &
                4 * tau79(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau102(NAO, NAO))
    !$omp single
    tau102 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau102(p, q) = tau102(p, q) + ( &
                4 * tau79(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau79)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau80(p, q) = tau80(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau81(NAO, NAO))
    !$omp single
    tau81 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau81(p, q) = tau81(p, q) + ( &
                u(p) * tau80(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    W110 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) + ( &
                NAO**2 * v(p)**2 * v(q)**2 * tau80(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau80)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau81(p, q) = tau81(p, q) + ( &
                2 * u(p)**3 * h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W200(p, q) = W200(p, q) - ( &
                2*NAO**2 * v(q)**2 * v(p) * tau81(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau81)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau82(p, q) = tau82(p, q) + ( &
                u(q)**3 * v(q) * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W200(p, q) = W200(p, q) + ( &
                4*NAO**2 * u(p)**2 * tau82(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau82)

    allocate(tau83(NAO, NAO))
    !$omp single
    tau83 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau83(p, q) = tau83(p, q) + ( &
                h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau83(p, q) = tau83(p, q) + ( &
                h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau83(p, q) = tau83(p, q) + ( &
                2 * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau111(NAO, NAO))
    !$omp single
    tau111 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau111(p, q) = tau111(p, q) + ( &
                2 * v(p)**3 * tau83(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau84(NAO))
    !$omp single
    tau84 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau84(p) = tau84(p) - ( &
            v(p)**4&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau84(p) = tau84(p) + ( &
            3 * u(p)**2 * v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) - ( &
                4*NAO**2 * tau84(p) * u(q) * v(q) * tau83(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau83)

    deallocate(tau84)

    allocate(tau85(NAO))
    !$omp single
    tau85 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau85(p) = tau85(p) + ( &
            u(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau85(p) = tau85(p) - ( &
            v(p)**2&
        )
    
    end do
    !$omp end do

    allocate(tau86(NAO, NAO))
    !$omp single
    tau86 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau86(p, q) = tau86(p, q) + ( &
                v(p)**3 * tau85(q) * u(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau86(p, q) = tau86(p, q) - ( &
                u(p)**3 * tau85(q) * v(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) - ( &
                4*NAO**2 * tau86(q, p) * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) - ( &
                4*NAO**2 * tau86(p, q) * h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau86)

    allocate(tau87(NAO, NAO))
    !$omp single
    tau87 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau87(p, q) = tau87(p, q) - ( &
                    4 * tau85(r) * h021(q, p, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau96(NAO, NAO))
    !$omp single
    tau96 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau96(p, q) = tau96(p, q) - ( &
                u(q)**2 * v(q)**2 * tau85(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau96(p, q) = tau96(p, q) + ( &
                v(q)**4 * tau85(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) + ( &
                4*NAO**2 * tau96(q, p) * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau96)

    allocate(tau104(NAO, NAO))
    !$omp single
    tau104 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau104(p, q) = tau104(p, q) - ( &
                u(q)**2 * v(q)**2 * tau85(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau104(p, q) = tau104(p, q) + ( &
                v(q)**4 * tau85(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau117(NAO, NAO))
    !$omp single
    tau117 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau117(p, q) = tau117(p, q) + ( &
                v(q)**2 * tau85(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau118(NAO, NAO))
    !$omp single
    tau118 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau118(p, q) = tau118(p, q) - ( &
                u(q)**2 * tau85(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau119(NAO, NAO))
    !$omp single
    tau119 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau119(p, q) = tau119(p, q) - ( &
                u(p)**2 * tau85(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau120(NAO, NAO))
    !$omp single
    tau120 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau120(p, q) = tau120(p, q) + ( &
                v(q)**2 * tau85(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau121(NAO, NAO, NAO))
    !$omp single
    tau121 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau121(p, q, r) = tau121(p, q, r) + ( &
                    2 * tau85(q) * u(p) * u(r) * v(p) * v(r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau121(p, q, r) = tau121(p, q, r) + ( &
                    2 * tau85(p) * u(q) * u(r) * v(q) * v(r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau121(p, q, r) = tau121(p, q, r) + ( &
                    v(r)**2 * tau85(p) * tau85(q)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W021 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W021(p, q, r) = W021(p, q, r) - ( &
                    NAO**3 * h120(p, q, r) * tau121(r, q, p)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau121)

    allocate(tau123(NAO, NAO))
    !$omp single
    tau123 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau123(p, q) = tau123(p, q) - ( &
                tau85(p) * tau85(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau124(NAO, NAO))
    !$omp single
    tau124 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau124(p, q) = tau124(p, q) - ( &
                tau85(p) * tau85(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) + ( &
                2*NAO**2 * tau85(p) * u(q) * v(q) * tau77(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau77)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) + ( &
                4*NAO**2 * v(q)**2 * tau85(p) * tau74(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau74)

    !$omp single
    W011 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) + ( &
                4*NAO**2 * tau85(q) * u(p) * v(p) * h020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) - ( &
                NAO**2 * tau85(p) * u(q) * v(q) * h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    W120 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W120(p, q, r) = W120(p, q, r) - ( &
                    2*NAO**3 * tau85(r) * u(p) * u(q) * v(p) * v(q) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W120(p, q, r) = W120(p, q, r) + ( &
                    NAO**3 * u(p)**2 * tau85(q) * tau85(r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W012 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W012(p, q, r) = W012(p, q, r) - ( &
                    2*NAO**3 * v(p)**2 * tau85(q) * u(r) * v(r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W210 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W210(p, q, r) = W210(p, q, r) + ( &
                    2*NAO**3 * u(p)**2 * tau85(q) * u(r) * v(r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W021(p, q, r) = W021(p, q, r) - ( &
                    4*NAO**3 * tau85(q) * u(p) * u(r) * v(p) * v(r) * h021(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W111 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W111(p, q, r) = W111(p, q, r) + ( &
                    2*NAO**3 * tau85(p) * tau85(r) * u(q) * v(q) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W030 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W030(p, q, r) = W030(p, q, r) - ( &
                    NAO**3 * tau85(q) * tau85(r) * u(p) * v(p) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau87(p, q) = tau87(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) - ( &
                2*NAO**2 * v(q)**2 * u(p) * v(p) * tau87(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau87)

    allocate(tau88(NAO))
    !$omp single
    tau88 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau88(p) = tau88(p) - ( &
            u(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau88(p) = tau88(p) + ( &
            3 * v(p)**2&
        )
    
    end do
    !$omp end do

    allocate(tau89(NAO, NAO))
    !$omp single
    tau89 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau89(p, q) = tau89(p, q) + ( &
                    tau88(r) * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau90(NAO, NAO))
    !$omp single
    tau90 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau90(p, q) = tau90(p, q) + ( &
                tau89(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) - ( &
                NAO**2 * u(q)**2 * u(p) * v(p) * tau89(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau89)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau90(p, q) = tau90(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau90(p, q) = tau90(p, q) + ( &
                h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau92(NAO, NAO))
    !$omp single
    tau92 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau92(p, q) = tau92(p, q) - ( &
                v(p) * v(q) * tau90(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau106(NAO, NAO))
    !$omp single
    tau106 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau106(p, q) = tau106(p, q) - ( &
                v(p) * v(q) * tau90(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau90)

    allocate(tau91(NAO, NAO))
    !$omp single
    tau91 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau91(p, q) = tau91(p, q) + ( &
                h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau91(p, q) = tau91(p, q) + ( &
                h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau91(p, q) = tau91(p, q) + ( &
                h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau91(p, q) = tau91(p, q) + ( &
                2 * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau92(p, q) = tau92(p, q) + ( &
                2 * v(p)**3 * v(q) * tau91(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau91)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau92(p, q) = tau92(p, q) + ( &
                2 * v(q)**3 * v(p) * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) + ( &
                2*NAO**2 * u(p) * u(q) * tau92(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau92)

    allocate(tau93(NAO, NAO, NAO))
    !$omp single
    tau93 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau93(p, q, r) = tau93(p, q, r) + ( &
                    h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau93(p, q, r) = tau93(p, q, r) + ( &
                    2 * h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    allocate(tau94(NAO, NAO))
    !$omp single
    tau94 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau94(p, q) = tau94(p, q) + ( &
                    u(r) * v(r) * tau93(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    allocate(tau95(NAO, NAO))
    !$omp single
    tau95 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau95(p, q) = tau95(p, q) + ( &
                3 * tau94(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau97(NAO, NAO))
    !$omp single
    tau97 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau97(p, q) = tau97(p, q) + ( &
                tau94(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) - ( &
                2*NAO**2 * tau88(q) * u(p) * v(p) * tau94(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau88)

    deallocate(tau94)

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W120(p, q, r) = W120(p, q, r) - ( &
                    2*NAO**3 * tau85(q) * u(p) * u(r) * v(p) * v(r) * tau93(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau93)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau95(p, q) = tau95(p, q) + ( &
                2 * h020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) - ( &
                2*NAO**2 * v(q)**2 * u(p) * v(p) * tau95(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau95)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau97(p, q) = tau97(p, q) + ( &
                2 * h020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) + ( &
                2*NAO**2 * u(q)**2 * u(p) * v(p) * tau97(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau97)

    allocate(tau98(NAO, NAO))
    !$omp single
    tau98 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau98(p, q) = tau98(p, q) + ( &
                u(q)**3 * u(p) * v(p) * v(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau98(p, q) = tau98(p, q) + ( &
                u(p)**2 * u(q)**2 * v(q)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) - ( &
                4*NAO**2 * tau98(p, q) * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau98)

    allocate(tau99(NAO, NAO))
    !$omp single
    tau99 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau99(p, q) = tau99(p, q) + ( &
                h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau99(p, q) = tau99(p, q) + ( &
                h120(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau99(p, q) = tau99(p, q) + ( &
                h120(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau103(p, q) = tau103(p, q) + ( &
                4 * v(p)**2 * v(q)**2 * tau99(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau99)

    allocate(tau100(NAO))
    !$omp single
    tau100 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau100(p) = tau100(p) - ( &
            u(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau100(p) = tau100(p) + ( &
            2 * v(p)**2&
        )
    
    end do
    !$omp end do

    allocate(tau101(NAO, NAO))
    !$omp single
    tau101 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau101(p, q) = tau101(p, q) + ( &
                    tau100(r) * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau102(p, q) = tau102(p, q) + ( &
                2 * tau101(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) + ( &
                2 * tau101(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau101)

    !$omp single
    W201 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W201(p, q, r) = W201(p, q, r) - ( &
                    4*NAO**3 * tau100(r) * u(p) * u(q) * v(p) * v(q) * h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W102 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W102(p, q, r) = W102(p, q, r) - ( &
                    4*NAO**3 * tau100(p) * u(q) * u(r) * v(q) * v(r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau100)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau102(p, q) = tau102(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau102(p, q) = tau102(p, q) + ( &
                h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau103(p, q) = tau103(p, q) - ( &
                v(q)**2 * tau102(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau102)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) + ( &
                NAO**2 * u(p)**2 * tau103(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau103)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau104(p, q) = tau104(p, q) + ( &
                2 * v(q)**3 * u(p) * u(q) * v(p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) + ( &
                2*NAO**2 * tau104(q, p) * tau48(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau48)

    deallocate(tau104)

    allocate(tau105(NAO, NAO))
    !$omp single
    tau105 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau105(p, q) = tau105(p, q) + ( &
                h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau105(p, q) = tau105(p, q) + ( &
                2 * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau106(p, q) = tau106(p, q) + ( &
                2 * v(p)**3 * v(q) * tau105(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau108(p, q) = tau108(p, q) + ( &
                4 * u(p)**2 * tau105(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau105)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau106(p, q) = tau106(p, q) + ( &
                2 * v(q)**3 * v(p) * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) + ( &
                2*NAO**2 * u(p) * u(q) * tau106(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau106)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) + ( &
                h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) + ( &
                8 * u(p)**2 * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau107(p, q) = tau107(p, q) + ( &
                4 * u(q)**2 * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) + ( &
                NAO**2 * v(p)**2 * v(q)**2 * tau107(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau107)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau108(p, q) = tau108(p, q) + ( &
                h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau108(p, q) = tau108(p, q) + ( &
                h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) - ( &
                NAO**2 * u(q)**2 * v(p)**2 * tau108(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau108)

    allocate(tau109(NAO, NAO))
    !$omp single
    tau109 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau109(p, q) = tau109(p, q) + ( &
                u(p)**3 * u(q) * h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau109(p, q) = tau109(p, q) + ( &
                u(q)**3 * u(p) * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) - ( &
                4*NAO**2 * v(p) * v(q) * tau109(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau109)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau110(p, q) = tau110(p, q) + ( &
                u(q)**2 * h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W011(p, q) = W011(p, q) + ( &
                NAO**2 * u(p)**2 * tau110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau110)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau111(p, q) = tau111(p, q) - ( &
                v(p) * h110(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) + ( &
                NAO**2 * tau85(q) * u(p) * tau111(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau85)

    deallocate(tau111)

    allocate(tau112(NAO))
    !$omp single
    tau112 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau112(p) = tau112(p) + ( &
            5 * v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau112(p) = tau112(p) - ( &
            3 * u(p)**2&
        )
    
    end do
    !$omp end do

    allocate(tau113(NAO, NAO))
    !$omp single
    tau113 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau113(p, q) = tau113(p, q) + ( &
                    tau112(r) * h120(p, q, r)&
                )
            end do
        end do
    end do
    !$omp end do

    deallocate(tau112)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau113(p, q) = tau113(p, q) + ( &
                4 * u(q)**2 * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) + ( &
                NAO**2 * v(q)**2 * u(p) * v(p) * tau113(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau113)

    allocate(tau114(NAO))
    !$omp single
    tau114 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau114(p) = tau114(p) + ( &
            2 * u(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau114(p) = tau114(p) - ( &
            v(p)**2&
        )
    
    end do
    !$omp end do

    allocate(tau116(NAO, NAO))
    !$omp single
    tau116 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau116(p, q) = tau116(p, q) - ( &
                v(q)**2 * tau114(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau122(NAO, NAO))
    !$omp single
    tau122 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau122(p, q) = tau122(p, q) - ( &
                v(p)**2 * tau114(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) - ( &
                NAO**2 * v(q)**2 * tau114(p) * h020(q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W201(p, q, r) = W201(p, q, r) + ( &
                    4*NAO**3 * tau114(p) * u(q) * u(r) * v(q) * v(r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W102(p, q, r) = W102(p, q, r) + ( &
                    4*NAO**3 * tau114(r) * u(p) * u(q) * v(p) * v(q) * h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau114)

    allocate(tau115(NAO))
    !$omp single
    tau115 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau115(p) = tau115(p) + ( &
            6 * u(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau115(p) = tau115(p) - ( &
            5 * v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) - ( &
                NAO**2 * v(q)**2 * tau115(p) * tau61(q, p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau115)

    deallocate(tau61)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau116(p, q) = tau116(p, q) + ( &
                u(p)**2 * u(q)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W120(p, q, r) = W120(p, q, r) - ( &
                    NAO**3 * v(r)**2 * tau116(p, q) * h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W030(p, q, r) = W030(p, q, r) - ( &
                    NAO**3 * u(r) * v(r) * tau116(p, q) * h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau116)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau117(p, q) = tau117(p, q) + ( &
                2 * u(p) * u(q) * v(p) * v(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W012(p, q, r) = W012(p, q, r) - ( &
                    2*NAO**3 * u(q) * v(q) * tau117(r, p) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau117)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau118(p, q) = tau118(p, q) + ( &
                u(p) * u(q) * v(p) * v(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W012(p, q, r) = W012(p, q, r) - ( &
                    4*NAO**3 * u(p) * v(p) * tau118(q, r) * h021(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau118)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau119(p, q) = tau119(p, q) + ( &
                2 * u(p) * u(q) * v(p) * v(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W210(p, q, r) = W210(p, q, r) - ( &
                    2*NAO**3 * u(q) * v(q) * tau119(p, r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau119)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau120(p, q) = tau120(p, q) + ( &
                u(p) * u(q) * v(p) * v(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W210(p, q, r) = W210(p, q, r) - ( &
                    4*NAO**3 * u(p) * v(p) * tau120(q, r) * h021(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau120)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau122(p, q) = tau122(p, q) + ( &
                u(p)**2 * u(q)**2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W021(p, q, r) = W021(p, q, r) + ( &
                    NAO**3 * u(r)**2 * tau122(q, p) * h021(q, p, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau122)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau123(p, q) = tau123(p, q) + ( &
                2 * u(p) * u(q) * v(p) * v(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W111(p, q, r) = W111(p, q, r) - ( &
                    4*NAO**3 * u(p) * v(p) * tau123(q, r) * h021(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau123)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau124(p, q) = tau124(p, q) + ( &
                4 * u(p) * u(q) * v(p) * v(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W111(p, q, r) = W111(p, q, r) - ( &
                    2*NAO**3 * u(r) * v(r) * tau124(p, q) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau124)

    allocate(tau125(NAO, NAO, NAO))
    !$omp single
    tau125 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau125(p, q, r) = tau125(p, q, r) + ( &
                    u(r)**2 * u(p) * v(p) * h021(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau125(p, q, r) = tau125(p, q, r) - ( &
                    v(p)**2 * u(r) * v(r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W003 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W003(p, q, r) = W003(p, q, r) + ( &
                    4*NAO**3 * u(q) * v(q) * tau125(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau125)

    allocate(tau126(NAO, NAO, NAO))
    !$omp single
    tau126 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau126(p, q, r) = tau126(p, q, r) + ( &
                    v(r)**2 * u(p) * v(p) * h021(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                tau126(p, q, r) = tau126(p, q, r) - ( &
                    u(p)**2 * u(r) * v(r) * h120(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    !$omp single
    W300 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
    
                W300(p, q, r) = W300(p, q, r) - ( &
                    4*NAO**3 * u(q) * v(q) * tau126(p, q, r)&
                )
    
            end do
        end do
    end do
    !$omp end do

    deallocate(tau126)

    !$omp do schedule(static) reduction(+:W000)
    
    do p=1, NAO
        W000 = W000 + ( &
            u(p)**2 * h100(p)&
        )
    end do
    
    !$omp end do

    !$omp do schedule(static) reduction(+:W000)
    
    do p=1, NAO
        W000 = W000 + ( &
            4 * u(p)**3 * v(p) * h020(p, p)&
        )
    end do
    
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            NAO * u(p)**2 * h100(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            2*NAO * v(p)**4 * h011(p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W100(p) = W100(p) + ( &
            16*NAO * u(p)**2 * v(p)**4 * h120(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            NAO * v(p)**2 * h100(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        W001(p) = W001(p) - ( &
            8*NAO * u(p)**4 * v(p)**2 * h021(p, p, p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W002(p, q) = W002(p, q) + ( &
                2*NAO**2 * u(q)**2 * u(p) * v(p) * h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W002(p, q) = W002(p, q) + ( &
                4*NAO**2 * v(p)**2 * v(q)**3 * u(q) * h120(p, q, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W200(p, q) = W200(p, q) + ( &
                4*NAO**2 * v(p)**3 * v(q)**2 * u(p) * h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W200(p, q) = W200(p, q) + ( &
                8*NAO**2 * v(p)**4 * u(q) * v(q) * h021(p, q, p)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W101(p, q) = W101(p, q) + ( &
                2*NAO**2 * u(q)**2 * u(p) * v(p) * h011(p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W110(p, q) = W110(p, q) - ( &
                4*NAO**2 * u(p)**3 * u(q) * v(p) * v(q) * h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            W020(p, q) = W020(p, q) + ( &
                4*NAO**2 * u(p)**2 * v(p)**2 * u(q) * v(q) * h021(p, p, q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp end parallel
    
    !symmetrize W002 = PpPq
    do p = 1, NAO
    do q = p, NAO
       tmp = 0.5_pr * (W002(p,q) + (W002(q,p)))
       !tmp = 0.5_pr * (W002(p,q) + (conjg(W002(q,p)))
       W002(p,q) = tmp
       W002(q,p) = (tmp)
       W002(p,p) = (0.0_pr,0.0_pr)
    end do
    end do

    W200 = W002
    
    !symmetrize W210
    do r = 1, NAO
    do p = 1, NAO
       W210 (p,p,r) = (0.0_pr,0.0_pr)
    do q = p, NAO
      tmp = 0.5_pr * (W210(p,q,r) + W210(q,p,r))            ! real
      ! tmp = 0.5_pr * (H210(p,q,r) + conjg(H210(q,p,r)))   ! complex "Hermitian" in pq
      W210(p,q,r) = tmp
      W210(q,p,r) = tmp    ! or conjg(tmp) for the complex variant above
      !H210 (p,p,r) = 0.0_pr
    end do
    end do
    end do

    ! symmetrize W012

    do p = 1, NAO
    do q = 1, NAO
        W012(p,q,q) = (0.0_pr,0.0_pr)                  ! or (0.0_pr,0.0_pr) if complex
    do r = q+1, NAO
      tmp = 0.5_pr * (W012(p,q,r) + W012(p,r,q))
      W012(p,q,r) = tmp
      W012(p,r,q) = tmp
    end do
    end do
    end do

    !symmetrize W030

    do p = 1, NAO
    do q = p, NAO
    do r = q, NAO
      avg = ( W030(p,q,r) + W030(p,r,q) + W030(q,p,r) &
            + W030(q,r,p) + W030(r,p,q) + W030(r,q,p) ) / 6.0_pr

      W030(p,q,r) = avg
      W030(p,r,q) = avg
      W030(q,p,r) = avg
      W030(q,r,p) = avg
      W030(r,p,q) = avg
      W030(r,q,p) = avg
    end do
    end do
    end do
    
    ! symmetrize W021
    do r = 1, NAO
    do p = 1, NAO
    do q = p, NAO
      tmp = 0.5_pr * (W021(p,q,r) + W021(q,p,r))
      W021(p,q,r) = tmp
      W021(q,p,r) = tmp
    end do
    end do
    end do
    

    ! symmetrize W201
    do r = 1, NAO
    do p = 1, NAO
      W201(p,p,r) = (0.0_pr,0.0_pr)
    do q = p+1, NAO
      tmp = 0.5_pr * (W201(p,q,r) + W201(q,p,r))
      W201(p,q,r) = tmp
      W201(q,p,r) = tmp
    end do
    end do
    end do



    !Symmetrize W021
    do r = 1, NAO
    do p = 1, NAO
    do q = p, NAO
      tmp = 0.5_pr * (W021(p,q,r) + W021(q,p,r))
      W021(p,q,r) = tmp
      W021(q,p,r) = tmp
    end do
    end do
    end do


    
    !symmetrize W102
    do p = 1, NAO
    do q = 1, NAO
      W102(p,q,q) = (0.0_pr,0.0_pr)
    do r = q+1, NAO
      tmp = 0.5_pr * (W102(p,q,r) + W102(p,r,q))
      W102(p,q,r) = tmp
      W102(p,r,q) = tmp
    end do
    end do
    end do


    !symmetrize W300

    do p = 1, NAO
    do q = 1, NAO
    do r = 1, NAO
      if (p==q .or. q==r .or. p==r) then
        W300(p,q,r) = (0.0_pr,0.0_pr)
      end if
    end do
    end do
    end do

    do p = 1, NAO
    do q = p+1, NAO
    do r = q+1, NAO
      avg = ( W300(p,q,r) + W300(p,r,q) + W300(q,p,r) &
            + W300(q,r,p) + W300(r,p,q) + W300(r,q,p) ) / 6.0_pr
      W300(p,q,r) = avg
      W300(p,r,q) = avg
      W300(q,p,r) = avg
      W300(q,r,p) = avg
      W300(r,p,q) = avg
      W300(r,q,p) = avg
    end do
    end do
    end do

    !symmetrize W003

    W003 = W300





    Return
    
    End Subroutine
